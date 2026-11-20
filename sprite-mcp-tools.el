;;; sprite-mcp-tools.el --- MCP tools for managing plain sprite daemons -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (sprite "0.1.0") (mcpkit "0.1.0"))
;; URL: https://github.com/tychoish/sprite-mcp
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Registers MCP tools, on an mcpkit service named `sprite', for driving
;; *plain* sprite daemons: ephemeral, isolated Emacs processes used to
;; run ERT suites or byte-compile checks per git worktree without
;; touching the user's main daemon.  This is distinct from
;; `sprite-mcp-spawn' (sprite-mcp.el), which additionally creates a git
;; worktree + branch and hosts its own MCP service inside the sprite.
;;
;; Tools:
;;   sprite_start            start a sprite (optionally rooted in a directory)
;;   sprite_eval             evaluate an expression in a sprite
;;   sprite_restart          restart a sprite, re-applying its directory setup
;;   sprite_stop             stop a sprite
;;   sprite_list             list running sprites
;;   sprite_cleanup_stale    report/remove stale state directories
;;
;; Teardown is always `sprite-stop', never `sprite-decommission'.
;; Decommissioning writes a permanent DECOMMISSIONED marker into the
;; sprite's state directory; because sprite indexes are reused once a
;; sprite is gone, a leftover marker later makes `sprite-create' refuse
;; the reused full name.  `sprite_cleanup_stale' removes such leftovers.
;;
;; Every tool returns a plist.  Success: (:ok t ...).  Failure:
;; (:ok nil :error MESSAGE).  Handlers never signal and never prompt.
;; The current Emacs instance and its parent are never stopped or
;; evaluated in by these tools.
;;
;; Evaluation in the user's main daemon (e.g. through the `emacs' MCP
;; service) is NOT isolated; use `sprite_eval' against a per-worktree
;; sprite for anything that loads code or runs tests.
;;
;; How values come back: `sprite_eval' wraps the form in a
;; `condition-case' that returns (:ok PRINTED-STRING) or (:error MSG)
;; over the sprite's server connection, so a nil result, a remote error
;; and a dead connection are all distinguishable.  (`with-sprite' and
;; `sprite-direct-eval-blocking' return plain nil for all three.)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sprite)
(require 'sprite-daemon)
(require 'mcpkit)

;;;; Customization and state

(defcustom sprite-mcp-tools-default-timeout 30
  "Default seconds for sprite start, restart and eval operations."
  :type 'integer
  :group 'sprite)

(defvar sprite-mcp-tools--directories (make-hash-table :test #'equal)
  "Map of sprite full name to the directory given at start/restart.
Used by `sprite_restart' to re-apply the setup of a blank restarted daemon.")

;; Set inside a sprite by the setup form.  emacsclient binds
;; `default-directory' to the *client's* cwd for every eval, so a plain
;; `setq' of it would not persist; `sprite-mcp-tools--remote-eval' rebinds
;; it from this variable instead.
(defvar sprite-mcp-tools-directory nil
  "Worktree directory a sprite was set up with; meaningful inside the sprite.")

(defvar sprite-mcp-tools--poll-interval 0.2
  "Seconds between readiness/stop polls.")

;;;; Result and error helpers

(defun sprite-mcp-tools--error (fmt &rest args)
  "Return a structured error result formatted from FMT and ARGS."
  (list :ok nil :error (apply #'format fmt args)))

(defmacro sprite-mcp-tools--guard (&rest body)
  "Run BODY; convert any signalled error into a structured error result."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (error (sprite-mcp-tools--error "%s" (error-message-string err)))))

(defun sprite-mcp-tools--timeout (args)
  "Return the positive numeric timeout in ARGS, or the default."
  (let ((v (plist-get args :timeout)))
    (if (and (numberp v) (> v 0)) v sprite-mcp-tools-default-timeout)))

(defun sprite-mcp-tools--string-arg (args key)
  "Return the non-empty string at KEY in ARGS, or signal `user-error'."
  (let ((v (plist-get args key)))
    (unless (and (stringp v) (not (string-empty-p v)))
      (user-error "Argument %s is required and must be a non-empty string"
                  (substring (symbol-name key) 1)))
    v))

;;;; Name resolution and self-protection

(defun sprite-mcp-tools--protected-names ()
  "Return the names of this instance and its parent, which must be left alone."
  (let* ((own (sprite-instance-name))
         (parts (sprite--parse-full-name own)))
    (delq nil (list own
                    (car parts)
                    (bound-and-true-p server-name)))))

(defun sprite-mcp-tools--resolve (name)
  "Resolve NAME (full or unique) to a full sprite name; refuse protected ones.
Signals `user-error' when NAME is unknown or names this Emacs or its parent."
  (when (member name (sprite-mcp-tools--protected-names))
    (user-error "Refusing to operate on the current Emacs instance or its parent: %s"
                name))
  (sprite--discover-and-sync-registry)
  (let ((full (sprite--resolve-name name)))
    (when (member full (sprite-mcp-tools--protected-names))
      (user-error "Refusing to operate on the current Emacs instance or its parent: %s"
                  full))
    full))

;;;; Remote evaluation

(defun sprite-mcp-tools--call (full-name form timeout)
  "Evaluate FORM in sprite FULL-NAME; return the read value, or nil on failure.
TIMEOUT is in seconds.  Uses the direct backend when
`sprite-communication-backend' is `direct', else emacsclient with `-w'
so that an unresponsive sprite cannot hang the caller indefinitely.
This is the single function the unit tests mock."
  (if (eq sprite-communication-backend 'direct)
      (let ((sprite-direct-blocking-timeout timeout))
        (sprite--call-and-read full-name form))
    (with-temp-buffer
      (when (eql 0 (apply #'call-process "emacsclient" nil (current-buffer) nil
                          (append (sprite--emacsclient-address-args full-name)
                                  (list "-w" (number-to-string (ceiling timeout))
                                        "--eval" (format "%S" form)))))
        (condition-case nil (read (buffer-string)) (error nil))))))

(defun sprite-mcp-tools--remote-eval (full-name form timeout)
  "Evaluate FORM in FULL-NAME, distinguishing nil results from failures.
FORM runs with `default-directory' bound to the sprite's configured
directory, if any.  Returns (:ok PRINTED) on success or (:error MESSAGE)
otherwise."
  (let* ((wrapped `(condition-case err
                       (list :ok (prin1-to-string
                  (let ((default-directory
                         (or (bound-and-true-p sprite-mcp-tools-directory)
                             default-directory)))
                    ,form)))
                     (error (list :error (error-message-string err)))))
         (res (condition-case err
                  (sprite-mcp-tools--call full-name wrapped timeout)
                (error (list :error (error-message-string err))))))
    (cond
     ((and (consp res) (memq (car res) '(:ok :error))) res)
     (t (list :error (format "No response from %s (not running, or timed out after %ss)"
                             full-name timeout))))))

(defun sprite-mcp-tools--ping (full-name timeout)
  "Return non-nil when sprite FULL-NAME answers within TIMEOUT seconds."
  (eq :ok (car (sprite-mcp-tools--remote-eval full-name t timeout))))

(defun sprite-mcp-tools--wait-until (pred timeout)
  "Poll PRED until non-nil or TIMEOUT seconds pass; return its last value."
  (let ((deadline (+ (float-time) timeout))
        (val (funcall pred)))
    (while (and (not val) (< (float-time) deadline))
      (sleep-for sprite-mcp-tools--poll-interval)
      (setq val (funcall pred)))
    val))

(defun sprite-mcp-tools--wait-ready (full-name timeout)
  "Poll until sprite FULL-NAME answers a ping, up to TIMEOUT seconds."
  (sprite-mcp-tools--wait-until
   (lambda () (sprite-mcp-tools--ping full-name 2))
   timeout))

(defun sprite-mcp-tools--wait-stopped (full-name timeout)
  "Poll until sprite FULL-NAME is no longer running, up to TIMEOUT seconds."
  (sprite-mcp-tools--wait-until
   (lambda () (not (sprite--running-p full-name)))
   timeout))

;;;; Directory setup

(defun sprite-mcp-tools--setup-form (directory)
  "Return the form that applies DIRECTORY (expanded) to a blank sprite.
Sets `default-directory' (globally and in non-file buffers) and prepends
DIRECTORY, plus its \"lisp\" subdirectory when present, to `load-path'."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (lisp (expand-file-name "lisp" dir))
         (paths (if (file-directory-p lisp) (list dir lisp) (list dir))))
    `(progn
       ;; `default-directory' is buffer-local: set the default for new
       ;; buffers and every existing non-file buffer (the buffer current
       ;; during a server eval is typically *scratch*).
       (set 'sprite-mcp-tools-directory ,(file-name-as-directory dir))
       (setq-default default-directory ,(file-name-as-directory dir))
       (dolist (b (buffer-list))
         (with-current-buffer b
           (unless buffer-file-name
             (setq default-directory ,(file-name-as-directory dir)))))
       (let ((new ',paths))
         (setq load-path (append new (seq-remove (lambda (p) (member p new))
                                                 load-path))))
       (list sprite-mcp-tools-directory (car load-path)))))

(defun sprite-mcp-tools--apply-directory (full-name directory timeout)
  "Apply DIRECTORY setup to FULL-NAME; return nil on success or an error string."
  (let ((res (sprite-mcp-tools--remote-eval
              full-name (sprite-mcp-tools--setup-form directory) timeout)))
    (if (eq :ok (car res))
        (progn (puthash full-name (expand-file-name directory)
                        sprite-mcp-tools--directories)
               nil)
      (cadr res))))

(defun sprite-mcp-tools--check-directory (directory)
  "Signal `user-error' unless DIRECTORY is a string naming an existing directory."
  (unless (and (stringp directory) (file-directory-p directory))
    (user-error "directory does not exist: %s" directory)))

(defun sprite-mcp-tools--pid (full-name)
  "Return the real Emacs pid of FULL-NAME, falling back to the launcher pid."
  (let ((res (sprite-mcp-tools--remote-eval full-name '(emacs-pid) 3)))
    (or (and (eq :ok (car res)) (ignore-errors (read (cadr res))))
        (when-let* ((s (sprite--registry-get full-name))) (sprite-pid s)))))

;;;; Tool handlers

(defun sprite-mcp-tools--start (args)
  "Handle `sprite_start' with ARGS."
  (sprite-mcp-tools--guard
    (let* ((name (sprite-mcp-tools--string-arg args :name))
           (dir (plist-get args :directory))
           (timeout (sprite-mcp-tools--timeout args)))
      (when (or (string-match-p "\\." name) (string-match-p "[[:space:]/]" name))
        (user-error "name must be a short name without dots, spaces or slashes: %s" name))
      (when dir (sprite-mcp-tools--check-directory dir))
      (when (member name (sprite-mcp-tools--protected-names))
        (user-error "Refusing to use the name of the current instance or its parent: %s"
                    name))
      (let* ((s (sprite-create name :timeout timeout))
             (full (sprite-name s))
             (dir-err (when dir (sprite-mcp-tools--apply-directory full dir timeout))))
        (if dir-err
            (list :ok nil :name full
                  :error (format "Sprite started but directory setup failed: %s" dir-err))
          (list :ok t
                :name full
                :unique_name (sprite-unique-name s)
                :pid (sprite-mcp-tools--pid full)
                :directory (let ((r (sprite-mcp-tools--remote-eval
                                     full 'default-directory 3)))
                             (and (eq :ok (car r)) (ignore-errors (read (cadr r)))))))))))

(defun sprite-mcp-tools--eval (args)
  "Handle `sprite_eval' with ARGS."
  (sprite-mcp-tools--guard
    (let* ((name (sprite-mcp-tools--string-arg args :name))
           (expr (sprite-mcp-tools--string-arg args :expression))
           (timeout (sprite-mcp-tools--timeout args))
           (full (sprite-mcp-tools--resolve name))
           (form (condition-case err
                     (read (concat "(progn " expr "\n)"))
                   (error (user-error "Cannot read expression: %s"
                                      (error-message-string err)))))
           (res (sprite-mcp-tools--remote-eval full form timeout)))
      (if (eq :ok (car res))
          (list :ok t :name full :result (cadr res))
        (list :ok nil :name full :error (cadr res))))))

(defun sprite-mcp-tools--restart (args)
  "Handle `sprite_restart' with ARGS."
  (sprite-mcp-tools--guard
    (let* ((name (sprite-mcp-tools--string-arg args :name))
           (timeout (sprite-mcp-tools--timeout args))
           (full (sprite-mcp-tools--resolve name))
           (dir (or (plist-get args :directory)
                    (gethash full sprite-mcp-tools--directories))))
      (when dir (sprite-mcp-tools--check-directory dir))
      (when (sprite--decommissioned-p full)
        (user-error "Sprite %s is decommissioned; run sprite_cleanup_stale" full))
      (when (sprite--running-p full)
        (sprite-stop full)
        (unless (sprite-mcp-tools--wait-stopped full 10)
          (user-error "Sprite %s did not stop within 10s" full)))
      (sprite-restart full)
      (unless (sprite-mcp-tools--wait-ready full timeout)
        (user-error "Sprite %s did not answer within %ss after restart" full timeout))
      (let ((dir-err (when dir (sprite-mcp-tools--apply-directory full dir timeout))))
        (if dir-err
            (list :ok nil :name full
                  :error (format "Sprite restarted but directory setup failed: %s"
                                 dir-err))
          (list :ok t
                :name full
                :pid (sprite-mcp-tools--pid full)
                :directory (and dir (file-name-as-directory (expand-file-name dir)))))))))

(defun sprite-mcp-tools--stop (args)
  "Handle `sprite_stop' with ARGS.  Never decommissions."
  (sprite-mcp-tools--guard
    (let* ((name (sprite-mcp-tools--string-arg args :name))
           (full (sprite-mcp-tools--resolve name)))
      (when (sprite--running-p full)
        (sprite-stop full))
      (if (sprite-mcp-tools--wait-stopped full 10)
          (progn (remhash full sprite-mcp-tools--directories)
                 (list :ok t :name full :running nil))
        (list :ok nil :name full :running t
              :error (format "Sprite %s is still running after 10s" full))))))

(defun sprite-mcp-tools--list-entry (s)
  "Return a result plist describing running sprite struct S."
  (let* ((full (sprite-name s))
         (info (sprite-mcp-tools--remote-eval
                full '(list (emacs-pid) default-directory
                            (float-time (time-since before-init-time)))
                3))
         (vals (and (eq :ok (car info)) (ignore-errors (read (cadr info))))))
    (list :name full
          :unique_name (sprite-unique-name s)
          :pid (or (nth 0 vals) (sprite-pid s))
          :uptime_seconds (cond ((nth 2 vals) (round (nth 2 vals)))
                                ((sprite-start-time s)
                                 (round (float-time (time-since (sprite-start-time s)))))
                                (t nil))
          :directory (nth 1 vals))))

(defun sprite-mcp-tools--list (_args)
  "Handle `sprite_list'."
  (sprite-mcp-tools--guard
    (sprite--discover-and-sync-registry)
    (let ((entries
           (seq-map (lambda (s)
                      (condition-case err
                          (sprite-mcp-tools--list-entry s)
                        (error (list :name (sprite-name s)
                                     :unique_name (sprite-unique-name s)
                                     :error (error-message-string err)))))
                    (seq-remove (lambda (s)
                                  (member (sprite-name s) (sprite-mcp-tools--protected-names)))
                                (sprite--running-list)))))
      (list :ok t :count (length entries) :sprites (vconcat entries)))))

(defun sprite-mcp-tools--stale-reason (full)
  "Return a reason string when state dir of FULL holds nothing but staleness.
That is: no files, or only a DECOMMISSIONED marker.  Otherwise nil."
  (let* ((dir (sprite-state-directory :full-name full))
         (files (and (file-directory-p dir)
                     (not (file-symlink-p (directory-file-name dir)))
                     (directory-files dir nil directory-files-no-dot-files-regexp))))
    (cond
     ((not (file-directory-p dir)) nil)
     ((file-symlink-p (directory-file-name dir)) nil)
     ((null files) "empty")
     ((equal files '("DECOMMISSIONED")) "decommissioned-marker-only"))))

(defun sprite-mcp-tools--cleanup-stale (args)
  "Handle `sprite_cleanup_stale' with ARGS.
Dry-run unless :dry_run is explicitly false."
  (sprite-mcp-tools--guard
    (let* ((dry-run (or (not (plist-member args :dry_run))
                        (not (memq (plist-get args :dry_run) '(nil :json-false)))))
           found removed)
      (dolist (full (sprite--discover-state-dirs))
        (unless (or (member full (sprite-mcp-tools--protected-names))
                    (sprite--running-p full))
          (when-let* ((reason (sprite-mcp-tools--stale-reason full)))
            (let* ((dir (sprite-state-directory :full-name full))
                   (entry (list :name full :directory (expand-file-name dir)
                         :reason reason)))
              (push entry found)
              (unless dry-run
                (condition-case err
                    (progn
                      (when (equal reason "decommissioned-marker-only")
                        (delete-file (expand-file-name "DECOMMISSIONED" dir)))
                      (delete-directory dir)
                      (sprite--registry-remove full)
                      (remhash full sprite-mcp-tools--directories)
                      (push entry removed))
                  (error (setf (plist-get (car found) :error)
                               (error-message-string err)))))))))
      (list :ok t
            :dry_run (if dry-run t :json-false)
            :found (vconcat (nreverse found))
            :removed (vconcat (nreverse removed))))))

;;;; Service and tool registration

(defvar sprite-mcp-tools-service
  (or (mcpkit-get-service 'sprite)
      (mcpkit-define-service 'sprite
        :description "Ephemeral sprite Emacs daemons for isolated testing"))
  "The sprite `mcpkit-service' instance.")

(mcpkit-register-autostart 'sprite :on-collision 'namespace)

(mcpkit-register-tool 'sprite_start 'sprite
  :description "Start an ephemeral, isolated Emacs daemon (a sprite) for per-git-worktree ERT or byte-compile checks.  Use this instead of evaluating in the user's main daemon, which is NOT isolated.  If `directory' is given, the sprite's default-directory is set to it and it (plus <directory>/lisp when present) is prepended to load-path.  Returns the full name (use it, or the short name, with the other sprite_* tools), pid and directory.  Stop it with sprite_stop when done."
  :input-schema '(:type "object"
                  :properties (:name (:type "string" :description "Short unique name for the sprite (no dots), e.g. wt-foo")
                               :directory (:type "string" :description "Optional worktree directory to set as default-directory and add to load-path")
                               :timeout (:type "number" :description "Seconds to wait for the daemon to become ready (default 30)"))
                  :required ["name"])
  (sprite-mcp-tools--start args))

(mcpkit-register-tool 'sprite_eval 'sprite
  :description "Evaluate Emacs Lisp (one or more forms; the last value is returned, printed) inside a sprite started with sprite_start, e.g. to run ERT tests or byte-compile files in isolation.  `name' may be the full name or the short name.  Returns the printed result or the error text; a nil result is distinguishable from a failure.  Evaluating in the user's main daemon instead is NOT isolated.  Refuses the current Emacs instance and its parent."
  :input-schema '(:type "object"
                  :properties (:name (:type "string" :description "Full sprite name (e.g. work.6.wt-foo) or its short unique name")
                               :expression (:type "string" :description "Emacs Lisp to evaluate; may contain several forms")
                               :timeout (:type "number" :description "Seconds to wait for the result (default 30)"))
                  :required ["name" "expression"])
  (sprite-mcp-tools--eval args))

(mcpkit-register-tool 'sprite_restart 'sprite
  :description "Restart a sprite under the same name, for a clean Emacs state (a restarted daemon is blank).  The directory/load-path setup is re-applied from `directory', or from the directory the sprite was last started/restarted with."
  :input-schema '(:type "object"
                  :properties (:name (:type "string" :description "Full or short sprite name")
                               :directory (:type "string" :description "Optional directory to re-apply (defaults to the one used at start)")
                               :timeout (:type "number" :description "Seconds to wait for readiness (default 30)"))
                  :required ["name"])
  (sprite-mcp-tools--restart args))

(mcpkit-register-tool 'sprite_stop 'sprite
  :description "Stop a sprite (kill its Emacs daemon) and confirm it is no longer running.  This never decommissions: decommissioning leaves a permanent marker that later breaks reuse of the same sprite index.  Use this for ephemeral teardown.  Refuses the current Emacs instance and its parent."
  :input-schema '(:type "object"
                  :properties (:name (:type "string" :description "Full or short sprite name"))
                  :required ["name"])
  (sprite-mcp-tools--stop args))

(mcpkit-register-tool 'sprite_list 'sprite
  :description "List running sprites with full name, unique name, pid, uptime in seconds and default-directory (directory/uptime come from querying each sprite and may be null if it does not answer)."
  :input-schema '(:type "object" :properties ())
  (sprite-mcp-tools--list args))

(mcpkit-register-tool 'sprite_cleanup_stale 'sprite
  :description "Find, and optionally delete, stale sprite state directories: ones for sprites that are not running and contain nothing, or only a DECOMMISSIONED marker (which otherwise makes a reused sprite index fail to start).  Dry run by default; set dry_run to false to delete.  Never touches a running sprite."
  :input-schema '(:type "object"
                  :properties (:dry_run (:type "boolean" :description "Report only (default true); false deletes"))
                  :required [])
  (sprite-mcp-tools--cleanup-stale args))

(provide 'sprite-mcp-tools)
;;; sprite-mcp-tools.el ends here
