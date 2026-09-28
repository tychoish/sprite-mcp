;;; sprite-mcp.el --- Spawn sprite daemons hosting mcpkit MCP services -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (sprite "0.1.0"))
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
;; sprite-mcp.el spawns sprite daemons that each host a single mcpkit
;; MCP tool service, rooted in a dedicated `git worktree' so that
;; concurrent sprites never share (and stomp on) a working tree.
;;
;; `sprite-mcp-spawn' does the following:
;;   1. Creates a linked git worktree under "<repo-root>/.wt/<id>" on a
;;      fresh branch.
;;   2. Spawns a sprite daemon via `sprite-create' (see sprite.el).
;;   3. Allocates a free local TCP port and, inside the new daemon,
;;      loads mcpkit and starts an MCP service listening on it, rooted
;;      at the worktree.
;;   4. Loads mcpkit-editing (a sibling package) inside the new daemon
;;      and registers its core editing/inspection MCP tools on that
;;      same service.
;;   5. Registers a reverse-proxy route for the sprite's opaque id with
;;      mcpkit-proxy (see mcpkit-proxy.el, a sibling package), so the
;;      proxy gateway can forward MCP traffic to this sprite.
;;
;; `sprite-mcp-kill' undoes all of the above.  Sprites spawned through
;; this file are pooled rather than killed automatically after each
;; use; `sprite-mcp-start-idle-reaper' installs a repeating timer that
;; kills any of them whose `sprite-last-contact' (tracked by sprite.el
;; itself, via `with-sprite' and sprite-heartbeat.el) has gone stale
;; past `sprite-mcp-idle-timeout'.
;;
;; This file does not implement the session registry or the gptel
;; bridge -- those are separate, later rounds.  It only knows how to
;; stand up and tear down one MCP-hosting sprite at a time and reap
;; idle ones.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'sprite)
(require 'sprite-daemon)

;; mcpkit-proxy.el is an optional, sibling-repo dependency: only needed
;; on the parent side, to register/unregister the reverse-proxy route.
;; We do not require it at top level so this file byte-compiles cleanly
;; even when mcpkit-proxy is not on `load-path'; it is soft-required at
;; runtime, immediately before use.
(declare-function mcpkit-proxy-register-route "mcpkit-proxy" (target-id port))
(declare-function mcpkit-proxy-unregister-route "mcpkit-proxy" (target-id))

;;;; Customization

(defcustom sprite-mcp-idle-timeout 1800
  "Seconds since last-contact after which an idle sprite-mcp sprite is reaped.
\"Last contact\" is `sprite-last-contact', tracked by sprite.el itself."
  :type 'integer
  :group 'sprite)

(defcustom sprite-mcp-reap-interval 60
  "Seconds between sweeps of the sprite-mcp idle reaper."
  :type 'integer
  :group 'sprite)

;;;; Struct and registry

(cl-defstruct (sprite-mcp (:constructor sprite-mcp--make) (:copier nil))
  "Handle to a sprite daemon hosting a single mcpkit MCP service."
  sprite-id
  full-name
  port
  worktree
  branch
  repo-root)

(defvar sprite-mcp--registry (make-hash-table :test #'equal)
  "Maps sprite-mcp `sprite-id' strings to their `sprite-mcp' struct.")

;;;; Worktree helpers

(defun sprite-mcp--generate-id (name)
  "Return a collision-safe id for NAME, combining a timestamp and random suffix.
Used both as the sprite's unique-name and the worktree directory leaf,
so concurrent `sprite-mcp-spawn' calls (even for the same NAME) never
collide on either."
  (format "sp-%d-%04x-%s" (floor (float-time)) (random #x10000) name))

(defun sprite-mcp--run-git (repo-root &rest args)
  "Run git ARGS in REPO-ROOT; return (EXIT-CODE . OUTPUT-STRING)."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory repo-root))
           (code (apply #'call-process "git" nil t nil args)))
      (cons code (buffer-string)))))

(defun sprite-mcp--git-worktree-add (repo-root worktree-path branch-name)
  "Create a linked git worktree at WORKTREE-PATH on a new BRANCH-NAME.
Runs in REPO-ROOT.  Signals a clear `error' including git's output on
failure rather than swallowing it."
  (let ((result (sprite-mcp--run-git repo-root "worktree" "add" worktree-path
                                     "-b" branch-name)))
    (unless (= 0 (car result))
      (error "sprite-mcp: `git worktree add' failed (exit %s): %s"
             (car result) (string-trim (cdr result))))
    worktree-path))

(defun sprite-mcp--git-worktree-remove (repo-root worktree-path)
  "Remove the linked git worktree at WORKTREE-PATH, running in REPO-ROOT.
Signals a clear `error' including git's output on failure."
  (let ((result (sprite-mcp--run-git repo-root "worktree" "remove"
                                     worktree-path "--force")))
    (unless (= 0 (car result))
      (error "sprite-mcp: `git worktree remove' failed (exit %s): %s"
             (car result) (string-trim (cdr result))))
    t))

;;;; Port allocation

(defun sprite-mcp--allocate-port ()
  "Return a free local TCP port number.
Briefly binds an ephemeral server socket (asking the OS for an unused
port via `:service t'), reads back the bound port, then releases it.
As with any free-port probe, there is a small race between release and
the caller's actual bind; nothing else in this repository already
performs port allocation, so this is a minimal, local implementation."
  (let ((proc (make-network-process :name " sprite-mcp-port-probe"
                                    :server t :host 'local
                                    :service t :noquery t
                                    :family 'ipv4)))
    (unwind-protect
        (process-contact proc :service)
      (delete-process proc))))

;;;; Spawn / kill

;;;###autoload
(cl-defun sprite-mcp-spawn (&key name repo-root tools)
  "Spawn a sprite daemon hosting an mcpkit MCP service.
NAME is a short human-readable label combined with a collision-safe
suffix to form the sprite's unique-name, worktree directory leaf, and
branch name.  REPO-ROOT is the git repository to create a linked
worktree from.  TOOLS is accepted for forward-compatibility with a
later round that will let callers select which MCP tools get
registered on the service; it is currently unused, since all of
mcpkit-editing's tools are always registered.

Creates a linked git worktree, spawns the sprite daemon (via
`sprite-create'), starts an mcpkit MCP service inside it on a freshly
allocated port, registers mcpkit-editing's core editing/inspection
tools (`mcp_buffer_read', `mcp_buffer_replace_region', `mcp_ast_grep',
`mcp_project_diagnostics') on that service, and registers a
reverse-proxy route for the sprite's id with mcpkit-proxy.  Any
failure partway through cleans up whatever was already created and
re-signals the original error.

Returns a `sprite-mcp' struct."
  (ignore tools)
  (unless (and name (stringp name) (not (string-empty-p name)))
    (user-error "sprite-mcp-spawn: :name is required"))
  (unless (and repo-root (file-directory-p repo-root))
    (user-error "sprite-mcp-spawn: :repo-root must be an existing directory"))
  (let* ((id (sprite-mcp--generate-id name))
         (branch (format "sprite-mcp/%s" id))
         (worktree (expand-file-name
                    id (file-name-concat (file-name-as-directory repo-root) ".wt")))
         (service-sym (intern id))
         worktree-created
         sprite-spawned
         full-name)
    (condition-case err
        (progn
          (sprite-mcp--git-worktree-add repo-root worktree branch)
          (setq worktree-created t)
          (let ((s (sprite-create id)))
            (setq full-name (sprite-name s))
            (setq sprite-spawned t))
          (let ((port (sprite-mcp--allocate-port)))
            (unless (sprite--call-and-read
                     full-name
                     `(progn
                        (setq default-directory ,(file-name-as-directory worktree))
                        (require 'mcpkit)
                        (mcpkit-define-service ',service-sym
                          :port ,port
                          :description ,(format "sprite-mcp service for %s" id))
                        (mcpkit-start-service ',service-sym
                          :on-collision 'error :port ,port)
                        (require 'mcpkit-editing)
                        (mcpkit-editing-register-tools ',service-sym)
                        t))
              (error "sprite-mcp-spawn: failed to start mcpkit service in sprite %s"
                     full-name))
            (require 'mcpkit-proxy nil t)
            (unless (fboundp 'mcpkit-proxy-register-route)
              (error "sprite-mcp-spawn: mcpkit-proxy-register-route unavailable; \
is mcpkit-proxy.el on `load-path'?"))
            (mcpkit-proxy-register-route id port)
            (let ((entry (sprite-mcp--make :sprite-id id
                                           :full-name full-name
                                           :port port
                                           :worktree worktree
                                           :branch branch
                                           :repo-root repo-root)))
              (puthash id entry sprite-mcp--registry)
              entry)))
      (error
       (when sprite-spawned
         (ignore-errors (sprite-stop full-name)))
       (when worktree-created
         (ignore-errors (sprite-mcp--git-worktree-remove repo-root worktree)))
       (signal (car err) (cdr err))))))

;;;###autoload
(cl-defun sprite-mcp-kill (sprite-or-id &key (cleanup-worktree t))
  "Tear down an MCP-hosting sprite previously spawned by `sprite-mcp-spawn'.
SPRITE-OR-ID is either a `sprite-mcp' struct or its `sprite-id' string.

Stops the mcpkit service inside the sprite, unregisters its
mcpkit-proxy route, stops the sprite daemon, and, unless
CLEANUP-WORKTREE is nil, removes the linked git worktree and branch.
Failures stopping the (possibly already-dead) remote service or daemon
are logged and swallowed, since a stale registry entry pointing at a
dead sprite is exactly the case this function exists to clean up; a
failure to remove the git worktree is not swallowed, and propagates as
a clear `error'."
  (let* ((entry (if (sprite-mcp-p sprite-or-id)
                    sprite-or-id
                  (or (gethash sprite-or-id sprite-mcp--registry)
                      (user-error "sprite-mcp-kill: no registered sprite-mcp for id %s"
                                  sprite-or-id))))
         (id (sprite-mcp-sprite-id entry))
         (full-name (sprite-mcp-full-name entry))
         (service-sym (intern id)))
    (condition-case err
        (sprite--call-and-read
         full-name
         `(progn (require 'mcpkit) (mcpkit-stop-service ',service-sym) t))
      (error
       (message "sprite-mcp-kill: failed to stop mcpkit service in %s: %s"
                full-name (error-message-string err))))
    (require 'mcpkit-proxy nil t)
    (when (fboundp 'mcpkit-proxy-unregister-route)
      (mcpkit-proxy-unregister-route id))
    (condition-case err
        (sprite-stop full-name)
      (error
       (message "sprite-mcp-kill: failed to stop sprite daemon %s: %s"
                full-name (error-message-string err))))
    (when cleanup-worktree
      (sprite-mcp--git-worktree-remove (sprite-mcp-repo-root entry)
                                       (sprite-mcp-worktree entry)))
    (remhash id sprite-mcp--registry)
    id))

;;;; Idle reaper

(defun sprite-mcp--idle-p (entry)
  "Return non-nil when ENTRY's sprite is idle past `sprite-mcp-idle-timeout'.
Reuses `sprite-last-contact' from the sprite registry directly, rather
than tracking a second last-activity timestamp; a sprite with no
registry entry, or no recorded contact yet, counts as idle."
  (let ((s (sprite--registry-get (sprite-mcp-full-name entry))))
    (or (null s)
        (null (sprite-last-contact s))
        (> (float-time (time-since (sprite-last-contact s)))
           sprite-mcp-idle-timeout))))

;;;###autoload
(defun sprite-mcp-reap-idle ()
  "Kill every registered sprite-mcp sprite idle past `sprite-mcp-idle-timeout'.
Failures killing an individual sprite are logged and do not stop the
sweep from continuing on to the rest."
  (dolist (entry (hash-table-values sprite-mcp--registry))
    (when (sprite-mcp--idle-p entry)
      (condition-case err
          (sprite-mcp-kill entry)
        (error
         (message "sprite-mcp-reap-idle: failed to kill %s: %s"
                  (sprite-mcp-sprite-id entry) (error-message-string err)))))))

(defvar sprite-mcp--idle-timer nil
  "Timer that periodically runs `sprite-mcp-reap-idle'.")

;;;###autoload
(defun sprite-mcp-start-idle-reaper ()
  "Start a repeating timer that runs `sprite-mcp-reap-idle'.
Cancels any existing timer first."
  (sprite-mcp-stop-idle-reaper)
  (setq sprite-mcp--idle-timer
        (run-with-timer sprite-mcp-reap-interval sprite-mcp-reap-interval
                        #'sprite-mcp-reap-idle)))

;;;###autoload
(defun sprite-mcp-stop-idle-reaper ()
  "Cancel the sprite-mcp idle-reaper timer, if running."
  (when sprite-mcp--idle-timer
    (cancel-timer sprite-mcp--idle-timer)
    (setq sprite-mcp--idle-timer nil)))

(provide 'sprite-mcp)
;;; sprite-mcp.el ends here
