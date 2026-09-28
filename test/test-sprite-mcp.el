;;; test-sprite-mcp.el --- ERT tests for sprite-mcp.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^sprite-mcp/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/external/sprite \
;;     -l ~/.emacs.d/external/sprite/test/test-sprite-mcp.el \
;;     --eval '(ert-run-tests-batch-and-exit "sprite-mcp/")'

;;; Commentary:
;;
;; Unit tests for sprite-mcp.el.  `sprite-create', `sprite-stop',
;; `sprite--call-and-read', and the mcpkit-proxy route functions are all
;; mocked via `cl-letf', so no real `emacs --daemon' subprocess, mcpkit
;; service, or mcpkit-proxy gateway is ever started.  A handful of tests
;; do shell out to the real `git' binary against a scratch temp
;; directory (worktree-add/remove error-path checks and the
;; worktree/branch naming test go through `sprite-mcp-spawn' with a
;; *mocked* `sprite-mcp--git-worktree-add'/`-remove', so only the direct
;; unit tests on those two functions touch git itself) -- that is a
;; local subprocess call, not a daemon, so it stays in the fast suite.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)

;; Same reasoning as test-sprite.el: server-use-tcp/server-auth-dir are
;; read dynamically by sprite.el's internals pulled in transitively.
(defvar server-use-tcp)
(defvar server-auth-dir)

(require 'sprite)
(require 'sprite-daemon)
(require 'sprite-mcp)

(setq sprite-instance-id "work")

;;;; Helpers

(defmacro sprite-mcp-test/with-mocks (&rest body)
  "Run BODY with all of sprite-mcp's external dependencies mocked out.
Binds and exposes the following accumulator variables to BODY, each a
list of records in most-recent-first order:
  `git-calls'             -- (:add REPO-ROOT WORKTREE BRANCH) / (:remove REPO-ROOT WORKTREE)
  `create-calls'          -- unique-name strings passed to `sprite-create'
  `stop-calls'            -- full-name strings passed to `sprite-stop'
  `call-and-read-calls'   -- (NAME FORM) passed to `sprite--call-and-read'
  `register-calls'        -- (TARGET-ID . PORT) passed to the proxy register fn
  `unregister-calls'      -- TARGET-ID strings passed to the proxy unregister fn
Also isolates `sprite-mcp--registry' and `sprite--registry' as fresh
per-test hash tables, stubs port allocation to a fixed value, and binds
`repo-root' to a real (but not a git repo -- `sprite-mcp--git-worktree-add'
is mocked, so nothing ever actually invokes git against it) scratch
temp directory, cleaned up afterwards."
  (declare (indent 0))
  `(let ((sprite-mcp--registry (make-hash-table :test #'equal))
         (sprite--registry (make-hash-table :test #'equal))
         (repo-root (make-temp-file "sprite-mcp-test-repo-" t))
         (git-calls nil)
         (create-calls nil)
         (stop-calls nil)
         (call-and-read-calls nil)
         (register-calls nil)
         (unregister-calls nil))
     (unwind-protect
         (cl-letf (((symbol-function 'sprite-mcp--git-worktree-add)
                    (lambda (repo-root worktree branch)
                      (push (list :add repo-root worktree branch) git-calls)
                      worktree))
                   ((symbol-function 'sprite-mcp--git-worktree-remove)
                    (lambda (repo-root worktree)
                      (push (list :remove repo-root worktree) git-calls)
                      t))
                   ((symbol-function 'sprite-mcp--allocate-port)
                    (lambda () 54321))
                   ((symbol-function 'sprite-create)
                    (lambda (id &rest _)
                      (push id create-calls)
                      (sprite--make :name (format "work.0.%s" id)
                                   :idx 0 :parent "work" :unique-name id)))
                   ((symbol-function 'sprite-stop)
                    (lambda (name) (push name stop-calls)))
                   ((symbol-function 'sprite--call-and-read)
                    (lambda (name form) (push (list name form) call-and-read-calls) t))
                   ((symbol-function 'mcpkit-proxy-register-route)
                    (lambda (id port) (push (cons id port) register-calls)))
                   ((symbol-function 'mcpkit-proxy-unregister-route)
                    (lambda (id) (push id unregister-calls))))
           ,@body)
       (delete-directory repo-root t))))

;;;; sprite-mcp--generate-id

(ert-deftest sprite-mcp/generate-id-differs-across-calls ()
  "Two ids generated for the same NAME never collide."
  (let ((a (sprite-mcp--generate-id "demo"))
        (b (sprite-mcp--generate-id "demo")))
    (should-not (equal a b))))

(ert-deftest sprite-mcp/generate-id-includes-name-suffix ()
  "The generated id ends with the given NAME, prefixed \"sp-\"."
  (let ((id (sprite-mcp--generate-id "demo")))
    (should (string-prefix-p "sp-" id))
    (should (string-suffix-p "-demo" id))))

;;;; sprite-mcp-spawn: validation

(ert-deftest sprite-mcp/spawn-requires-name ()
  (sprite-mcp-test/with-mocks
    (should-error (sprite-mcp-spawn :repo-root "/tmp") :type 'user-error)))

(ert-deftest sprite-mcp/spawn-requires-existing-repo-root ()
  (sprite-mcp-test/with-mocks
    (should-error (sprite-mcp-spawn :name "demo" :repo-root "/no/such/dir/xyz")
                  :type 'user-error)))

;;;; sprite-mcp-spawn: worktree / branch naming

(ert-deftest sprite-mcp/spawn-computes-worktree-path-and-branch ()
  "Worktree path is <repo-root>/.wt/<id>; branch is \"sprite-mcp/<id>\"."
  (sprite-mcp-test/with-mocks
    (let* ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root))
           (id (sprite-mcp-sprite-id entry)))
      (should (equal (expand-file-name id (expand-file-name ".wt" repo-root)) (sprite-mcp-worktree entry)))
      (should (equal (format "sprite-mcp/%s" id) (sprite-mcp-branch entry)))
      (should (equal (list :add repo-root (sprite-mcp-worktree entry) (sprite-mcp-branch entry))
                     (car (last git-calls)))))))

(ert-deftest sprite-mcp/spawn-uses-worktree-as-default-directory-in-remote-form ()
  "The remote form sets `default-directory' to the new worktree."
  (sprite-mcp-test/with-mocks
    (let* ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root))
           (form-str (format "%S" (nth 1 (car call-and-read-calls)))))
      (should (string-match-p (regexp-quote (sprite-mcp-worktree entry)) form-str)))))

;;;; sprite-mcp-spawn: proxy route registration

(ert-deftest sprite-mcp/spawn-registers-proxy-route-with-id-and-port ()
  (sprite-mcp-test/with-mocks
    (let ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
      (should (equal (list (cons (sprite-mcp-sprite-id entry) 54321)) register-calls))
      (should (= 54321 (sprite-mcp-port entry))))))

(ert-deftest sprite-mcp/spawn-sends-mcpkit-start-with-allocated-port ()
  (sprite-mcp-test/with-mocks
    (sprite-mcp-spawn :name "demo" :repo-root repo-root)
    (let ((form-str (format "%S" (nth 1 (car call-and-read-calls)))))
      (should (string-match-p "54321" form-str))
      (should (string-match-p "mcpkit-start-service" form-str))
      (should (string-match-p "mcpkit-define-service" form-str)))))

(ert-deftest sprite-mcp/spawn-registers-editing-tools-in-remote-form ()
  "The remote form requires mcpkit-editing and registers its tools on the
service after the service is defined and started."
  (sprite-mcp-test/with-mocks
    (sprite-mcp-spawn :name "demo" :repo-root repo-root)
    (let ((form-str (format "%S" (nth 1 (car call-and-read-calls)))))
      (should (string-match-p "(require 'mcpkit-editing)" form-str))
      (should (string-match-p "mcpkit-editing-register-tools" form-str))
      ;; Ordering: define/start must precede registering tools on the
      ;; service, since `mcpkit-register-tool' requires an already
      ;; `mcpkit-define-service'd service.
      (should (< (string-match "mcpkit-start-service" form-str)
                 (string-match "mcpkit-editing-register-tools" form-str))))))

(ert-deftest sprite-mcp/spawn-returns-struct-with-required-fields ()
  (sprite-mcp-test/with-mocks
    (let ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
      (should (sprite-mcp-p entry))
      (should (stringp (sprite-mcp-sprite-id entry)))
      (should (stringp (sprite-mcp-full-name entry)))
      (should (integerp (sprite-mcp-port entry)))
      (should (stringp (sprite-mcp-worktree entry)))
      (should (stringp (sprite-mcp-branch entry))))))

;;;; sprite-mcp-spawn: failure cleanup

(ert-deftest sprite-mcp/spawn-cleans-up-worktree-and-sprite-on-mcpkit-failure ()
  "A failed remote mcpkit start stops the sprite and removes the worktree."
  (sprite-mcp-test/with-mocks
    (cl-letf (((symbol-function 'sprite--call-and-read) (lambda (&rest _) nil)))
      (should-error (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
    (should (member :remove (mapcar #'car git-calls)))
    (should stop-calls)))

(ert-deftest sprite-mcp/spawn-does-not-register-route-on-mcpkit-failure ()
  (sprite-mcp-test/with-mocks
    (cl-letf (((symbol-function 'sprite--call-and-read) (lambda (&rest _) nil)))
      (should-error (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
    (should (null register-calls))))

(ert-deftest sprite-mcp/spawn-does-not-register-in-registry-on-failure ()
  (sprite-mcp-test/with-mocks
    (cl-letf (((symbol-function 'sprite--call-and-read) (lambda (&rest _) nil)))
      (should-error (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
    (should (= 0 (hash-table-count sprite-mcp--registry)))))

(ert-deftest sprite-mcp/spawn-propagates-git-worktree-add-failure ()
  "A `git worktree add' failure never reaches `sprite-create'."
  (sprite-mcp-test/with-mocks
    (cl-letf (((symbol-function 'sprite-mcp--git-worktree-add)
               (lambda (&rest _) (error "boom"))))
      (should-error (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
    (should (null create-calls))))

;;;; sprite-mcp--git-worktree-add / -remove: real git, no daemon

(ert-deftest sprite-mcp/git-worktree-add-signals-clear-error-on-failure ()
  "Adding a worktree in a non-git directory signals an error with git's output."
  (let ((tmpdir (make-temp-file "sprite-mcp-test-" t)))
    (unwind-protect
        (should-error
         (sprite-mcp--git-worktree-add tmpdir (expand-file-name "wt" tmpdir) "some-branch"))
      (delete-directory tmpdir t))))

(ert-deftest sprite-mcp/git-worktree-remove-signals-clear-error-on-failure ()
  "Removing a nonexistent worktree signals an error with git's output."
  (let ((tmpdir (make-temp-file "sprite-mcp-test-" t)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "init" "-q" tmpdir)
          (should-error
           (sprite-mcp--git-worktree-remove tmpdir (expand-file-name "no-such-worktree" tmpdir))))
      (delete-directory tmpdir t))))

;;;; sprite-mcp--allocate-port

(ert-deftest sprite-mcp/allocate-port-returns-positive-integer ()
  (let ((port (sprite-mcp--allocate-port)))
    (should (integerp port))
    (should (> port 0))))

;;;; sprite-mcp-kill

(ert-deftest sprite-mcp/kill-unregisters-route-and-stops-sprite ()
  (sprite-mcp-test/with-mocks
    (let ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
      (setq stop-calls nil unregister-calls nil git-calls nil)
      (sprite-mcp-kill entry)
      (should (equal (list (sprite-mcp-sprite-id entry)) unregister-calls))
      (should (equal (list (sprite-mcp-full-name entry)) stop-calls))
      (should (member :remove (mapcar #'car git-calls))))))

(ert-deftest sprite-mcp/kill-removes-registry-entry ()
  (sprite-mcp-test/with-mocks
    (let* ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root))
           (id (sprite-mcp-sprite-id entry)))
      (sprite-mcp-kill entry)
      (should (null (gethash id sprite-mcp--registry))))))

(ert-deftest sprite-mcp/kill-accepts-id-string ()
  (sprite-mcp-test/with-mocks
    (let* ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root))
           (id (sprite-mcp-sprite-id entry)))
      (sprite-mcp-kill id)
      (should (null (gethash id sprite-mcp--registry))))))

(ert-deftest sprite-mcp/kill-skips-worktree-removal-when-disabled ()
  (sprite-mcp-test/with-mocks
    (let ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
      (setq git-calls nil)
      (sprite-mcp-kill entry :cleanup-worktree nil)
      (should-not (member :remove (mapcar #'car git-calls))))))

(ert-deftest sprite-mcp/kill-unknown-id-signals-user-error ()
  (sprite-mcp-test/with-mocks
    (should-error (sprite-mcp-kill "nonexistent-id") :type 'user-error)))

(ert-deftest sprite-mcp/kill-tolerates-dead-remote-service ()
  "A sprite whose remote mcpkit-stop-service call fails is still fully torn down."
  (sprite-mcp-test/with-mocks
    (let ((entry (sprite-mcp-spawn :name "demo" :repo-root repo-root)))
      (cl-letf (((symbol-function 'sprite--call-and-read)
                 (lambda (&rest _) (error "connection refused"))))
        (sprite-mcp-kill entry))
      (should (equal (list (sprite-mcp-sprite-id entry)) unregister-calls))
      (should (null (gethash (sprite-mcp-sprite-id entry) sprite-mcp--registry))))))

;;;; Idle reaper: sprite-mcp--idle-p

(defun sprite-mcp-test--entry (id full-name)
  "Return a minimal `sprite-mcp' struct for ID/FULL-NAME for reaper tests."
  (sprite-mcp--make :sprite-id id :full-name full-name :port 1
                    :worktree "/tmp/wt" :branch "b" :repo-root "/tmp"))

(ert-deftest sprite-mcp/idle-p-true-when-last-contact-stale ()
  (let ((sprite--registry (make-hash-table :test #'equal))
        (sprite-mcp-idle-timeout 100))
    (let ((s (sprite--make :name "work.0.demo" :idx 0 :parent "work" :unique-name "demo")))
      (setf (sprite-last-contact s) (time-subtract (current-time) (seconds-to-time 500)))
      (puthash "work.0.demo" s sprite--registry))
    (should (sprite-mcp--idle-p (sprite-mcp-test--entry "demo" "work.0.demo")))))

(ert-deftest sprite-mcp/idle-p-false-when-last-contact-recent ()
  (let ((sprite--registry (make-hash-table :test #'equal))
        (sprite-mcp-idle-timeout 100))
    (let ((s (sprite--make :name "work.0.demo" :idx 0 :parent "work" :unique-name "demo")))
      (setf (sprite-last-contact s) (current-time))
      (puthash "work.0.demo" s sprite--registry))
    (should-not (sprite-mcp--idle-p (sprite-mcp-test--entry "demo" "work.0.demo")))))

(ert-deftest sprite-mcp/idle-p-true-when-no-last-contact-recorded ()
  (let ((sprite--registry (make-hash-table :test #'equal))
        (sprite-mcp-idle-timeout 100))
    (let ((s (sprite--make :name "work.0.demo" :idx 0 :parent "work" :unique-name "demo")))
      (puthash "work.0.demo" s sprite--registry))
    (should (sprite-mcp--idle-p (sprite-mcp-test--entry "demo" "work.0.demo")))))

(ert-deftest sprite-mcp/idle-p-true-when-sprite-missing-from-registry ()
  (let ((sprite--registry (make-hash-table :test #'equal)))
    (should (sprite-mcp--idle-p (sprite-mcp-test--entry "demo" "work.0.demo")))))

;;;; Idle reaper: sprite-mcp-reap-idle

(ert-deftest sprite-mcp/reap-idle-kills-only-stale-entries ()
  (let ((sprite--registry (make-hash-table :test #'equal))
        (sprite-mcp--registry (make-hash-table :test #'equal))
        (sprite-mcp-idle-timeout 100)
        (killed nil))
    (let ((stale-s (sprite--make :name "work.0.stale" :idx 0 :parent "work" :unique-name "stale"))
          (fresh-s (sprite--make :name "work.1.fresh" :idx 1 :parent "work" :unique-name "fresh")))
      (setf (sprite-last-contact stale-s) (time-subtract (current-time) (seconds-to-time 500)))
      (setf (sprite-last-contact fresh-s) (current-time))
      (puthash "work.0.stale" stale-s sprite--registry)
      (puthash "work.1.fresh" fresh-s sprite--registry))
    (puthash "stale" (sprite-mcp-test--entry "stale" "work.0.stale") sprite-mcp--registry)
    (puthash "fresh" (sprite-mcp-test--entry "fresh" "work.1.fresh") sprite-mcp--registry)
    (cl-letf (((symbol-function 'sprite-mcp-kill)
               (lambda (entry &rest _) (push (sprite-mcp-sprite-id entry) killed))))
      (sprite-mcp-reap-idle)
      (should (equal '("stale") killed)))))

(ert-deftest sprite-mcp/reap-idle-continues-after-individual-kill-failure ()
  "One sprite failing to die does not stop the sweep reaping the rest."
  (let ((sprite--registry (make-hash-table :test #'equal))
        (sprite-mcp--registry (make-hash-table :test #'equal))
        (sprite-mcp-idle-timeout 100)
        (killed nil))
    (let ((s1 (sprite--make :name "work.0.a" :idx 0 :parent "work" :unique-name "a"))
          (s2 (sprite--make :name "work.1.b" :idx 1 :parent "work" :unique-name "b")))
      (dolist (s (list s1 s2))
        (setf (sprite-last-contact s) (time-subtract (current-time) (seconds-to-time 500))))
      (puthash "work.0.a" s1 sprite--registry)
      (puthash "work.1.b" s2 sprite--registry))
    (puthash "a" (sprite-mcp-test--entry "a" "work.0.a") sprite-mcp--registry)
    (puthash "b" (sprite-mcp-test--entry "b" "work.1.b") sprite-mcp--registry)
    (cl-letf (((symbol-function 'sprite-mcp-kill)
               (lambda (entry &rest _)
                 (when (equal "a" (sprite-mcp-sprite-id entry))
                   (error "simulated failure"))
                 (push (sprite-mcp-sprite-id entry) killed))))
      (sprite-mcp-reap-idle)
      (should (equal '("b") killed)))))

;;;; Idle reaper: timer lifecycle

(ert-deftest sprite-mcp/start-idle-reaper-creates-timer ()
  (let ((sprite-mcp--idle-timer nil))
    (unwind-protect
        (progn
          (sprite-mcp-start-idle-reaper)
          (should (timerp sprite-mcp--idle-timer)))
      (sprite-mcp-stop-idle-reaper))))

(ert-deftest sprite-mcp/stop-idle-reaper-cancels-timer ()
  (let ((sprite-mcp--idle-timer nil))
    (sprite-mcp-start-idle-reaper)
    (sprite-mcp-stop-idle-reaper)
    (should (null sprite-mcp--idle-timer))))

(ert-deftest sprite-mcp/start-idle-reaper-replaces-existing-timer ()
  (let ((sprite-mcp--idle-timer nil))
    (unwind-protect
        (progn
          (sprite-mcp-start-idle-reaper)
          (let ((first sprite-mcp--idle-timer))
            (sprite-mcp-start-idle-reaper)
            (should-not (eq first sprite-mcp--idle-timer))))
      (sprite-mcp-stop-idle-reaper))))

(provide 'test-sprite-mcp)
;;; test-sprite-mcp.el ends here
