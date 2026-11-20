;;; test-sprite-mcp-tools.el --- ERT tests for sprite-mcp-tools.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Batch run (from the sprite-mcp directory):
;;   emacs --batch -Q -L . -L ../sprite -l ert -l test/test-sprite-mcp-tools.el \
;;     --eval '(ert-run-tests-batch-and-exit "sprite-mcp-tools/")'
;;
;; The real create/eval/restart/stop cycle runs only with
;; SPRITE_MCP_INTEGRATION=1 (see the last test).

;;; Commentary:
;;
;; Unit tests mock `sprite-create', `sprite-stop', `sprite-restart',
;; `sprite--running-p' and the single transport function
;; `sprite-mcp-tools--call' (which here evaluates the wrapped form in
;; the test Emacs, standing in for the remote sprite).  No daemon is
;; spawned and the user's Emacs is never contacted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar server-use-tcp)
(defvar server-auth-dir)

(require 'sprite)
(require 'sprite-daemon)
(require 'sprite-mcp-tools)

(defun sprite-mcp-tools-test/eval-isolated (form)
  "Evaluate remote FORM without changing the test runner's buffers."
  (let ((original-default (default-value 'default-directory)))
    (unwind-protect
        (with-temp-buffer
          (let ((buffer (current-buffer)))
            (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (list buffer))))
              (eval form t))))
      (set-default 'default-directory original-default))))

(defmacro sprite-mcp-tools-test/with-env (&rest body)
  "Run BODY with a hermetic sprite state dir and mocked sprite lifecycle.
Binds `services' (alist of full name to MCP port started), `running' (list of running full names), `calls' (list of
\(:create|:stop|:restart NAME) in most-recent-first order) and
`root' (the temp `user-emacs-directory')."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sprite-mcp-tools-" t)))
          (user-emacs-directory root)
          (sprite-instance-id "work")
          (sprite--registry (make-hash-table :test #'equal))
          (sprite-mcp-tools--directories (make-hash-table :test #'equal))
          (sprite-mcp-tools--poll-interval 0.01)
          (default-directory default-directory)
          (sprite-mcp-tools-directory nil)
          (load-path load-path)
          (running nil)
          (calls nil)
          (services nil))
     (unwind-protect
         (cl-letf (((symbol-function 'sprite-create)
                    (lambda (unique-name &rest _)
                      (let* ((idx (sprite--next-idx "work"))
                             (full (sprite--format-full-name "work" idx unique-name)))
                        (sprite--ensure-state-dir full)
                        (push (list :create full) calls)
                        (push full running)
                        (let ((s (sprite--make :name full :idx idx :parent "work"
                                               :unique-name unique-name :pid 111)))
                          (sprite--registry-put s)
                          s))))
                   ((symbol-function 'sprite-stop)
                    (lambda (name)
                      (push (list :stop name) calls)
                      (setq running (delete name running))))
                   ((symbol-function 'sprite-restart)
                    (lambda (name)
                      (push (list :restart name) calls)
                      (cl-pushnew name running :test #'equal)))
                   ((symbol-function 'sprite--running-p)
                    (lambda (name) (and (member name running) t)))
                   ((symbol-function 'sprite-mcp-tools--start-service)
                    (lambda (full port _timeout)
                      (and (member full running)
                           (push (cons full port) services)
                           t)))
                   ((symbol-function 'sprite-mcp-tools--call)
                    (lambda (full form _timeout)
                      (when (member full running)
                        ;; Stand-in for the remote sprite: evaluate locally
                        ;; (state is isolated by the outer let).
                        (sprite-mcp-tools-test/eval-isolated form)))))
           ,@body)
       (delete-directory root t))))

(defun sprite-mcp-tools-test/make-dir (full &rest files)
  "Create the state dir for FULL containing FILES; return its path."
  (let ((dir (sprite-state-directory :full-name full)))
    (make-directory dir t)
    (dolist (f files) (write-region "" nil (expand-file-name f dir)))
    dir))

;;;; Registration

(ert-deftest sprite-mcp-tools/registers-all-tools-on-sprite-service ()
  (let ((svc (mcpkit-get-service 'sprite)))
    (should svc)
    (dolist (n '("sprite_start" "sprite_eval" "sprite_restart"
                 "sprite_stop" "sprite_list" "sprite_cleanup_stale"))
      (should (gethash n (mcpkit-service-tools svc))))))

(ert-deftest sprite-mcp-tools/descriptions-warn-about-isolation-and-decommission ()
  (let ((tools (mcpkit-service-tools (mcpkit-get-service 'sprite))))
    (should (string-match-p "NOT isolated"
                            (mcpkit-tool-description (gethash "sprite_eval" tools))))
    (should (string-match-p "never decommissions"
                            (mcpkit-tool-description (gethash "sprite_stop" tools))))))

;;;; sprite_start

(ert-deftest sprite-mcp-tools/start-returns-name-pid-directory ()
  (sprite-mcp-tools-test/with-env
    (let* ((dir (make-temp-file "wt-" t))
           (res (sprite-mcp-tools--start (list :name "wt-foo" :directory dir))))
      (unwind-protect
          (progn
            (should (eq t (plist-get res :ok)))
            (should (equal "work.0.wt-foo" (plist-get res :name)))
            (should (equal (file-name-as-directory dir) (plist-get res :directory)))
            (should (member '(:create "work.0.wt-foo") calls)))
        (delete-directory dir t)))))

(ert-deftest sprite-mcp-tools/start-setup-form-sets-load-path ()
  (let* ((dir (make-temp-file "wt-" t))
         (lisp (expand-file-name "lisp" dir)))
    (unwind-protect
        (progn
          (make-directory lisp)
          (let ((load-path '("/x")) (default-directory "/") (sprite-mcp-tools-directory nil))
            (let ((result (sprite-mcp-tools-test/eval-isolated
                           `(progn ,(sprite-mcp-tools--setup-form dir)
                                   (list default-directory load-path)))))
              (should (equal (file-name-as-directory dir) (car result)))
              (should (equal (list (directory-file-name dir) lisp "/x")
                             (cadr result))))))
      (delete-directory dir t))))

(ert-deftest sprite-mcp-tools/start-setup-form-skips-missing-lisp-dir ()
  (let ((dir (make-temp-file "wt-" t)))
    (unwind-protect
        (let ((load-path '("/x")) (default-directory "/") (sprite-mcp-tools-directory nil))
          (let ((result (sprite-mcp-tools-test/eval-isolated
                         `(progn ,(sprite-mcp-tools--setup-form dir) load-path))))
            (should (equal (list (directory-file-name dir) "/x") result))))
      (delete-directory dir t))))

(ert-deftest sprite-mcp-tools/start-rejects-bad-input-without-signalling ()
  (sprite-mcp-tools-test/with-env
    (should-not (plist-get (sprite-mcp-tools--start nil) :ok))
    (should-not (plist-get (sprite-mcp-tools--start '(:name "a.b")) :ok))
    (let ((res (sprite-mcp-tools--start '(:name "ok" :directory "/no/such/dir/x"))))
      (should-not (plist-get res :ok))
      (should (string-match-p "does not exist" (plist-get res :error))))
    (should-not calls)))

(ert-deftest sprite-mcp-tools/start-refuses-own-instance-name ()
  (sprite-mcp-tools-test/with-env
    (let ((res (sprite-mcp-tools--start '(:name "work"))))
      (should-not (plist-get res :ok))
      (should (string-match-p "Refusing" (plist-get res :error)))
      (should-not calls))))

;;;; sprite_eval

(ert-deftest sprite-mcp-tools/eval-returns-printed-result-by-full-and-short-name ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (let ((a (sprite-mcp-tools--eval '(:name "work.0.wt-foo" :expression "(+ 1 2)")))
          (b (sprite-mcp-tools--eval '(:name "wt-foo" :expression "(+ 1 2) (* 2 3)"))))
      (should (equal "3" (plist-get a :result)))
      (should (equal "6" (plist-get b :result)))
      (should (equal "work.0.wt-foo" (plist-get b :name))))))

(ert-deftest sprite-mcp-tools/eval-nil-result-is-success ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (let ((res (sprite-mcp-tools--eval '(:name "wt-foo" :expression "nil"))))
      (should (eq t (plist-get res :ok)))
      (should (equal "nil" (plist-get res :result))))))

(ert-deftest sprite-mcp-tools/eval-remote-error-is-structured ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (let ((res (sprite-mcp-tools--eval '(:name "wt-foo" :expression "(error \"boom\")"))))
      (should-not (plist-get res :ok))
      (should (equal "boom" (plist-get res :error))))))

(ert-deftest sprite-mcp-tools/eval-unreadable-expression-is-structured ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (let ((res (sprite-mcp-tools--eval '(:name "wt-foo" :expression "(unbalanced"))))
      (should-not (plist-get res :ok))
      (should (string-match-p "Cannot read" (plist-get res :error))))))

(ert-deftest sprite-mcp-tools/eval-unknown-or-dead-sprite-is-structured ()
  (sprite-mcp-tools-test/with-env
    (should-not (plist-get (sprite-mcp-tools--eval '(:name "nope" :expression "1")) :ok))
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (setq running nil)
    (let ((res (sprite-mcp-tools--eval '(:name "wt-foo" :expression "1"))))
      (should-not (plist-get res :ok))
      (should (string-match-p "No response" (plist-get res :error))))))

(ert-deftest sprite-mcp-tools/eval-refuses-current-instance-and-parent ()
  (sprite-mcp-tools-test/with-env
    (let ((sprite-instance-id "work.6.wt-self")
          (evaluated nil))
      (cl-letf (((symbol-function 'sprite-mcp-tools--call)
                 (lambda (&rest _) (setq evaluated t) '(:ok "1"))))
        (dolist (n '("work.6.wt-self" "work"))
          (let ((res (sprite-mcp-tools--eval (list :name n :expression "1"))))
            (should-not (plist-get res :ok))
            (should (string-match-p "Refusing" (plist-get res :error)))))
        (should-not evaluated)))))

;;;; sprite_stop

(ert-deftest sprite-mcp-tools/stop-stops-never-decommissions ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (let ((decommissioned nil))
      (cl-letf (((symbol-function 'sprite-decommission)
                 (lambda (&rest _) (setq decommissioned t))))
        (let ((res (sprite-mcp-tools--stop '(:name "wt-foo"))))
          (should (eq t (plist-get res :ok)))
          (should-not (plist-get res :running))
          (should (member '(:stop "work.0.wt-foo") calls))
          (should-not decommissioned)
          (should-not (sprite--decommissioned-p "work.0.wt-foo")))))))

(ert-deftest sprite-mcp-tools/stop-reports-still-running ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (cl-letf (((symbol-function 'sprite-stop) (lambda (_) nil))
              ((symbol-function 'sprite-mcp-tools--wait-stopped) (lambda (&rest _) nil)))
      (let ((res (sprite-mcp-tools--stop '(:name "wt-foo"))))
        (should-not (plist-get res :ok))
        (should (eq t (plist-get res :running)))))))

(ert-deftest sprite-mcp-tools/stop-refuses-current-instance ()
  (sprite-mcp-tools-test/with-env
    (let ((res (sprite-mcp-tools--stop '(:name "work"))))
      (should-not (plist-get res :ok))
      (should-not calls))))

;;;; sprite_restart

(ert-deftest sprite-mcp-tools/restart-reapplies-remembered-directory ()
  (sprite-mcp-tools-test/with-env
    (let ((dir (make-temp-file "wt-" t))
          (applied nil))
      (unwind-protect
          (progn
            (sprite-mcp-tools--start (list :name "wt-foo" :directory dir))
            (cl-letf (((symbol-function 'sprite-mcp-tools--apply-directory)
                       (lambda (full d _t) (push (cons full d) applied) nil)))
              (let ((res (sprite-mcp-tools--restart '(:name "wt-foo"))))
                (should (eq t (plist-get res :ok)))
                (should (equal (list (cons "work.0.wt-foo" (expand-file-name dir)))
                               applied))
                (should (member '(:stop "work.0.wt-foo") calls))
                (should (member '(:restart "work.0.wt-foo") calls)))))
        (delete-directory dir t)))))

(ert-deftest sprite-mcp-tools/restart-refuses-decommissioned ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-foo"))
    (sprite--write-decommissioned-file "work.0.wt-foo")
    (let ((res (sprite-mcp-tools--restart '(:name "wt-foo"))))
      (should-not (plist-get res :ok))
      (should-not (member '(:restart "work.0.wt-foo") calls)))))

;;;; sprite_list

(ert-deftest sprite-mcp-tools/list-reports-running-sprites ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-a"))
    (sprite-mcp-tools--start '(:name "wt-b"))
    (sprite-mcp-tools--stop '(:name "wt-b"))
    (let* ((res (sprite-mcp-tools--list nil))
           (sprites (append (plist-get res :sprites) nil)))
      (should (eq t (plist-get res :ok)))
      (should (= 1 (plist-get res :count)))
      (should (equal "work.0.wt-a" (plist-get (car sprites) :name)))
      (should (equal "wt-a" (plist-get (car sprites) :unique_name))))))

(ert-deftest sprite-mcp-tools/list-tolerates-unresponsive-sprite ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start '(:name "wt-a"))
    (cl-letf (((symbol-function 'sprite-mcp-tools--call) (lambda (&rest _) nil)))
      (let* ((res (sprite-mcp-tools--list nil))
             (e (aref (plist-get res :sprites) 0)))
        (should (eq t (plist-get res :ok)))
        (should (equal "work.0.wt-a" (plist-get e :name)))
        (should (equal 111 (plist-get e :pid)))
        (should-not (plist-get e :directory))))))

;;;; sprite_cleanup_stale

(ert-deftest sprite-mcp-tools/cleanup-defaults-to-dry-run ()
  (sprite-mcp-tools-test/with-env
    (let ((d (sprite-mcp-tools-test/make-dir "work.3.old" "DECOMMISSIONED")))
      (let ((res (sprite-mcp-tools--cleanup-stale nil)))
        (should (eq t (plist-get res :dry_run)))
        (should (= 1 (length (plist-get res :found))))
        (should (= 0 (length (plist-get res :removed))))
        (should (file-directory-p d))))))

(ert-deftest sprite-mcp-tools/cleanup-removes-marker-only-and-empty-dirs ()
  (sprite-mcp-tools-test/with-env
    (let ((marker (sprite-mcp-tools-test/make-dir "work.3.old" "DECOMMISSIONED"))
          (empty (sprite-mcp-tools-test/make-dir "work.4.empty"))
          (real (sprite-mcp-tools-test/make-dir "work.5.real" "DECOMMISSIONED" "other"))
          (live (sprite-mcp-tools-test/make-dir "work.6.live" "DECOMMISSIONED")))
      (push "work.6.live" running)
      (dolist (false-val '(:json-false nil))
        (when (eq false-val nil)
          ;; recreate for the second encoding of false
          (sprite-mcp-tools-test/make-dir "work.3.old" "DECOMMISSIONED")
          (sprite-mcp-tools-test/make-dir "work.4.empty"))
        (let ((res (sprite-mcp-tools--cleanup-stale (list :dry_run false-val))))
          (should (eq :json-false (plist-get res :dry_run)))
          (should (= 2 (length (plist-get res :removed))))
          (should-not (file-exists-p marker))
          (should-not (file-exists-p empty))
          (should (file-directory-p real))
          (should (file-directory-p live)))))))

(ert-deftest sprite-mcp-tools/cleanup-leaves-protected-instance-dir ()
  (sprite-mcp-tools-test/with-env
    (let ((sprite-instance-id "work.6.wt-self"))
      (let ((d (sprite-state-directory :full-name "work.6.wt-self")))
        (make-directory d t)
        (sprite-mcp-tools--cleanup-stale '(:dry_run :json-false))
        (should (file-directory-p d))))))

;;;; Integration (real daemons)

;;;; Proxy routes

(ert-deftest sprite-mcp-tools/start-registers-proxy-route ()
  (sprite-mcp-tools-test/with-env
    (unwind-protect
        (let ((res (sprite-mcp-tools--start (list :name "px"))))
          (should (equal "work.0.px" (plist-get res :target_id)))
          (should (equal (cdr (assoc "work.0.px" services))
                         (map-elt mcpkit-proxy--routes "work.0.px"))))
      (mcpkit-proxy-unregister-route "work.0.px"))))

(ert-deftest sprite-mcp-tools/start-succeeds-without-route-when-service-fails ()
  (sprite-mcp-tools-test/with-env
    (cl-letf (((symbol-function 'sprite-mcp-tools--start-service) (lambda (&rest _) nil)))
      (let ((res (sprite-mcp-tools--start (list :name "nr"))))
        (should (eq t (plist-get res :ok)))
        (should-not (plist-get res :target_id))
        (should-not (map-elt mcpkit-proxy--routes "work.0.nr"))))))

(ert-deftest sprite-mcp-tools/stop-unregisters-proxy-route ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start (list :name "ps"))
    (should (map-elt mcpkit-proxy--routes "work.0.ps"))
    (sprite-mcp-tools--stop (list :name "ps"))
    (should-not (map-elt mcpkit-proxy--routes "work.0.ps"))))

(ert-deftest sprite-mcp-tools/restart-reregisters-proxy-route-on-new-port ()
  (sprite-mcp-tools-test/with-env
    (unwind-protect
        (progn
          (sprite-mcp-tools--start (list :name "pr"))
          (let ((res (sprite-mcp-tools--restart (list :name "pr"))))
            (should (equal "work.0.pr" (plist-get res :target_id)))
            (should (equal (car services)
                           (cons "work.0.pr" (map-elt mcpkit-proxy--routes "work.0.pr"))))
            (should (= 2 (length services)))))
      (mcpkit-proxy-unregister-route "work.0.pr"))))

(ert-deftest sprite-mcp-tools/service-form-starts-and-registers-editing-tools ()
  (let ((form (sprite-mcp-tools--service-form 'sprite-x 4242)))
    (should (string-match-p "mcpkit-start-service" (format "%S" form)))
    (should (string-match-p "4242" (format "%S" form)))
    (should (string-match-p "mcpkit-editing-register-tools" (format "%S" form)))))

;;;; Stale server files

(ert-deftest sprite-mcp-tools/cleanup-flags-refused-server-files ()
  (sprite-mcp-tools-test/with-env
    (let* ((server-auth-dir (file-name-as-directory (make-temp-file "srv-" t)))
           (port (mcpkit-allocate-free-port))
           (stale (expand-file-name "work.0.gone" server-auth-dir)))
      (unwind-protect
          (progn
            (write-region (format "127.0.0.1:%s 1\nkey" port) nil stale)
            (let ((res (sprite-mcp-tools--cleanup-stale nil)))
              (should (equal (list stale) (append (plist-get res :stale_server_files) nil)))
              (should (file-exists-p stale)))
            (sprite-mcp-tools--cleanup-stale (list :dry_run :json-false))
            (should-not (file-exists-p stale)))
        (delete-directory server-auth-dir t)))))

(ert-deftest sprite-mcp-tools/cleanup-keeps-live-and-protected-server-files ()
  (sprite-mcp-tools-test/with-env
    (let* ((server-auth-dir (file-name-as-directory (make-temp-file "srv-" t)))
           (srv (make-network-process :name "live" :server t :host 'local
                                      :service t :noquery t :family 'ipv4))
           (live (expand-file-name "work.0.live" server-auth-dir))
           (prot (expand-file-name "work" server-auth-dir)))
      (unwind-protect
          (progn
            (write-region (format "127.0.0.1:%s 1\nk" (process-contact srv :service)) nil live)
            (write-region "127.0.0.1:1 1\nk" nil prot)
            (should (zerop (length (plist-get (sprite-mcp-tools--cleanup-stale nil)
                                              :stale_server_files)))))
        (delete-process srv)
        (delete-directory server-auth-dir t)))))

;;;; Startup options

(ert-deftest sprite-mcp-tools/start-rejects-bad-options ()
  (sprite-mcp-tools-test/with-env
    (should-not (plist-get (sprite-mcp-tools--start
                            (list :name "o1" :load_path ["/nonexistent/x"])) :ok))
    (should-not (plist-get (sprite-mcp-tools--start
                            (list :name "o2" :load ["/nonexistent/x.el"])) :ok))
    (should-not (plist-get (sprite-mcp-tools--start
                            (list :name "o3" :require [1])) :ok))
    (should-not (member '(:create "work.0.o1") calls))))

(ert-deftest sprite-mcp-tools/start-applies-and-restart-reapplies-options ()
  (sprite-mcp-tools-test/with-env
    (let* ((dir (file-name-as-directory (make-temp-file "lp-" t)))
           (file (expand-file-name "loadme.el" dir)))
      (unwind-protect
          (progn
            (write-region ";;; x -*- lexical-binding: t; -*-
(setq sprite-mcp-tools-test--loaded (1+ (or (bound-and-true-p sprite-mcp-tools-test--loaded) 0)))"
                          nil file)
            (defvar sprite-mcp-tools-test--loaded nil)
            (setq sprite-mcp-tools-test--loaded nil)
            (let ((res (sprite-mcp-tools--start
                        (list :name "opt" :load_path (vector dir)
                              :require ["subr-x"] :load (vector file)))))
              (should (eq t (plist-get res :ok)))
              (should (member (directory-file-name dir)
                              (mapcar #'directory-file-name load-path)))
              (should (= 1 sprite-mcp-tools-test--loaded)))
            (sprite-mcp-tools--restart (list :name "opt"))
            (should (= 2 sprite-mcp-tools-test--loaded)))
        (delete-directory dir t)
        (setq load-path (delete dir load-path))
        (mcpkit-proxy-unregister-route "work.0.opt")))))

(ert-deftest sprite-mcp-tools/start-reports-failing-option-setup ()
  (sprite-mcp-tools-test/with-env
    (let ((res (sprite-mcp-tools--start (list :name "bad" :require ["no-such-feature-xyz"]))))
      (should-not (plist-get res :ok))
      (should (string-match-p "no-such-feature-xyz" (plist-get res :error))))))

;;;; sprite_run_tests

(defmacro sprite-mcp-tools-test/with-test-file (var &rest body)
  "Bind VAR to a temp ERT file with one passing, one failing, one skipped test."
  (declare (indent 1))
  `(let ((,var (make-temp-file "smt-" nil ".el")))
     (unwind-protect
         (progn
           (write-region ";;; x -*- lexical-binding: t; -*-
(require 'ert)
(ert-deftest smt-fixture/passes () (should t))
(ert-deftest smt-fixture/fails () (should (equal 1 2)))
(ert-deftest smt-fixture/skips () (ert-skip \"nope\"))
" nil ,var)
           ,@body)
       (dolist (n '(smt-fixture/passes smt-fixture/fails smt-fixture/skips))
         (ert-delete-test n))
       (delete-file ,var))))

(ert-deftest sprite-mcp-tools/run-tests-reports-pass-fail-and-names ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools-test/with-test-file file
      (sprite-mcp-tools--start (list :name "rt"))
      (let ((res (sprite-mcp-tools--run-tests
                  (list :name "rt" :test_files (vector file) :selector "^smt-fixture/"))))
        (should (eq t (plist-get res :ok)))
        (should (eq :json-false (plist-get res :success)))
        (should (= 1 (plist-get res :passed)))
        (should (= 1 (plist-get res :failed)))
        (should (= 1 (plist-get res :skipped)))
        (should (= 3 (plist-get res :total)))
        (should (equal "smt-fixture/fails"
                       (plist-get (aref (plist-get res :failing) 0) :name)))))
    (mcpkit-proxy-unregister-route "work.0.rt")))

(ert-deftest sprite-mcp-tools/run-tests-selector-form-and-success ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools-test/with-test-file file
      (sprite-mcp-tools--start (list :name "rs"))
      (let ((res (sprite-mcp-tools--run-tests
                  (list :name "rs" :test_files (vector file)
                        :selector "(or \"smt-fixture/passes\" \"smt-fixture/skips\")"))))
        (should (eq t (plist-get res :success)))
        (should (= 0 (length (plist-get res :failing))))
        (should (= 2 (plist-get res :total)))))
    (mcpkit-proxy-unregister-route "work.0.rs")))

(ert-deftest sprite-mcp-tools/run-tests-load-error-and-bad-input-are-structured ()
  (sprite-mcp-tools-test/with-env
    (sprite-mcp-tools--start (list :name "re"))
    (should-not (plist-get (sprite-mcp-tools--run-tests
                            (list :name "re" :main_features ["no-such-feature-xyz"])) :ok))
    (should-not (plist-get (sprite-mcp-tools--run-tests
                            (list :name "re" :test_files ["/nonexistent.el"])) :ok))
    (should-not (plist-get (sprite-mcp-tools--run-tests (list :name "unknown")) :ok))
    (mcpkit-proxy-unregister-route "work.0.re")))

(ert-deftest sprite-mcp-tools/registers-run-tests-tool ()
  (should (gethash "sprite_run_tests" (mcpkit-service-tools (mcpkit-get-service 'sprite)))))

(ert-deftest sprite-mcp-tools/integration-create-eval-restart-stop ()
  "Real create/eval/restart/stop cycle; skipped unless SPRITE_MCP_INTEGRATION=1.
Run it only in an isolated batch Emacs with its own HOME and
`user-emacs-directory' and a private `sprite-instance-id', because it
spawns real daemons."
  (skip-unless (equal "1" (getenv "SPRITE_MCP_INTEGRATION")))
  (let* ((dir (make-temp-file "sprite-mcp-it-" t))
         (_ (make-directory (expand-file-name "lisp" dir)))
         (start (sprite-mcp-tools--start (list :name "it" :directory dir :timeout 60)))
         (full (plist-get start :name)))
    (unwind-protect
        (progn
          (should (eq t (plist-get start :ok)))
          (should (equal "3" (plist-get (sprite-mcp-tools--eval
                                         (list :name "it" :expression "(+ 1 2)"))
                                        :result)))
          (should (equal (file-name-as-directory dir)
                         (read (plist-get (sprite-mcp-tools--eval
                                           (list :name full :expression "default-directory"))
                                          :result))))
          (should (= 1 (plist-get (sprite-mcp-tools--list nil) :count)))
          (sprite-mcp-tools--eval (list :name full :expression "(setq sprite-it-var 1)"))
          (should (eq t (plist-get (sprite-mcp-tools--restart (list :name "it" :timeout 60))
                                   :ok)))
          (should (equal "(unbound)"
                         (plist-get (sprite-mcp-tools--eval
                                     (list :name full
                                           :expression "(if (boundp 'sprite-it-var) 'bound '(unbound))"))
                                    :result)))
          (should (equal (file-name-as-directory dir)
                         (read (plist-get (sprite-mcp-tools--eval
                                           (list :name full :expression "default-directory"))
                                          :result)))))
      (sprite-mcp-tools--stop (list :name full))
      (should-not (sprite--running-p full))
      (delete-directory dir t))))

(ert-deftest sprite-mcp-tools/integration-options-routes-and-run-tests ()
  "Real sprite: startup options, proxy route and sprite_run_tests.
Skipped unless SPRITE_MCP_INTEGRATION=1; see the test above for isolation."
  (skip-unless (equal "1" (getenv "SPRITE_MCP_INTEGRATION")))
  (let* ((dir (make-temp-file "sprite-mcp-it2-" t))
         (tfile (expand-file-name "it-tests.el" dir))
         (_ (write-region ";;; x -*- lexical-binding: t; -*-
(require 'ert)
(ert-deftest it-fixture/ok () (should (featurep 'subr-x)))
(ert-deftest it-fixture/bad () (should (= 1 2)))
" nil tfile))
         (start (sprite-mcp-tools--start
                 (list :name "it2" :directory dir :require ["subr-x"] :timeout 60
                       ;; the isolated sprite has no config: hand it our load-path
                       :load_path (vconcat (seq-filter (function file-directory-p)
                                                       (mapcar (function expand-file-name) load-path))))))
         (full (plist-get start :name)))
    (unwind-protect
        (progn
          (should (eq t (plist-get start :ok)))
          (should (equal full (plist-get start :target_id)))
          (should (map-elt mcpkit-proxy--routes full))
          (let ((res (sprite-mcp-tools--run-tests
                      (list :name "it2" :test_files (vector tfile)
                            :selector "^it-fixture/" :timeout 60))))
            (should (= 1 (plist-get res :passed)))
            (should (= 1 (plist-get res :failed)))
            (should (equal "it-fixture/bad"
                           (plist-get (aref (plist-get res :failing) 0) :name)))))
      (sprite-mcp-tools--stop (list :name full))
      (should-not (map-elt mcpkit-proxy--routes full))
      (delete-directory dir t))))

(provide 'test-sprite-mcp-tools)
;;; test-sprite-mcp-tools.el ends here
