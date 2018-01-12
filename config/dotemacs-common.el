(require 'dotemacs-customs)

(eval-when-compile (require 'cl))

(let ((base (concat user-emacs-directory "elisp/")))
  (add-to-list 'load-path base)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(defadvice require (around dotemacs activate)
  (let ((elapsed)
        (loaded (memq feature features))
        (start (current-time)))
    (prog1
        ad-do-it
      (unless loaded
        (with-current-buffer (get-buffer-create "*Require Times*")
          (when (= 0 (buffer-size))
            (insert "| feature | timestamp | elapsed |\n")
            (insert "|---------+-----------+---------|\n"))
          (goto-char (point-max))
          (setq elapsed (float-time (time-subtract (current-time) start)))
          (insert (format "| %s | %s | %f |\n"
                          feature
                          (format-time-string "%Y-%m-%d %H:%M:%S.%3N" (current-time))
                          elapsed)))))))

(defmacro require-package (package)
  "Ensures that PACKAGE is installed."
  `(progn (unless (or (package-installed-p ,package)
                      (require ,package nil 'noerror))
            (unless (assoc ,package package-archive-contents)
              (package-refresh-contents))
            (package-install ,package))
          (require ,package)))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1))
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.

FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(defmacro lazy-major-mode (pattern mode &rest body)
  "Defines a new major-mode matched by PATTERN, installs MODE if necessary, and activates it."
  (declare (indent 1))
  `(progn (add-to-list 'auto-mode-alist
                       '(,pattern . (lambda ()
                                      (require-package (quote ,mode))
                                      (,mode))))
          (after (quote ,mode)
            ,@body)))

(defmacro delayed-init (&rest body)
  "Runs BODY after idle for a predetermined amount of time."
  `(run-with-idle-timer
    0.5
    nil
    (lambda () ,@body)))

(autoload 'evil-evilified-state "evil-evilified-state")
(autoload 'evilified-state-evilify "evil-evilified-state")

(require 'evil-evilified-state)

(defmacro evilify (mode map &rest body)
  (declare (indent defun))
  (if body
      `(after 'evil (evilified-state-evilify ,mode ,map ,@body))
    `(after 'evil (evilified-state-evilify ,mode ,map))))

(defun recompile-all-packages()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

(defun recompile-config ()
  "Recompile all config files"
  (interactive)
  (byte-recompile-directory dotemacs-config-directory 0 t))

(provide 'dotemacs-common)
