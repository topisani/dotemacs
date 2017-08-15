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

(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

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

(defmacro lazy-major-mode (pattern mode)
  "Defines a new major-mode matched by PATTERN, installs MODE if necessary, and activates it."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (require-package (quote ,mode))
                               (,mode)))))

(defmacro delayed-init (&rest body)
  "Runs BODY after idle for a predetermined amount of time."
  `(run-with-idle-timer
    0.5
    nil
    (lambda () ,@body)))

(autoload 'evil-evilified-state "evil-evilified-state")
(autoload 'evilified-state-evilify "evil-evilified-state")

(defmacro evilify (mode map &rest body)
  (declare (indent defun))
  (if body
      `(after 'evil (evilified-state-evilify ,mode ,map ,@body))
    `(after 'evil (evilified-state-evilify ,mode ,map))))

(defun recompile-all-packages()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))




(defgroup dotemacs-bindings nil
  "Configuration options for general key bindings"
  :group 'dotemacs
  :prefix 'dotemacs-bindings)

(defcustom dotemacs-bindings/major-key
  ","
  "The prefix key for major-mode specific bindings.
   Remaps to `<leader> m'"
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/leader-key
  "SPC"
  "The prefix key for general leader bindings."
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/fallback-major-key
  "C-,"
  "fallback `<major-key>'"
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/fallback-leader-key
  "C-SPC"
  "Fallback `leader-key'"
  :type 'key-sequence
  :group 'dotemacs-bindings)

(global-unset-key (kbd "C-SPC"))



(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda (arg)
     (interactive "P")
     ,@commands))

(require-package 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(which-key-mode)

(require-package 'bind-map)

(defvar dotemacs/leader-map (make-keymap))

(bind-map dotemacs/leader-map
  :keys (dotemacs-bindings/fallback-leader-key)
  :evil-keys (dotemacs-bindings/leader-key)
  :evil-states (visual normal motion))

(defun dotemacs/get-major-key-map (mode)
  (let ((leaders (list
                  dotemacs-bindings/major-key
		  (concat dotemacs-bindings/leader-key " m")))
        (fallback-leaders (list
			   dotemacs-bindings/fallback-major-key
                           (concat dotemacs-bindings/fallback-leader-key " m")))
        (map (intern (format "dotemacs/%s-map" (symbol-name mode)))))
    (if (boundp map)
        map
      (progn
        (eval `(bind-map ,map
                 :evil-keys ,leaders
                 :evil-states (normal motion visual)
                 :keys ,fallback-leaders
                 :major-modes (,mode)))
        (dolist (prefix (append leaders fallback-leaders))
          (which-key-declare-prefixes-for-mode mode prefix (symbol-name mode)))
        map))))

  (defun dotemacs/describe-function (func desc)
    "Set a function description for `which-key'.
   FUNC is the function symbol to describe, and DESC is one of
   `string' and `list', where the list will evalualte to a string"
    (if (stringp desc)
        (add-to-list 'which-key-replacement-alist `((nil . ,(symbol-name func)) . (nil . ,desc)))
      (push (cons `(nil . ,(symbol-name func))
                  `(lambda (kb)
                     (cons (car kb)
                           ,desc)))
            which-key-replacement-alist)))

(defun dotemacs/describe-key (key desc)
  "Set a key description for `which-key'.
   KEY is the a `string', signifying a key to describe, and DESC is one of
   `string' and `list', where the list will evalualte to a string"
  (if (stringp desc)
      (add-to-list 'which-key-replacement-alist `((,key . nil) . (nil . ,desc)))
    (push (cons `(,key . nil)
                `(lambda (kb)
                   (cons (car kb)
                         ,desc)))
          which-key-replacement-alist)))

(defun dotemacs/describe-leader-key (key desc)
  (dotemacs/describe-key (concat dotemacs-bindings/leader-key " " key) desc)
  (dotemacs/describe-key (concat dotemacs-bindings/fallback-leader-key " " key) desc))

(defun dotemacs/describe-major-key (key desc)
  (dotemacs/describe-key (concat dotemacs-bindings/leader-key " m " key) desc)
  (dotemacs/describe-key (concat dotemacs-bindings/fallback-leader-key " m " key) desc)
  (dotemacs/describe-key (concat dotemacs-bindings/major-key " " key) desc)
  (dotemacs/describe-key (concat dotemacs-bindings/fallback-major-key " " key) desc))

(defun dotemacs/bind-major-key (mode key func &optional desc)
  (let ((map (dotemacs/get-major-key-map mode)))
    (progn
      (define-key (symbol-value map) (kbd key) func)
      (when desc
        (if (symbolp func)
            (dotemacs/describe-function func desc)
          (dotemacs/describe-major-key key desc))))))

(defun dotemacs/bind-leader-key (key func &optional desc)
  (define-key dotemacs/leader-map (kbd key) func)
  (when desc
    (if (symbolp func)
        (dotemacs/describe-function func desc)
      (dotemacs/describe-leader-key key desc))))

(dotemacs/bind-major-key 'emacs-lisp-mode "e" 'eval-buffer "eval buffer")
(dotemacs/bind-leader-key "SPC" 'execute-extended-command)



;; Macros

(defmacro define-major (mode &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect `(let ((seq ,(car binding))
                               (func ,(cadr binding))
                               (desc ,(caddr binding)))
                           (dotemacs/bind-major-key ,mode seq func desc)))))

(defmacro define-leader (&rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect `(let ((seq ,(car binding))
                               (func ,(cadr binding))
                               (desc ,(caddr binding)))
                           (dotemacs/bind-leader-key seq func desc)))))

(defmacro -define-key (keymap sequence binding &optional description)
  (declare (indent defun))
  `(progn
     (if ,keymap
	 (define-key ,keymap (kbd ,sequence) ,binding)
       (after 'evil
         (define-key evil-visual-state-map (kbd ,sequence) ,binding)
         (define-key evil-motion-state-map (kbd ,sequence) ,binding)
         (define-key evil-normal-state-map (kbd ,sequence) ,binding)))
     (when ,description
       (which-key-add-key-based-replacements ,sequence ,description))))

(defmacro -define-keys (keymap &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect `(let ((seq ,(car binding))
                               (func ,(cadr binding))
                               (desc ,(caddr binding)))
                           (-define-key ,keymap seq func desc)))))




(provide 'init-boot)
