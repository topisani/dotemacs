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
      (add-to-list 'which-key-replacement-alist `((nil . ,(concat "^" (symbol-name func) "$")) . (nil . ,desc)))
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
      (add-to-list 'which-key-replacement-alist `((,(concat "^" key "$") . nil) . (nil . ,desc)))
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
      (when desc (dotemacs/describe-major-key key desc)))))

(defun dotemacs/bind-leader-key (key func &optional desc)
  (define-key dotemacs/leader-map (kbd key) func)
  (when desc (dotemacs/describe-leader-key key desc)))

(defun dotemacs/bind-evil-key (state keymap sequence binding &optional description)
  (let ((key (if (stringp sequence) (kbd sequence) sequence)))
    (if (listp state)
        (dolist (elm state)
          (dotemacs/bind-evil-key elm keymap sequence binding description))
      (progn
        (evil-define-key state keymap key binding)
        (when description
          (if (symbolp binding)
              (dotemacs/describe-function binding description)
            (dotemacs/describe-key sequence description)))))))



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
     (define-key ,keymap (kbd ,sequence) ,binding)
     (when ,description
       (if (symbolp ,binding)
           (dotemacs/describe-function ,binding ,description)
         (dotemacs/describe-key ,sequence ,description)))))

(defmacro -define-keys (keymap &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect `(let ((seq ,(car binding))
                               (func ,(cadr binding))
                               (desc ,(caddr binding)))
                           (-define-key ,keymap seq func desc)))))

(defmacro define-evilified-keys (keymap &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect `(let ((seq ,(car binding))
                               (func ,(cadr binding))
                               (desc ,(caddr binding)))
                           (dotemacs/bind-evil-key '('evilified 'normal 'motion) ,keymap seq func desc)))))


(provide 'def-bindings)
