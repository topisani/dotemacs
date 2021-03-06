(require 'dotemacs-common)



(add-hook 'evil-jumps-post-jump-hook #'recenter)

(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(require-package 'evil)

(evil-mode)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))

(cl-loop for hook in dotemacs-evil/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))

(cl-loop for mode in dotemacs-evil/emacs-state-major-modes
         do (evil-set-initial-state mode 'emacs))

(add-hook 'after-init-hook
          (lambda ()
            (evil-put-property 'evil-state-properties 'normal   :tag " N ")
            (evil-put-property 'evil-state-properties 'insert   :tag " I ")
            (evil-put-property 'evil-state-properties 'visual   :tag " V ")
            (evil-put-property 'evil-state-properties 'motion   :tag " M ")
            (evil-put-property 'evil-state-properties 'emacs    :tag " E ")
            (evil-put-property 'evil-state-properties 'replace  :tag " R ")
            (evil-put-property 'evil-state-properties 'operator :tag " O ")))

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)



(when dotemacs-evil/emacs-insert-mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state))


(unless (display-graphic-p)
  (evil-esc-mode))


(require-package 'evil-commentary)
(evil-commentary-mode t)


(require-package 'evil-surround)
(global-evil-surround-mode t)


(require-package 'evil-exchange)
(evil-exchange-install)


(require-package 'evil-anzu)
(require 'evil-anzu)
(after 'all-the-icons
  (setq anzu-cons-mode-line-p nil
        anzu-replace-to-string-separator (all-the-icons-faicon "long-arrow-right")))


(require-package 'evil-ediff)
(evil-ediff-init)


(after 'magit
  (require-package 'evil-magit)
  (require 'evil-magit))


(require-package 'evil-avy)
(evil-avy-mode)


(require-package 'evil-matchit)
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items))
(global-evil-matchit-mode t)


(require-package 'evil-indent-textobject)
(require 'evil-indent-textobject)


(require-package 'evil-visualstar)
(global-evil-visualstar-mode t)


(require-package 'evil-numbers)

(require-package 'evil-lion)
(evil-lion-mode)

(require-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))
(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)


(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))



(with-current-buffer "*Messages*"
  (evil-evilified-state))

(provide 'init-evil)
