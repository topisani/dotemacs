;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;  WINDOW PURPOSE                                 ;;
;;                                                 ;;
;;   Dedicate windows to purposes or buffers       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'window-purpose)
(purpose-mode)
(require 'window-purpose-x)
(purpose-x-kill-setup)
(purpose-x-popwin-setup)

(add-hook 'after-init-hook (lambda () (purpose-compile-user-configuration)))

(define-leader
  ("W d" #'purpose-toggle-window-purpose-dedicated "dedicate to purpose")
  ("W D" #'purpose-toggle-window-buffer-dedicated "dedicate to buffer")
  ("W c" #'purpose-delete-non-dedicated-windows "close non-dedicated windows")
  ("W b" #'purpose-switch-buffer-with-purpose "switch buffer")
  ("W s" #'purpose-save-window-layout "save layout")
  ("W l" #'purpose-load-window-layout "load layout"))


(add-to-list 'purpose-user-mode-purposes '(fundamental-mode . code))
(add-to-list 'purpose-user-mode-purposes '(compilation-mode . popup))
(add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . popup))
(add-to-list 'purpose-user-mode-purposes '(ag-mode . popup))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;  WINUM AND RELATED                              ;;
;;                                                 ;;
;;   Give windows numbers and switch between them  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq winum-auto-setup-mode-line nil)

(require-package 'winum)
(winum-mode t)

;; https://github.com/deb0ch/spacemacs/blob/fa32a93a08c6363a679e61771b4c4c8252d5166f/layers/%2Bdistributions/spacemacs-base/funcs.el#L215
(defun spacemacs/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
   controls whether focus moves to new window (with buffer), or stays on
   current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

(defun spacemacs/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
   follow-focus-p controls whether focus moves to new window (with buffer), or
   stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (select-window-by-number windownum)))

;; Generate commands
(dotimes (i 10)
  (eval `(defun ,(intern (format "buffer-to-window-%s" i)) (&optional arg)
           ,(format "Move buffer to the window with number %i." i)
           (interactive "P")
           (if arg
               (spacemacs/swap-buffers-to-window ,i t)
             (spacemacs/move-buffer-to-window ,i t)))))

;; Add which-key replacements
(after 'which-key
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)

  (push '(("\\(.*\\) 0" . "buffer-to-window-0") . ("\\1 0..9" . "buffer to window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "buffer-to-window-[1-9]") . t) which-key-replacement-alist))



;; Persp-mode
(require-package 'persp-mode)
(require 'persp-mode)
(setq persp-autokill-buffer-on-remove 'kill-weak)

(require 'persp-mode-projectile-bridge)
(setq persp-mode-projectile-bridge-persp-name-prefix "")

(add-hook 'persp-mode-projectile-bridge-mode-hook
          #'(lambda ()
              (if persp-mode-projectile-bridge-mode
                  (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                (persp-mode-projectile-bridge-kill-perspectives))))
(add-hook 'after-init-hook
          #'(lambda ()
              (persp-mode 1)
              (persp-mode-projectile-bridge-mode 1))
          t)

(setq persp-add-buffer-on-find-file 'if-not-autopersp)
(add-hook 'persp-after-load-state-functions #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)

(provide 'init-windows)
