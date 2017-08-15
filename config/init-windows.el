;; (require-package 'window-purpose)
;; (purpose-mode)

;; (add-hook 'after-init-hook (lambda ()
;;                              (purpose-compile-user-configuration)))

;; 

;; (define-leader nil
;;   ("W d" #'purpose-toggle-window-purpose-dedicated "dedicate to purpose")
;;   ("W D" #'purpose-toggle-window-buffer-dedicated "dedicate to buffer")
;;   ("W c" #'purpose-delete-non-dedicated-windows "close non-dedicated windows")
;;   ("W b" #'purpose-switch-buffer-with-purpose "switch buffer")
;;   ("W s" #'purpose-save-window-layout "save layout")
;;   ("W l" #'purpose-load-window-layout "load layout"))

;; 

;; ;; c++ setup
;; (add-to-list 'purpose-user-mode-purposes '(fundamental-mode . code))
;; (add-to-list 'purpose-user-mode-purposes '(compilation-mode . popup))
;; (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . popup))

(require-package 'shackle)

(setq shackle-select-reused-windows nil) ; default nil
(setq shackle-default-alignment 'below)  ; default below
(setq shackle-default-ratio 0.4)         ; default 0.5

(setq shackle-rules
      '((compilation-mode                       :select nil     )
        (c-mode                                 :same t  )
        ("*Flycheck errors*"
         :popup t
         :select t
         :align 'below)
        (cscope                                 :same nil)
        ))
(shackle-mode t)

(provide 'init-windows)
