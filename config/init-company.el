(when (eq dotemacs-completion-engine 'company)

  (defgroup dotemacs-company nil
    "Configuration options for company-mode."
    :group 'dotemacs
    :prefix 'dotemacs-company)

  (defcustom dotemacs-company/ycmd-server-command nil
    "The path to the ycmd package."
    :group 'dotemacs-company)

  (require-package 'company)
  (require 'company)

  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)

  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)

  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-code-everywhere t)

  (setq company-etags-ignore-case t)

  (unless (face-attribute 'company-tooltip :background)
    (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
    (set-face-attribute 'company-preview nil :background "black")
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
    (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))

  (when (executable-find "tern")
    (after "company-tern-autoloads"
      (add-to-list 'company-backends 'company-tern)))

  (setq company-global-modes
        '(not
          eshell-mode comint-mode org-mode erc-mode))

  (defun check-expansion ()
    (save-excursion
      ;; (if (outline-on-heading-p t)
      ;;     nil
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete))))

  (after 'yasnippet
    (define-key yas-minor-mode-map [tab] 'tab-indent-or-complete)
    (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete))

  (global-company-mode)

  ;; has big performance impact
  ;; (require-package 'company-flx)
  ;; (company-flx-mode)

  (when (display-graphic-p)
    (require-package 'pos-tip)
    (require-package 'company-quickhelp)
    (setq company-quickhelp-delay 0.2)
    (company-quickhelp-mode t)))

(provide 'init-company)
