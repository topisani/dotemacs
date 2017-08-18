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
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (indent-for-tab-command)))))))))

  (defun tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if company-candidates
            (company-complete-selection)
          (if (check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (yas-next-field))))
            (yas-next-field)))))

  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (define-key company-active-map [tab] 'tab-indent-or-complete)
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map [(control return)] 'company-complete-common)

  (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

  (after 'yasnippet
    (define-key yas-minor-mode-map [tab] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)

    (define-key yas-keymap [tab] 'tab-complete-or-next-field)
    (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
    (define-key yas-keymap [(control tab)] 'yas-next-field)
    (define-key yas-keymap (kbd "esc") 'abort-company-or-yas))
  
  (after 'evil
    (define-key evil-insert-state-map (kbd "<C-tab>") 'company-complete))

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
