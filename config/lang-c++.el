;; (require-package 'rtags)
(require-package 'modern-cpp-font-lock)
(require-package 'company-irony-c-headers)
(require-package 'company-irony)
(require-package 'irony)
(require-package 'irony-eldoc)

(require 'irony)
(require-package 'cmake-ide)
(cmake-ide-setup)
(require 'flycheck)
(require-package 'flycheck-irony)
(flycheck-irony-setup)

(defun my-c++-mode-hook ()
  (modern-c++-font-lock-mode)
  (flycheck-mode)

  (set (make-local-variable 'company-backends) '(company-irony-c-headers company-irony company-yasnippet))
  (setq flycheck-check-syntax-automatically '(mode-enabled new-line idle-change save)
        flycheck-idle-change-delay 1))

(add-hook 'c++-mode-hook #'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc)

(defun company-fuzzy-irony--filter-candidates (prefix candidates)
  (cl-loop for candidate in candidates
           collect (propertize (car candidate) 'company-irony candidate)))

(with-eval-after-load 'company-irony
  (advice-add 'company-irony--filter-candidates :override #'company-fuzzy-irony--filter-candidates))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (setq rtags-autostart-diagnostics nil)
;; (setq company-rtags-begin-after-member-access nil)
;; (setq rtags-completions-enabled nil)

;; Disassembler
(require-package 'disaster)

;; Bindings
(define-major 'c++-mode
  ("TAB" 'projectile-find-other-file)
  ("<C-tab>" 'projectile-find-other-file-other-window)
  ;; ("m." 'rtags-find-symbol-at-point)
  ;; ("m," 'rtags-find-references-at-point)
  ;; ("mv" 'rtags-find-virtuals-at-point)
  ;; ("mV" 'rtags-print-enum-value-at-point)
  ;; ("m/" 'rtags-find-all-references-at-point)
  ;; ("my" 'rtags-cycle-overlays-on-screen)
  ;; ("m>" 'rtags-find-symbol)
  ;; ("m<" 'rtags-find-references)
  ;; ("mN" 'rtags-location-stack-back)
  ;; ("mn" 'rtags-location-stack-forward)
  ;; ("md" 'rtags-diagnostics)
  ;; ("mg" 'rtags-guess-function-at-point)
  ;; ("mp" 'rtags-set-current-project)
  ;; ("mP" 'rtags-print-dependencies)
  ;; ("me" 'rtags-reparse-file)
  ;; ("mE" 'rtags-preprocess-file)
  ;; ("mr" 'rtags-rename-symbol)
  ;; ("mm" 'rtags-symbol-info)
  ;; ("ms" 'rtags-display-summary)
  ;; ("mo" 'rtags-goto-offset)
  ;; ("m;" 'rtags-find-file)
  ;; ("mf" 'rtags-fixit)
  ;; ("ml" 'rtags-copy-and-print-current-location)
  ;; ("mx" 'rtags-fix-fixit-at-point)
  ;; ("mb" 'rtags-show-rtags-buffer)
  ;; ("mi" 'rtags-imenu)
  ;; ("mt" 'rtags-taglist)
  ;; ("mh" 'rtags-print-class-hierarchy)
  ;; ("ma" 'rtags-print-source-arguments)

  ("a" 'disaster))



;; CMake
(require-package 'cmake-mode)

(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))




;; GDB
(require 'gud)

(setq gdb-many-windows nil)
(add-hook 'gud-mode-hook (defun my-gud-mode-hook ()
                           (company-mode -1)))

(defun set-gdb-layout(&optional c-buffer)
  (interactive)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* ((w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height))) 'below))) ;; left bottom
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-disassembly-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)))

(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let ((c-buffer (window-buffer (selected-window)))) ;; save current buffer)
    ad-do-it
    (set-gdb-layout c-buffer))
  (gud-tooltip-mode t))

(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing)
  (gud-tooltip-mode nil))



;; LLDB

(require 'gud-lldb)


(provide 'lang-c++)
