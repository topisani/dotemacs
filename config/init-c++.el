

(require-package 'cmake-ide)
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

(require-package 'modern-cpp-font-lock)
(require-package 'company-irony-c-headers)

(defun my-c++-mode-hook ()
  (setq flycheck-check-syntax-automatically t)
  ;;(modern-c++-font-lock-mode t)
  (set (make-local-variable 'company-backends)
       '((company-irony company-irony-c-headers)))
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change newline)))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(require 'company-rtags)
;; (setq rtags-autostart-diagnostics t)
(setq company-rtags-begin-after-member-access t)
(setq rtags-completions-enabled t)

;; Disassembler
(require-package 'disaster)



;; CMake
(require-package 'cmake-mode)

(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))



;; Bindings
(define-major 'c++-mode
  ("TAB" 'projectile-find-other-file)
  ("\\C \\t" 'projectile-find-other-file-other-window)
  ("m." 'rtags-find-symbol-at-point)
  ("m," 'rtags-find-references-at-point)
  ("mv" 'rtags-find-virtuals-at-point)
  ("mV" 'rtags-print-enum-value-at-point)
  ("m/" 'rtags-find-all-references-at-point)
  ("my" 'rtags-cycle-overlays-on-screen)
  ("m>" 'rtags-find-symbol)
  ("m<" 'rtags-find-references)
  ("mN" 'rtags-location-stack-back)
  ("mn" 'rtags-location-stack-forward)
  ("md" 'rtags-diagnostics)
  ("mg" 'rtags-guess-function-at-point)
  ("mp" 'rtags-set-current-project)
  ("mP" 'rtags-print-dependencies)
  ("me" 'rtags-reparse-file)
  ("mE" 'rtags-preprocess-file)
  ("mr" 'rtags-rename-symbol)
  ("mm" 'rtags-symbol-info)
  ("ms" 'rtags-display-summary)
  ("mo" 'rtags-goto-offset)
  ("m;" 'rtags-find-file)
  ("mf" 'rtags-fixit)
  ("ml" 'rtags-copy-and-print-current-location)
  ("mx" 'rtags-fix-fixit-at-point)
  ("mb" 'rtags-show-rtags-buffer)
  ("mi" 'rtags-imenu)
  ("mt" 'rtags-taglist)
  ("mh" 'rtags-print-class-hierarchy)
  ("ma" 'rtags-print-source-arguments)

  ("a" 'disaster))
