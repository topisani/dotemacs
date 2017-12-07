(require-package 'modern-cpp-font-lock)

(require 'cc-mode)
(custom-set-variables '(c-noise-macro-names '("constexpr" "noexcept")))
(c-make-noise-macro-regexps)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Disassembler
(require-package 'disaster)



(after 'cc-mode

  ;; Generic doxygen formatting
  (defconst custom-font-lock-doc-comments
    (let ((symbol "[a-zA-Z0-9_]+"))
      `((,(concat "\`[^\`]+\`") ; `symbol`
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat "[\\][^[:space:]]+") ; \doxy OR @doxy
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat "[@][^[:space:]\n]+") ; \doxy OR @doxy
         0 ,font-lock-warning-face prepend nil)
        (,(concat "\\\\t?param " symbol) ; \param PAR OR \tparam PAR
         0 ,c-doc-markup-face-name prepend nil)
        )))

  ;; Matches across multiple lines:
  ;;   /** doxy comments */
  ;;   /*! doxy comments */
  ;;   /// doxy comments
  ;; Doesn't match:
  ;;   /*******/
  (defconst custom-font-lock-keywords
    `((,(lambda (limit)
          (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
              limit custom-font-lock-doc-comments)))))
  (setq-default c-doc-comment-style (quote (custom))))


;; Hooks
(defun my-c++-mode-hook ()
  (modern-c++-font-lock-mode)
  (flycheck-mode)
  (auto-fill-mode)
  (lsp-ui-mode)
  (lsp-cquery-enable)
  (setq
   lsp-enable-codeaction t
   lsp-enable-xref t
   lsp-enable-flycheck t)
  (set (make-local-variable 'company-backends) '(company-lsp company-yasnippet))
  (setq flycheck-check-syntax-automatically '(mode-enabled new-line idle-change save)
        flycheck-idle-change-delay 1))

(add-hook 'c++-mode-hook #'my-c++-mode-hook)

(require-package 'lsp-mode)
(after 'lsp-mode
  (require 'lsp-ui)
  (require-package 'company-lsp))
(require 'cquery)

(define-evilified-keys lsp-xref-mode-map
  ("n" 'lsp-xref--select-next)
  ("N" 'lsp-xref--select-prev)
  ("p" 'lsp-xref--select-prev)
  ("j" 'lsp-xref--select-next)
  ("k" 'lsp-xref--select-prev)
  ("<return>" (bind (lsp-xref--goto-xref) (lsp-xref--abort)))
  ("<esc>" 'lsp-xref--abort)
  ("q" 'lsp-xref--abort))


;; Bindings
(define-major 'c++-mode
  ("TAB" 'projectile-find-other-file)
  ("<C-tab>" 'projectile-find-other-file-other-window)
  ("." 'lsp-xref-find-definitions)
  ("?" 'lsp-xref-find-references)
  ("," 'xref-pop-marker-stack)
  ("=" 'clang-format)
  ("f" 'cquery-select-codeaction)
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
                           (setq comint-scroll-to-bottom-on-input t)
                           (setq comint-scroll-to-bottom-on-output 'others)
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
    (set-window-dedicated-p w-gdb t)

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
