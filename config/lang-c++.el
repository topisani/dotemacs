;;; -*- lexical-binding: t -*-
(require 'dotemacs-common)

(require-package 'modern-cpp-font-lock)

(require 'cc-mode)
(custom-set-variables '(c-noise-macro-names '("constexpr" "noexcept")))
(c-make-noise-macro-regexps)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))


;; Disassembler
(require-package 'disaster)


;; Generic doxygen formatting

(defconst doxygen-font-lock-doc-comments
  (let ((symbol "[a-zA-Z0-9_]+"))
    `((,(concat "\`[^\`]+\`") ; `symbol`
       0 ,c-doc-markup-face-name prepend nil)
      (,(concat "[\\\\@][^[:space:]]+") ; \doxy OR @doxy
       0 ,c-doc-markup-face-name prepend nil)
      (,(concat "\\\\t?param " symbol) ; \param PAR OR \tparam PAR
       0 ,c-doc-markup-face-name prepend nil)
      )))

;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit doxygen-font-lock-doc-comments)))))

(after 'cc-mode
  (setq-default c-doc-comment-style 'doxygen))


;; Hooks
(defun my-c++-mode-hook ()
  (modern-c++-font-lock-mode)
  (flycheck-mode)
  (auto-fill-mode)
  (lsp-ui-mode)
  (lsp-ui-sideline-mode)
  (lsp-cquery-enable)
  (standardese-mode )
  (setq
   lsp-enable-codeaction t
   lsp-enable-xref t
   lsp-enable-flycheck nil)
  (set (make-local-variable 'company-backends) '(company-lsp company-yasnippet))
  (setq flycheck-check-syntax-automatically '(mode-enabled new-line idle-change save)
        flycheck-idle-change-delay 1))

(add-hook 'c++-mode-hook #'my-c++-mode-hook)

(defun my-c-mode-hook ()
  (flycheck-mode)
  (auto-fill-mode)
  (lsp-ui-mode)
  (lsp-cquery-enable)
  (standardese-mode)
  (setq
   lsp-enable-codeaction t
   lsp-enable-xref t
   lsp-enable-flycheck t)
  (set (make-local-variable 'company-backends) '(company-lsp company-yasnippet))
  (setq flycheck-check-syntax-automatically '(mode-enabled new-line idle-change save)
        flycheck-idle-change-delay 1))

(add-hook 'c-mode-hook #'my-c-mode-hook)

(require-package 'lsp-mode)
(after 'lsp-mode
  (require 'lsp-ui)
  (require-package 'company-lsp))
(require 'cquery)
(setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))

(-define-keys lsp-ui-peek-mode-map
  ("n" 'lsp-ui-peek--select-next)
  ("N" 'lsp-ui-peek--select-prev)
  ("p" 'lsp-ui-peek--select-prev)
  ("j" 'lsp-ui-peek--select-next)
  ("k" 'lsp-ui-peek--select-prev)
  ("<return>" 'lsp-ui-peek--goto-xref)
  ("C-<return>" 'lsp-ui-peek--goto-xref-other-window)
  ("<esc>" 'lsp-ui-peek--abort)
  ("q" 'lsp-ui-peek--abort))

(setq lsp-ui-sideline-code-actions-prefix
      (propertize "" 'face '(:foreground "yellow" :weight 900)))



;; Standardese documentation

;; Inline markup
(defconst standardese-font-lock-doc-comments
  (let ((symbol "[a-zA-Z0-9_]+")
        (command (regexp-opt
                  '("exclude" "unique_name" "output_name" "synopsis" "group" "module" "output_section"
                    "entity" "file" "brief" "details" "requires" "effects" "synchronization" "postconditions"
                    "returns" "throws" "complexity" "remarks" "error_conditions" "notes" "preconditions"
                    "constraints" "diagnostics" "see")))
        (param (regexp-opt '("tparam" "param" "base"))))
    `((,(concat "\`[^\`]+\`") ; `symbol`
       0 ,c-doc-markup-face-name prepend nil)
      (,(concat "[@\\\\][^[:space:]]+") ; \unknown OR @unknown
       0 ,font-lock-warning-face t nil)
      (,(concat "\\\\" command) ; \command
       0 ,c-doc-markup-face-name t nil)
      (,(concat "\\\\" param " " symbol) ; \param PAR
       0 ,c-doc-markup-face-name t nil)
      )))

;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst standardese-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit standardese-font-lock-doc-comments)))))

(defun standardese-command-to-header (string)
  (save-match-data
    (let* ((first-space (string-match " " string))
           (command (substring string 0 first-space))
           (header (mapconcat 'identity
                              (mapcar
                               (lambda (word) (capitalize (downcase word)))
                               (split-string command "_")) " ")))
      (concat header (when first-space (substring string first-space nil))))))

(defun standardese-render-doc (string)
  (if (or
       (string-match-p "\\\\exclude" string)
       (string-prefix-p "// " string)
       (string-prefix-p "/* " string))
      ""
    (with-temp-buffer
      (insert string)
      ;; Trim comment markers
      (goto-char (point-min))
      (while (re-search-forward "[ \\t]*/// ?" nil t)
        (replace-match ""))

      (goto-char (point-min))
      (while (re-search-forward "\\[\\(.*?\\)\\]()" nil t)
        (replace-match "\\\\ref `\\1`"))

      ;; Markdown mode
      (delay-mode-hooks
        (markdown-view-mode)
        (ignore-errors
          (font-lock-ensure)))

      (setq-local inhibit-read-only t)
      ;; Format links
      (goto-char (point-min))
      (while (re-search-forward "\\\\ref `\\(.*?\\)`" nil t)
        (let ((map (make-sparse-keymap))
              (dst (match-string-no-properties 1)))
          (define-key map [mouse-1]
            (lambda () (interactive)
              (select-window (lsp-ui-doc--get-parent :window))
              (lsp-ui-peek-find-workspace-symbol
               (replace-regexp-in-string "<.*>" "" dst))))
          (replace-match (propertize (concat dst)
                                     'face 'markdown-link-face
                                     'mouse-face 'cquery-code-lens-mouse-face
                                     'local-map map) t t)))
      ;; Format sections
      (goto-char (point-min))
      (while (re-search-forward "[ \\t]*\\\\\\(t?param [a-zA-Z]+\\|[a-zA-Z]+\\) ?" nil t)
        (replace-match (propertize (concat (standardese-command-to-header (match-string-no-properties 1)) ": ")
                                   'face 'markdown-header-face
                                   )) t t)

      (string-remove-suffix "\n" (buffer-string)))))

(define-minor-mode standardese-mode
  "Standardese documentation highlighting and lsp-ui support"
  :init-value nil
  :group dotemacs
  (cond
   (standardese-mode
    (setq lsp-ui-doc-render-function 'standardese-render-doc)
    (setq c-doc-comment-style 'standardese)
    (font-lock-remove-keywords nil doxygen-font-lock-keywords)
    (font-lock-add-keywords nil standardese-font-lock-keywords)
    (c-setup-doc-comment-style))
   (t
    (setq lsp-ui-doc-render-function nil)
    (setq c-doc-comment-style 'doxygen)
    (font-lock-remove-keywords nil standardese-font-lock-keywords)
    (font-lock-add-keywords nil doxygen-font-lock-keywords)
    (c-setup-doc-comment-style))))



;; Bindings
(define-major 'c++-mode
  ("TAB" 'projectile-find-other-file)
  ("<C-tab>" 'projectile-find-other-file-other-window)
  ("." 'lsp-ui-peek-find-definitions)
  ("?" 'lsp-ui-peek-find-references)
  ("," 'xref-pop-marker-stack)
  ("=" 'clang-format)
  ("f" 'cquery-select-codeaction)
  ("s" (bind (lsp--text-document-code-action)))
  ("a" 'disaster))

(define-major 'c-mode
  ("TAB" 'projectile-find-other-file)
  ("<C-tab>" 'projectile-find-other-file-other-window)
  ("." 'lsp-ui-peek-find-definitions)
  ("?" 'lsp-ui-peek-find-references)
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
