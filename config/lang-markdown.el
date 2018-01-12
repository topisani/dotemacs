(require 'dotemacs-common)

(lazy-major-mode "\\.\\(md\\|markdown\\)$" 'markdown-mode
  (add-named-hook 'markdown-mode-hook
    (olivetti-mode)))

(provide 'lang-markdown)
