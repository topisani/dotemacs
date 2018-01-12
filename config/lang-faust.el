(require 'dotemacs-common)

(lazy-major-mode "\\.dsp$" faust-mode)
(after 'faust-mode
  (require-package 'faustine))
