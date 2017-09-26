(defvar flatblue-ml/height 120
  "Modeline height")

(defface flatblue-ml/base
  `((t . (:background "grey13" :height ,flatblue-ml/height)))
  "Base face for modeline")

(defface flatblue-ml/bright
  `((t . (:background "DodgerBlue4"
                      :height  ,flatblue-ml/height
                      :foreground "white"
                      :inherit flatblue-ml/base)))
  "Base face for modeline")



(defun dotemacs//icon-for-buffer (&rest arg-overrides)
  "Get icon info for the current buffer."
  (let* ((file-f (intern "all-the-icons-icon-for-file"))
         (mode-f (intern "all-the-icons-icon-for-mode"))
         (icon (if (and (buffer-file-name)
                        (all-the-icons-auto-mode-match?))
                   (apply file-f (append `(,(file-name-nondirectory (buffer-file-name))) arg-overrides))
                 (apply mode-f (append `(,major-mode) arg-overrides)))))
    (if (eq icon major-mode)
        (propertize (symbol-name icon) 'face (or (plist-get arg-overrides :face) '()))
      icon)))

(defun flatblue-ml//padding (face &optional length)
  (unless length (setq length 2))
  (propertize (format (format "%%%ds" length) " ") 'face face))



(defun flatblue-ml//winum ()
  (propertize (format " %d  " (winum-get-number))
              'face 'flatblue-ml/bright
              'display '(raise 0.0)))

(defun flatblue-ml//modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit ))))

(defun flatblue-ml//mode-icon ()
  (format "%s" (dotemacs//icon-for-buffer :face 'flatblue-ml/bright :height 0.8 :v-adjust -0.1)))



(defun flatblue-ml/mode-line-format (&optional mode)
  '(:eval 
    (concat
     (flatblue-ml//winum)
     (flatblue-ml//padding 'flatblue-ml/bright 1)
     (flatblue-ml//mode-icon)
     (flatblue-ml//padding 'flatblue-ml/bright 2))))


(defun flatblue-modeline/setup ()
  (setq-default mode-line-format `("%e" ,(flatblue-ml/mode-line-format))))

(defun flatblue-ml/reload ()
  (save-excursion
    (dolist (buf (buffer-list))
      (switch-to-buffer buf)
      (setq mode-line-format `("%e" ,(flatblue-ml/mode-line-format major-mode))))))
