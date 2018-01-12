(require 'dotemacs-common)
(require 'def-bindings)

(require-package 'neotree)

(defun dotemacs/neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)
        (neotree-hide)))))

(defun dotemacs/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun dotemacs/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (if node
        (if (file-directory-p node)
            (if (neo-buffer--expanded-node-p node)
                (dotemacs/neotree-collapse)
              (neotree-select-up-node))
          (neotree-select-up-node))
      (neotree-select-up-node))))

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun dotemacs/neotree-toggle ()
  "if the neotree buffer is hidden, display and select it. If in the neotree buffer,
   hide it. Otherwise, switch to it"
  (interactive)
  (if (neo-global--window-exists-p)
      (if (eq (current-buffer) (neo-global--get-buffer))
          (neotree-hide)
        (neo-global--select-window))
    (neotree-show)))



;; Neotree on startup
(defcustom dotemacs/neotree-on-startup t
  "if non-nil neotree will be open on startup"
  :group 'dotemacs
  :type 'boolean)

(defface neotree-hidden-file-face
  '((t                   (:foreground "DimGrey")))
  "The face to use for hidden files/folders when displayed"
  :group 'neotree :group 'font-lock-highlighting-faces)

(defun dotemacs//neotree-startup ()
  (interactive)
  (neotree-show)
  (setq cursor-in-non-selected-windows nil)
  (buffer-face-set 'cursor '(:inherit 'hl-line-face))
  (hl-line-mode t)
  (internal-show-cursor nil nil)
  (nlinum-mode -1)
  (call-interactively 'other-window))

(if dotemacs/neotree-on-startup
    (if (daemonp)
        (add-hook 'server-switch-hook #'dotemacs//neotree-startup)
      (add-hook 'after-init-hook #'dotemacs//neotree-startup)))



;; Visuals

(defun dotemacs//neotree-hidden-file-p (node)
  (let ((shortname (neo-path--file-short-name node)))
    (not (null (neo-util--filter
                (lambda (x) (not (null (string-match-p x shortname))))
                neo-hidden-regexp-list)))))

(defun dotemacs//neotree-file-face (node)
  (if (dotemacs//neotree-hidden-file-p node)
      'neotree-hidden-file-face
    (if neo-vc-integration
        (cdr (neo-vc-for-node node))
      'neo-file-link-face)))

(defun dotemacs//neotree-dir-face (node)
  (if (dotemacs//neotree-hidden-file-p node)
      'neotree-hidden-file-face
    'neo-dir-link-face))

(defun dotemacs//icon-for-dir (dir &rest arg-overrides)
  (let* ((icon (all-the-icons-match-to-alist (file-name-base dir) all-the-icons-dir-icon-alist))
         (args (cdr icon))
         (path (expand-file-name dir)))
    (cond
     ((file-symlink-p path)
      (setq icon '(all-the-icons-octicon) args '("file-symlink-directory")))
     ((all-the-icons-dir-is-submodule path)
      (setq icon '(all-the-icons-octicon) args '("file-submodule")))
     ((file-exists-p (format "%s/.git" path))
      (setq icon '(all-the-icons-octicon) args '("repo")))
     ((null (car icon))
      (setq icon '(all-the-icons-octicon) args '("file-directory"))))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

(defun dotemacs//neo-buffer--insert-root-entry (node)
  (neo-buffer--node-list-set nil node)
  (let ((face '(:inherit neo-root-dir-face :height 1.5))
        (header (or (car (last (split-string node "/" t))) "/")))
    (if (display-graphic-p)
        (insert (concat (propertize " " 'face face )
                        (propertize (dotemacs//icon-for-dir node :face face :v-adjust 0.02))
                        (propertize " " 'face face ))))
    (insert (propertize (concat header "\n") 'face face 'line-height 2.5 'line-spacing 0.8))))

(defun dotemacs//get-chevron (dir &rest arg-overrides)
  (let ((args `(,(format "chevron-%s" dir) :height 0.8 :v-adjust -0.1)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply 'all-the-icons-octicon args)))

(defun dotemacs//neo-buffer--insert-fold-symbol (name &optional node-name)
  "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon.
Optional NODE-NAME is used for the `icons' theme"
  (let ((n-insert-image (lambda (n)
                          (insert-image (neo-buffer--get-icon n))))
        (n-insert-symbol (lambda (n)
                           (neo-buffer--insert-with-face
                            n 'neo-expand-btn-face))))
    (if (display-graphic-p)
        (progn
          (unless (require 'all-the-icons nil 'noerror)
            (error "Package `all-the-icons' isn't installed"))
          (setq-local tab-width 1)
          (cond ((equal name 'open)
                 (insert (format "\t%s\t%s\t"
                                 (dotemacs//get-chevron "down" :face (dotemacs//neotree-dir-face node-name))
                                 (dotemacs//icon-for-dir node-name :face (dotemacs//neotree-dir-face node-name)))))
                ((equal name 'close)
                 (insert (format "\t%s\t%s\t"
                                 (dotemacs//get-chevron "right" :face (dotemacs//neotree-dir-face node-name))
                                 (dotemacs//icon-for-dir node-name :face (dotemacs//neotree-dir-face node-name)))))
                ((equal name 'leaf)
                 (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file node-name :face (dotemacs//neotree-file-face node-name)))))))
      (or (and (equal name 'open)  (funcall n-insert-symbol "- "))
          (and (equal name 'close) (funcall n-insert-symbol "+ "))))))

(defun dotemacs//neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    (neo-buffer--insert-fold-symbol
     (if expanded 'open 'close) node)
    (insert-button (concat node-short-name)
                   'follow-link t
                   'face (dotemacs//neotree-dir-face node)
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap
                   'help-echo (neo-buffer--help-echo-message node-short-name))
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun dotemacs//neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (when neo-vc-integration (neo-vc-for-node node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char (car vc))
      (insert-char ?\s))
    (neo-buffer--insert-fold-symbol 'leaf node-short-name)
    (insert-button node-short-name
                   'follow-link t
                   'face (dotemacs//neotree-file-face node)
                   'neo-full-path node
                   'keymap neotree-file-button-keymap
                   'help-echo (neo-buffer--help-echo-message node-short-name))
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


(require 'all-the-icons)
(advice-add #'neo-buffer--insert-root-entry :override #'dotemacs//neo-buffer--insert-root-entry)
(advice-add #'neo-buffer--insert-fold-symbol :override #'dotemacs//neo-buffer--insert-fold-symbol)
(advice-add #'neo-buffer--insert-dir-entry :override #'dotemacs//neo-buffer--insert-dir-entry)
(advice-add #'neo-buffer--insert-file-entry :override #'dotemacs//neo-buffer--insert-file-entry)


;; Settings
(setq neo-window-width 32
      neo-create-file-auto-open t
      neo-show-updir-line nil
      neo-mode-line-type 'default
      neo-smart-open nil
      neo-dont-be-alone t
      neo-persist-show nil
      neo-show-hidden-files nil
      neo-auto-indent-point t
      neo-modern-sidebar t
      neo-vc-state-char-alist `((up-to-date       . ?\s)
                                (edited           . ?\s)
                                (added            . ?\s)
                                (removed          . ?\s)
                                (missing          . ?\s)
                                (needs-merge      . ?\s)
                                (conflict         . ?\s)
                                (unlocked-changes . ?\s)
                                (needs-update     . ?\s)
                                (ignored          . ?\s)
                                (user             . ?\s)
                                (unregistered     . ?\s)
                                (nil              . ?\s)))



(require 'def-bindings)
;; Bindings

(evilified-state--evilified-state-on-entry)
(define-evilified-keys neotree-mode-map
  ("RET" 'dotemacs/neotree-expand-or-open "open")
  ("TAB" 'neotree-stretch-toggle "shring/enlarge")
  ("|" 'neotree-enter-vertical-split "vertical split")
  ("-" 'neotree-enter-horizontal-split "horizontal split")
  ("'" 'neotree-quick-look "quick look")
  ("c" 'neotree-create-node "create")
  ("C" 'neotree-copy-node "copy")
  ("d" 'neotree-delete-node "delete")
  ;; ("gr" ' neotree-refresh "refresh")
  ("h" 'dotemacs/neotree-collapse-or-up "up/collapse")
  ("H" 'neotree-select-previous-sibling-node "previous sibling")
  ("j" 'neotree-next-line "line down")
  ("J" 'neotree-select-down-node "goto child")
  ("k" 'neotree-previous-line "line up")
  ("K" 'neotree-select-up-node "goto parent")
  ("l" 'dotemacs/neotree-expand-or-open "open/expand")
  ("L" 'neotree-select-next-sibling-node "next sibling")
  ("r" 'neotree-rename-node "rename")
  ("R" 'neotree-change-root "change root")
  ("q" 'neotree-hide "close")
  ("s" 'neotree-hidden-file-toggle
   '(if neo-buffer--show-hidden-file-p "[x] hidden files" "[ ] hidden files")))

(provide 'init-neotree)
