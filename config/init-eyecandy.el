(defgroup dotemacs-eyecandy nil
  "Configuration options for eye candy."
  :group 'dotemacs
  :prefix 'dotemacs-eyecandy)

(defcustom dotemacs-eyecandy/mode-line
  'spaceline
  "List of hooks to automatically start up in Evil Emacs state."
  :type '(radio
          (const :tag "smart mode line" sml)
          (const :tag "telephone line" telephone-line)
          (const :tag "spaceline" spaceline))
  :group 'dotemacs-eyecandy)



(when (eq dotemacs-pair-engine 'emacs)
  (show-paren-mode)
  (setq show-paren-delay 0))


(require-package 'nlinum)
(column-number-mode t)
(global-nlinum-mode t)
(display-time-mode t)
(size-indication-mode t)

(defun my-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col (save-excursion
                 (move-end-of-line 0)
                 (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "%s [ %d ] ... "
                           (make-string (- (window-width) col 32) (string-to-char "."))
                           count)))))
(setq hs-set-up-overlay 'my-fold-overlay)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

;; (add-hook 'prog-mode-hook
;;           #'add-fira-code-symbol-keywords)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)



(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode "a"))
(after 'company (diminish 'company-mode "C"))
(after 'projectile (diminish 'projectile-mode ""))
(after 'yasnippet (diminish 'yas-minor-mode "Y"))
(after 'which-key (diminish 'which-key-mode))
(after 'eldoc (diminish 'eldoc-mode "e"))
(after 'smartparens (diminish 'smartparens-mode "p"))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'highlight-symbol (diminish 'highlight-symbol-mode "H"))
(after 'indent-guide (diminish 'indent-guide-mode "I"))
(after 'hideshow (diminish 'hs-minor-mode))
(after 'ivy (diminish 'ivy-mode))
(after 'helm-mode (diminish 'helm-mode))
(after 'evil-commentary (diminish 'evil-commentary-mode "c"))
(after 'page-break-lines (diminish 'page-break-lines-mode ))
(after 'flycheck (diminish 'flycheck-mode "s"))
(after 'flyspell (diminish 'flyspell-mode "S"))
(after 'aggressive-indent (diminish 'aggressive-indent-mode))
(after 'counsel (diminish #'counsel-mode))
(after 'window-purpose (diminish 'purpose-mode))


(defun dotemacs/setup-modeline ()
  (pcase dotemacs-eyecandy/mode-line
    ('sml (progn
            (require-package 'smart-mode-line)
            (sml/setup)
            (after 'evil
              (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
              (defadvice evil-set-cursor-color (after dotemacs activate)
                (cond ((evil-emacs-state-p)
                       (set-face-background 'mode-line "#440000"))
                      ((evil-insert-state-p)
                       (set-face-background 'mode-line "#002244"))
                      ((evil-visual-state-p)
                       (set-face-background 'mode-line "#440044"))
                      (t
                       (set-face-background 'mode-line dotemacs--original-mode-line-bg)))))))
    ('spaceline (progn
                  (require-package 'spaceline)
                  (require 'spaceline-config)
                  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
                  (dotemacs-spaceline/setup)
                  (after "helm-autoloads"
                    (spaceline-helm-mode))))
    ('telephone-line (after 'all-the-icons
                       (telephone-line-mode 1)))))


(dotemacs/setup-modeline)


(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . 955) prettify-symbols-alist)
              (push '("return" . 8592) prettify-symbols-alist))))


(delayed-init
 (require-package 'color-identifiers-mode)
 (global-color-identifiers-mode -1)
 (diminish 'color-identifiers-mode))


(require-package 'all-the-icons)

;; (require-package 'fancy-narrow)
;; (fancy-narrow-mode)


(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
;; (add-hook 'prog-mode-hook 'highlight-symbol-mode)


(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)


(require-package 'page-break-lines)
(global-page-break-lines-mode)


(add-hook 'find-file-hook 'hl-line-mode)


;; Theme packages
(require-package 'gruvbox-theme)


(provide 'init-eyecandy)
