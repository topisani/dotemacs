(defgroup dotemacs-vcs nil
  "Configuration options for version control."
  :group 'dotemacs
  :prefix 'dotemacs-vcs)

(defcustom dotemacs-vcs/inhibit-vc-integration (eq system-type 'windows-nt)
  "When non-nil, disables integration with `vc.el'.
This is non-nil by default on Windows machines, where this is a heavy performance cost."
  :type 'boolean
  :group 'dotemacs-vcs)



(when dotemacs-vcs/inhibit-vc-integration
  (after 'vc-hooks
    (setq vc-handled-backends nil)))

(setq vc-make-backup-files t)




(if (display-graphic-p)
    (progn
      (require-package 'git-gutter-fringe+)
      (require 'git-gutter-fringe+))
  (require-package 'git-gutter+))
(global-git-gutter+-mode)



(require-package 'diff-hl)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(unless (display-graphic-p)
  (diff-hl-margin-mode))



(after 'eshell
  (require-package 'pcmpl-git)
  (require 'pcmpl-git))



(require-package 'with-editor)
(autoload 'with-editor-export-editor "with-editor")
(defun init-vcs/export-with-editor ()
  (with-editor-export-editor)
  (message ""))
(add-hook 'shell-mode-hook #'init-vcs/export-with-editor)
(add-hook 'term-exec-hook #'init-vcs/export-with-editor)
(add-hook 'eshell-mode-hook #'init-vcs/export-with-editor)



(lazy-major-mode "^\\.gitignore$" gitignore-mode)
(lazy-major-mode "^\\.gitattributes$" gitattributes-mode)



(evilify diff-mode diff-mode-map
  "j" #'diff-hunk-next
  "k" #'diff-hunk-prev)
(evilify vc-annotate-mode vc-annotate-mode-map
  (kbd "M-p") #'vc-annotate-prev-revision
  (kbd "M-n") #'vc-annotate-next-revision
  "l" #'vc-annotate-show-log-revision-at-line
  "J" #'vc-annotate-revision-at-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require-package 'magit)

(setq magit-section-show-child-count t)
(setq magit-diff-arguments '("--histogram"))
(setq magit-ediff-dwim-show-on-hunks t)

;; Based on spacemacs

(setq magit-completing-read-function
      (if (eq dotemacs-completion-engine 'ivy)
          'ivy-completing-read
        'magit-builtin-completing-read))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; On Windows, we must use Git GUI to enter username and password
;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
(when (eq window-system 'w32)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

;; key bindings
(dotemacs/describe-leader-key "gd" "diff")
(dotemacs/describe-leader-key "gf" "file")

(define-leader
  ;; "gb"  'spacemacs/git-blame-micro-state
  ("gfh" 'magit-log-buffer-file)
  ("gm"  'magit-dispatch-popup)
  ("gs"  'magit-status)
  ("gS"  'magit-stage-file)
  ("gU"  'magit-unstage-file))

;; seems to be necessary at the time of release
(require 'git-rebase)
(-define-key magit-mode-map "<tab>" 'magit-section-toggle "toggle selection")

(provide 'init-vcs)
