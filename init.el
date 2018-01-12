(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(defun dotemacs//load-file (file)
  (condition-case ex
      (load file)
    ('error (with-current-buffer "*scratch*"
              (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex))))))

(defun dotemacs//load-theme (theme)
  (condition-case ex
      (load-theme theme)
    ('error (with-current-buffer "*scratch*"
              (insert (format "[INIT ERROR]\n%s\n%s\n\n" theme ex))))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (config-directory (concat user-emacs-directory "config/")))

  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (unless (display-graphic-p) (menu-bar-mode -1))

  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)

  (add-to-list 'load-path config-directory)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (dotemacs//load-file custom-file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "\\(init.*\\)\\.elc$" file)
           do (dotemacs//load-file file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "keys.*\\.elc$" file)
           do (dotemacs//load-file file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "lang-.*\\.elc$" file)
           do (dotemacs//load-file file))

  ;; Reload themes
  (dotemacs//load-theme 'flatblue))
