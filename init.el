(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (config-directory (concat user-emacs-directory "config/")))

  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (unless (display-graphic-p) (menu-bar-mode -1))

  (defgroup dotemacs nil
    "Custom configuration for dotemacs."
    :group 'local)

  (defcustom dotemacs-cache-directory (concat user-emacs-directory ".cache/")
    "The storage location for various persistent files."
    :group 'dotemacs)

  (defcustom dotemacs-completion-engine
    'company
    "The completion engine the use."
    :type '(radio
            (const :tag "company-mode" company)
            (const :tag "auto-complete-mode" auto-complete))
    :group 'dotemacs)

  (defcustom dotemacs-switch-engine
    'helm
    "The primary engine to use for narrowing and navigation."
    :type '(radio
            (const :tag "helm" helm)
            (const :tag "ido" ido)
            (const :tag "ivy" ivy))
    :group 'dotemacs)

  (defcustom dotemacs-pair-engine
    'emacs
    "The primary engine to use auto-pairing and parens matching."
    :type '(radio
            (const :tag "emacs" emacs)
            (const :tag "smartparens" smartparens))
    :group 'dotemacs)

  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)

  (require 'init-boot (concat config-directory "init-boot.el"))

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "def.*\\.el$" file)
           do (load file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "init.*\\.el$" file)
           do (load file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "keys.*\\.el$" file)
           do (load file))

  (cl-loop for file in (directory-files config-directory t)
           when (string-match "lang-.*\\.el$" file)
           do (load file)))

;; do (condition-case ex
;;        (load file)
;;      ('error (with-current-buffer "*scratch*"
;;                (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))))
