(require 'dotemacs-common)

(after 'org
  (defgroup dotemacs-org nil
    "Configuration options for org-mode."
    :group 'dotemacs
    :prefix 'dotemacs-org)

  (defcustom dotemacs-org/journal-file (concat org-directory "/journal.org")
    "The path to the file where you want to make journal entries."
    :type 'file
    :group 'dotemacs-org)

  (defcustom dotemacs-org/inbox-file (concat org-directory "/inbox.org")
    "The path to the file where to capture notes."
    :type 'file
    :group 'dotemacs-org)

  (unless (file-exists-p org-directory)
    (make-directory org-directory))

  (setq org-default-notes-file (expand-file-name dotemacs-org/inbox-file))
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 2)
  (setq org-fontify-emphasized-text t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)

  (setq org-agenda-files `(,org-directory))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (expand-file-name dotemacs-org/inbox-file) "INBOX")
           "* TODO %?\n%U\n%a\n")
          ("n" "Note" entry (file+headline (expand-file-name dotemacs-org/inbox-file) "NOTES")
           "* %? :NOTE:\n%U\n%a\n")
          ("m" "Meeting" entry (file (expand-file-name dotemacs-org/inbox-file))
           "* MEETING %? :MEETING:\n%U")
          ("j" "Journal" entry (file+datetree (expand-file-name dotemacs-org/journal-file))
           "* %U\n** %?")))

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d@)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-todo-state-tags-triggers
        ' (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("TODO" ("WAITING") ("CANCELLED"))
           ("NEXT" ("WAITING") ("CANCELLED"))
           ("DONE" ("WAITING") ("CANCELLED"))))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido t)

  (when (boundp 'org-plantuml-jar-path)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((plantuml . t))))

  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  (defun my-org-mode-hook ()
    (toggle-truncate-lines t))
  (add-hook 'org-mode-hook #'my-org-mode-hook)

  (require-package 'ob-async)

  (defun endless/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  (add-hook 'org-mode-hook #'endless/org-ispell)

  (require-package 'org-bullets)
  (setq org-bullets-bullet-list '("●" "○" "◆" "◇" "▸"))
  (add-hook 'org-mode-hook #'org-bullets-mode))

(require-package 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
;;(evil-org-set-key-theme '(navigation insert textobjects additional))

;; (require 'ob-plantuml)
;; (after 'ob-plantuml
;;   (when (executable-find "npm")
;;     (let ((default-directory (concat user-emacs-directory "/extra/plantuml-server/")))
;;       (unless (file-exists-p "node_modules/")
;;         (shell-command "npm install"))

;;       (ignore-errors
;;         (let ((kill-buffer-query-functions nil))
;;           (kill-buffer "*plantuml-server*")))
;;       (start-process "*plantuml-server*" "*plantuml-server*" "npm" "start"))

;;     (defun init-org/generate-diagram (uml)
;;       (let ((url-request-method "POST")
;;             (url-request-extra-headers '(("Content-Type" . "text/plain")))
;;             (url-request-data uml))
;;         (let* ((buffer (url-retrieve-synchronously "http://localhost:8182/png")))
;;           (with-current-buffer buffer
;;             (goto-char (point-min))
;;             (search-forward "\n\n")
;;             (buffer-substring (point) (point-max))))))

;;     (defun org-babel-execute:plantuml (body params)
;;       (let* ((out-file (or (cdr (assoc :file params))
;;                            (error "PlantUML requires a \":file\" header argument"))))
;;         (let ((png (init-org/generate-diagram
;;                     (concat "
;; @startuml

;; skinparam defaultFontName Hack
;; skinparam shadowing false
;; skinparam monochrome true

;; " body "
;; @enduml"))))
;;           (with-temp-buffer
;;             (insert png)
;;             (write-file out-file)))))))

(provide 'init-org)
