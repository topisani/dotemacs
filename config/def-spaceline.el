;;; spaceline-all-the-icons.el --- Custom install for all the icons Spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'spaceline)
(require 'spaceline-config)
(require 'all-the-icons)

;;---------------;;
;; First Segment ;;
;;---------------;;

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(spaceline-define-segment
    ati-modified "An `all-the-icons' modified segment"
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))

      (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit )))
    :tight t)

(spaceline-define-segment
    ati-window-numbering "An `all-the-icons' window numbering segment"
    (propertize (format "%d" (winum-get-number))
                'face `(:height 0.97 :inherit)
                'display '(raise 0.05))
    :when (fboundp 'winum-mode))

(spaceline-define-segment
    ati-perspective "Shows the current perspective and lets you click to switch"
    (propertize (format " %s " (safe-persp-name (get-current-persp)))
                'face '(:height 0.9 :inherit)
                'display '(raise 0.12)
                'help-echo "Switch perspective"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (persp-next))))
    :tight t :when (and (fboundp 'get-current-persp) (fboundp 'safe-persp-name)))

(spaceline-define-segment
    ati-mode-icon "An `all-the-icons' segment for the current buffer mode"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize icon
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0.0)
                    'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

(spaceline-define-segment
    ati-buffer-id "An `all-the-icons' segment for the current buffer id"
    (if (fboundp 'projectile-project-root)
        (let* ((buf (or (buffer-file-name) (buffer-name)))
               (proj (ignore-errors (projectile-project-root)) )
               (name (if (buffer-file-name)
                         (or (cadr (split-string buf proj))
                             (format-mode-line "%b"))
                       (format-mode-line "%b"))))
          (propertize (format "%s" name)
                      'face `(:height 0.8 :inherit)
                      'display '(raise 0.12)
                      'help-echo (format "Major-mode: `%s`" major-mode)))
      (propertize (format-mode-line "%b ") 'face '(:height 0.8 :inherit) 'display '(raise 0.12))))

(spaceline-define-segment
    ati-process "An `all-the-icons' segment for the current process"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (concat
       (when (or (symbolp icon) mode-line-process)
         (propertize (format-mode-line "%m") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))
       (when mode-line-process
         (propertize (format-mode-line mode-line-process) 'face '(:height 0.8 :inherit) 'display '(raise 0.2))))))

(spaceline-define-segment
    ati-position "An `all-the-icons' segment for the Row and Column of the current point"
    (propertize (format-mode-line "%l:%c") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
    :when (not (bound-and-true-p anzu--state)))

(spaceline-define-segment
    ati-anzu "Anzu"
    (concat 
     (propertize (format "%s " (all-the-icons-octicon "search"))
                 'face `(:height 0.9 :family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.16))
     (propertize (anzu--update-mode-line)
                 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))
    :when (bound-and-true-p anzu--state))

(spaceline-define-segment
    ati-region-info "An `all-the-icons' segment for the currently marked region"
    (when mark-active
      (let ((lines (count-lines (region-beginning) (region-end)))
            (chars (- (region-end) (region-beginning))))
        (concat
         (propertize (format "%s " (all-the-icons-octicon "pencil"))
                     'face `(:height 0.9 :family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.16))
         (propertize (format "(%s, %s)" lines chars)
                     'face `(:height 0.9 :inherit) 'display '(raise 0.1))))))


;;----------------;;
;; Third Segement ;;
;;----------------;;

(defun spaceline---github-vc ()
  "Function to return the Spaceline formatted GIT Version Control text."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (propertize (concat
                 ;; (propertize (all-the-icons-alltheicon "git") 'face '(:height 1.1 :inherit) 'display '(raise 0.1))
                 ;; (propertize " · ")
                 (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                             'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                             'display '(raise 0.13))
                 (propertize (format " %s " branch) 'face `(:height 0.9 :inherit) 'display '(raise 0.12)))
                'help-echo "Git status"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (magit-status))))))

(defun spaceline---svn-vc ()
  "Function to return the Spaceline formatted SVN Version Control text."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     ;; (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format "%s" revision) 'face `(:height 0.9)))))


(spaceline-define-segment
    ati-vc-icon "An `all-the-icons' segment for the current Version Control icon"
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode) (spaceline---github-vc))
            ((string-match "SVN-" vc-mode) (spaceline---svn-vc))
            (t (propertize (format "%s" vc-mode)))))
    :when active)

(spaceline-define-segment
    ati-flycheck-status "An `all-the-icons' representaiton of `flycheck-status'"
    (let* ((text
            (pcase flycheck-last-status-change
              (`finished (if flycheck-current-errors
                             (let-alist (flycheck-count-errors flycheck-current-errors)
                               (format "%s %s"
                                       (if .error
                                           (concat
                                            (propertize
                                             (format " %s"
                                                     (all-the-icons-octicon "bug"))
                                             'face `(:family ,(all-the-icons-octicon-family)
                                                             :foreground ,(face-attribute 'spaceline-flycheck-error :foreground)
                                                             :height 0.9))
                                            (propertize
                                             (format " %s "
                                                     .error)
                                             'face `(:foreground ,(face-attribute 'spaceline-flycheck-error :foreground)
                                                                 :height 0.9))
                                            )
                                         "")
                                       (if .warning
                                           (concat
                                            (propertize
                                             (format " %s"
                                                     (all-the-icons-octicon "alert"))
                                             'face `(:family ,(all-the-icons-octicon-family)
                                                             :foreground ,(face-attribute 'spaceline-flycheck-warning :foreground)
                                                             :height 0.8))
                                            (propertize
                                             (format " %s "
                                                     .warning)
                                             'face `(:foreground ,(face-attribute 'spaceline-flycheck-warning :foreground)
                                                                 :height 0.9))
                                            )
                                         "")))
                           (propertize " ✔ "               'face `(:height 0.8))))
              (`running     (propertize " ⟲ "              'face `(:height 0.8)))
              (`no-checker  (propertize " ⚠ No Checker "   'face `(:height 0.8)))
              (`not-checked (propertize ""                 'face `(:height 0.8)))
              (`errored     (propertize " ⚠ Error "        'face `(:height 0.8)))
              (`interrupted (propertize " ⛔ Interrupted " 'face `(:height 0.8)))
              (`suspicious  (propertize ""                 'face `(:height 0.8))))))
      (propertize (format  "%s" text)
                  'help-echo "Show Flycheck Errors"
                  'display '(raise 0.12)
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    :when active)

(defvar spaceline--upgrades nil)
(defun spaceline--count-upgrades ()
  "Function to count the number of package upgrades needed."
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq spaceline--upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'spaceline--count-upgrades)

(spaceline-define-segment
    ati-package-updates "An `all-the-icons' spaceline segment to indicate number of package updates needed"
    (let ((num (or spaceline--upgrades (spaceline--count-upgrades))))
      (propertize
       (concat
        (propertize (format " %s" (all-the-icons-octicon "package"))
                    'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                    'display '(raise 0.1))
        (propertize (format " %d updates " num) 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))
       'help-echo "Open Packages Menu"
       'mouse-face '(:box 1)
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (package-list-packages)))))
    :when (and active (> (or spaceline--upgrades (spaceline--count-upgrades)) 0)))


(spaceline-define-segment
    ati-time "Time"
    (let* ((hour (string-to-number (format-time-string "%I")))
           (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.0)))
      (concat
       (propertize (format-time-string "%H:%M ") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
       (propertize (format "%s" icon)
                   'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit)
                   'display '(raise 0.1)))))

(spaceline-define-segment
    ati-height-modifier "Modifies the height of inactive buffers"
    (propertize " " 'face '(:height 1.3 :inherit))
    :when (not active))

(spaceline-define-segment
    ati-buffer-size "Buffer Size"
    (propertize (format-mode-line "%I") 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))

(defun purpose-cycle-dedicated ()
  (pcase `(,(purpose-window-purpose-dedicated-p) . ,(window-dedicated-p))
    ('(nil . nil) (set-window-dedicated-p (selected-window) t))
    ('(nil . t) (purpose-set-window-purpose-dedicated-p (selected-window) t))
    ('(t . t) (set-window-dedicated-p (selected-window) nil))
    ('(t . nil) (purpose-set-window-purpose-dedicated-p (selected-window) nil))))

(spaceline-define-segment
    ati-window-purpose "Window purpose"
    (let ((text (replace-regexp-in-string "^.*\\[\\(.*\\)\\].*$" "\\1" (purpose--modeline-string)))
          (help (pcase `(,(purpose-window-purpose-dedicated-p) . ,(window-dedicated-p))
                  ('(nil . nil) "Window purpose")
                  ('(nil . t) "Dedicated to buffer")
                  ('(t . t) "Dedicated to buffer and purpose")
                  ('(t . nil) "Dedicated to purpose"))
                ))
      (propertize text 'face `(:height 0.9 :inherit) 'display '(raise 0.1)
                  'help-echo help
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (purpose-cycle-dedicated))))))

(spaceline-define-segment
    ati-battery-status "Show battery information"
    (let* ((charging? (equal "AC" (cdr (assoc ?L fancy-battery-last-status))))
           (percentage (string-to-int (cdr (assoc ?p fancy-battery-last-status))))
           (time (format "%s" (cdr (assoc ?t fancy-battery-last-status))))
           (icon-set (if charging? 'alltheicon 'faicon))
           (icon-alist
            (cond
             (charging? '((icon . "charging") (inherit . success) (height . 1.3) (raise . -0.1)))
             ((> percentage 95) '((icon . "full") (inherit . success)))
             ((> percentage 70) '((icon . "three-quarters")))
             ((> percentage 35) '((icon . "half")))
             ((> percentage 15) '((icon . "quarter") (inherit . warning)))
             (t '((icon . "empty") (inherit . error)))))
           (icon-f (all-the-icons--function-name icon-set))
           (family (funcall (all-the-icons--family-name icon-set))))
      (let-alist icon-alist
        (concat
         (if .inherit
             (let ((fg (face-attribute .inherit :foreground)))
               (propertize (funcall icon-f (format "battery-%s" .icon))
                           'face `(:height ,(or .height 1.0) :family ,family :foreground ,fg)
                           'display `(raise ,(or .raise 0.0))))
           (propertize (funcall icon-f (format "battery-%s" .icon))
                       'face `(:family ,family :inherit)
                       'display '(raise 0.0)))
         " "
         (if .inherit
             (let ((fg (face-attribute .inherit :foreground)))
               (propertize (if charging? (format "%s%%%%" percentage) time) 'face `(:height 0.9 :foreground ,fg)))
           (propertize time 'face '(:height 0.9 :inherit)))
         )))
    :global-override fancy-battery-mode-line :when (and active (fboundp 'fancy-battery-mode) fancy-battery-mode))

(defmacro pl/rightslant (dir)
  "Generate a slant XPM function for DIR."
  (if (eq dir 'left)
      (let* ((row-modifier 'identity ))
        (pl/wrap-defun "rightslant" dir 'width
                       '((width (1- (ceiling height 2))))
                       `((cl-loop for i downfrom (1- height) to 0
                                  concat (pl/pattern-to-string (,row-modifier (pl/row-pattern (/ i 2) width)))))
                       `((cl-loop for i downfrom (1- (* height 2)) to 0
                                  concat (pl/pattern-to-string (,row-modifier (pl/row-pattern (/ i 2) (* width 2))))))))
    (let* ((row-modifier 'reverse))
      (pl/wrap-defun "rightslant" dir 'width
                     '((width (1- (ceiling height 2))))
                     `((cl-loop for i from 0 to (1- height) 
                                concat (pl/pattern-to-string (,row-modifier (pl/row-pattern (/ i 2) width)))))
                     `((cl-loop for i from 0 to (1- (* height 2))
                                concat (pl/pattern-to-string (,row-modifier (pl/row-pattern (/ i 2) (* width 2)))))))))) 

(pl/memoize (pl/rightslant left))
(pl/memoize (pl/rightslant right))

(setq powerline-default-separator-dir '(left . left))
(setq powerline-default-separator nil)
(setq powerline-height 20)
(setq mode-line-default-help-echo "")

(defun dotemacs-spaceline/setup ()
  (after 'spaceline
    (spaceline-compile
      "ati"
      '(((ati-perspective ati-window-numbering) :face highlight-face)
        ((ati-mode-icon ati-buffer-id) :face default-face)
        ((ati-process ati-position ati-anzu) :face highlight-face
         :separator (propertize " | " 'display '(raise 0.1)))
        ((ati-vc-icon ati-flycheck-status ati-package-updates) :separator ""))

      '(((ati-window-purpose) :separator "" :face other-face :tight-right t)))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))))

(provide 'def-spaceline)
;;; spaceline-all-the-icons.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
