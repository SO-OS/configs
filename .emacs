;; BINDING
;; Naming current buffer
(global-set-key (kbd "<f1>") 'rename-buffer)
;; Shortcut for eshell
(global-set-key (kbd "<f3>") 'eshell)
;; F11 = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; When use GNOME
(setq-default font-use-system-font t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set tab to display as 4 spaces
(setq-default tab-width 4)

;; Set stop-tabs to be 4 written as spaces
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Set default tab space for various modes
(setq-default sgml-basic-offset 4)
(setq-default py-indent-offset 4)
(setq-default python-indent 4)

;; Display battery percent
(display-battery-mode t)

;; Display time and date in status bar
(setq display-time-day-and-date t
    display-time-24hr-format t)

;; Colortheme
;; (load-theme 'deeper-blue)

;; Automatically show completion
(icomplete-mode 1)

;; cscope support
;; (add-to-list 'load-path "~/.emacs.d/extend" t)
;; (require 'xcscope)

;; c-mode add funcs by hook (* from pluskid *)
(add-hook 'c-mood-hook
	  '(lambda ()
	     (c-toggle-auto-newline 1)
	     ;; Backspace to delete space amsp
	     (c-toggle-hungry-state)
	     ;; show func name where the cursor is
	     (which-function-mode t)
	     (auto-fill-mode t)
	     ;; no using tab to indent
	     (setq indent-tabs-mode nil)
	     (c-subword-mode 1)
	     (smartparens-mode 1)))

;; Command to add a cool box (* from pluskid *)
(defun kid-cool-box (title begin end)
  "Wrap the region with a cool box.
The result is like this:
,-------------[ Title ]
| This is the marked region
| that will be boxed
`-------------
"
  (interactive "sTitle: \nr")
  (setq end (copy-marker end t))
  (save-excursion
    (goto-char begin)
    (unless (looking-back "^")
      (insert "\n"))
    (insert ",----------[ ")
    (insert title)
    (insert " ]\n")
    (while (< (point) end)
      (insert "| ")
      (next-line)
      (beginning-of-line))
    (goto-char end)
    (unless (looking-back "^")
      (insert "\n"))
    (insert "`----------\n")))

;; move mouse away when cursor around
(mouse-avoidance-mode 'jump)

;; do not back file
(setq-default make-backup-files nil)

(require 'uniquify)
(setq unifuify-buffer-name-style 'forward)

;; use extra package archive
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; 'y or n' instead of 'yes or no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Nightlight regions and add special behaviors to regions
;;"C-h d transient" for more info
(setq transient-mark-mode t)

;; Show the line number
;;(global-linum-mode t)

;; No toolbar
(tool-bar-mode -1)

;; No menubar
(menu-bar-mode -1)

;;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Visible-bell for beep
(setq visible-bell t)

;; Show matching parens
(show-paren-mode t)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; This is the binary name of my scheme
(setq scheme-program-name "guile")


;; easy-kill
;; (global-set-key [remap kill-ring-save] 'easy-kill)

;; For Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
