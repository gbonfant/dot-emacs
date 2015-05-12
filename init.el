; Load the package manager
(require 'package)

; Add MELPA repository for packages source
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

; Initialise the package manager
(package-initialize)

; Install packages if they are not installed
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil solarized-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Enable evil mode
(require 'evil)
(evil-mode t)

; ------------------------------------------
; Functions
; ------------------------------------------
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

; ------------------------------------------
; Key mappings
; ------------------------------------------
; Escape as a universal cancel
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

; ------------------------------------------
; Appearance
; ------------------------------------------
; Don't display the ugly startup message (particularly ugly in the GUI)
(setq inhibit-startup-message t)

; No toolbar
(tool-bar-mode -1)

; Enable line numbers
(global-linum-mode 1)


; Get rid of the butt ugly OSX scrollbars in GUI
(when (display-graphic-p) (set-scroll-bar-mode nil))

; Use solarized dark (in GUI)
(when (display-graphic-p) (load-theme 'solarized-dark t))

; Use Incosolata 14pt in GUI
(when (display-graphic-p) (set-face-attribute 'default nil :font "Inconsolata-16"))

; Set cursor colours depending on mode
(when (display-graphic-p)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow)))

(custom-set-variables
  ; Start in fullscreen
  '(initial-frame-alist (quote ((fullscreen . maximized)))))
