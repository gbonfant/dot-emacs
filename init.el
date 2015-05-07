; Load the package manager
(require 'package)

; Add MELPA repository for packages source
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

; Initialise the package manager
(package-initialize)

; ------------------------------------------
; Install packages if they are not installed
; ------------------------------------------
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil company))

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
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
