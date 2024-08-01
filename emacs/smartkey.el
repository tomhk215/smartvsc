(require 'smartkey)

(global-smart-mode t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Solving conflict between company mode and yasnippet  
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (region-active-p) (looking-back "^[[:space:]]*"))
          (indent-for-tab-command)
        (if (looking-at "[[:space:]]\\|$")
            (if (or (not yas/minor-mode)(null (do-yas-expand)))
                (if (check-expansion)
                    (company-complete-common)
                  (indent-for-tab-command)))))))
(define-key smartmap (kbd "<tab>" ) 'tab-indent-or-complete)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; -----------------------------------------------------------------------
;; Temp keybind
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(define-key smartmap (kbd "<f8>") 'check-parens)
(define-key smartmap (kbd "<f9>") 'eval-buffer)
(define-key smartmap (kbd "<f10>") 'paradox-list-packages)
(define-key smartmap (kbd "<f11>") 'restart-emacs)
;; -----------------------------------------------------------------------


;; -----------------------------------------------------------------------
;; Keybind for other packages ar mode
;; Keybind for help
(define-key smartmap (kbd "<f12> C-k") 'describe-keymap)


;; Keybind for smartparens
(define-key smartmap (kbd "C-u") 'sp-beginning-of-sexp)
(define-key smartmap (kbd "C-o") 'sp-end-of-sexp)

;; Keybind for multiple-cursors
(define-key smartmap (kbd "C-.") 'mc/mark-next-like-this)
(define-key smartmap (kbd "C-,") 'mc/mark-previous-like-this)
(define-key smartmap (kbd "M-.") 'mc/unmark-next-like-this)
(define-key smartmap (kbd "M-,") 'mc/unmark-previous-like-this)
(define-key smartmap (kbd "C-M-.") 'mc/skip-to-next-like-this)
(define-key smartmap (kbd "C-M-,") 'mc/skip-to-previous-like-this)
(define-key smartmap (kbd "C-/") 'mc/mark-all-like-this)
(define-key smartmap (kbd "M-/") 'mc/mark-all-dwim)

;; Keybind for helm
(define-key smartmap (kbd "C-a") 'helm-find-files)
(define-key smartmap (kbd "M-a") 'helm-recentf)
(define-key smartmap (kbd "M-e") 'helm-show-kill-ring)
(define-key smartmap (kbd "M-x") 'helm-M-x)

;; Keybind for projectile
(define-key smartmap (kbd "C-p") nil)
(define-key smartmap (kbd "C-p C-p") 'helm-projectile)
(define-key smartmap (kbd "C-p C-a") 'helm-projectile-find-file)
(define-key smartmap (kbd "C-p C-d") 'projectile-dired)

;; Keybinc for ecb
(define-key smartmap (kbd "C-x C-e") 'ecb-activate)
(with-eval-after-load "ecb"
  (define-key smartmap (kbd "C-x C-e") 'ecb-toggle-ecb-windows))
(defun smart-ecb-change-window-enable ()
  (interactive)
  (define-key smartmap (kbd "M-1") 'ecb-goto-window-edit1)
  (define-key smartmap (kbd "M-2") 'ecb-goto-window-directories)
  (define-key smartmap (kbd "M-3") 'ecb-goto-window-sources)
  (define-key smartmap (kbd "M-4") 'ecb-goto-window-methods)
  (define-key smartmap (kbd "M-5") 'ecb-goto-window-history)
  (define-key smartmap (kbd "M-6") 'ecb-goto-window-compilation))
(defun smart-ecb-change-window-disable ()
  (interactive)
  (define-key smartmap (kbd "M-1") 'other-window)
  (define-key smartmap (kbd "M-2") 'windmove-left)
  (define-key smartmap (kbd "M-3") 'windmove-right)
  (define-key smartmap (kbd "M-4") 'digit-argument)
  (define-key smartmap (kbd "M-5") 'digit-argument))
(add-hook 'ecb-activate-hook 'smart-ecb-change-window-enable)
(add-hook 'ecb-deactivate-hook 'smart-ecb-change-window-disable)
(add-hook 'ecb-show-ecb-windows-after-hook 'smart-ecb-change-window-enable)
(add-hook 'ecb-hide-ecb-windows-after-hook 'smart-ecb-change-window-disable)


;; Keybind for flycheck
(define-key smartmap (kbd "C-'") 'flycheck-next-error)
(define-key smartmap (kbd "M-'") 'flycheck-previous-error)
(define-key smartmap (kbd "C-M-'") 'flycheck-list-errors)

;; Keybind for engine-mode
(define-key smartmap (kbd "C-x C-f") 'engine/search-google)

;; Keybind for helm-swoop
(define-key smartmap (kbd "C-f") 'helm-swoop)
(define-key smartmap (kbd "M-f") 'helm-multi-swoop)
(defun helm-swoop-multiline (arg)
  (interactive "nNumber of line: ")
  ;; run after exit the current minibuffer operation
  (run-with-timer
   0 nil (funcall (lambda () (helm-swoop
                     :$query ""
                     :$multiline arg) arg)))
  (exit-minibuffer))
(define-key smartmap (kbd "C-M-f") 'helm-swoop-multiline)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Overriding keybind

;; Override keybind for package menu mode
(defun smart-package-menu-override ()
  (smartoverride (kbd "<return>") 'package-menu-describe-package)
  (smartoverride (kbd "M-g") 'quit-window))
(add-hook 'package-menu-mode-hook 'smart-package-menu-override)

;; Override keybind for help mode
(defun smart-help-mode-override ()
  (smartoverride (kbd "<return>") (lambda ()
                    (interactive) (unless (push-button) (help-follow))))
  (smartoverride (kbd "<tab>") 'forward-button)
  (smartoverride (kbd "M-g") 'quit-window))
(add-hook 'help-mode-hook 'smart-help-mode-override)

;; Override keybind for flycheck list mode
(defun smart-flycheck-list-override ()
  (smartoverride (kbd "C-i") 'flycheck-error-list-previous-error)
  (smartoverride (kbd "C-M-i") 'beginning-of-buffer)
  (smartoverride (kbd "C-M-k") 'end-of-buffer)
  (smartoverride (kbd "C-k") 'flycheck-error-list-next-error)
  (smartoverride (kbd "<tab>") 'forward-button)
  (smartoverride (kbd "<return>") 'flycheck-error-list-goto-error)
  (smartoverride (kbd "M-g") (lambda () (interactive)(quit-window)(delete-other-windows)))
  (smartoverride (kbd "C-f") 'flycheck-error-list-set-filter)
  (smartoverride (kbd "M-f") 'flycheck-error-list-reset-filter)
  (smartoverride (kbd "C-s") 'tabulated-list-sort))
(add-hook 'flycheck-error-list-mode-hook 'smart-flycheck-list-override)

;; Override keybind for shell mode
(defun smart-shell-mode-override()
  (smartoverride (kbd "<up>") 'comint-previous-input)
  (smartoverride (kbd "<down>") 'comint-next-input)
  (smartoverride (kbd "<return>") 'comint-send-input)
  (smartoverride (kbd "C-c") 'comint-interrupt-subjob))
(add-hook 'shell-mode-hook 'smart-shell-mode-override)

;; Override keybind for eshell
(defun smart-eshell-override ()
  (smartoverride (kbd "M-i") 'eshell-previous-prompt)
  (smartoverride (kbd "M-k") 'eshell-next-prompt)
  (smartoverride (kbd "<up>") 'eshell-previous-input)
  (smartoverride (kbd "C-<up>") 'eshell-previous-matching-input-from-input)
  (smartoverride (kbd "<down>") 'eshell-next-input)
  (smartoverride (kbd "C-<down>") 'eshell-next-matching-input-from-input)
  (smartoverride (kbd "<return>") 'eshell-send-input)
  (smartoverride (kbd "C-h") 'eshell-bol)
  (smartoverride (kbd "<backtab>") 'pcomplete-reverse)
  (smartoverride (kbd "C-<tab>") 'eshell-pcomplete)
  (smartoverride (kbd "C-x C-p") 'list-processes)
  (smartoverride (kbd "C-x C-k") 'eshell-kill-process)
  (smartoverride (kbd "C-x C-h") 'eshell-list-history)
  (smartoverride (kbd "C-x C-v") 'eshell-insert-envvar)
  (smartoverride (kbd "<f12> 1") 'eshell-completion-help)
  (smartoverride (kbd "<f12> 2") 'eshell-display-modifier-help)
  (smartoverride (kbd "<f12> 3") 'eshell-display-predicate-help)
  (smartoverride (kbd "C-c") 'eshell-interrupt-process))
(add-hook 'eshell-mode-hook 'smart-eshell-override)
