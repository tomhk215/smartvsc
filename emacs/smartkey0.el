;; mwim stands for move where I mean
;; which is a package created by Alex Kost <alezost@gmail.com>
;; https://github.com/alezost/mwim.el
(require 'mwim)

(defun smart-highlight (beg end)
  "Maintain a temporary highlight region"
  (push-mark beg nil t)
  (goto-char end)
  (setq deactivate-mark nil)
  (setq-local transient-mark-mode
              (cons 'only
                    (unless (eq transient-mark-mode 'lambda)
                      transient-mark-mode))))

(defun smart-region ()
  "Expand the current selected region"
  (interactive)
  (if (use-region-p)
      (let ((beg (save-excursion (goto-char (region-beginning))(line-beginning-position)))
            (end (save-excursion (goto-char (region-end))(line-end-position))))
        (smart-highlight beg end))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (smart-highlight beg end))))

(defun smart-forward-char (arg)
  "Ignore consecutive spaces while moving forward"
  (interactive "p")
  (if (and (equal 1 arg) (looking-at "[[:space:]]\\{2,\\}\\|$"))
      (while (looking-at "[[:space:]]\\|$")
        (forward-char 1))
    (forward-char arg)))

(defun smart-backward-char (arg)
  "Ignore consecutive spaces while moving backward-button"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^\\|[[:space:]]\\{2,\\}"))
      (while (looking-back "^\\|[[:space:]]")
        (backward-char 1))
    (backward-char arg)))

(defun smart-shift (distance)
  "Shift the highlighted region by distance"
  (smart-region)
  (let* ((beg (region-beginning))
         (end (progn (save-excursion (indent-rigidly beg (region-end) distance))
                     (point))))
    (smart-highlight beg end)))

(defvar smart-shift-count 4
  "Number of space to be shift")
(defun smart-shift-right (arg)
  "Region is expanded. Shift right by a number of space. The default number of space is 4. It can be set by variable smart-shift-count."
  (interactive "p")
  (smart-shift (* arg smart-shift-count)))
(defun smart-shift-left (arg)
  "Region is expanded. Shift left by a number of space. The default number of space is 4. It can be set by variable smart-shift-count."
  (interactive "p")
  (smart-shift (* arg (- smart-shift-count))))

(defun smart-remove-previous-whitespace ()
  "Remove previous whitespaces and also newline in between"
  (let ((newline-counter 0))
    (while (looking-back "[[:space:]]\\|^")
      (when (and (<= newline-counter 1)(looking-back "^"))
        (setq newline-counter (+ 1 newline-counter)))
      (delete-backward-char 1))
    (when (>= newline-counter 2)
      (newline-and-indent))))
(defun smart-remove-following-whitespace ()
  "Remove following whitespaces and also newline in between"
  (let ((newline-counter 0))
    (while (looking-at "[[:space:]]\\|$")
      (when (and (<= newline-counter 1) (looking-at "$"))
        (setq newline-counter (+ 1 newline-counter)))
      (delete-forward-char 1))
    (when (>= newline-counter 2)
      (newline-and-indent)
      (backward-char))))

(defun smart-delete-backward-char (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^[[:space:]]*"))
      (smart-remove-previous-whitespace)
    (delete-backward-char arg)))

(defun smart-delete-forward-char (arg)
  (interactive "p")
  "Remove trailing whitespace if necessary"
  (if (and (equal 1 arg) (looking-at "[[:space:]]*$"))
      (smart-remove-following-whitespace)
    (delete-forward-char arg)))

(defun smart-backward-kill-word (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^[[:space:]]*"))
      (smart-remove-previous-whitespace)
    (backward-kill-word arg)))

(defun smart-kill-word (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-at "[[:space:]]*$"))
      (smart-remove-following-whitespace)
    (kill-word arg)))

(defun smart-open-next-line (arg)
  "Open next line and move to it"
  (interactive "p")
  (end-of-line)
  (newline-and-indent))
(defun smart-open-previous-line (arg)
  "Open a new line and move the it."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(defun smart-insert-previous-line (arg)
  "Insert a new line before the current one without move to it"
  (interactive "p")
  (save-excursion (beginning-of-line)
                  (open-line arg)))

(defun smart-comment-region ()
  "Expand the region and comment it. Temporary highlight commented region"
  (interactive)
  (smart-region)
  (let ((beg (region-beginning))
        (end (progn (comment-region (region-beginning) (region-end) 2)
                    (point))))
    (smart-highlight beg end)))
(defun smart-uncomment-region ()
  "Expand the region and uncomment it. Temporary highlight uncommented region"
  (interactive)
  (smart-region)
  (let ((beg (region-beginning))
        (end (progn (comment-region (region-beginning) (region-end) -2)
                    (point))))
    (smart-highlight beg end)))

(defun smart-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defvar smart-lastcopy '(0 0 0))
(defun smart-copy ()
  "If no active region, copy whole line. If there is a region, copy the region. If the same region is copied the sencond time within 3 second, convert to rectanglar copy. Push the rectanglar copy to both kill ring and register."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
                  (end (region-end)))
        (if (and
             (< (- (nth 1 (current-time))(car smart-lastcopy)) 3) ;; check last copy time
             (equal (region-beginning)(nth 1 smart-lastcopy))
             (equal (region-end)(nth 2 smart-lastcopy)))
            (progn
              (copy-rectangle-as-kill beg end)
              (smart-copy-rectangle-to-kill-ring beg end)
              (smart-highlight beg end)
              (rectangle-mark-mode)
              (message "Rectangle region copied")
              (setq smart-lastcopy (list (nth 1 (current-time)) (region-beginning) (region-end))))
          (kill-ring-save beg end)
          (smart-highlight beg end)
          (message "Region copied")
          (setq smart-lastcopy (list (nth 1 (current-time)) (region-beginning) (region-end)))))
    (smart-region)
    (let ((beg (region-beginning))
          (end (region-end)))
      (kill-ring-save beg end)
      (message "Region copied")
      (smart-highlight beg end))))
(defun smart-copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))(defvar smart-copy-last nil)

(defun smart-cut()
  "If no active region, cut whole line. If line is empty after cut, shift the next line"
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (if (smart-current-line-empty-p)
                 (delete-forward-char 1)))
    (kill-region (line-beginning-position) (line-end-position))
    (delete-forward-char 1)))

(defun smart-indent ()
  "Indent whole document"
  (interactive)
  (save-excursion (smart-highlight (point-min) (point-max))
                  (indent-for-tab-command)))

(defun smart-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun smart-remove-blank-lines ()
  "Delete blank lines."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (search-forward-regexp "\n\n" nil t)
      (replace-match "\n")
      (goto-char 1))))

(defvar smartmap (make-keymap)
  "The smart-mode keymap")

;; -----------------------------------------------------------------------
;; Keybind for point movement
;;               ____
;;              | i  |
;;              |    |
;;   ____  ____  ____  ____  ____
;;  | h  || j  || k  || l  || ;  |
;;  |    ||    ||    ||    ||    |
(define-key smartmap (kbd "C-j") 'smart-backward-char)
(define-key smartmap (kbd "M-j") 'backward-word)

(define-key smartmap (kbd "C-l") 'smart-forward-char)
(define-key smartmap (kbd "M-l") 'forward-word)

(define-key smartmap (kbd "C-i") 'previous-line)
(define-key smartmap (kbd "M-i") 'scroll-down-command)
(define-key smartmap (kbd "C-M-i") 'beginning-of-buffer)

(define-key smartmap (kbd "C-k") 'next-line)
(define-key smartmap (kbd "M-k") 'scroll-up-command)
(define-key smartmap (kbd "C-M-k") 'end-of-buffer)

(define-key smartmap (kbd "C-h") 'mwim-beginning-of-code-or-line)
(define-key smartmap (kbd "M-h") 'backward-sentence)
(define-key smartmap (kbd "C-;") 'mwim-end-of-code-or-line)
(define-key smartmap (kbd "M-;") 'forward-sentence)

;; \ use to reposition window
(define-key smartmap (kbd "C-\\") 'recenter-top-bottom)

;; u and o are parenthesis related
(define-key smartmap (kbd "C-u") 'backward-sexp)
(define-key smartmap (kbd "M-u") 'backward-up-list)
(define-key smartmap (kbd "C-M-u") 'beginning-of-defun)
(define-key smartmap (kbd "C-o") 'forward-sexp)
(define-key smartmap (kbd "M-o") 'forward-list)
(define-key smartmap (kbd "C-M-o") 'end-of-defun)

;; Keybind for ace-jump-mode (jump directly to point)
(define-key smartmap (kbd "C-y") 'ace-jump-mode)
(define-key smartmap (kbd "M-y") 'ace-jump-char-mode)
(define-key smartmap (kbd "C-M-y") 'ace-jump-line-mode)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for delete behaviour
;; m and n are also used for deleting
(define-key smartmap (kbd "<backspace>") 'smart-delete-backward-char)
(define-key smartmap (kbd "C-<backspace>") 'smart-backward-kill-word)
(define-key smartmap (kbd "C-m") 'smart-delete-backward-char)
(define-key smartmap (kbd "M-m") 'smart-backward-kill-word)
(define-key smartmap (kbd "C-n") 'smart-delete-forward-char)
(define-key smartmap (kbd "M-n") 'smart-kill-word)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind to <tab> behaviour
;; <tab> must be redefined, many other packages heavily depend on <tab>
;; There must be conflict on <tab>
(define-key smartmap (kbd "<tab>") 'indent-for-tab-command) 
(define-key smartmap (kbd "C-<tab>") 'smart-shift-right)
(define-key smartmap (kbd "<backtab>") 'smart-shift-left)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind to <return> behaviour
(define-key smartmap (kbd "<return>") 'newline-and-indent)
(define-key smartmap (kbd "C-<return>") 'smart-open-next-line)
(define-key smartmap (kbd "S-<return>") 'smart-open-previous-line)
(define-key smartmap (kbd "M-<return>") 'smart-insert-previous-line)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; C-q, C-p, C-x, M-x, C-c are reserved
;; C-q for universal-argument
;; C-x for system prefix
;; M-x for execute-extended-command
;; C-c for user-define prefix
;; C-p will use as a prefix key
(define-key smartmap (kbd "C-p") nil)
(define-key smartmap (kbd "C-q") 'universal-argument)
(define-key smartmap (kbd "M-q") 'repeat)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for save behaviour
(define-key smartmap (kbd "C-s") 'save-buffer)
(define-key smartmap (kbd "M-s") 'write-file)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for opening file
(define-key smartmap (kbd "C-a") 'find-file)
(define-key smartmap (kbd "M-a") 'recentf-open-files)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for quit behaviour
(define-key smartmap (kbd "M-g") 'kill-this-buffer)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; keybind for cut, copy and paste behaviour
;; w and e are used for "copy and paste""
(define-key smartmap (kbd "C-w") 'smart-copy)
(define-key smartmap (kbd "M-w") 'smart-cut)
(define-key smartmap (kbd "C-M-w") 'copy-rectangle-as-kill)
(define-key smartmap (kbd "C-e") 'yank)
(define-key smartmap (kbd "M-e") 'yank-pop)
(define-key smartmap (kbd "C-M-e") 'yank-rectangle)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for undo behaviour
(define-key smartmap (kbd "C-z") 'undo)
(define-key smartmap (kbd "M-z") nil)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Key bind to search
;; f for find and r for replace
(define-key smartmap (kbd "C-f") 'isearch-forward)
(define-key smartmap (kbd "M-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-r") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "M-r") 'isearch-query-replace-regexp)
(define-key isearch-mode-map (kbd "C-g") 'isearch-cancel)
(define-key isearch-mode-map (kbd "<tab>") 'isearch-complete)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-j") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "C-l") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-j") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-l") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-m") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)
(define-key smartmap (kbd "C-r") 'query-replace)
(define-key smartmap (kbd "M-r") 'query-replace-regexp)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for dired mode
;; d for directory mode
(define-key smartmap (kbd "C-d") 'dired)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for buffer behaviour
;; b for buffer mode
(define-key smartmap (kbd "C-b") 'switch-to-buffer)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for expand-region
(define-key smartmap (kbd "C-v") 'er/expand-region)
(define-key smartmap (kbd "M-v") 'er/contract-region)
(define-key smartmap (kbd "C-M-v") 'smart-region)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for managing frame and windows
;; numbers are used to manage frame and windows
(define-key smartmap (kbd "C-1") 'delete-other-windows)
(define-key smartmap (kbd "C-2")
  (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(define-key smartmap (kbd "C-3")
  (lambda () (interactive)(split-window-vertically) (other-window 1)))
(define-key smartmap (kbd "C-4") 'delete-window)
;; There is a bug that nlinum prevent opening new frame
;; So nlinum is disable first
(define-key smartmap (kbd "C-0")
  (lambda ()
    (interactive)
    (global-nlinum-mode 0)
    (make-frame-command)
    (global-nlinum-mode t)))
(define-key smartmap (kbd "C-9") 'other-frame)
(define-key smartmap (kbd "C-8") 'delete-frame)
(define-key smartmap (kbd "C-7") 'delete-other-frames)

(define-key smartmap (kbd "M-1") 'other-window)
(define-key smartmap (kbd "M-2") 'windmove-left)
(define-key smartmap (kbd "M-3") 'windmove-right)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
(define-key smartmap (kbd "<f1>") 'eval-last-sexp)
(define-key smartmap (kbd "S-<f1>") 'eval-buffer)
(define-key smartmap (kbd "<f2>") 'eshell)
(define-key smartmap (kbd "S-<f2>") 'shell)

;; -----------------------------------------------------------------------
;; Keybind to help command <f12>
(define-key smartmap (kbd "<f12> t")  'help-with-tutorial)
(define-key smartmap (kbd "<f12> k")  'describe-key)
(define-key smartmap (kbd "<f12> K")  'Info-goto-emacs-key-command-node)
(define-key smartmap (kbd "<f12> w")  'where-is)
(define-key smartmap (kbd "<f12> f")  'describe-function)
(define-key smartmap (kbd "<f12> C-f")  'describe-face)
(define-key smartmap (kbd "<f12> F")  'Info-goto-emacs-command-node)
(define-key smartmap (kbd "<f12> a")  'apropos-command)
(define-key smartmap (kbd "<f12> d")  'apropos-documentation)
(define-key smartmap (kbd "<f12> m")  'describe-mode)
(define-key smartmap (kbd "<f12> b")  'describe-bindings)
(define-key smartmap (kbd "<f12> r")  'info-emacs-manual)
(define-key smartmap (kbd "<f12> i")  'info)
(define-key smartmap (kbd "<f12> p")  'finder-by-keyword)
(define-key smartmap (kbd "<f12> n")  'view-emacs-news)
(define-key smartmap (kbd "<f12> l")  'view-lossage)
(define-key smartmap (kbd "<f12> v")  'describe-variable)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Keybind for comment behaviour
(define-key smartmap (kbd "C-c C-c") 'smart-comment-region)
(define-key smartmap (kbd "C-c M-c") 'smart-uncomment-region)
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Override keybind for dired mode
(defun smart-dired-mode-override ()
  (smartoverride (kbd "<left>") 'dired-up-directory)
  (smartoverride (kbd "<up>") 'dired-previous-line)
  (smartoverride (kbd "<down>") 'dired-next-line)
  (smartoverride (kbd "<return>") 'dired-find-file)
  (smartoverride (kbd "C-<return>") 'dired-insert-subdir)
  
  (smartoverride (kbd "C-x C-r") 'dired-do-rename)
  (smartoverride (kbd "C-x M-r") 'dired-do-rename-regexp)
 (smartoverride (kbd "d") 'dired-flag-file-deletion)
  (smartoverride (kbd "C-x C-d") 'dired-do-delete)
  (smartoverride (kbd "C-x C-w") 'dired-do-copy)
  (smartoverride (kbd "M-g") 'quit-window)
  (smartoverride (kbd "SPC") 'dired-mark)
  (smartoverride (kbd "S-SPC") 'dired-unmark)
  
  )
(add-hook 'dired-mode-hook 'smart-dired-mode-override)
;; -----------------------------------------------------------------------

(define-minor-mode smart-mode
  "Smart key mode make key more efficient"
  :lighter " Smart"
  :init-value nil
  :keymap smartmap)

(defun smart-mode-on ()
  (interactive)
  (smart-mode t))
(defun smart-mode-off ()
  (interactive)
  (smart-mode 0))

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (unless (eq (caar minor-mode-map-alist) 'smart-mode)
    (let ((mykeys (assq 'smart-mode minor-mode-map-alist)))
      (assq-delete-all 'smart-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun smartoverride (key def)
  "Overrides a minor mode keybinding for the local
   buffer, by creating or altering keymaps stored in buffer-local
   `minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc 'smart-mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc 'smart-mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(smart-mode . ,map) minor-mode-overriding-map-alist) 
                       map))))
    (define-key newmap key def)))

(define-globalized-minor-mode global-smart-mode smart-mode smart-mode-on)

(add-hook 'minibuffer-setup-hook 'smart-mode-off)

(provide 'smartkey)
