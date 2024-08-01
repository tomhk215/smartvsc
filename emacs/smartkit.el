;; mwim stands for move where I mean
;; which is a package created by Alex Kost <alezost@gmail.com>
;; https://github.com/alezost/mwim.el
(require 'mwim)

(defun smartkit-highlight (beg end)
  "Maintain a temporary highlight region"
  (push-mark beg nil t)
  (goto-char end)
  (setq deactivate-mark nil)
  (setq-local transient-mark-mode
              (cons 'only
                    (unless (eq transient-mark-mode 'lambda)
                      transient-mark-mode))))

(defun smartkit-region ()
  "Expand the current selected region"
  (interactive)
  (if (use-region-p)
      (let ((point (point))
            (reg-beg (region-beginning))
            (beg (save-excursion (goto-char (region-beginning))(line-beginning-position)))
            (end (save-excursion (goto-char (region-end))(line-end-position))))
        (if (eq point reg-beg)
            (smartkit-highlight end beg)
         (smartkit-highlight beg end)))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (smartkit-highlight beg end))))

(defun smartkit-forward-char (arg)
  "Ignore consecutive spaces while moving forward"
  (interactive "p")
  (if (and (eq arg 1) (looking-at "[[:space:]]\\{2,\\}\\|$"))
      (while (looking-at "[[:space:]]\\|$")
        (forward-char 1))
    (forward-char arg)))

(defun smartkit-backward-char (arg)
  "Ignore consecutive spaces while moving backward-button"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^\\|[[:space:]]\\{2,\\}"))
      (while (looking-back "^\\|[[:space:]]")
        (backward-char 1))
    (backward-char arg)))

(defun smartkit-shift (distance)
  "Shift the highlighted region by distance"
  (smartkit-region)
  (let* ((beg (region-beginning))
         (end (progn (save-excursion (indent-rigidly beg (region-end) distance))
                     (point))))
    (smartkit-highlight beg end)))

(defvar smartkit-shift-count 4
  "Number of space to be shift")
(defun smartkit-shift-right (arg)
  "Region is expanded. Shift right by a number of space. The default number of space is 4. It can be set by variable smartkit-shift-count."
  (interactive "p")
  (smartkit-shift (* arg smartkit-shift-count)))
(defun smartkit-shift-left (arg)
  "Region is expanded. Shift left by a number of space. The default number of space is 4. It can be set by variable smartkit-shift-count."
  (interactive "p")
  (smartkit-shift (* arg (- smartkit-shift-count))))

(defun smartkit-remove-previous-whitespace ()
  "Remove previous whitespaces and also newline in between"
  (let ((newline-counter 0))
    (while (looking-back "[[:space:]]\\|^")
      (when (and (<= newline-counter 1)(looking-back "^"))
        (setq newline-counter (+ 1 newline-counter)))
      (delete-backward-char 1))
    (when (>= newline-counter 2)
      (newline-and-indent))))
(defun smartkit-remove-following-whitespace ()
  "Remove following whitespaces and also newline in between"
  (let ((newline-counter 0))
    (while (looking-at "[[:space:]]\\|$")
      (when (and (<= newline-counter 1) (looking-at "$"))
        (setq newline-counter (+ 1 newline-counter)))
      (delete-forward-char 1))
    (when (>= newline-counter 2)
      (newline-and-indent)
      (backward-char))))

(defun smartkit-delete-backward-char (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^[[:space:]]*"))
      (smartkit-remove-previous-whitespace)
    (delete-backward-char arg)))

(defun smartkit-delete-forward-char (arg)
  (interactive "p")
  "Remove trailing whitespace if necessary"
  (if (and (equal 1 arg) (looking-at "[[:space:]]*$"))
      (smartkit-remove-following-whitespace)
    (delete-forward-char arg)))

(defun smartkit-backward-kill-word (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-back "^[[:space:]]*"))
      (smartkit-remove-previous-whitespace)
    (backward-kill-word arg)))

(defun smartkit-kill-word (arg)
  "Remove last indent if necessary"
  (interactive "p")
  (if (and (equal 1 arg) (looking-at "[[:space:]]*$"))
      (smartkit-remove-following-whitespace)
    (kill-word arg)))

(defun smartkit-open-next-line (arg)
  "Open next line and move to it"
  (interactive "p")
  (end-of-line)
  (newline-and-indent))
(defun smartkit-open-previous-line (arg)
  "Open a new line and move the it."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(defun smartkit-insert-previous-line (arg)
  "Insert a new line before the current one without move to it"
  (interactive "p")
  (save-excursion (beginning-of-line)
                  (open-line arg)))

(defun smartkit-comment-region ()
  "Expand the region and comment it. Temporary highlight commented region"
  (interactive)
  (smartkit-region)
  (let ((beg (region-beginning))
        (end (progn (comment-region (region-beginning) (region-end) 2)
                    (point))))
    (smartkit-highlight beg end)))
(defun smartkit-uncomment-region ()
  "Expand the region and uncomment it. Temporary highlight uncommented region"
  (interactive)
  (smartkit-region)
  (let ((beg (region-beginning))
        (end (progn (comment-region (region-beginning) (region-end) -2)
                    (point))))
    (smartkit-highlight beg end)))

(defun smartkit-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defvar smartkit-lastcopy '(0 0 0))
(defun smartkit-copy ()
  "If no active region, copy whole line. If there is a region, copy the region. If the same region is copied the sencond time within 3 second, convert to rectanglar copy. Push the rectanglar copy to both kill ring and register."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
                  (end (region-end)))
        (if (and
             (< (- (nth 1 (current-time))(car smartkit-lastcopy)) 3) ;; check last copy time
             (equal (region-beginning)(nth 1 smartkit-lastcopy))
             (equal (region-end)(nth 2 smartkit-lastcopy)))
            (progn
              (copy-rectangle-as-kill beg end)
              (smartkit-copy-rectangle-to-kill-ring beg end)
              (smartkit-highlight beg end)
              (rectangle-mark-mode)
              (message "Rectangle region copied")
              (setq smartkit-lastcopy (list (nth 1 (current-time)) (region-beginning) (region-end))))
          (kill-ring-save beg end)
          (smartkit-highlight beg end)
          (message "Region copied")
          (setq smartkit-lastcopy (list (nth 1 (current-time)) (region-beginning) (region-end)))))
    (smartkit-region)
    (let ((beg (region-beginning))
          (end (region-end)))
      (kill-ring-save beg end)
      (message "Region copied")
      (smartkit-highlight beg end))))
(defun smartkit-copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))

(defun smartkit-cut()
  "If no active region, cut whole line. If line is empty after cut, shift the next line"
  (interactive)
  (if (use-region-p)
      (progn (kill-region (region-beginning) (region-end))
             (if (smartkit-current-line-empty-p)
                 (delete-forward-char 1)))
    (kill-whole-line)))

(defun smartkit-indent ()
  "Indent whole document"
  (interactive)
  (save-excursion (smartkit-highlight (point-min) (point-max))
                  (indent-for-tab-command)))

(defun smartkit-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun smartkit-remove-blank-lines ()
  "Delete blank lines."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (search-forward-regexp "\n\n" nil t)
      (replace-match "\n")
      (goto-char 1))))

(provide 'smartkit)
