(add-to-list 'load-path (locate-user-emacs-file "packages/asbish"))
(require 'asbish)

(require 'f)
(require 'dash)
(require 'levenshtein)

(defun my/delete-window ()
  (interactive)
  (let ((wl (length (window-list)))
        (wh (window-height))
        (h (- (frame-height) 1))) ;sub minibuffer
    (cond ((and (> wl 3) (> h wh)) (delete-window))
          ((= 2 wl)                (delete-window))
          (t                       (message "use \"C-x 0\"")))))

(define-key global-map (kbd "C-z") 'my/delete-window)

(defun my/current-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time))))

(define-key global-map (kbd "C-c D") 'my/current-time)

(defun my/scale (real target)
  (interactive "nReal: \nnTarget: ")
  (let* ((lt (> real target))
         (fr (float real))
         (ft (float target))
         (fmt (if lt "1/%s" "%s/1"))
         (scale (if lt (/ fr ft) (/ ft fr)))
         (mul-10 (* 10 scale))
         (correct (= (truncate mul-10) mul-10))
         (finalize (if (= (truncate scale) scale) (truncate scale)
                     (/ (truncate mul-10) 10.0))))
    (message (format "%s%s (Real:%s Target:%s)"
                     (format fmt (number-to-string finalize))
                     (if correct ""
                       (format (concat " ~ " fmt) (number-to-string scale)))
                     (number-to-string real)
                     (number-to-string target)))))

(defun my/levenshtein (str1 str2)
  (interactive "sstr1: \nsstr2: ")
  (message
   (format "%d d(%s, %s)" (levenshtein-distance str1 str2) str1 str2)))

(defun my/print-point ()
  (interactive)
  (message (concat "Line: " (format-mode-line "%l")
                   ", Point: " (number-to-string (point)))))

(defconst my/frame-file (locate-user-emacs-file "rc/my-frame"))

(defun my/frame-write-file ()
  (when (file-writable-p my/frame-file)
    (let ((pos (frame-position)))
      (f-write-text
       (format "%d %d %d %d"
               (car pos) (cdr pos) (frame-width) (frame-height))
       'utf-8 my/frame-file))))

(defun my/frame-read-file ()
  (let ((input (ignore-errors (f-read-text my/frame-file 'utf-8))))
    (when (stringp input)
      (let ((keys '(left top width height))
            (size (split-string input)))
        (setq initial-frame-alist
              (append (-zip-with #'my/frame-default keys size)
                      initial-frame-alist))))))

(defun my/frame-default (sym num)
  (pcase (list (symbol-name sym)
               (string-match "\\`[1-9]?[0-9]?[0-9]?[0-9]\\'" num))
    (`("top" 0)     (cons sym (min (string-to-number num) 500)))
    (`("left" 0)    (cons sym (min (string-to-number num) 4500)))
    (`("width" 0)   (cons sym (max (string-to-number num) 80)))
    (`("height" 0)  (cons sym (max (string-to-number num) 37)))
    (`("width" ,_)  (cons sym 80))
    (`("height" ,_) (cons sym 37))
    (_              (cons sym 0))))

(when (display-graphic-p)
  (add-hook 'after-init-hook #'my/frame-read-file)
  (add-hook 'kill-emacs-hook #'my/frame-write-file))
