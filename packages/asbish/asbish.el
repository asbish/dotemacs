;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dabbrev)
(require 'face-remap)
(require 'json)
(require 'ido)

(defconst asbish/quick-window-register 65) ;; C-x r j A

(defun asbish/quick-window-set (msg)
  (interactive (list t))
  (window-configuration-to-register asbish/quick-window-register)
  (when msg (message "registered window")))

(defun asbish/quick-window ()
  (interactive)
  (if (not (get-register asbish/quick-window-register))
      (asbish/quick-window-set t)
    (jump-to-register asbish/quick-window-register)))

(defmacro asbish/--rebind-key (map orig-kbd new-kbd fun)
  (let ((orig-fun (make-symbol "orig-fun")))
    `(let ((,orig-fun (lookup-key ,map ,orig-kbd)))
       (when (and (symbolp ,orig-fun) (eq ,orig-fun ,fun))
         (define-key ,map ,orig-kbd nil))
       (define-key ,map ,new-kbd ,fun))))

(defmacro asbish/rebind-keys (map &rest args)
  (declare (indent 1))
  (let ((x (make-symbol "x"))
        (k1 (make-symbol "k1"))
        (k2 (make-symbol "k2"))
        (fun (make-symbol "fun")))
    `(dolist (,x (list ,@args))
       (let ((,k1 (kbd (plist-get ,x :from)))
             (,k2 (kbd (plist-get ,x :to)))
             (,fun (plist-get ,x :bind)))
         (if (functionp ,fun)
             (asbish/--rebind-key ,map ,k1 ,k2 ,fun)
           (error (format "'%s' must be a function" ,fun)))))))

(defun asbish/filter (pred lis)
  (seq-reduce
   (lambda (m x)
     (if (funcall pred x)
         (list (cons x (car m)) (cadr m))
       (list (car m) t)))
   (reverse lis)
   nil))

(defun asbish/compose (fun &rest funs)
  (seq-reduce (lambda (f g)
                (lambda (&rest args)
                  (funcall f (apply g args))))
              funs
              fun))

(defun asbish/dir (dir &optional fun)
  (let* ((d (expand-file-name dir))
         (l (if fun (funcall fun d) (directory-files d nil "^[^.]"))))
    (mapcar (lambda (x) (concat d x)) l)))

(defun asbish/shell-command-to-string (command)
  "with status. based on `shell-command-to-string' in simple.el"
  (let (ret out)
    (setq out (with-output-to-string
                (with-current-buffer standard-output
                  (setq ret (process-file
                             shell-file-name nil t nil
                             shell-command-switch command)))))
    (list ret out)))

(defun asbish/load-file-if-exists (path &optional msg)
  (if (file-exists-p path)
      (load-file path)
    (when msg (warn msg))))

(defun asbish/load-file-shell-command (cmd &optional msg)
  (let ((coding-system-for-read 'utf-8)
        (run (asbish/shell-command-to-string cmd)))
    (when (= 0 (car run)) (asbish/load-file-if-exists (cadr run) msg))))

(defun asbish/find-in (dir path &optional wildcards)
  (let* ((file-dir (file-name-directory (buffer-file-name)))
         (root (locate-dominating-file file-dir dir))
         (fun (if wildcards
                  (asbish/compose #'car #'file-expand-wildcards)
                #'identity))
         (path (and root
                    (funcall fun (concat root
                                         (file-name-as-directory dir)
                                         path)))))
    (and path (expand-file-name path))))

(defun asbish/find-executable-node_modules (path &optional wildcards)
  (let ((file (asbish/find-in "node_modules" path wildcards)))
    (when (and file (file-executable-p file)) file)))

(cl-defun asbish/get-direnv-content (&optional (dir default-directory))
  (with-temp-buffer
    (let ((d (locate-dominating-file dir ".envrc")))
      (when d
        (setq default-directory d)
        (if (= 0 (call-process "direnv" nil '(t nil) nil "export" "json"))
            (progn
              (goto-char (point-min))
              (json-read))
          (error "direnv error") nil)))))

(defun asbish/line-strip (re)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (beg1 (string-match re (buffer-substring-no-properties beg end))))
    (if beg1 (list (+ beg beg1) end) (list beg end))))

(defmacro asbish/try-comp (abbrev)
  (let ((fun (intern (concat "asbish/try-comp-" abbrev)))
        (abb (substring abbrev 0 (- (length abbrev) 1))))
    `(cl-defun ,fun (&optional (x ,abbrev))
       (dabbrev--reset-global-variables)
       (try-completion x (dabbrev--find-all-expansions ,abb nil)))))

(defun asbish/find-buffer (regex)
  (cl-loop for buffer in (buffer-list)
           do (when (string-match regex (buffer-name buffer))
                (cl-return buffer))))

(defun asbish/jq-file (file-path arg)
  (with-temp-buffer
    (insert-file-contents file-path)
    (and
     (equal 0 (call-shell-region
               (point-min)
               (point-max)
               (mapconcat #'shell-quote-argument (list "jq" arg) " ")
               t
               t))
     (buffer-string))))

(defconst asbish/--once-hash-table
  (make-hash-table :test 'eq :size 10 :weakness t))

(defun asbish/once (fun)
  (let ((key (symbol-function fun)))
   (unless (gethash key asbish/--once-hash-table)
     (puthash key t asbish/--once-hash-table)
     (funcall fun))))

(defconst asbish/os
  (pcase system-type
    ('darwin "osx")
    ('gnu/linux "linux")
    (_ "")))

(defconst asbish/find-cmd
  (if (eq system-type 'gnu/linux) "find" "gfind"))

(defvar asbish/proj-root nil)
(defvar asbish/proj-find-file-cmd nil)
(defvar asbish/proj-find-file-cmd-git nil)
(defvar asbish/proj-find-file-limit nil)
(defconst asbish/proj-find-file-limit-default 400)

(defun asbish/proj-init (limit)
  (interactive "nLimit: ")
  (let* ((lim (floor limit))
         (lim-str (number-to-string lim)))
    (setq asbish/proj-find-file-cmd
          (concat
           asbish/find-cmd " . -type d \\( "
           "-path '*/\.*' -o -path '*/_*' -o -path '*/test' "
           "-o -path '*/build' -o -path '*/dist' "
           "-o -path '*/vendor' -o -path '*/node_modules' "
           "\\) -prune -o -type f \\( "
           "! -iname '.*' ! -iname '*.o' ! -iname '*.so' ! -iname '*.out' "
           "! -iname '*.a' ! -iname '*.lib' ! -iname '*.map' "
           "! -iname '*.tar' ! -iname '*.gz' ! -iname '*.zip' "
           "! -iname '#*' ! -iname '*~' "
           "! -iname GTAGS ! -iname GPATH ! -iname GRTAGS ! -iname GSYMS "
           "! -iname TAGS ! -iname compile_commands.json "
           "\\) -printf '%P\n' | head -" lim-str))
    (setq asbish/proj-find-file-cmd-git
          (concat "git ls-files -c -o --exclude-standard | head -" lim-str))
    (setq asbish/proj-find-file-limit lim)))

(asbish/proj-init asbish/proj-find-file-limit-default)

(defun asbish/proj-set-root ()
  (interactive)
  (let* ((prompt (if asbish/proj-root
                     (format "Set root [current:%s]: " asbish/proj-root)
                   "Set root: "))
         (dir (ido-read-directory-name prompt default-directory)))
    (setq asbish/proj-root dir) dir))

(defun asbish/proj-find-file (limit)
  (interactive "p")
  (when (> limit asbish/proj-find-file-limit-default)
    (asbish/proj-init limit))
  (let* ((root (if (and asbish/proj-root (not limit)) asbish/proj-root
                 (asbish/proj-set-root)))
         (cmd (if (file-exists-p (concat root ".git"))
                  asbish/proj-find-file-cmd-git
                asbish/proj-find-file-cmd))
         (run (asbish/shell-command-to-string
               (format "cd %s && %s" root cmd))))
    (if (= 0 (car run))
        (let* ((idolist (split-string (cadr run)))
               (len (safe-length idolist))
               (prompt (if (> asbish/proj-find-file-limit len)
                           (format "%s[%s]: " root len)
                         (format "%s[%s+](truncated): " root len))))
          (find-file (concat root (ido-completing-read prompt idolist))))
      (error "find error"))))

(defun asbish/fmt-alist (l s)
  (if l (mapconcat (lambda (x) (format "(%s . %s)" (car x) (cdr x))) l s) ""))

(defun asbish/out-file-path (file-name &optional clobber)
  (let ((path (concat
               (ido-read-directory-name
                (concat "Where to generate `" file-name "' ?: ")
                default-directory)
               file-name)))
    (if (and (or clobber (not (file-exists-p path)))
             (file-writable-p path))
        path
      (error (concat "Abort. `" file-name "' already exists or readonly.")))))

(defun asbish/gen-dir-locals--preset ()
  (let* ((presets '(("cc-gnu"
                     (c-file-style . "\"GNU\""))
                    ("cc-gnu-tab"
                     (c-file-style . "\"GNU\"")
                     (indent-tabs-mode . t))
                    ("cc-linux"
                     (c-file-style . "\"linux\"")
                     (c-basic-offset . 4))
                    ("cc-linux-tab"
                     (c-file-style . "\"linux\"")
                     (indent-tabs-mode . t)
                     (c-basic-offset . 4))
                    ("none")))
         (presets-lis (mapcar #'car presets)))
    (cdr (assoc
          (ido-completing-read "Preset?: " presets-lis nil nil nil nil "none")
          presets))))

(defun asbish/gen-dir-locals--flycheck-disable ()
  (let* ((checkers '("irony"))
         (checkers-prefix-c/c++ '("clang" "gcc" "cppcheck"))
         (inputs (split-string
                  (read-from-minibuffer
                   (concat "Disable checkers {"
                           (mapconcat 'identity
                                      (append checkers checkers-prefix-c/c++)
                                      ", ")
                           "}: "))))
         (lis (seq-reduce
               (lambda (a b)
                 (if (member b checkers)
                     (cons (make-symbol b) a)
                   (if (member b checkers-prefix-c/c++)
                       (cons (make-symbol (concat "c/c++-" b)) a)
                     a)))
               (reverse inputs)
               nil)))
    (list `(flycheck-disabled-checkers . ,lis))))

(defun asbish/gen-dir-locals--flycheck-include-path ()
  (let ((lis (list)))
    (while (let* ((default-dir (expand-file-name default-directory))
                  (dir (ido-read-directory-name
                        "Include path:"
                        default-directory))
                  (dir-str (format "\"%s\"" dir)))
             (if (equal dir default-dir)
                 (progn
                   (when (and (not (cl-member dir-str lis))
                              (y-or-n-p "Include path: add? \".\" "))
                     (setq lis (cl-adjoin dir-str lis)))
                   (not (y-or-n-p "Include path: end? ")))
               (setq lis (cl-adjoin dir-str lis))
               t)))
    (list `(flycheck-gcc-include-path . ,lis)
          `(flycheck-clang-include-path . ,lis))))

(defun asbish/gen-dir-locals ()
  (interactive)
  (let* ((indent "\n         ")
         (path (asbish/out-file-path ".dir-locals.el"))
         (preset (asbish/gen-dir-locals--preset))
         (fc-disabled (asbish/gen-dir-locals--flycheck-disable))
         (fc-include-path (asbish/gen-dir-locals--flycheck-include-path))
         (result (remove nil (list preset fc-disabled fc-include-path))))
    (with-temp-file path
      (setq buffer-file-coding-system 'utf-8)
      (insert (format
               "((nil . (%s)))"
               (mapconcat (lambda (x) (asbish/fmt-alist x indent))
                          result
                          indent))))))

(defconst asbish/whitespace-background (face-attribute 'default :background))

(defun asbish/whitespace-tab-toggle ()
  (interactive)
  (let ((sym 'asbish/whitespace-tab-cookie))
    (unless (local-variable-p sym) (make-variable-buffer-local sym))
    (let ((cookie (buffer-local-value sym (current-buffer))))
      (if (not cookie)
          (set sym (face-remap-add-relative
                    'whitespace-tab
                    `((:background ,asbish/whitespace-background))))
        (face-remap-remove-relative cookie)
        (set sym nil)))))

(provide 'asbish)
