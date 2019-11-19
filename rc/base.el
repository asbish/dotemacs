;; 24.4+

(menu-bar-mode -1)

(fset 'yes-or-no-p #'y-or-n-p)
(set-language-environment "English")
(prefer-coding-system 'utf-8)

(setq default-directory "~/"
      inhibit-startup-message t
      inhibit-startup-echo-area-message -1
      initial-scratch-message ""
      visible-bell nil
      ring-bell-function #'ignore
      max-specpdl-size 5000
      max-lisp-eval-depth 50000
      message-log-max 512
      vc-follow-symlinks t
      load-prefer-newer t
      auto-save-list-file-prefix nil
      backup-by-copying t
      create-lockfiles nil
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      vc-handled-backends ()
      tab-stop-list (number-sequence 4 120 4)
      read-file-name-completion-ignore-case t
      browse-url-browser-function 'eww-browse-url
      custom-file (locate-user-emacs-file ".custom.el")
      backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "backup/" user-emacs-directory) t)))

(setq-default tab-width 4
              tab-always-indent 'nil
              indent-tabs-mode nil
              line-spacing 1
              history-length 100
              history-delete-duplicates t
              gc-cons-threshold (* gc-cons-threshold 10))

(custom-set-variables
 '(cua-enable-cua-keys nil)
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(ediff-use-last-dir t)
 '(ediff-keep-variants nil)
 '(ediff-make-buffers-readonly-at-startup t)
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(ediff-split-window-function (quote split-window-horizontally)))

(cd "~/")
(electric-indent-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(winner-mode 1)
(cua-mode 1)

(when (fboundp 'save-place-mode) (save-place-mode 1))

(unless (display-graphic-p)
  (define-key input-decode-map "\e[5;0A" [C-return])
  (define-key input-decode-map "\e[5;1A" [C-up])
  (define-key input-decode-map "\e[5;1B" [C-down])
  (define-key input-decode-map "\e[5;1C" [C-right])
  (define-key input-decode-map "\e[5;1D" [C-left])
  (define-key input-decode-map "\e[5;28" [?\C-\(])
  (define-key input-decode-map "\e[5;29" [?\C-\)])
  (define-key input-decode-map "\e[5;2B" [?\C-+])
  (define-key input-decode-map "\e[5;2C" [?\C-,])
  (define-key input-decode-map "\e[5;2E" [?\C-.])
  (define-key input-decode-map "\e[5;2F" [?\C-/])
  (define-key input-decode-map "\e[5;3A" [?\C-:])
  (define-key input-decode-map "\e[5;3B" [?\C-\;])
  (define-key input-decode-map "\e[5;3C" [?\C-<])
  (define-key input-decode-map "\e[5;3E" [?\C->])
  (define-key input-decode-map "\e[5;5C" [?\C-\\])
  (define-key input-decode-map "\e[5;7C" [?\C-|])
  (define-key input-decode-map "\e[6;2C" [?\M-\C-,])
  (define-key input-decode-map "\e[6;2E" [?\M-\C-.])
  (define-key input-decode-map "\e[6;3A" [?\M-\C-:])
  (define-key input-decode-map "\e[6;3B" [?\M-\C-\;])
  (define-key input-decode-map "\e[6;68" [?\M-\C-h])
  (define-key input-decode-map "\e[6;5C" [?\M-\C-\\])
  (define-key input-decode-map "\e[6;7C" [?\M-\C-|]))

(keyboard-translate ?\C-h ?\C-?)
(define-key global-map (kbd "C-M-l") 'other-window)
(define-key global-map (kbd "<C-right>") 'other-window)
(define-key global-map (kbd "C-M-k")
  (lambda () (interactive) (other-window -1)))
(define-key global-map (kbd "<C-left>")
  (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-o") 'occur)
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

(when (eq system-type 'darwin)
  (setq-default ns-command-modifier 'meta))

(custom-set-faces
 '(cursor ((t (:foreground "white"))))
 '(region ((t (:background "#696969" :foreground "#d7af87"))))
 '(fringe ((t (:background "#1c1c1c"))))
 '(default ((t (:background "#262626" :foreground "white"))))
 '(highlight ((t (:background "#444444"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#add8e6"))))
 '(font-lock-variable-name-face ((t (:bold t :foreground "#afd7af"))))
 '(font-lock-keyword-face ((t (:foreground "#d7afd7"))))
 '(font-lock-string-face ((t (:foreground  "#d7af00"))))
 '(font-lock-doc-face ((t (:foreground "#8a8a8a"))))
 '(font-lock-comment-face ((t (:foreground "#8a8a8a"))))
 '(font-lock-type-face ((t (:foreground "#d7d75f"))))
 '(font-lock-builtin-face ((t (:foreground "#ff8700"))))
 '(font-lock-constant-face ((t (:foreground "#d78787"))))
 '(font-lock-warning-face ((t (:foreground "#ff5faf"))))
 '(font-lock-preprocessor-face ((t (:foreground "#afafd7"))))
 '(ediff-odd-diff-A ((t (:background "#444444"))))
 '(ediff-odd-diff-Ancestor ((t (:background "#444444"))))
 '(ediff-odd-diff-B ((t (:background "#444444"))))
 '(ediff-odd-diff-C ((t (:background "#444444"))))
 '(ediff-even-diff-A ((t (:background "#444444"))))
 '(ediff-even-diff-Ancestor ((t (:background "#444444"))))
 '(ediff-even-diff-B ((t (:background "#444444"))))
 '(ediff-even-diff-C ((t (:background "#444444"))))
 '(ediff-current-diff-A ((t (:background "#5f005f" :foreground "white"))))
 '(ediff-current-diff-Ancestor ((t (:background "#5f0000" :foreground "white"))))
 '(ediff-current-diff-B ((t (:background "#335533" :foreground "white"))))
 '(ediff-current-diff-C ((t (:background "#5f5f00" :foreground "white"))))
 '(ediff-fine-diff-A ((t (:background "#d0d0d0" :foreground "black"))))
 '(ediff-fine-diff-Ancestor ((t (:background "#d0d0d0" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#d0d0d0" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "#d0d0d0" :foreground "black")))))
(global-font-lock-mode 1)

(defun my/set-key-other-window ()
  (local-set-key (kbd "C-M-l") 'other-window))

(require 'ido)
(setq ido-enable-regexp t
      ido-enable-flex-matching t)
(ido-everywhere 1)
(ido-mode 1)

(require 'comint)
(setq comint-input-ignoredups t
      comint-input-ring-size 1000)
(add-hook 'comint-mode-hook #'my/set-key-other-window)
(add-hook 'eshell-mode-hook #'my/set-key-other-window)

(require 'dired)
(setq dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-listing-switches "-lah")

(require 'recentf)
(setq recentf-max-menu-items 30
      recentf-max-saved-items 30)
(global-set-key (kbd "<f9>") 'recentf-open-files)
(add-to-list 'recentf-exclude "/mnt")
(add-to-list 'recentf-exclude "/usr/include")
(recentf-mode 1)

(defun w-split ()
  (interactive)
  (split-window (selected-window) 80 'right)
  (other-window 1)
  (split-window (selected-window) 80 'right))

(defun xsel-naive-copy ()
  (interactive)
  (let* ((buf (buffer-name))
         (region (region-active-p))
         (beg (if region (mark) (point)))
         (end (if region (point) (line-end-position))))
    (when region (setq deactivate-mark t))
    (unless (= beg end)
      (message "copy to clipboard...")
      (with-temp-buffer
        (insert-buffer-substring-no-properties buf beg end)
        (call-process-region
         (point-min) (point-max) "xsel" nil 0 nil "-ib")))))

(define-key global-map (kbd "C-c w") 'xsel-naive-copy)

(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "<f5>") 'revert-buffer)
(define-key mode-specific-map (kbd "C-c") 'compile)

(when (display-graphic-p)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode -1)
  (setq-default x-select-enable-clipboard t))
