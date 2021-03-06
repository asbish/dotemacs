(declare-function w-split "base.el" nil)
(declare-function my/set-key-other-window "base.el" nil)

(add-to-list 'load-path (locate-user-emacs-file "packages/asbish"))
(require 'asbish)

(add-to-list 'load-path (locate-user-emacs-file "packages/quelpa"))
(require 'quelpa)

(require 'xref)
(require 'recentf)

(define-key global-map (kbd "C-c C-+") 'asbish/draft)
(add-hook 'ediff-mode-hook (lambda () (asbish/quick-window-set nil)))

(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(require 'winner)
(defhydra hydra-winner (global-map "<f10>")
  ("<left>" winner-undo "undo")
  ("<right>" winner-redo "redo"))

(global-set-key (kbd "<f10> q") 'asbish/quick-window)
(global-set-key (kbd "<f10> Q") 'asbish/quick-window-set)
(global-set-key (kbd "<f10> c") 'tramp-cleanup-connection)
(global-set-key (kbd "<f10> B") 'tramp-cleanup-all-buffers)
(global-set-key (kbd "<f10> C") 'tramp-cleanup-all-connections)

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-position 'below
        imenu-list-focus-after-activation t)
  (defun my/imenu-list-after-jump ()
    (if imenu-list-minor-mode
        (imenu-list-minor-mode -1)
      (message "Notice: `imenu-list-minor-mode' is not active.")
      (let ((win (get-buffer-window "*Ilist*")))
        (when win (quit-restore-window win 'bury)))))
  (setq imenu-list-after-jump-hook '(my/imenu-list-after-jump))
  (global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
  (define-key mode-specific-map (kbd "I") 'imenu-list-smart-toggle))

(use-package delight
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-mode 1))

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t)
  (global-set-key (kbd "<f11>") 'ag))

(use-package ddskk
  :ensure t
  :bind (("C-c o" . skk-mode))
  :init
  (custom-set-variables
   '(skk-egg-like-newline t)
   '(skk-kakutei-key "")
   '(skk-use-color-cursor nil))
  :config
  (setq default-input-method "japanese-skk"
        skk-preload t))

(use-package magit
  :ensure t)

(use-package monky
  :ensure t)

(global-set-key
 (kbd "<f12>")
 (lambda ()
   (interactive)
   (if (locate-dominating-file default-directory ".hg")
       (call-interactively 'monky-status)
     (call-interactively 'magit-status))))

(use-package iter2
  :ensure t)

(use-package comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'zscript-mode-hook #'enable-paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "<C-left>") nil)
  (define-key paredit-mode-map (kbd "<C-right>") nil))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  :config
  (setq multiple-cursors-mode nil
        mc--read-char nil
        mc--read-quoted-char nil
        rectangular-region-mode nil))

(use-package expand-region
  :ensure t
  :bind (("C-+" . er/expand-region)))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style
        '(empty face spaces space-mark tabs tab-mark trailing))
  (setq whitespace-space-regexp "\\(\u3000+\\)"
        whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])))
  (set-face-attribute 'whitespace-trailing nil :background "red")
  (set-face-attribute 'whitespace-empty nil :background "#878787")
  (set-face-attribute 'whitespace-space-after-tab nil :background "red")
  (set-face-attribute 'whitespace-tab nil :background "#303030")
  (set-face-attribute 'whitespace-space
                      nil
                      :weight 'bold
                      :foreground "red"
                      :background asbish/whitespace-background)
  (global-set-key (kbd "<f7> <f7>") 'global-whitespace-mode)
  (global-set-key (kbd "<f7> t") 'asbish/whitespace-tab-toggle)
  (global-whitespace-mode 1))

(defun my/whitespace-trailing-space-remap ()
  (face-remap-add-relative 'whitespace-trailing '((:background "#585858"))))

(use-package direnv
  :ensure t
  :init
  (custom-set-variables
   '(direnv-always-show-summary nil))
  :config
  (direnv-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq
   projectile-enable-caching t
   projectile-file-exists-remote-cache-expire nil
   projectile-indexing-method 'alien
   projectile-dynamic-mode-line nil
   projectile-mode-line-function (lambda () "")
   projectile-sort-order 'recentf)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode 1))

(use-package flymake
  :defer t
  :functions my/flymake-mode-setup
  :init
  (custom-set-variables
   '(flymake-gui-warnings-enabled nil)
   '(flymake-start-syntax-check-on-newline nil)
   '(flymake-start-syntax-check-on-newline nil)
   '(flymake-master-file-dirs (quote ("." "./src"))))
  (defun my/flymake-mode-setup ()
    (local-set-key (kbd "C-c `") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c C-v") 'flymake-display-err-menu-for-current-line))
  (add-hook 'flymake-mode-hook #'my/flymake-mode-setup))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :functions my/flycheck-error
  :config
  (setq flycheck-display-errors-delay 0.5
        flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (when (fboundp 'flycheck-overlays-at)
    (defun my/flycheck-error (str &rest args)
      (if (and flycheck-mode (flycheck-overlays-at (point))) t nil))
    (advice-add eldoc-message-function :before-until #'my/flycheck-error))
  (global-flycheck-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode)

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)
  (custom-set-variables
   '(lsp-ui-doc-enable nil)
   '(lsp-ui-doc-header t)
   '(lsp-ui-doc-max-width 80)
   '(lsp-ui-doc-position 'bottom)
   '(lsp-ui-doc-include-signature t)
   '(lsp-ui-flycheck-enable t)
   '(lsp-ui-peek-enable t)
   '(lsp-ui-peek-list-width 60)
   '(lsp-ui-sideline-enable nil)
   '(lsp-document-sync-method 'full))
  (custom-set-faces
   '(lsp-ui-doc-background
     ((t (:background "brightblack"))))
   '(lsp-ui-sideline-code-action
     ((t (:background "brightblack" :foreground "yellow"))))))

(use-package lsp-mode
  :ensure t
  :requires lsp-ui
  :diminish
  :init
  (custom-set-variables
   '(lsp-response-timeout 5)
   '(lsp-links-check-internal 0.5)
   '(lsp-prefer-flymake nil))
  (add-to-list 'safe-local-variable-values '(my/lsp-off . t))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package ccls
  :ensure t)

;; https://github.com/MaskRay/ccls/wiki/lsp-mode

(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))

(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))

(defun ccls/base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))

(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))

(defun ccls/member (kind)
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; References w/ Role::Role
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom
   "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom
   "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

(let ((map lsp-mode-map))
  (define-key map (kbd "C-c , /") 'ccls-call-hierarchy)
  (define-key map (kbd "C-c , ,") 'ccls/callee)
  (define-key map (kbd "C-c , .") 'ccls/caller)
  (define-key map (kbd "C-c , r") 'ccls/references-read)
  (define-key map (kbd "C-c , w") 'ccls/references-write)
  (define-key map (kbd "C-c , #") 'ccls/references-macro)
  (define-key map (kbd "C-c , n") 'ccls/references-not-call))

(defvar my/company-backends nil)
(use-package company
  :ensure t
  :diminish company-mode
  :bind (("C-c /" . company-complete-common)
         :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection))
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq my/company-backends
        (cl-set-difference company-backends
                           '(company-bbdb
                             company-clang
                             company-cmake
                             company-css
                             company-nxml
                             company-eclim
                             company-semantic
                             company-oddmuse
                             company-xcode)))
  (setq company-backends my/company-backends))

(use-package treemacs
  :ensure t
  :functions w-split-treemacs
  :config
  (setq
   treemacs-position 'left
   treemacs-tag-follow-mode nil
   treemacs-filewatch-mode nil
   treemacs-eldoc-display nil
   treemacs-fringe-indicator-mode nil
   treemacs-no-png-images t
   treemacs-indentation 1)
  (custom-set-faces
   '(treemacs-root-face ((t (:inherit font-lock-type-face :underline t)))))
  (add-to-list 'recentf-exclude "treemacs-persist")
  (defun w-split-treemacs (&optional w)
    (interactive)
    (or w (setq w 82))
    (delete-other-windows)
    (when (and (fboundp 'treemacs-get-local-window)
               (treemacs-get-local-window))
      (treemacs))
    (let ((c 0) (tw (window-body-width)))
      (while (> tw w) (setq c (+ c 1)) (setq tw (- tw w)))
      (if (or (< tw 10) (= c 2))
          (progn
            (message "Too small window width. Skip `treemacs`")
            (w-split))
        (setq treemacs-width tw)
        (treemacs)
        (dotimes (i (- c 1))
          (other-window 1)
          (split-window (selected-window) w 'right))
        (select-window
         (get-buffer-window (asbish/find-buffer "treemacs")))))))

(require 'hideshow)
(diminish 'hs-minor-mode)
(asbish/rebind-keys hs-minor-mode-map
  '(:from "C-c @ C-c" :to "C-c f" :bind hs-toggle-hiding)
  '(:from "C-c @ C-h" :to "C-c @ h" :bind hs-hide-block)
  '(:from "C-c @ C-s" :to "C-c @ s" :bind hs-show-block)
  '(:from "C-c @ C-M-h" :to "C-c @ H" :bind hs-hide-all)
  '(:from "C-c @ C-M-s" :to "C-c @ S" :bind hs-show-all))

(setq-default tags-revert-without-query 1)

(use-package nvm
  :ensure t)

(defvar prettier-el-home "~/opt/prettier.el/dist/")
(use-package prettier
  :load-path prettier-el-home
  :init
  (defun my/prettier-mode-ignore ()
    (and buffer-file-name
         (or
          (string-match
           "/\\(node_modules/\\|flow-typed/\\|package\\.json\\)"
           buffer-file-name)
          (not (local-variable-p 'my/prettier-on)))))
  (custom-set-variables
   '(prettier-mode-ignore-buffer-function #'my/prettier-mode-ignore)
   '(prettier-pre-warm 'some))
  (add-to-list 'safe-local-variable-values '(my/prettier-on . t))
  (add-hook 'after-init-hook #'global-prettier-mode))

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  (asbish/rebind-keys dumb-jump-mode-map
    '(:from "C-M-g" :to "C-M-." :bind dumb-jump-go)
    '(:from "C-M-p" :to "C-M-," :bind dumb-jump-back)
    '(:from "C-M-q" :to "C-c d q" :bind dumb-jump-quick-look))
  (define-key dumb-jump-mode-map (kbd "C-c d i") 'dumb-jump-go-prompt)
  (define-key dumb-jump-mode-map (kbd "C-c d .") 'dumb-jump-go-other-window))

(defvar my/gtags (executable-find "gtags"))
(use-package ggtags
  :ensure t
  :config
  (define-key ggtags-mode-map (kbd "C-c M-k") nil)
  (define-key ggtags-mode-map (kbd "C-c M-b") nil)
  (define-key ggtags-mode-map (kbd "C-c M-i") nil)
  (define-key ggtags-mode-map (kbd "C-c M-?") nil)
  (define-key ggtags-mode-map (kbd "C-c M-DEL") nil)
  (asbish/rebind-keys ggtags-mode-map
    '(:from "M-." :to "C-c g ." :bind ggtags-find-tag-dwim)
    '(:from "M-]" :to "C-c g M-." :bind ggtags-find-reference)
    '(:from "C-c %" :to "C-c g RET" :bind ggtags-query-replace)
    '(:from "C-c U" :to "C-c g U" :bind ggtags-update-tags)
    '(:from "C-M-." :to "C-c g r" :bind ggtags-find-tag-regexp)
    '(:from "C-c M-p" :to "C-c g <" :bind ggtags-prev-mark)
    '(:from "C-c M-n" :to "C-c g >" :bind ggtags-next-mark)
    '(:from "C-c M-%" :to "C-c g %" :bind ggtags-query-replace)
    '(:from "C-c M-g" :to "C-c g g" :bind ggtags-grep)
    '(:from "C-c M-f" :to "C-c g f" :bind ggtags-find-file)
    '(:from "C-c M-o" :to "C-c g o" :bind ggtags-find-other-symbol)
    '(:from "C-c M-j" :to "C-c g p" :bind ggtags-visit-project-root)
    '(:from "C-c M-h" :to "C-c g h" :bind ggtags-view-tag-history)
    '(:from "C-c M-/" :to "C-c g s" :bind ggtags-view-search-history)
    '(:from "C-c M-SPC" :to "C-c g SPC" :bind ggtags-save-to-register))
  (set-face-attribute 'ggtags-highlight nil :underline nil))

(require 'prog-mode)
(define-key prog-mode-map (kbd "C-c C-c") nil)
(define-key prog-mode-map (kbd "C-M-;") 'comment-or-uncomment-region)

(require 'make-mode)
(add-to-list 'auto-mode-alist '("\\.incl\\'" . makefile-mode))
(define-key makefile-mode-map (kbd "C-c C-c") nil)
(define-key makefile-mode-map (kbd "C-M-;") 'comment-or-uncomment-region)
(asbish/rebind-keys makefile-mode-map
  '(:from "\e\t" :to "C-c M-/" :bind completion-at-point)
  '(:from "C-c C-\\" :to "C-c M-\\" :bind makefile-backslash-region))
(add-hook 'makefile-mode-hook
          (lambda ()
            (asbish/whitespace-tab-toggle)))

(use-package cmake-mode
  :ensure t
  :init
  (add-hook 'cmake-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-cmake my/company-backends)))))

(use-package meson-mode
  :ensure t)

(use-package gn-mode
  :ensure t
  :mode "\\.gni?\\'"
  :init
  (custom-set-variables
   '(gn-cleanup-on-load nil)
   '(gn-cleanup-on-save nil)))

(use-package ninja-mode
  :ensure t
  :init
  (add-hook 'ninja-mode-hook
            (lambda ()
              (setq tab-width 2
                    indent-line-function 'insert-tab))))

(use-package bazel-mode
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(require 'gud)
(require 'gdb-mi)
(setq-default gdb-many-windows t
              gdb-show-main t)

(define-key gud-minor-mode-map (kbd "C-x C-a M-j") 'gud-jump)

(defun my/gdb-start ()
  (interactive)
  ;; TODO: better way to check gdb-many-windows layout
  (let ((gdb-layout (<= 6 (length (window-list))))
        (buffer (asbish/find-buffer "^\\*gud")))
    (unless gdb-layout (asbish/quick-window-set nil))
    (when (and (not gdb-layout) buffer (get-buffer-process buffer))
      (gdb-restore-windows)))
  (call-interactively 'gdb))

(use-package realgud
  :ensure t
  :init
  (custom-set-variables
   '(realgud-populate-common-fn-keys-function nil)))

(use-package realgud-lldb :ensure t)

(defvar my/realgud-alist-mode-preset
  '((sh-mode ((realgud:bashdb . "^\\*bashdb") (realgud:zshdb . "^\\*zshdb")))
    (cperl-mode ((realgud:perldb . "^\\*perldb")))
    (python-mode ((realgud:pdb . "^\\*pdb")))))

(defun my/realgud-start ()
  (interactive)
  (let* ((lis (or (cadr (assoc major-mode my/realgud-alist-mode-preset))
                  '((lldb . "^\\*lldb"))))
         (dbg (if (= 1 (length lis)) (car lis)
                (assoc
                 (intern (ido-completing-read
                          "Select: "
                          (mapcar (asbish/compose #'symbol-name #'car) lis)))
                 lis)))
         (source-buffer (current-buffer)))
    (unless (symbol-value 'realgud-short-key-mode)
      (asbish/quick-window-set nil)
      (call-interactively (car dbg)))
    (delete-other-windows)
    (set-window-buffer (selected-window) source-buffer)
    (set-window-buffer (split-window-horizontally)
                       (asbish/find-buffer (cdr dbg)))))

(global-set-key (kbd "<f6> g") 'my/gdb-start)
(global-set-key (kbd "<f6> r") 'my/realgud-start)
(global-set-key (kbd "<f6> <f6>") 'asbish/quick-window) ;; reset window

(use-package docker
  :ensure t
  :init
  (global-set-key (kbd "<f10> d") 'docker))

(use-package dockerfile-mode
  :ensure t
  :config
  (define-key dockerfile-mode-map (kbd "C-c C-b") nil)
  (define-key dockerfile-mode-map (kbd "C-c M-b") nil))

(use-package docker-compose-mode :ensure t)

(use-package docker-tramp
  :ensure t
  :init
  (custom-set-variables
   '(docker-tramp-use-names t)))

(use-package sh-script
  :defer t
  :init
  (setq-default sh-basic-offset 4
                sh-indentation 4)
  :config
  (asbish/rebind-keys sh-mode-map
    '(:from "C-c TAB" :to "C-c C-i i" :bind sh-if)
    '(:from "C-c C-c" :to "C-c C-i c" :bind sh-case)
    '(:from "C-c C-(" :to "C-c C-i f" :bind sh-function)
    '(:from "C-c C-+" :to "C-c C-i a" :bind sh-add)
    '(:from "C-c C-f" :to "C-c C-i f" :bind sh-for)
    '(:from "C-c C-l" :to "C-c C-i l" :bind sh-indexed-loop)
    '(:from "C-c C-t" :to "C-c C-i t" :bind sh-tmp-file)
    '(:from "C-c C-u" :to "C-c C-i u" :bind sh-until)
    '(:from "C-c C-w" :to "C-c C-i w" :bind sh-while)
    '(:from "C-c C-o" :to "C-c C-i o" :bind sh-while-getopts))
  (add-hook 'sh-mode-hook (lambda () (hs-minor-mode 1))))

(use-package cperl-mode
  :defer t
  :mode ("\\.\\([pP][Llm]\\|al\\|t\\)\\'" . cperl-mode)
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :init
  (defalias 'perl-mode 'cperl-mode)
  (custom-set-faces
   '(cperl-array-face ((t (:inherit font-lock-builtin-face))))
   '(cperl-hash-face ((t (:inherit font-lock-builtin-face))))
   '(cperl-nonoverridable-face ((t (:inherit font-lock-constant-face)))))
  :config
  (setq cperl-indent-level 4
        cperl-invalid-face nil
        cperl-electric-backspace-untabify nil
        cperl-merge-trailing-else nil
        cperl-indent-parens-as-block t
        cperl-indent-region-fix-constructs nil)
  (define-key cperl-mode-map (kbd "C-c C-a") nil) ;; cperl-toggle-auto-newline
  (define-key cperl-mode-map (kbd "C-c C-e") nil) ;; cperl-toggle-electric
  (define-key cperl-mode-map (kbd "C-c C-f") nil) ;; auto-fill-mode
  (define-key cperl-mode-map (kbd "C-c C-w") nil) ;; cperl-toggle-construct-fix
  (define-key cperl-mode-map (kbd "C-c C-k") nil) ;; cperl-toggle-abbrev
  (define-key cperl-mode-map (kbd "C-c C-n") nil) ;; cperl-narrow-to-here-doc
  (define-key cperl-mode-map (kbd "C-c C-t") nil) ;; cperl-invert-if-unless
  (define-key cperl-mode-map (kbd "C-c C-j") nil) ;; cperl-linefeed
  (define-key cperl-mode-map (kbd "C-c C-d") nil) ;; cperl-here-doc-spell
  (define-key cperl-mode-map (kbd "C-c C-p") nil) ;; cperl-pod-spell
  (asbish/rebind-keys cperl-mode-map
    '(:from "C-c C-b" :to "C-c l" :bind cperl-find-bad-style)
    '(:from "C-c C-h F" :to "C-c C-d F" :bind cperl-info-on-command)
    '(:from "C-c C-h P" :to "C-c C-d p" :bind cperl-perldoc-at-point)
    '(:from "C-c C-h a" :to "C-c C-d a" :bind cperl-toggle-autohelp)
    '(:from "C-c C-h f" :to "C-c C-d f" :bind cperl-info-on-current-command)
    '(:from "C-c C-h p" :to "C-c C-d P" :bind cperl-perldoc)
    '(:from "C-c C-h v" :to "C-c C-d v" :bind cperl-get-help)))

(require 'asm-mode)
(define-key asm-mode-map (kbd ":") nil)
(define-key asm-mode-map (kbd "RET") 'newline)
(add-hook 'asm-mode-hook
          (lambda ()
            (setq fill-prefix nil)
            (electric-indent-local-mode -1)))

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(define-key c-mode-base-map (kbd "C-c .") nil)
(define-key c-mode-base-map (kbd "C-c C-c") nil) ;; comment-region
(define-key c-mode-base-map (kbd "C-c C-b") nil) ;; c-submit-bug-report
(define-key c-mode-base-map (kbd "C-c C-a") nil) ;; c-toggle-auto-newline
(define-key c-mode-base-map (kbd "C-c C-l") nil) ;; c-toggle-electric-state
(define-key c-mode-base-map (kbd "C-c C-w") nil) ;; c-subword-mode
(define-key c-mode-base-map (kbd "C-M-j") nil) ;; c-indent-new-comment-line
(define-key c-mode-base-map (kbd "C-M-;") 'comment-or-uncomment-region)
(define-key c-mode-base-map (kbd "M-RET") 'lsp-rename)

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'google-set-c-style))

(use-package company-c-headers
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (define-key c-mode-base-map (kbd "C-c A") 'clang-format-buffer))

(add-to-list 'load-path (locate-user-emacs-file "packages/rtags/_build/src"))
(require 'rtags)
(require 'company-rtags)
(require 'flycheck-rtags)
(setq-default rtags-autostart-diagnostics t
              rtags-completions-enabled t
              rtags-popup-results-buffer nil)
(custom-set-faces '(rtags-skippedline ((t (:background "#1c1c1c")))))

(defun my/rtags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (and (not (rtags-find-symbol-at-point prefix))
          rtags-last-request-not-indexed))

(defun my/rtags-find-references-at-point (&optional prefix)
  (interactive "P")
  (and (not (rtags-find-references-at-point prefix))
          rtags-last-request-not-indexed))

(let ((map c-mode-base-map))
  (define-key map (kbd "C-c . .") 'my/rtags-find-symbol-at-point)
  (define-key map (kbd "C-c . ,") 'my/rtags-find-references-at-point)
  (define-key map (kbd "C-c . ;") 'rtags-find-symbol)
  (define-key map (kbd "C-c . ?") 'rtags-find-references)
  (define-key map (kbd "C-c . /") 'rtags-find-all-references-at-point)
  (define-key map (kbd "C-c . v") 'rtags-find-virtuals-at-point)
  (define-key map (kbd "C-c . <") 'rtags-location-stack-back)
  (define-key map (kbd "C-c . >") 'rtags-location-stack-forward)
  (define-key map (kbd "C-c . h") 'rtags-location-stack-visualize)
  (define-key map (kbd "C-c . I") 'rtags-imenu)
  (define-key map (kbd "C-c . f") 'rtags-find-file)
  (define-key map (kbd "C-c . b") 'rtags-list-results)
  (define-key map (kbd "C-c . i") 'rtags-symbol-info)
  (define-key map (kbd "C-c . t") 'rtags-symbol-type)
  (define-key map (kbd "C-c . l") 'rtags-diagnostics)
  (define-key map (kbd "C-c . s") 'rtags-display-summary)
  (define-key map (kbd "C-c . d") 'rtags-dependency-tree)
  (define-key map (kbd "C-c . D") 'rtags-dependency-tree-all)
  (define-key map (kbd "C-c . c") 'rtags-print-class-hierarchy)
  (define-key map (kbd "C-c . a") 'rtags-print-source-arguments)
  (define-key map (kbd "C-c . e") 'rtags-print-enum-value-at-point)
  (define-key map (kbd "C-c . U") 'rtags-restart-process)
  (define-key map (kbd "C-c . C-p") 'rtags-preprocess-file)
  (define-key map (kbd "C-c . C-c") 'rtags-compile-file)
  (define-key map (kbd "C-c . RET") 'rtags-rename-symbol))

(declare-function flycheck-add-next-checker "flycheck")
(flycheck-add-next-checker 'rtags 'c/c++-cppcheck)
(defun my/cc-mode-setup ()
  (c-set-style "linux")
  (setq-local c-basic-offset 4)
  (hs-minor-mode 1)
  (when (memq major-mode '(c-mode c++-mode objc-mode))
    (setq-local company-backends
                (append '(company-rtags
                          company-c-headers
                          company-semantic)
                        my/company-backends))
    (ggtags-mode 1)
    (add-hook 'hack-local-variables-hook
              (lambda ()
                (unless (local-variable-p 'my/lsp-off)
                  (require 'ccls)
                  (lsp-deferred)))
              nil t)))

(add-hook 'c-mode-hook #'my/cc-mode-setup t)
(add-hook 'c++-mode-hook #'my/cc-mode-setup t)
(add-hook 'objc-mode-hook #'my/cc-mode-setup t)

(use-package cuda-mode :ensure t)
(add-hook 'cuda-mode-hook #'my/cc-mode-setup t)

(use-package protobuf-mode
  :ensure t)

(use-package bison-mode
  :defer t
  :load-path "packages/bison-mode")

(use-package glsl-mode
  :ensure t
  :init
  (custom-set-faces
   '(glsl-deprecated-builtin-face
     ((t (:inherit glsl-builtin-face :underline t))))
   '(glsl-deprecated-keyword-face
     ((t (:inherit glsl-keyword-face :underline t))))
   '(glsl-deprecated-variable-name-face
     ((t (:inherit glsl-variable-name-face :underline t)))))
  :config
  (use-package company-glsl :ensure t)
  (add-hook 'glsl-mode-hook
            (lambda ()
              (when (executable-find "glslangValidator")
                (setq-local company-backends
                            (cons 'company-glsl my/company-backends))))))

(use-package cmm-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :config
  (use-package company-lua :ensure t)
  (add-hook 'lua-mode-hook
            (lambda ()
              (ggtags-mode 1)
              (setq-local company-backends
                          (cons 'company-lua my/company-backends)))))

(use-package arduino-mode :ensure t)

(use-package f90
  :defer t
  :init
  (add-hook 'f90-mode-hook (lambda ()
                             (hs-minor-mode 1)
                             (ggtags-mode 1)))
  :config
  (setq f90-do-indent 2
        f90-if-indent 2
        f90-type-indent 2
        f90-continuation-indent 2
        f90-auto-keyword-case nil
        f90-beginning-ampersand nil)
  (define-key f90-mode-map (kbd "C-c A") 'f90-indent-subprogram)
  (asbish/rebind-keys f90-mode-map
    '(:from "C-c C-a" :to "C-c C-p" :bind f90-previous-block)
    '(:from "C-c C-e" :to "C-c C-n" :bind f90-next-block)))

(use-package rustic
  :ensure t
  :init
  (custom-set-variables
   '(rustic-format-on-save nil)
   '(rustic-display-spinner nil)
   '(rustic-format-trigger 'on-save)
   '(rustic-format-display-method 'ignore)
   '(rustic-lsp-client 'eglot)
   '(rustic-lsp-server 'rust-analyzer))
  (custom-set-faces
   '(rustic-builtin-formatting-macro-face
     ((t (:inherit font-lock-preprocessor-face)))))
  :config
  (define-key rustic-mode-map (kbd "C-c A") 'rustic-format-buffer)
  (push 'rustic-clippy flycheck-checkers)
  (setq rustic-flycheck-clippy-params "--message-format=json")
  (add-hook 'rustic-mode-hook (lambda () (hs-minor-mode 1))))

(use-package rust-mode
  :requires rustic
  :ensure t
  :init
  ;; Remove `rustic-mode'
  (when (assoc "\\.rs\\'" auto-mode-alist)
    (cl-delete "\\.rs\\'" auto-mode-alist :test #'equal :key #'car))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (custom-set-variables
   '(rust-format-on-save nil))
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (asbish/read-only-mode "/\\(\\.rustup\\|\\.cargo\\|target\\)/")
              (rustic-mode))))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-mode
  :ensure t
  :config
  (use-package go-tag :ensure t)
  (use-package go-add-tags :ensure t)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c A") 'gofmt)
  (asbish/rebind-keys go-mode-map
    '(:from "C-x 4 C-c C-j" :to "C-x 4 M-." :bind godef-jump-other-window))
  (add-hook 'go-mode-hook
            (lambda ()
              (asbish/whitespace-tab-toggle)
              (lsp-deferred))))

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package meghanada
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (hs-minor-mode 1)
              (meghanada-mode t)))
  :config
  (setq meghanada-maven-path "mvn")
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  (define-key meghanada-mode-map (kbd "C-c b t") 'meghanada-switch-testcase)
  (define-key meghanada-mode-map (kbd "M-RET") 'meghanada-local-variable)
  (define-key meghanada-mode-map (kbd "C-c M-i") 'meghanada-reference)
  (define-key meghanada-mode-map (kbd "C-c M-t") 'meghanada-typeinfo)
  :commands
  (meghanada-mode))

(use-package groovy-imports
  :ensure t)

(use-package groovy-mode
  :ensure t
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
                (hs-minor-mode 1)))
  :config
  (setq groovy-highlight-assignments t))

(use-package scala-mode
  :ensure t
  :init
  (add-hook 'scala-mode
            (lambda () (hs-minor-mode 1))))

(use-package sbt-mode
  :ensure t
  :config
  (setq sbt:scroll-to-bottom-on-output t
        sbt:clear-buffer-before-command nil)
  (add-hook 'sbt-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers))))

(use-package dart-server :ensure t)
(use-package dart-mode
  :ensure t
  :config
  (define-key dart-mode-map (kbd "C-c A") 'dart-server-format)
  (add-hook 'dart-mode-hook
            (lambda ()
              (lsp-deferred))))

(use-package erlang
  :config
  (setq-default inferior-erlang-prompt-timeout t
                inferior-erlang-machine-options '("-sname" "emacs"))
  (setq-default erl-nodename-cache
                (make-symbol (concat "emacs@" (system-name))))
  (define-key erlang-mode-map (kbd "C-c C-c") nil) ;; comment-region
  (asbish/rebind-keys erlang-mode-map
    '(:from "C-c C-j" :to "C-c C-i" :bind erlang-generate-new-clause)
    '(:from "C-c C-a" :to "C-c A" :bind erlang-align-arrows))
  (add-hook 'erlang-mode-hook
            (lambda ()
              (lsp-deferred))))

(add-to-list 'load-path (locate-user-emacs-file "packages/j-mode"))
(use-package j-mode
  :defer t
  :config
  (setq j-console-cmd "ijconsole")
  (asbish/rebind-keys j-mode-map
    '(:from "C-c !" :to "C-c C-z" :bind j-console)
    '(:from "C-c h" :to "C-c M-d" :bind j-help-lookup-symbol)
    '(:from "C-c C-h" :to "C-c C-d" :bind j-help-lookup-symbol-at-point)
    '(:from "C-c C-c" :to "C-c C-b" :bind j-console-execute-buffer)
    '(:from "C-c C-l" :to "C-c C-n" :bind j-console-execute-line))
  (custom-set-faces
   '(j-verb-face ((t (:inherit font-lock-variable-name-face))))
   '(j-adverb-face ((t (:inherit font-lock-keyword-face :bold t))))
   '(j-conjunction-face ((t (:inherit font-lock-constant-face :bold t))))
   '(j-other-face ((t (:foreground "#bcbcbc"))))))

(use-package zig-mode :ensure t)

(use-package ruby-mode
  :ensure t
  :init
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil)
   '(ruby-use-encoding-map nil))
  :config
  (define-key ruby-mode-map (kbd "M-RET") 'lsp-rename)
  (define-key ruby-mode-map (kbd "C-c . .") 'xref-find-definitions)
  (define-key ruby-mode-map (kbd "C-c . /") 'xref-find-references)
  (define-key ruby-mode-map (kbd "C-c . ,") 'xref-pop-marker-stack)
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                 (lambda (arg) (ruby-end-of-block)) nil)))

(use-package bundler :ensure t)

(use-package inf-ruby
  :ensure t
  :config
  (setq-default inf-ruby-default-implementation "pry")
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rufo
  :ensure t
  :config
  (define-key ruby-mode-map (kbd "C-c A") 'rufo-format))

(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (xref-etags-mode 1)
            (asbish/read-only-mode "/\\(vendor\/bundle\\)/")
            (lsp-deferred)))

(require 'python)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --pprint")
(define-key python-mode-map (kbd "C-c C-j") nil) ;; imenu

(asbish/rebind-keys python-mode-map
  '(:from "C-c C-p" :to "C-c C-s" :bind run-python)
  '(:from "C-c C-c" :to "C-c C-b" :bind python-shell-send-buffer)
  '(:from "C-c C-f" :to "C-c C-d" :bind python-eldoc-at-point)
  '(:from "C-c C-v" :to "C-c l" :bind python-check)
  '(:from "C-c C-s" :to "C-c :" :bind python-shell-send-string)
  '(:from "C-c <" :to "C-c C-q <" :bind python-indent-shift-left)
  '(:from "C-c >" :to "C-c C-q >" :bind python-indent-shift-right)
  '(:from "C-c C-t c" :to "C-c C-i c" :bind python-skeleton-class)
  '(:from "C-c C-t d" :to "C-c C-i d" :bind python-skeleton-def)
  '(:from "C-c C-t f" :to "C-c C-i f" :bind python-skeleton-for)
  '(:from "C-c C-t i" :to "C-c C-i i" :bind python-skeleton-if)
  '(:from "C-c C-t m" :to "C-c C-i m" :bind python-skeleton-import)
  '(:from "C-c C-t t" :to "C-c C-i t" :bind python-skeleton-try)
  '(:from "C-c C-t w" :to "C-c C-i w" :bind python-skeleton-while))

(add-hook 'python-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (asbish/read-only-mode "/\\(\\.venv\\|venv\\)/")
            (setq imenu-create-index-function 'python-imenu-create-index)
            (setq-default flycheck-disabled-checkers '(python-pylint))))

(use-package lsp-python-ms
  :ensure t
  :init
  (custom-set-variables
   '(lsp-python-ms-auto-install-server nil)
   '(lsp-python-ms-cache "Library"))
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-python-ms)
              (lsp-deferred))))

(use-package python-black
  :ensure t
  :init
  (define-key python-mode-map (kbd "C-c A") 'python-black-buffer)
  :config
  (add-hook 'python-mode-hook
            (lambda () (python-black-on-save-mode-enable-dwim))))

(use-package conda
  :ensure t
  :init
  (setq-default mode-line-format
                (cons '(:exec conda-env-current-name) mode-line-format))
  :config
  (conda-env-initialize-interactive-shells))

(use-package nim-mode
  :ensure t
  :config
  (setq nimsuggest-path "~/.nimble/bin/nimsuggest")
  (add-hook 'nim-mode-hook (lambda () (nimsuggest-mode 1))))

(use-package ob-nim
  :ensure t
  :defer t)

(use-package solidity-mode
  :ensure t
  :config
  (use-package company-solidity :ensure t)
  (use-package solidity-flycheck :ensure t)
  (add-hook 'solidity-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-solidity my/company-backends)))))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)

(use-package racket-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ss\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
  :config
  (define-key racket-mode-map (kbd "C-c C-c") nil)
  (define-key racket-mode-map (kbd "C-c C-l") nil) ;; racket-logger
  (asbish/rebind-keys racket-mode-map
    '(:from "C-x C-e" :to "C-c C-n" :bind racket-send-last-sexp))
  (custom-set-faces
   '(racket-selfeval-face ((t (:foreground "white"))))
   '(racket-here-string-face ((t (:inherit font-lock-doc-face))))
   '(racket-logger-warning-face ((t (:inherit font-lock-warning-face))))))

(use-package slime
  :ensure t
  :init
  (asbish/load-file-if-exists "~/quicklisp/slime-helper.el")
  :config
  (define-key slime-mode-map (kbd "C-c C-j") nil)
  (asbish/rebind-keys slime-mode-map
    '(:from "C-c C-b" :to "C-c C" :bind slime-interrupt)
    '(:from "C-c C-c" :to "C-c C-n" :bind slime-compile-defun)
    '(:from "C-c TAB" :to "C-c M-/" :bind completion-at-point)
    '(:from "C-c C-e" :to "C-c :" :bind slime-interactive-eval)
    '(:from "C-c M-e" :to "C-c C-e" :bind macrostep-expand)
    '(:from "C-c M-m" :to "C-c M-e" :bind slime-macroexpand-all)
    '(:from "C-M-." :to "C-c . >" :bind slime-next-location)
    '(:from "C-M-," :to "C-c . <" :bind slime-previous-location))
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2GB")
                :coding-system utf-8-unix)
          (ccl ("lx86cl64")))) ;; x8664linux
  (setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
        slime-protocol-version 'ignore
        slime-net-coding-system 'utf-8-unix)
  (setq-default slime-company-completion 'fuzzy
                lisp-simple-loop-indentation 1
                lisp-loop-keyword-indentation 6
                lisp-loop-forms-indentation 6)
  (setq slime-contribs
        '(slime-fancy
          slime-asdf
          slime-banner
          slime-company ;; no need to load
          slime-indentation)))

(add-to-list 'load-path (locate-user-emacs-file "packages/forth-mode"))
(when (require 'forth-mode nil t)
  (require 'forth-block-mode)
  (require 'forth-interaction-mode))

(use-package brainfuck-mode
  :ensure t)

(use-package mips-mode
  :defer t
  :load-path "packages/emacs-mips-mode"
  :init
  (add-hook 'mips-mode-hook
            (lambda () (electric-indent-local-mode -1)))
  :config
  (asbish/rebind-keys mips-mode-map
    '(:from "C-c C-c" :to "C-c C-b" :bind mips-run-buffer) ;; TODO: not work
    '(:from "C-c C-l" :to "M-." :bind mips-goto-label-at-cursor)))

(use-package sml-mode
  :ensure t
  :interpreter "sml"
  :init
  (add-hook 'sml-mode-hook (lambda () (electric-indent-local-mode -1)))
  :config
  (asbish/rebind-keys sml-mode-map
    '(:from "C-c C-c" :to "C-c C-k" :bind sml-prog-proc-compile)))

(let ((run (asbish/shell-command-to-string "opam config var share")))
  (when (= 0 (car run))
    (add-to-list 'load-path
                 (expand-file-name "emacs/site-lisp/"
                                   (string-trim (cadr run))))
    (load "tuareg-site-file")))

(autoload 'merlin-mode "merlin" nil t)
(autoload 'ocp-index-mode "ocp-index" nil t)
(autoload 'ocp-setup-indent "ocp-indent" nil t)

(defvar tuareg-mode-map)
(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "C-c TAB") nil)
  (define-key tuareg-mode-map (kbd "C-c C-r") 'tuareg-eval-region)
  (asbish/rebind-keys tuareg-mode-map
    '(:from "C-c C-h" :to "C-c C-d" :bind caml-help)
    '(:from "C-c C-f" :to "C-c M-t" :bind caml-types-show-call)
    '(:from "C-c C-s" :to "C-c C-x" :bind tuareg-run-ocaml)
    '(:from "C-c C-k" :to "C-c C" :bind tuareg-kill-ocaml)
    '(:from "C-c TAB" :to "C-c C-/" :bind tuareg-complete)
    '(:from "C-x C-e" :to "C-c C-n" :bind tuareg-eval-phrase)
    '(:from "C-c C-a" :to "C-c C-f" :bind tuareg-find-alternate-file)
    '(:from "C-c . b" :to "C-c C-i b" :bind tuareg-insert-begin-form)
    '(:from "C-c . c" :to "C-c C-i c" :bind tuareg-insert-class-form)
    '(:from "C-c . f" :to "C-c C-i f" :bind tuareg-insert-for-form)
    '(:from "C-c . i" :to "C-c C-i i" :bind tuareg-insert-if-form)
    '(:from "C-c . l" :to "C-c C-i l" :bind tuareg-insert-let-form)
    '(:from "C-c . m" :to "C-c C-i m" :bind tuareg-insert-match-form)
    '(:from "C-c . t" :to "C-c C-i t" :bind tuareg-insert-try-form)
    '(:from "C-c . w" :to "C-c C-i w" :bind tuareg-insert-while-form)))

(setq-default merlin-command 'opam
              merlin-use-auto-complete-mode 'easy)

(defvar merlin-mode-map)
(with-eval-after-load 'merlin
  (asbish/rebind-keys merlin-mode-map
    '(:from "C-c C-r" :to "C-c l" :bind merlin-error-check)
    '(:from "C-c C-d" :to "C-c A" :bind merlin-destruct)
    '(:from "C-c C-x" :to "C-c `" :bind merlin-error-next)
    '(:from "C-c C-n" :to "C-c M-n" :bind merlin-phrase-next)
    '(:from "C-c C-p" :to "C-c M-p" :bind merlin-phrase-prev)
    '(:from "C-c C-l" :to "M-." :bind merlin-locate)
    '(:from "C-c &" :to "M-," :bind merlin-pop-stack)))

(add-hook 'inferior-caml-mode-hooks #'my/set-key-other-window)
(add-hook 'caml-mode-hook (lambda () (ocp-index-mode 1) (hs-minor-mode 1)))
(add-hook 'tuareg-mode-hook (lambda () (merlin-mode 1) (hs-minor-mode 1)))

(defvar my/stack-site-lisp
  '((hs-lint "hlint" "hs-lint.el")
    (hindent "hindent" "elisp/hindent.el")))

(let ((reg-ver "\s?v?\\([0-9.]+\\),?")
      (share (format "~/.stack/snapshots/*%s*/lts-*/*/share/*/" asbish/os)))
  (dolist (x my/stack-site-lisp)
    (let* ((bin (nth 1 x))
           (run (asbish/shell-command-to-string (concat bin " --version")))
           (out (cadr run))
           (ver (when (= 0 (car run))
                  (save-match-data
                    (and (string-match reg-ver out) (match-string 1 out)))))
           (path (when ver
                   (car (file-expand-wildcards
                         (concat share bin "-" ver "/" (nth 2 x)))))))
      (when path
        (add-to-list 'load-path (file-name-directory path))
        (require (nth 0 x))))))

(defvar my/haskdogs-input
  (concat asbish/find-cmd " . -type d \\( "
          "-path ./.git -o -path ./.svn -o -path ./_darcs "
          "-o -path ./.stack-work -o -path ./dist -o -path ./.cabal-sandbox "
          "\\) -prune -o -type f \\( "
          "-name '*.hs' -o -name '*.lhs' -o -name '*.hsc' "
          "\\) -exec readlink -f \{\} \\;"))

(defun my/haskdogs (dir)
  (let* ((file-list (make-temp-file "my-haskdogs-inputs"))
         (run (shell-command (format "cd %s && %s > %s"
                                     dir my/haskdogs-input file-list))))
    (if (= 0 run)
        (let ((buf (get-buffer-create "*output-haskdogs*")))
          (switch-to-buffer buf)
          (erase-buffer)
          (redisplay)
          (setq default-directory dir)
          (call-process
           "haskdogs" nil (current-buffer) t
           "--hasktags-args=-e -x --ignore-close-implementation"
           "--file-list" file-list)
          (delete-file file-list))
      (error "Could not generate input file."))))


(defvar haskell-mode-map)
(defvar haskell-collapse-mode-map)
(defvar interactive-haskell-mode-map)
(add-to-list 'load-path (locate-user-emacs-file "packages/haskell-mode/"))
(require 'haskell-mode-autoloads)
(require 'haskell-cabal)
(add-to-list 'Info-default-directory-list
             (locate-user-emacs-file "packages/haskell-mode/"))

(custom-set-variables
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(haskell-process-log t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -fshow-loaded-modules"
     "--no-build"
     "--no-load")))

(custom-set-faces
 '(haskell-constructor-face ((t (:foreground "#afaf00" :bold t))))
 '(haskell-operator-face ((t (:foreground "#5faf87" :bold t))))
 '(haskell-quasi-quote-face ((t (:inherit font-lock-constant-face)))))

(with-eval-after-load 'haskell-mode
  (defun my/haskell-tags ()
    (interactive)
    (if (y-or-n-p "use haskdogs? ")
        (let ((dir-target (or tags-file-name (haskell-cabal-find-file))))
          (if dir-target (my/haskdogs (file-name-directory dir-target))
            (error "Could not generate TAGS file.")))
      (haskell-mode-generate-tags)))
  (define-key haskell-mode-map (kbd "C-c C-,") nil)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c A") 'haskell-mode-stylish-buffer)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c U") 'my/haskell-tags)
  (when (fboundp 'hs-lint)
    (define-key haskell-mode-map (kbd "C-c l") 'hs-lint)))

(defun my/haskell-collapse-mode-setup ()
  (diminish 'haskell-collapse-mode)
  (define-key haskell-collapse-mode-map (kbd "C-c @ C-M-c") nil)
  (asbish/rebind-keys haskell-collapse-mode-map
    '(:from "C-c @ C-c" :to "C-c f" :bind haskell-hide-toggle)
    '(:from "C-c @ C-M-h" :to "C-c @ H" :bind haskell-hide-toggle-all)
    '(:from "C-c @ C-M-s" :to "C-c @ S" :bind haskell-hide-toggle-all)))

(defun my/interactive-haskell-mode-setup ()
  (diminish 'interactive-haskell-mode)
  (define-key interactive-haskell-mode-map (kbd "M-.") nil)
  (define-key interactive-haskell-mode-map (kbd "C-c C-b") nil)
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") nil)
  (asbish/rebind-keys interactive-haskell-mode-map
    '(:from "C-c C-r" :to "C-c C-a C-l" :bind haskell-process-reload)
    '(:from "C-c TAB" :to "C-c C-d" :bind haskell-process-do-info)
    '(:from "C-c C-x" :to "C-c C-k" :bind haskell-process-cabal)
    '(:from "C-c v c" :to "C-c C-f c" :bind haskell-cabal-visit-file)))

(use-package lsp-haskell
  :ensure t
  ;; TODO: Add nix integration. `lsp-haskell-process-wrapper-function`
  )

(defun my/haskell-mode-setup ()
  (if (locate-dominating-file
       (file-name-directory (buffer-file-name))
       "stack.yaml")
      (setq haskell-process-type 'stack-ghci)
    (setq flycheck-disabled-checkers '(haskell-stack-ghc)))
  (interactive-haskell-mode 1)
  (asbish/once #'my/interactive-haskell-mode-setup)
  (haskell-collapse-mode 1)
  (asbish/once #'my/haskell-collapse-mode-setup)
  (haskell-decl-scan-mode 1)
  (add-hook 'hack-local-variables-hook
            (lambda () (unless (local-variable-p 'my/lsp-off) (lsp-deferred)))
            nil t))

(add-hook 'haskell-mode-hook #'my/haskell-mode-setup)

(when (fboundp 'hindent-mode)
  (diminish 'hindent-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode))

(asbish/load-file-shell-command "agda-mode locate")
(with-eval-after-load 'agda2-mode
  (custom-set-faces
   '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
   '(agda2-highlight-string-face ((t (:inherit font-lock-string-face))))
   '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-module-face ((t (:foreground "white"))))
   '(agda2-highlight-number-face ((t (:foreground "white"))))
   '(agda2-highlight-record-face ((t (:foreground "#afafd7"))))
   '(agda2-highlight-field-face ((t (:foreground "#afafd7"))))
   '(agda2-highlight-postulate-face ((t (:foreground "#afafd7"))))
   '(agda2-highlight-inductive-constructor-face ((t (:foreground "#ff8700"))))
   '(agda2-highlight-function-face
     ((t (:inherit font-lock-function-name-face))))
   '(agda2-highlight-primitive-face
     ((t (:weight bold :foreground "#add8e6"))))
   '(agda2-highlight-primitive-type-face
     ((t (:weight bold :foreground "#d7d75f"))))))

(add-to-list 'load-path (locate-user-emacs-file "packages/PG"))
(load (locate-user-emacs-file "packages/PG/generic/proof-site"))
(require 'pg-vars)
(require 'proof-script)
(custom-set-variables
 '(proof-splash-enable nil))
(custom-set-faces
 '(proof-active-area-face ((t (:background "brightblack"))))
 '(proof-eager-annotation-face ((t (:foreground "#ff8700"))))
 '(proof-error-face ((t (:inherit font-lock-warning-face))))
 '(proof-locked-face ((t (:background "#005f87"))))
 '(proof-highlight-dependency-face
   ((t (:background "#bcbcbc" :slant italic))))
 '(proof-highlight-dependent-face
   ((t (:background "#bcbcbc" :slant italic))))
 '(proof-script-highlight-error-face
   ((t (:inherit font-lock-warning-face :underline t)))))
(defun my/proof-layout-windows ()
  "favorite layout"
  (interactive)
  (when (equal (current-buffer) proof-script-buffer)
    (delete-other-windows)
    (let* ((goals (get-buffer "*goals*"))
           (response (get-buffer "*response*"))
           (goals-win (split-window-horizontally))
           (goals-win-height (window-height))
           (response-win
            (or (other-window 1)
                (split-window-vertically (- goals-win-height 5)))))
      (set-window-buffer goals-win goals)
      (set-window-buffer response-win response))))
(define-key proof-mode-map (kbd "<f6> g") 'my/proof-layout-windows)

(with-eval-after-load 'proof
  (asbish/rebind-keys proof-mode-map
    '(:from "C-c C-l" :to "<f6> l" :bind proof-layout-windows)
    '(:from "C-c C-v" :to "C-c :" :bind proof-minibuffer-cmd)
    '(:from "C-c C-p" :to "C-c l" :bind proof-prf)
    '(:from "C-c C-c" :to "C-c C" :bind proof-interrupt-process)
    '(:from "C-c C-r" :to "C-c C-U" :bind proof-retract-buffer)))

(use-package company-coq
  :ensure t
  :config
  (add-hook 'coq-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-coq my/company-backends)))))

(use-package ess
  :ensure t
  :config
  (defun my/ess-eval-line-strip (&optional vis)
    (interactive "P")
    (apply 'ess-eval-region
           (append (asbish/line-strip "[^#][^#>\s]+")
                   (list vis "Eval line"))))
  (add-hook 'ess-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-n") 'my/ess-eval-line-strip)
              (setq ess-indent-with-fancy-comments nil))))

(use-package gnuplot-mode
  :ensure t)

(use-package lean-mode
  :ensure t
  :config
  (use-package company-lean :ensure t)
  (add-hook 'lean-mode-hook
            (lambda ()
              (setq-local company-backends
                          (cons 'company-lean my/company-backends)))))

(add-to-list 'load-path (locate-user-emacs-file "packages/zscript-mode"))
(require 'zscript-mode)
(define-key zscript-mode-map (kbd "C-c C-d") 'zscript-browse-command)
(define-key zscript-mode-map (kbd "TAB") 'tab-to-tab-stop)

(global-set-key (kbd "C-c L") 'add-change-log-entry)
(add-hook 'change-log-mode-hook (lambda () (asbish/whitespace-tab-toggle)))

(use-package tex-jp
  :ensure auctex
  :functions my/LaTeX-mode-setup
  :config
  (use-package reftex)
  (use-package company-math :ensure t)
  (use-package company-auctex :ensure t)
  (unless (image-type-available-p 'xpm) (setq LaTeX-enable-toolbar nil))
  (custom-set-variables
   '(japanese-TeX-engine-default 'uptex)
   '(japanese-LaTeX-default-style "jarticle")
   '(TeX-auto-save t)
   '(TeX-parse-self t)
   '(TeX-source-correlate-start-server t)
   '(TeX-command-output-list (quote (("LaTeX" ("pdf")))))
   '(reftex-plug-into-AUCTeX t))
  (asbish/rebind-keys LaTeX-mode-map
    '(:from "C-c C-v" :to "C-c m" :bind TeX-view)
    '(:from "C-c TAB" :to "C-c C-?" :bind TeX-goto-info-page)
    '(:from "C-c C-j" :to "C-c C-i" :bind LaTeX-insert-item))
  (asbish/rebind-keys reftex-mode-map
    '(:from "C-c /" :to "C-c _" :bind reftex-index-selection-or-word))
  (defun my/LaTeX-mode-setup ()
    (setq-local company-backends
                (append '(company-math-symbols-latex company-latex-commands)
                        my/company-backends))
    (my/whitespace-trailing-space-remap)
    (company-auctex-init)
    (turn-on-reftex))
  (add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-setup))

(require 'rst)

(use-package flycheck-vale
  :requires flycheck
  :ensure t
  :functions flycheck-buffer
  :config
  (flycheck-vale-setup)
  (setq-default flycheck-vale-enabled nil)
  (defun my/vale-toggle ()
    (interactive)
    (flycheck-vale-toggle-enabled)
    (flycheck-buffer))
  (define-key rst-mode-map (kbd "C-c l") 'my/vale-toggle)
  (define-key org-mode-map (kbd "C-c l") 'my/vale-toggle)
  (define-key text-mode-map (kbd "C-c l") 'my/vale-toggle)
  (define-key markdown-mode-map (kbd "C-c l") 'my/vale-toggle))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
            (lambda () (my/whitespace-trailing-space-remap)))
  :config
  (custom-set-variables
   '(markdown-coding-system "utf-8")
   '(markdown-indent-on-enter nil)
   '(markdown-hide-urls nil)))

(use-package json-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.stylelintrc\\'" . json-mode))
  :config
  (define-key json-mode-map (kbd "C-c P") nil)
  (define-key json-mode-map (kbd "C-c C-p") nil)
  (define-key json-mode-map (kbd "C-c C-f") nil)
  (define-key json-mode-map (kbd "C-c A") 'json-mode-beautify))

(use-package yaml-mode
  :ensure t
  :config
  (defun my/yaml-forward-sexp (&optional arg) ;; TODO: use arg
    (interactive)
    (let ((ind (current-indentation))
          (pmax (point-max)))
      (unless (asbish/empty-line-p)
        (while (progn
                 (forward-line)
                 (skip-chars-forward "[:blank:]")
                 (or (looking-at "#")
                     (end-of-line)
                     (> (current-indentation) ind)
                     (and (not (= (point) pmax))
                          (= (current-indentation) 0)
                          (asbish/empty-line-p)))))
        (while (progn ; backward
                 (forward-line -1)
                 (or (< (current-indentation) ind) (asbish/empty-line-p))))
        (move-end-of-line 1))))

  (add-to-list 'hs-special-modes-alist
               '(yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>" "" "#"
                           my/yaml-forward-sexp nil))

  (add-hook 'yaml-mode-hook
            (lambda () (hs-minor-mode 1))))

(use-package toml-mode
  :ensure t
  :init
  (add-hook 'toml-mode-hook
            (lambda () (hs-minor-mode 1))))

(use-package csv-mode
  :ensure t
  :config
  (asbish/rebind-keys csv-mode-map
    '(:from "C-c C-a" :to "C-c A" :bind csv-align-fields)))

(use-package simple-httpd
  :ensure t
  :config
  (custom-set-variables
   '(httpd-port 8888)
   '(httpd-root "~/public/"))
  (defun my/httpd-set-root (root)
    (interactive (list default-directory))
    (setq httpd-root root)
    (message (format "set 'httpd-root \"%s\"" httpd-root))))

(defun my/yarn-pnpify-sdk ()
  (let ((tsserver-path
         (asbish/find-in-rec ".yarn" "sdks/typescript/bin/tsserver"))
        (eslint-path
         (asbish/find-in-rec ".yarn" "sdks/eslint/bin/eslint.js"))
        (stylelint-path
         (asbish/find-in-rec ".yarn" "sdks/stylelint/bin/stylelint.js")))
    (when (or tsserver-path eslint-path stylelint-path)
      `(:tsserver ,tsserver-path :eslint ,eslint-path :stylelint ,stylelint-path))))

(use-package tide
  :ensure t
  :diminish tide-mode
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (define-key tide-mode-map (kbd "C-c A") 'prettier-prettify)
  (define-key tide-mode-map (kbd "M-RET") 'tide-rename-symbol)
  (define-key tide-mode-map (kbd "C-c C-d") 'tide-documentation-at-point))

(add-to-list 'load-path (locate-user-emacs-file "packages/js2-mode"))
(use-package js2-mode
  :defer t
  :init
  (setq-default js2-mirror-mode nil)
  (custom-set-faces
   '(js2-function-param ((t (:inherit font-lock-variable-name-face)))))
  (custom-set-variables
   '(js2-additional-externs nil t)
   '(js2-concat-multiline-strings 'eol)
   '(js2-highlight-external-variables nil)
   '(js2-idle-timer-delay 0.2)
   '(js2-include-browser-externs nil)
   '(js2-include-jslint-globals nil)
   '(js2-mode-show-parse-errors nil)
   '(js2-mode-show-strict-warnings nil)
   '(js2-strict-inconsistent-return-warning nil)
   '(js2-strict-missing-semi-warning nil)
   '(js2-strict-trainling-comma-warning nil))
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2)
  (define-key js2-mode-map (kbd "C-c C-a") nil)
  (define-key js2-mode-map (kbd "C-c C-e") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "C-c C-o") nil)
  (define-key js2-mode-map (kbd "C-c C-s") nil)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (define-key js2-mode-map (kbd "C-c C-w") nil)
  (define-key js-mode-map (kbd "C-c M-:") nil)
  (define-key js-mode-map (kbd "C-c C-j") nil)
  (define-key js-mode-map (kbd "C-M-x") nil))

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . rjsx-mode))
  :functions my/get-flow-bin
  :config
  (use-package flycheck-flow :ensure t)
  (use-package company-flow :ensure t)
  (asbish/rebind-keys rjsx-mode-map
    '(:from "C-c C-r" :to "C-c M-RET" :bind rjsx-rename-tag-at-point))
  (defun my/get-flow-bin ()
    (when (save-excursion
            (goto-char (point-min))
            (ignore-errors
              ;; TODO: need work
              (string-match-p "\/[\/|\*][\s\t]?+@flow[\s\t]?+"
                              (thing-at-point 'line t))))
      (or (asbish/find-in-rec
           "node_modules" (concat "flow-bin/*" asbish/os "*/flow"))
          "flow")))
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (let ((pnpify-sdk (my/yarn-pnpify-sdk))
                    (flow-bin (my/get-flow-bin))
                    (local-company-backends '(company-tide)))
                (setq-local tide-tsserver-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :tsserver))
                                nil))
                (setq-local flycheck-javascript-eslint-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :eslint))
                                (asbish/find-in-rec
                                 "node_modules" "eslint/bin/eslint.js")
                                "eslint"))
                (when flow-bin
                  (setq-local company-flow-executable flow-bin)
                  (setq-local flycheck-javascript-flow-executable flow-bin)
                  (cons 'company-flow local-company-backends))
                (setq-default flycheck-disabled-checkers
                              '(javascript-jshint))
                (setq-local company-backends
                            (append local-company-backends my/company-backends))
                (hs-minor-mode 1)
                (tide-setup)))))

(use-package typescript-mode
  :ensure t
  :delight
  (typescript-mode "TS")
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tslint 'javascript-eslint)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (let ((pnpify-sdk (my/yarn-pnpify-sdk)))
                (setq-local tide-tsserver-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :tsserver))
                                nil))
                (setq-local flycheck-javascript-eslint-executable
                            (or (and pnpify-sdk (plist-get pnpify-sdk :eslint))
                                (asbish/find-in-rec
                                 "node_modules" "eslint/bin/eslint.js")
                                "eslint"))
                (setq-local flycheck-typescript-tslint-executable
                            (or (asbish/find-in-rec
                                 "node_modules" "tslint/bin/tslint")
                                "tslint"))
                (setq-default flycheck-disabled-checkers
                              '(javascript-jshint jsx-tide))
                (setq-local company-backends
                            (cons 'company-tide my/company-backends))
                (eldoc-mode 1)
                (hs-minor-mode 1)
                (tide-setup)))))

(use-package coffee-mode
  :ensure t
  :init
  (custom-set-variables '(coffee-tab-width 2))
  :config
  (add-hook 'coffee-mode-hook
            (lambda ()
              (hs-minor-mode 1)
              (setq-local flycheck-coffee-executable
                          (or (asbish/find-in-rec
                               "node_modules" ".bin/coffee") "coffee")))))

(use-package php-mode
  :ensure t
  :config
  (use-package ac-php :ensure t)
  (use-package company-php :ensure t)
  (define-key php-mode-map (kbd "C-.") nil)
  (define-key php-mode-map (kbd "C-c C-f") nil)
  (define-key php-mode-map (kbd "C-c RET") nil)
  (define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
  (define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back)
  (define-key php-mode-map (kbd "C-c U") 'ac-php-remake-tags)
  (add-hook 'php-mode-hook
            (lambda ()
              (ac-php-core-eldoc-setup)
              (setq-local company-backends
                          (cons 'company-ac-php-backend my/company-backends)))))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.pug\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (use-package company-web :ensure t)
  (asbish/rebind-keys web-mode-map
    '(:from "C-c C-f" :to "C-c f" :bind web-mode-fold-or-unfold))
  (define-key web-mode-map (kbd "C-c C-w") nil)
  (add-hook 'web-mode-hook
            (lambda ()
              (toggle-truncate-lines)
              (setq web-mode-markup-indent-offset 2
                    web-mode-enable-auto-closing t)
              (setq-local company-backends
                          (cons 'company-web-html my/company-backends)))))

(use-package nxml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
  :config
  (setq nxml-child-indent 4)
  (setq nxml-outline-child-indent 4)
  (setq nxml-auto-insert-xml-declaration-flag t))

(use-package jade-mode
  :ensure t
  :init
  (add-hook 'jade-mode-hook
            (lambda ()
              (toggle-truncate-lines)
              (setq-local company-backends
                          (cons 'company-web-jade my/company-backends)))))

(use-package slim-mode
  :ensure t
  :init
  (add-hook 'slim-mode-hook
            (lambda ()
              (toggle-truncate-lines)
              (setq-local company-backends
                          (cons 'company-web-slim my/company-backends)))))

(use-package css-mode
  :ensure t
  :init
  (custom-set-variables '(css-indent-offset 2))
  (add-hook 'css-mode-hook
            (lambda ()
              (hs-minor-mode 1)
              (setq-local company-backends
                          (cons 'company-css my/company-backends))
              (setq-default flycheck-disabled-checkers '(css-css-lint))
              (let ((pnpify-sdk (my/yarn-pnpify-sdk)))
                (setq-local flycheck-css-stylelint-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :stylelint))
                                (asbish/find-in-rec
                                 "node_modules"
                                 "stylelint/bin/stylelint.js")
                                "stylelint"))))))

(use-package scss-mode
  :ensure t
  :init
  (define-key scss-mode-map (kbd "C-c C-c") nil)
  (add-hook 'scss-mode-hook
            (lambda ()
              (remove-hook 'after-save-hook 'scss-compile-maybe t)
              (hs-minor-mode 1)
              (setq-local company-backends
                          (cons 'company-css my/company-backends))
              (setq-default flycheck-disabled-checkers '(scss))
              (let ((pnpify-sdk (my/yarn-pnpify-sdk)))
                (setq-local flycheck-scss-stylelint-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :stylelint))
                                (asbish/find-in-rec
                                 "node_modules"
                                 "stylelint/bin/stylelint.js")
                                "stylelint"))))))

(use-package sass-mode
  :ensure t
  :init
  (add-hook 'sass-mode-hook
            (lambda ()
              (hs-minor-mode 1)
              (setq-local company-backends
                          (cons 'company-css my/company-backends))
              (setq-default flycheck-disabled-checkers '(sass))
              (let ((pnpify-sdk (my/yarn-pnpify-sdk)))
                (setq-local flycheck-css-stylelint-executable
                            (or (and pnpify-sdk
                                     (plist-get pnpify-sdk :stylelint))
                                (asbish/find-in-rec
                                 "node_modules"
                                 "stylelint/bin/stylelint.js")
                                "stylelint"))))))

(use-package emms
  :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (emms-all)
  (emms-default-players)
  (setq emms-player-list '(emms-player-mpg321
                           emms-player-ogg123
                           emms-player-mplayer
                           emms-player-vlc)))
