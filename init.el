(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(mapc (lambda (x) (unless (package-installed-p x) (package-install x)))
      '(f
        ag
        dash
        smex
        hydra
        popwin
        erlang
        realgud
        diminish
        which-key
        imenu-list
        levenshtein
        slime-company
        virtualenvwrapper
        use-package
        exec-path-from-shell))

(require 'server)
(unless (server-running-p) (server-start))

(require 'exec-path-from-shell)
(when (display-graphic-p)
  (let ((zsh (executable-find "zsh")))
    (setenv "SHELL" zsh))
  (exec-path-from-shell-initialize))

(let ((rc (file-name-as-directory
           (expand-file-name "rc" user-emacs-directory))))
  (mapc (lambda (x) (load (concat rc (symbol-name x))))
        '(base cap fun)))
