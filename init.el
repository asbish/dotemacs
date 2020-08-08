(require 'package)

(setq package-archives
      '(("my-elpa" . "~/elpapkgs/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("my-elpa" . 30)
        ("melpa" . 20)
        ("melpa-stable" . 15)
        ("gnu" . 10)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(mapc (lambda (x) (unless (package-installed-p x) (package-install x)))
      '(f
        dash
        hydra
        erlang
        levenshtein
        use-package
        slime-company
        exec-path-from-shell))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(let ((rc (file-name-as-directory
           (expand-file-name "rc" user-emacs-directory))))
  (mapc (lambda (x) (load (concat rc (symbol-name x))))
        '(base cap fun)))

;; Local system specific customizations.
(let ((my-local (locate-user-emacs-file "rc/my-local.el")))
  (when (file-exists-p my-local) (load my-local)))
