(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("melpa" . 20)
        ("gnu" . 10)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(mapc (lambda (x) (unless (package-installed-p x) (package-install x)))
      '(hydra
        erlang
        popwin
        use-package
        slime-company
        virtualenvwrapper
        exec-path-from-shell))

(mapc (lambda (x)
        (let* ((dir (locate-user-emacs-file "packages"))
               (files (directory-files-recursively dir (cdr x) (car x)))
               (newest (car (last (sort files 'string-version-lessp)))))
          (when newest
            (save-match-data
              (string-match (concat (car x) "/" (cdr x)) newest)
              (unless (package-installed-p
                       (intern (match-string 1 newest))
                       (version-to-list (match-string 2 newest)))
                (package-install-file newest))))))
      '(("prettier\\.el" . "\\(prettier\\)-\\([0-9\\.]+\\)\\.tar")))

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
