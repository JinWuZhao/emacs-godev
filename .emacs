;;; -*- lexical-binding:t -*-

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xclip color-theme-modern smex protobuf-mode real-auto-save company-restclient restclient zoom-window neotree f zoom highlight-parentheses markdown-mode counsel yasnippet-snippets eglot go-mode ace-window magit)))
 '(zoom-size (quote (0.618 . 0.618))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun require-packages (packages)
  (while packages
    (require-package (car packages))
    (setq packages (cdr packages)))
  t)

(require-packages package-selected-packages)

(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "custom")))

(display-time)
(setq explicit-shell-file-name "/bin/bash")

(load-theme 'calm-forest t t)
(enable-theme 'calm-forest)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(electric-pair-mode t)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(require 'real-auto-save)
(setq real-auto-save-interval 1)
(add-hook 'prog-mode-hook 'real-auto-save-mode)

(global-set-key (kbd "C-c .") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c ,") 'unhighlight-regexp)

(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c k") 'uncomment-region)
(global-set-key (kbd "C-c l") 'comment-line)

(global-set-key (kbd "C-c g") 'magit-status)

(require 'zoom)
(global-set-key (kbd "M-+") 'zoom)
(global-set-key (kbd "M-_") 'balance-windows)

(require 'zoom-window)
(global-set-key (kbd "C-x +") 'zoom-window-zoom)
(global-set-key (kbd "C-x _") 'zoom-window-next)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always nil)

(require 'f)
(require 'subr-x)
;; load gopath config file
(defun go-gopath-load (name)
  (interactive
   (let ((gopath-file ".gopath"))
     (list
      (read-file-name (format "GOPATH config file:(default '%s') " gopath-file)
                      default-directory gopath-file nil gopath-file))))
  (let ((filename (if name name (concat default-directory ".gopath"))))
    (if (f-exists? filename)
        (let ((contents (f-read-text filename 'utf-8)))
          (setenv "GOPATH" (string-trim-right (shell-command-to-string (concat "echo " contents)) "\n")))
      nil))
  nil)
;; save gopath config file
(defun go-gopath-save (name)
  (interactive
   (let ((gopath-file ".gopath"))
     (list
      (read-file-name (format "GOPATH config file:(default '%s') " gopath-file)
                      default-directory gopath-file nil gopath-file))))
  (let ((filename (if name name (concat default-directory ".gopath")))
        (gopath (getenv "GOPATH")))
    (if gopath
        (f-write-text gopath 'utf-8 filename)
      (message "the environment 'GOPATH' is empty")))
  nil)
;; autoload gopath
(defun go-gopath-autoload ()
  (interactive)
  (go-gopath-load nil))

(add-hook 'dired-mode-hook #'go-gopath-autoload)

(require 'go-mode)
(set-variable 'gofmt-command "goimports")

(defun go-mode-fmt ()
  (interactive)
  (when (eq major-mode 'go-mode)
    (gofmt)))
(global-set-key (kbd "C-c f") 'go-mode-fmt)

;; (require 'yasnippet)
;; (yas-global-mode 1)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-s") 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c m") 'counsel-bookmark)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c b") 'counsel-ibuffer)
(global-set-key (kbd "C-c j") 'counsel-file-jump)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'zone)
(zone-when-idle 300)

(setq confirm-kill-emacs 'yes-or-no-p)

(require 'bookmark)
(setq bookmark-default-file "/mnt/share/Documents/.bookmark")

(require 'company-restclient)
(push 'company-restclient company-backends)
(add-hook 'restclient-mode-hook #'company-mode-on)

(require 'eglot)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
(add-hook 'go-mode-hook #'eglot-ensure)
