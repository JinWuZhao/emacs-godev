;;; -*- lexical-binding:t -*-

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.emacs-china.org/gnu/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://elpa.emacs-china.org/melpa-stable/")) t)
)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-go company-restclient restclient zoom-window neotree f zoom highlight-parentheses flycheck-golangci-lint flycheck markdown-mode company-lsp counsel yasnippet-snippets go-mode ace-window magit)))
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

(display-time)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(electric-pair-mode t)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(global-set-key (kbd "C-c .") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c ,") 'unhighlight-regexp)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-k") 'uncomment-region)
(global-set-key (kbd "C-c C-l") 'comment-line)

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
          (setenv "GOPATH" contents))
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

(go-gopath-autoload)

(require 'go-mode)
(set-variable 'gofmt-command "goimports")
(add-hook 'go-mode-hook #'flycheck-mode)
(require 'lsp-go)
(add-hook 'go-mode-hook #'lsp-go-enable)
(add-hook 'go-mode-hook #'company-mode-on)
(add-hook 'before-save-hook #'lsp-format-buffer)

(require 'company-lsp)
(push 'company-lsp company-backends)

(require 'yasnippet)
(yas-global-mode 1)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c m") 'counsel-bookmark)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
