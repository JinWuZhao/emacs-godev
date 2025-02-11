;;; -*- lexical-binding:t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 '(package-selected-packages
   '(ace-jump-mode emojify pyim pyim-basedict xclip color-theme-modern solarized-theme spacemacs-theme monokai-theme dracula-theme smex protobuf-mode real-auto-save zoom-window neotree zoom highlight-parentheses flycheck markdown-mode counsel yasnippet-snippets go-mode ace-window magit))
 '(zoom-size '(0.618 . 0.618)))
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
(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "custom/lsp-bridge")))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(require 'emojify)
(let ((emoji-set "emojione-v2.2.6-22"))
  (if (not (file-exists-p (expand-file-name emoji-set emojify-emojis-dir)))
      (emojify-download-emoji emoji-set)
    nil)
  (set-variable 'emojify-emoji-set emoji-set))

(defun gui-setup ()
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Auto generated by cnfonts
  ;; <https://github.com/tumashu/cnfonts>
  (set-face-attribute
   'default nil
   :font (font-spec :name "-PfEd-DejaVu Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 11.5))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-ADBO-Source Han Sans CN-light-normal-normal-*-*-*-*-*-*-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 13.5)))
  (global-emojify-mode))

(if window-system
    (gui-setup)
  nil)

(require 'pyim)
(pyim-default-scheme 'microsoft-shuangpin)
(require 'pyim-basedict)
(pyim-basedict-enable)
(setq default-input-method "pyim")

(display-time)
(setq explicit-shell-file-name "/bin/bash")

(load-theme 'solarized-dark t)

(require 'awesome-tab)
(awesome-tab-mode t)
(setq awesome-tab-height 120)
(setq awesome-tab-active-bar-height 20)
(keymap-global-set "M-1" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-2" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-3" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-4" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-5" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-6" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-7" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-8" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-9" 'awesome-tab-select-visible-tab)
(keymap-global-set "M-0" 'awesome-tab-select-visible-tab)
(keymap-global-set "C-c ;" 'awesome-tab-forward)
(keymap-global-set "C-c h" 'awesome-tab-backward)
(keymap-global-set "C-c n" 'awesome-tab-forward-group)
(keymap-global-set "C-c p" 'awesome-tab-backward-group)
(keymap-global-set "C-c t" 'awesome-tab-counsel-switch-group)

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

(keymap-global-set "C-c ." 'highlight-symbol-at-point)
(keymap-global-set "C-c ," 'unhighlight-regexp)

(keymap-global-set "C-c /" 'comment-region)
(keymap-global-set "C-c k" 'uncomment-region)
(keymap-global-set "C-c l" 'comment-line)

(keymap-global-set "C-c g" 'magit-status)
(require 'magit)
(set-variable 'magit-process-yes-or-no-prompt-regexp "(\\(yes\\)/\\(no\\)/\\[fingerprint\\])\\? $")

(require 'zoom)
(keymap-global-set "M-+" 'zoom)
(keymap-global-set "M-_" 'balance-windows)

(require 'zoom-window)
(keymap-global-set "C-x +" 'zoom-window-zoom)
(keymap-global-set "C-x _" 'zoom-window-next)

(keymap-global-set "M-o" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always nil)

(require 'ace-jump-mode)
(keymap-global-set "M--" 'ace-jump-word-mode)
(keymap-global-set "C-M--" 'ace-jump-line-mode)

(keymap-global-set "C-c o" 'project-find-file)

(require 'yasnippet)
(yas-global-mode 1)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(keymap-global-set "C-c C-s" 'swiper)
(keymap-global-set "C-s" 'swiper-isearch)
(keymap-global-set "C-c C-r" 'ivy-resume)
(keymap-global-set "C-x C-f" 'counsel-find-file)
(keymap-global-set "C-c m" 'counsel-bookmark)
(keymap-global-set "C-c r" 'counsel-rg)
(keymap-global-set "C-c i" 'counsel-imenu)
(keymap-global-set "C-c b" 'counsel-ibuffer)
(keymap-global-set "C-c j" 'counsel-file-jump)
(define-key minibuffer-local-map "C-r" 'counsel-minibuffer-history)

(require 'smex)
(smex-initialize)

(keymap-global-set "M-x" 'smex)
(keymap-global-set "M-X" 'smex-major-mode-commands)

(setq confirm-kill-emacs 'yes-or-no-p)

(require 'bookmark)
(setq bookmark-default-file "/mnt/share/Documents/.bookmark")

(require 'go-mode)
(set-variable 'gofmt-command "goimports")

(defun go-mode-fmt ()
  (interactive)
  (when (eq major-mode 'go-mode)
    (gofmt)))

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(add-hook 'go-mode-hook
  (lambda ()
    (progn (keymap-local-set "C-c f" 'go-mode-fmt)
           (keymap-local-set "M-." 'lsp-bridge-peek)
           (keymap-local-set "C-." 'lsp-bridge-find-def-other-window)
           (keymap-local-set "C-c M-i" 'lsp-bridge-find-impl-other-window)
           (keymap-local-set "M-r" 'lsp-bridge-find-references)
           (keymap-local-set "C-c ?" 'lsp-bridge-show-documentation)
           (keymap-local-set "C-h ." 'lsp-bridge-popup-documentation)
           (keymap-local-set "C-c d" 'lsp-bridge-diagnostic-list)
           (keymap-local-set "C-c M-r" 'lsp-bridge-rename))))
