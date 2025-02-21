; ----
; language
; ----
;; set language as Japanese
(set-language-environment 'Japanese)
;; coding UTF8
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;;(set-fontset-font "fontset-default" 'Hack Nerd Font Mono "outline")

; ----
; preferences
; ----
;; tabにスペース４つを利用
(setq-default tab-width 4 indent-tabs-mode nil)
;; デフォルトの起動時のメッセージを表示しない
(setq inhibit-startup-message t)
;; 列の番号
(column-number-mode t)
;; メニュー/ ツールバーの非表示
(menu-bar-mode -1)
(tool-bar-mode -1)
;; デフォルトの起動時のメッセージを表示しない
(setq inhibit-startup-message t)
;; 行番号の表示
(global-display-line-numbers-mode)
;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)
;; for window system
(if window-system 
    (progn
      (set-frame-parameter nil 'alpha 95)))
;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

; ----
; package
; ----
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package がインストールされていない場合はインストール
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


; ----
; evil
; ----
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; evil-collection の設定
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; vertico の設定
(use-package vertico
  :init
  (vertico-mode))

; ----
; theme
; ----
(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-wave t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kanagawa-themes vertico undo-fu magit git-gutter evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack Nerd Font Mono" :foundry "outline" :slant normal :weight regular :height 218 :width normal)))))

; ----
; func
; ----
(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'bb/evil-delete)
