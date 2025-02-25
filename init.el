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
;; クリップボードからのペースト
(setq x-select-enable-clipboard t)
; ----
; package
; ----
;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; use-package がインストールされていない場合はインストール
(straight-use-package 'use-package)
;(unless (package-installed-p 'use-package)
;  (package-refresh-contents)
;  (package-install 'use-package))
;(require 'use-package)
(setq use-package-always-ensure t)

; ----
; UI
; ----
(use-package nerd-icons)
(use-package all-the-icons)

; ----
; filer
; ----
(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 30)
  (bind-key [f8] 'neotree-toggle)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map))
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
  :config
  (load-theme 'kanagawa-wave t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-statistics company neotree all-the-icons nerd-icons kanagawa-themes vertico undo-fu magit git-gutter evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack Nerd Font Mono" :foundry "outline" :slant normal :weight regular :height 218 :width normal)))))

; ----
; python
; ----
;; eglot(LSP)
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp"))
  (add-hook 'python-mode-hook #'eglot-ensure))

;; company-mode
(use-package company
  :after company-statistics
  :bind (("M-<tab>" . company-complete) ;; Tabで自動補完を起動する
         :map company-active-map
         ;; C-n, C-pで補完候補を次/前の候補を選択
         ("M-n" . nil)                      ;; M-nで次の候補への移動をキャンセル
         ("M-p" . nil)                      ;; M-pでの前の候補への移動をキャンセル
         ("C-n" . company-select-next)      ;; 次の補完候補を選択
         ("C-p" . company-select-previous);; 前の補完候補を選択
         ("C-s" . company-filter-candidates) ;; C-sで絞り込む
         :map company-search-map
         ;; 検索候補の移動をC-nとC-pで移動する
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  ;; 全バッファで有効にする
  (global-company-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil) ;; CUI版のためにemacs-lisp-modeでバインドされるC-M-iをアンバインド
  (global-set-key (kbd "C-M-i") 'company-complete)   ;; CUI版ではM-<tab>はC-M-iに変換されるのでそれを利用
  (setq completion-ignore-case t)
  (setq company-idle-delay 0)                    ;; 待ち時間を0秒にする
  (setq company-minimum-prefix-length 2)         ;; 補完できそうな文字が2文字以上入力されたら候補を表示
  (setq company-selection-wrap-around t)         ;; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))) ;; 利用頻度が高いものを候補の上に表示する

(use-package company-statistics
  :init
  (company-statistics-mode))

;; auto-completeに近い挙動で候補の絞り込みができる
(use-package company-dwim
  :straight '(company-dwim
              :type git
              :host github
              :repo "zk-phi/company-dwim")
  :init
  (define-key company-active-map (kbd "TAB") 'company-dwim)
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-dwim-frontend
        company-echo-metadata-frontend)))

;; カーソルの位置がどこであってもcompanyを起動できる
(use-package company-anywhere
  :straight '(company-anywhere
              :type git
              :host github
              :repo "zk-phi/company-anywhere"))

;; プログラムの関数、変数のキーワード補完を強化
(use-package company-same-mode-buffers
  :straight '(company-same-mode-buffers
              :type git
              :host github
              :repo "zk-phi/company-same-mode-buffers")
  :after company
  :init
  (require 'company-same-mode-buffers)
  (company-same-mode-buffers-initialize)
  ;;
  :config
  (setq company-backends
        '((company-capf :with company-same-mode-buffers)
          (company-dabbrev-code :with company-same-mode-buffers)
          company-keywords
          company-files
          company-dabbrev)))

(use-package pyvenv
  :config
  (pyvenv-mode 1)  ;; pyvenv を有効化
  (setq pyvenv-tracking-mode 1))  ;; 自動で仮想環境を切り替える
; ----
; func
; ----
(load "~/.emacs.d/conf/000-evil-func.el")
(load "~/.emacs.d/conf/001-custom-func.el")

; ----
; shell
; ----
(setq explicit-shell-file-name
      "C:/Users/tea-l/scoop/apps/git/current/bin/bash.exe")
(setq explicit-sh.exe-args '("--login" "-i"))
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/Users/tea-l/scoop/apps/git/current/bin/bash.exe")
(add-hook 'shell-mode-hook '(lambda () (set-buffer-process-coding-system 'sjis 'sjis)))
