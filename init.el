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
  (setq neo-theme 'nerd-icons)
  (setq neo-window-width 30)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "l" 'neotree-enter-hide neotree-mode-map)
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
  (define-prefix-command 'my-leader-map)
  :config
  (evil-mode 1))
  ;; custom command
  (evil-ex-define-cmd "pyvenv" #'pyvenv-activate)
  (evil-ex-define-cmd "Reload" #'eval-buffer)
  ;; custom leader
  (keymap-set evil-motion-state-map "," 'my-leader-map)
  (keymap-set evil-normal-state-map "," 'my-leader-map)
  (evil-define-key nil my-leader-map
      ;; add your bindings here:
      "b"  'bookmark-jump
      "e"  'neotree-toggle
      "f"  'text-scale-adjust
  )
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
; LSP
; ----
;; eglot(LSP)
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp"))
  (add-hook 'python-mode-hook #'eglot-ensure))

(use-package eglot-booster
  :straight '(eglot-booster
              :type git
              :host github
              :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

; ----
; completion
; ----

;; company ;; 
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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

; ----
; python
; ----
(use-package pyvenv
  :config
  (pyvenv-mode 1)  ;; pyvenv を有効化
  (setq pyvenv-tracking-mode 1))  ;; 自動で仮想環境を切り替える

; ----
; load files
; ----
(add-to-list 'load-path "~/.emacs.d/conf/")
(load "~/.emacs.d/conf/000-evil-func.el")

