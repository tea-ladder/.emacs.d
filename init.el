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
;; icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

;; buff lines
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp-icon t)
)
;; 
(use-package pulsar
  :config
  (pulsar-global-mode +1))

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
  (evil-mode 1)
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
      (kbd "SPC i")  'org-clock-in
      (kbd "SPC o")  'org-clock-out
      (kbd "SPC c")  'org-capture
  )
  (defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
    (apply orig-fn beg end type ?_ args))
  (advice-add 'evil-delete :around 'bb/evil-delete))
;; evil-collection の設定
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; vertico の設定
(use-package vertico
  :init
  (defvar +vertico-current-arrow t)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat (nerd-icons-faicon "nf-fa-hand_o_right") " " cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat " " (nerd-icons-faicon "nf-fa-hand_o_right") " " cand)
        (concat "    " cand))))
  (vertico-mode +1))

; ----
; theme
; ----
(use-package kanagawa-themes
  :config
  (load-theme 'kanagawa-wave t))

; ----
; tools
; ----
(use-package magit
  :straight '(magit
              :type git
              :host github
              :repo "magit/magit")

  :config
  (advice-add 'magit-status :around
              (lambda (orig-fun &rest args)
                ;; 現在のウィンドウ構成を :magit-fullscreen レジスタに保存
                (window-configuration-to-register :magit-fullscreen)
                ;; 元の magit-status 関数を実行
                (apply orig-fun args)
                ;; 他のウィンドウを削除してフルスクリーンにする
                (delete-other-windows)))
  (setq magit-git-executable "C:/Program Files/Git/cmd/git.exe")
  ;; (オプション) Magit バッファを抜ける際に元のウィンドウ構成に戻す設定
  ;; もし元のウィンドウ構成に戻したい場合は、以下のフックを追加します。
  (add-hook 'magit-mode-quit-hook
            (lambda ()
              (jump-to-register :magit-fullscreen))))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-set-groups nil)
  (defun my/centaur-tabs-buffer-groups ()
    (let ((buffer-name (buffer-name)))
      (cond
       ;; 以下はdefault
       ((string-prefix-p "*" (buffer-name))
        '("default"))
       ((derived-mode-p 'dired-mode 'magit-mode)
        '("default"))

       (t
        '("main")))))
  (defun my/centaur-tabs-hide-tab (x)
    "特定のバッファをタブに表示させない"
    (let ((name (format "%s" x)))
      (or
       ;; current window is not dedicated window
       (window-dedicated-p (selected-window))
       ;; すべての*で始まるバッファを非表示
       (string-prefix-p "*" name)
       ;; その他の特定のバッファも非表示
       (string-prefix-p "*epc" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p "*Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*Messages*" name)
       (derived-mode-p 'dired-mode 'magit-mode))))
  (centaur-tabs-mode t)
  (with-eval-after-load 'centaur-tabs
    (setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)
    (setq centaur-tabs-hide-tabs-function #'my/centaur-tabs-hide-tab))
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'nerd-icons)
  :bind
  (:map evil-normal-state-map
        ("C-<tab>" . centaur-tabs-forward-tab)
        ("C-S-<tab>" . centaur-tabs-backwrd-tab)))

       
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
(use-package corfu
  :custom (
           (corfu-auto t)
           (corfu-auto-delay 0)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           (corfu-on-exact-match nil)
           (tab-always-indent 'complete)
           ;; test
           (corfu-scroll-margin 8)
           )
  :bind (nil
         :map corfu-map
         ("TAB" . corfu-insert)
         ("<tab>" . corfu-insert)
         ("<backtab>" . corfu-previous) 
         ("RET" . nil )
         ("<return>" . nil )
         )
  :init
  (global-corfu-mode +1)
  :hook (corfu-mode . corfu-popupinfo-mode)

  :config
  ;; java-mode などの一部のモードではタブに `c-indent-line-or-region` が割り当てられているので、
  ;; 補完が出るように `indent-for-tab-command` に置き換える
  ;(defun my/corfu-remap-tab-command ()
  ;  (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command))
  ;(add-hook 'java-mode-hook #'my/corfu-remap-tab-command)

  ;; ミニバッファー上でverticoによる補完が行われない場合、corfuの補完が出るようにします。
  ;; https://github.com/minad/corfu#completing-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (custom-set-faces
   '(corfu-default ((t (:height 1.0)))))
  ;; lsp-modeでcorfuが起動するように設定する
  ;(with-eval-after-load 'lsp-mode
  ;  (setq lsp-completion-provider :none))
  )

 (use-package prescient
   :config
   (setq prescient-aggressive-file-save t)
   (prescient-persist-mode +1))

 (use-package corfu-prescient
   :after corfu
   :config
   (with-eval-after-load 'orderless
     (setq corfu-prescient-enable-filtering nil))
   (corfu-prescient-mode +1))


(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


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
;;(load "~/.emacs.d/conf/000-evil-func.el")
(load "~/.emacs.d/conf/001-org.el")
(load "~/.emacs.d/conf/010-myfunc.el")
