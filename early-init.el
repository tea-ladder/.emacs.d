; ----
; language
; ----
;; set language as Japanese
(set-language-environment 'Japanese)
;; coding UTF8
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;; UI
(setq default-frame-alist
  '(
    (fullscreen . maximized)
    (font . "-outline-Hack Nerd Font Mono-regular-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
    (alpha . 99)
   )
)

(add-hook 'after-init-hook (lambda () (setq text-scale-mode-step 1.0)))
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
(scroll-bar-mode -1)
;; デフォルトの起動時のメッセージを表示しない
(setq inhibit-startup-message t)
;; 行番号の表示
(global-display-line-numbers-mode)
;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)
;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)
;; クリップボードからのペースト
(setq x-select-enable-clipboard t)
;; yes-or-noをy-or-nに変更
(setq use-short-answers t)
; ----
; shell
; ----
(setq explicit-shell-file-name
      "C:/Users/tea-l/scoop/apps/git/current/bin/bash.exe")
(setq explicit-sh.exe-args '("--login" "-i"))
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:/Users/tea-l/scoop/apps/git/current/bin/bash.exe")
(add-hook 'shell-mode-hook '(lambda () (set-buffer-process-coding-system 'sjis 'sjis)))


