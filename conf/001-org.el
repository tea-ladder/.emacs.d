;; ファイルの場所
(setq org-directory "~/org")
(setq org-default-notes-file "notes.org")
;; todo keywords
(setq org-todo-keywords
  '((sequence "TODO(t)" "REMIND(r)" "WAITING(w)" "|" "DONE(d)" "SKIP(x)")))


;; structure template
;; https://orgmode.org/manual/Structure-Templates.html
(require 'org-tempo)
