(defun my/transpose-lines-and-columns ()
  "現在のバッファまたはリージョンの行と列を入れ替えます。セパレータを指定できます。
タブは <t, スペースは <s で指定します。何も入力がない場合は , がセパレータとなります。"
  (interactive)
  (let* ((region-active-p (region-active-p))
         (start (if region-active-p (region-beginning) (point-min)))
         (end (if region-active-p (region-end) (point-max)))
         (separator-input (read-string "Separator (<t:tab, <s:space, default:,): " nil nil ","))
         (separator
          (cond
           ((string= separator-input "<t") "\t")
           ((string= separator-input "<s") " ")
           (t separator-input))))
    (if region-active-p
        (let* ((original-text (buffer-substring start end))
               (has-trailing-newline (string-suffix-p "\n" original-text))
               (lines (split-string original-text "\n" t)) ; t を追加して空文字列を削除
               (split-lines (mapcar (lambda (line) (split-string line separator)) lines)))
          (when (and split-lines (cdr split-lines))
            (let* ((num-rows (length split-lines))
                   (num-cols (length (car split-lines)))
                   (transposed-lines
                    (cl-loop for col from 0 below num-cols
                             collect (cl-loop for row from 0 below num-rows
                                              collect (nth col (nth row split-lines)))))
                   (transposed-text
                    (mapconcat (lambda (line) (mapconcat 'identity line separator)) transposed-lines "\n")))
              (delete-region start end)
              (insert transposed-text)
              (when has-trailing-newline (insert "\n")))))
      (let* ((original-text (buffer-string)) ; バッファ全体のテキストを取得
             (lines (split-string original-text "\n" t)) ; t を追加して空文字列を削除
             (split-lines (mapcar (lambda (line) (split-string line separator)) lines)))
        (when (and split-lines (cdr split-lines))
          (let* ((num-rows (length split-lines))
                 (num-cols (length (car split-lines)))
                 (transposed-lines
                  (cl-loop for col from 0 below num-cols
                           collect (cl-loop for row from 0 below num-rows
                                            collect (nth col (nth row split-lines)))))
                 (transposed-text
                  (mapconcat (lambda (line) (mapconcat 'identity line separator)) transposed-lines "\n")))
            (delete-region (point-min) (point-max))
            (insert transposed-text)))))))

(defun maketable (beg end)
  "Convert CSV-formatted text in the region to a Markdown table with aligned columns."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties beg end))
         (lines (split-string content "\n" t))
         (rows (mapcar (lambda (line) (mapcar #'string-trim (split-string line ","))) lines))
         (col-widths (mapcar (lambda (col) (apply 'max (mapcar #'length col))) (apply #'cl-mapcar #'list rows)))
         (separator (concat "|" (mapconcat (lambda (w) (make-string w ?-)) col-widths "|") "|"))
         (table (mapconcat (lambda (row)
                             (concat "|" (mapconcat #'identity
                                                   (cl-mapcar (lambda (cell width) (format (format "%%-%ds" width) cell)) row col-widths)
                                                   "|") "|"))
                           rows "\n")))
    (delete-region beg end)
    (insert (concat (nth 0 (split-string table "\n" t)) "\n" separator "\n" (mapconcat 'identity (nthcdr 1 (split-string table "\n" t)) "\n")))))

(defun unmaketable (beg end)
  "Convert a Markdown table in the region back to CSV format."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties beg end))
         (lines (split-string content "\n" t))
         (filtered-lines (seq-filter (lambda (line) (not (string-match "^|[-]+|$" line))) lines))
         (csv-lines (mapcar (lambda (line)
                              (string-join (mapcar #'string-trim (split-string (string-trim line "| ") "|" t)) ", "))
                            filtered-lines)))
    (delete-region beg end)
    (insert (string-join csv-lines "\n"))))

;; cl-remove-if を使うために必要 (Emacs 24.3以降)
;; 古い Emacs の場合は (require 'cl)
(require 'cl-lib)

(defun unmaketable-debug (beg end)
  "Convert Markdown table format text in the region to CSV format (with debug messages)."
  (interactive "r")
  (message "--- unmaketable start ---")
  (let* ((content (buffer-substring-no-properties beg end))
         (_ (message "[1] Content:\n%s" content))
         (lines (split-string content "\n" t))
         (_ (message "[2] Lines: %S" lines))
         ;; 区切り行を除いたすべての行を取得
         (table-lines (cl-remove-if
                       (lambda (line)
                         ;; 区切り行かどうかの判定とデバッグメッセージ
                         (let ((is-separator (string-match-p "^ *|[- :|]+$ *" line)))
                           (message "[3] Checking line for separator: \"%s\" -> %s" line is-separator)
                           is-separator))
                       lines))
         (_ (message "[4] Table lines (after removing separators): %S" table-lines))
         (csv-content
          ;; 各行を処理してCSV文字列のリストを作成
          (mapconcat
           (lambda (line)
             (message "[5] Processing line: %s" line) ; デバッグ
             (let* ((trimmed-line (string-trim line))
                    (_ (message "  [5a] Trimmed line: %s" trimmed-line))
                    (line-no-prefix (if (string-prefix-p "|" trimmed-line)
                                        (substring trimmed-line 1)
                                      trimmed-line))
                    (_ (message "  [5b] Line no prefix: %s" line-no-prefix))
                    (line-no-suffix (if (string-suffix-p "|" line-no-prefix)
                                        (substring line-no-prefix 0 -1)
                                      line-no-prefix))
                    (_ (message "  [5c] Line no suffix: %s" line-no-suffix)))
               ;; "|" でセルに分割
               (let* ((cells (split-string line-no-suffix "\\|" t))
                      (_ (message "  [5d] Split cells: %S" cells))
                      ;; セルをトリム
                      (trimmed-cells (mapcar #'string-trim cells))
                      (_ (message "  [5e] Trimmed cells: %S" trimmed-cells))
                      ;; トリムされたセルをカンマで結合
                      (joined-row (mapconcat #'identity trimmed-cells ","))
                      (_ (message "  [5f] Joined row: %s" joined-row)))
                 joined-row))) ; mapconcatの結果を返す
           table-lines
           "\n")))

    (message "[6] Final CSV content:\n%s" csv-content)
    (message "--- unmaketable end ---")
    ;; 結果を確認するため、実際の置換はコメントアウト
    ;; (delete-region beg end)
    ;; (insert csv-content)
    ))
