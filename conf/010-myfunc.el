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
  "Convert a Markdown-style table to CSV format in the selected region."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties beg end))
         (lines (split-string content "\n" t))
         ;; セパレーター行（`|---` で始まる行）を削除
         (filtered-lines (seq-remove (lambda (line) (string-match-p "^| *-+" line)) lines))
         ;; 各行をパイプ `|` で分割し、不要な空白を削除
         (rows (mapcar (lambda (line)
                         (mapcar #'string-trim (split-string (string-trim line) "|" t)))
                       filtered-lines))
         ;; 各行をカンマで結合
         (csv (mapconcat (lambda (row) (string-join row ",")) rows "\n")))
    (delete-region beg end)
    (insert csv)))
