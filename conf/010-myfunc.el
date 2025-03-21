(defun transpose-lines-and-columns ()
  "現在のバッファまたはリージョンのスペース区切りの文字列の行と列を入れ替えます。"
  (interactive)
  (let* ((region-active-p (region-active-p))
         (start (if region-active-p (region-beginning) (point-min)))
         (end (if region-active-p (region-end) (point-max))))
    (if region-active-p
        (let* ((original-text (buffer-substring start end))
               (lines (split-string original-text "\n"))
               (split-lines (mapcar (lambda (line) (split-string line " ")) lines)))
          (when (and split-lines (cdr split-lines))
            (let* ((num-rows (length split-lines))
                   (num-cols (length (car split-lines)))
                   (transposed-lines
                    (cl-loop for col from 0 below num-cols
                             collect (cl-loop for row from 0 below num-rows
                                              collect (nth col (nth row split-lines)))))
                   (transposed-text
                    (mapconcat (lambda (line) (mapconcat 'identity line " ")) transposed-lines "\n")))
              (delete-region start end)
              (insert transposed-text))))
      (let* ((original-text (buffer-string)) ; バッファ全体のテキストを取得
             (lines (split-string original-text "\n"))
             (split-lines (mapcar (lambda (line) (split-string line " ")) lines)))
        (when (and split-lines (cdr split-lines))
          (let* ((num-rows (length split-lines))
                 (num-cols (length (car split-lines)))
                 (transposed-lines
                  (cl-loop for col from 0 below num-cols
                           collect (cl-loop for row from 0 below num-rows
                                            collect (nth col (nth row split-lines)))))
                 (transposed-text
                  (mapconcat (lambda (line) (mapconcat 'identity line " ")) transposed-lines "\n")))
            (delete-region (point-min) (point-max))
            (insert transposed-text)))))))
