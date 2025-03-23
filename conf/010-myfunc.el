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


(defun evil-visual-transpose-rectangle-with-separator (separator)
  "Transpose the rectangle in the current evil visual block selection using a separator.
   This swaps rows and columns of the selected rectangle.
   The separator can be specified as:
   - \"<s>\" for space
   - \"<t>\" for tab
   - Any other string to use as a separator"
  (interactive "sEnter separator (<s> for space, <t> for tab): ")
  
  ;; セパレータの処理
  (setq separator (cond ((string= separator "<s>") " ")
                         ((string= separator "<t>") "\t")
                         (t separator)))
  
  (when (eq evil-visual-selection 'block)
    (let* ((mark-col (evil-column evil-mark))
           (point-col (evil-column evil-visual-beginning))
           (begin-col (min mark-col point-col))
           (end-col (max mark-col point-col))
           (mark-row (line-number-at-pos evil-mark))
           (point-row (line-number-at-pos evil-visual-beginning))
           (begin-row (min mark-row point-row))
           (end-row (max mark-row point-row))
           (content '())
           (transposed '())
           (current-line begin-row))
      
      ;; 元の矩形領域の内容を取得
      (save-excursion
        (while (<= current-line end-row)
          (goto-char (point-min))
          (forward-line (1- current-line))
          (move-to-column begin-col t)
          (let ((row-content (buffer-substring-no-properties
                              (point)
                              (progn
                                (move-to-column end-col t)
                                (point)))))
            ;; セパレータで分割
            (push (split-string row-content separator) content)
            (setq current-line (1+ current-line)))))
      
      ;; contentを逆順から正順に戻す
      (setq content (nreverse content))
      
      ;; 各行の要素数を取得し、最大値を使用
      (let ((max-cols 0))
        (dolist (row content)
          (setq max-cols (max max-cols (length row))))
        
        ;; 不足している要素を空文字で埋める
        (setq content 
              (mapcar (lambda (row)
                        (let ((row-len (length row)))
                          (if (< row-len max-cols)
                              (append row (make-list (- max-cols row-len) ""))
                            row)))
                      content))
        
        ;; 行と列を入れ替え
        (dotimes (col max-cols)
          (let ((new-row '()))
            (dolist (row content)
              (push (nth col row) new-row))
            (push (nreverse new-row) transposed))))
      
      ;; transposeを逆順から正順に戻す
      (setq transposed (nreverse transposed))
      
      ;; 元の選択範囲を削除して転置した内容を挿入
      (evil-exit-visual-state)
      (goto-char (point-min))
      (forward-line (1- begin-row))
      (move-to-column begin-col t)
      (let ((start-point (point)))
        ;; 矩形選択領域を削除
        (dotimes (row (length content))
          (delete-region (point) (progn (move-to-column end-col t) (point)))
          (if (< (1+ row) (length content))
              (forward-line 1)
            (goto-char start-point)))
        
        ;; 転置された内容を挿入
        (goto-char start-point)
        (dotimes (row (length transposed))
          (insert (mapconcat #'identity (nth row transposed) separator))
          (if (< (1+ row) (length transposed))
              (progn
                (end-of-line)
                (insert "\n"))
            (goto-char start-point)))))))

;; キーバインドの例
(evil-define-key 'visual global-map (kbd "C-t") 
  (lambda () 
    (interactive)
    (call-interactively 'evil-visual-transpose-rectangle-with-separator)))
