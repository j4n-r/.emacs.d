;;; init-custom-func.el --- Dape -*- lexical-binding: t; -*-
(defun j4n/base64url-decode (str)
  (let* ((str (replace-regexp-in-string "-" "+" str))
         (str (replace-regexp-in-string "_" "/" str))
         (pad (mod (- 4 (mod (length str) 4)) 4)))
    (base64-decode-string (concat str (make-string pad ?=)))))

(defun j4n/decode-jwt ()
  "Decode JWT from clipboard/kill-ring and show it in *JWT*."
  (interactive)
  (let* ((tok (string-trim (current-kill 0 t)))
         (parts (split-string tok "\\.")))
    (unless (= (length parts) 3)
      (user-error "Clipboard does not contain a valid JWT"))
    (with-current-buffer (get-buffer-create "*JWT*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n%s"
                        (j4n/base64url-decode (nth 0 parts))
                        (j4n/base64url-decode (nth 1 parts))))
        (json-pretty-print-buffer))
      (display-buffer (current-buffer)))
    t))

(provide 'init-custom-func)
;;; init-custom-func.el ends here.
