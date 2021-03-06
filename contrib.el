
(defun foreach (alist func)
  (while alist
    (progn
      (funcall func (car alist))
      (setq alist (cdr alist)))
    ))

;; (foreach '("hello" "world") 
;;  (lambda (x) (message x)))



;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
