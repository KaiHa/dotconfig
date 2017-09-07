(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -e -R %s" (directory-file-name dir-name))))


(defun set-comment-char (char)
  "Set comment character for current buffer."
  (interactive "sComment char: ")
  (setq comment-start char)
  (font-lock-add-keywords nil `((,(concat comment-start ".+") . font-lock-comment-face)))
  (font-lock-mode 0)
  (font-lock-mode 1))


(defun unset-all-font-lock-keywords ()
  "Unset all font lock keywords for current buffer."
  (interactive)
  (font-lock-add-keywords nil () 'set)
  (font-lock-mode 0)
  (font-lock-mode 1))
