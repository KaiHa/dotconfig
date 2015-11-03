;; Create TAGS file
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -e -R %s" (directory-file-name dir-name))))
