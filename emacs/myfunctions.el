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

(defun gcal (month)
  "Open google calendar"
  (interactive "sWhich month? ")
  (let ((tmp-buff-name "*gcal*")
        (options " --nolineart --monday --military ")
        (calendars " --calendar 'Kai & Nina' --calendar kai.harries@gmail.com --calendar Optional")
        (start-time (format-time-string "%FT%R" (time-subtract nil 3600)))
        (end-time   (format-time-string "%FT%R" (time-add nil (* 21 86400)))))
    (switch-to-buffer tmp-buff-name)
    (read-only-mode -1)
    (if (equal month "")
        (shell-command (concat "gcalcli" options calendars " --nocolor --detail_all --detail_description_width 140 agenda " start-time " " end-time) tmp-buff-name)
      (shell-command (concat "gcalcli" options "--width 20 calm " month) tmp-buff-name))
    (ansi-color-apply-on-region (point-min) (point-max))
    (org-mode)
    (view-mode)
    (evil-motion-state)
    (visual-line-mode -1)))


(defun addresssearch (name)
  "Search for email addresses."
  (interactive "sSearch for name: ")
  (insert
   (nth 0 (split-string
           (completing-read
            "Search for name: "
            (cdr (split-string (shell-command-to-string (concat "lbdbq " name)) "[\r\n]+"))
            nil t)))))
