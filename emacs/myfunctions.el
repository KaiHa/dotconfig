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
    (visual-line-mode -1)))


;; My version of mime-to-mml which fixes the line endings
(defun mime-to-mml (&optional handles)
  "Translate the current buffer (which should be a message) into MML.
If HANDLES is non-nil, use it instead reparsing the buffer."
  ;; First decode the head.
  (save-excursion (icalendar--clean-up-line-endings))
  (save-restriction
    (message-narrow-to-head)
    (let ((rfc2047-quote-decoded-words-containing-tspecials t))
      (mail-decode-encoded-word-region (point-min) (point-max))))
  (unless handles
    (setq handles (mm-dissect-buffer t)))
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (delete-region (point) (point-max))
  (if (stringp (car handles))
      (mml-insert-mime handles)
    (mml-insert-mime handles t))
  (mm-destroy-parts handles)
  (save-restriction
    (message-narrow-to-head)
    ;; Remove them, they are confusing.
    (message-remove-header "Content-Type")
    (message-remove-header "MIME-Version")
    (message-remove-header "Content-Disposition")
    (message-remove-header "Content-Transfer-Encoding")))
