;; Kerbal Operating System Major Mode
;; Featuring Syntax Highlighting and
;; C-c C-e style live evaluation to
;; running KOS Terminal Session

;; Copyright (C)'2020 by Mathias Menzel-Nielsen <matze@matzsoft.de>
;; LICENSE: BSD
;; See LICENSE file



(defvar *kos-terminal-stream*)

(defvar *kos-current-cpu* 1)

(defvar *kos-repl* "*kos-repl*")
(defvar *kos-prompted* nil)

(defvar *kos-available-cpus* nil)

(defvar *kos-last-return* "") ;; artificial eval return, based on own prompt (via print), b/c kos has no prompt
(defvar *kos-last-line* "")

(defun kos-handler (process content)
	;; TODO parse selection screen
  (when (string-match "^> " content)
	(setq *kos-prompted* t))
  (when (string-match "^KerboScript \\(v[0-9\\.]+\\)" content)
	(message (format "Connected to: %s" (match-string 0 content))))
  (when (string-match "\s+\\[\\([0-9]+\\)\\]\s+\\(\\w+\\)\s+[0-9]+\s+\\(\\w+\\) (\\([^(]+\\)(\\(\\w*\\)))" content)
	;; 1 = num , 2 = yes/no , 3 = name, 4 = CPU, 5 =tag
	(let ((num (string-to-number  (match-string 1 content)))
		  (gui (string= "yes" (match-string 2 content)))
		  (name (match-string 3 content))
		  (cpu (match-string 4 content))
		  (tag (match-string 5 content)))
	  (when (= num 1)
		(setq *kos-available-cpus* nil)) ;reset
	  (push (cons num
				  (list gui name cpu tag))
			*kos-available-cpus*)))
  (setq *kos-last-return* (format "%s%s" *kos-last-return* content))
  (setq *kos-last-line* (string-trim content))
  (with-buffer *kos-repl*
	(insert content)))

(defun* kos-open-terminal-stream (&optional (host "localhost") (port 5410))
  (interactive)
  (setq *kos-terminal-stream* (open-network-stream "kos" "*kos-repl*" host port))
  (set-process-filter *kos-terminal-stream* 'kos-handler))

(defun kos-send-string (cmd)
  (process-send-string *kos-terminal-stream* cmd)
  (process-send-string *kos-terminal-stream* "\n"))

(defun* kos-wait-for (regex &optional (timeout 10))
  (let ((i 0))
	(loop until (or (string-match regex *kos-last-line*) (> i timeout)) do
	  (incf i)
	  (sleep-for 0 100))))


(defun kos-choose-cpu (num)
  (kos-send-string (format "%d" (setq *kos-current-cpu* num))))

(defun kos-reset-last ()
  (setq *kos-last-line* "")
  (setq *kos-last-return* ""))



(defun kos-send-command-cpu (cmd)
  (kos-send-string (format "%d" *kos-current-cpu*))
  (kos-wait-for-prompt)
  (let ((ret (kos-send-command cmd)))
	(kos-send-string (kbd "C-d"))
	ret))

(defun kos-send-command (cmd)
  (kos-reset-last)
  (kos-send-string cmd)
  (kos-send-string "print \"EOO\".") ;;Special EndOfOut
  (kos-wait-for "EOO" 100)
  (replace-regexp-in-string "\nEOO\r\n" "" *kos-last-return*))


(defun kos-get-last-command-at-point ()
  (save-excursion
	(cond ((eq (preceding-char) ?.)
		   ;;At end of cmd
		   (backward-char)
		   (let ((end (point))
				 (beg (search-backward-regexp "[}\\.]")))
			 ;;(forward-char)
			 (string-trim (buffer-substring (1+ beg) (1+ end)))))
		  ((eq (preceding-char) ?})
		   ;;At end of block
		   (backward-char)
		   (let ((end (point))
				 (beg (search-backward "{")))
			 ;;(forward-char)
			 (string-trim (buffer-substring beg (1+ end))))))))
		   
(defun kos-eval-region ()
  (interactive)
  (kos-send-command (buffer-substring-no-properties (region-beginning) (region-end))))

(defun kos-eval-buffer ()
  (interactive)
  (kos-send-command (buffer-string)))

(defun kos-eval ()
  "Sends the command before Point (before the last '.') to the connected kos process"
  (interactive)
  (let ((cmd (kos-get-last-command-at-point)))
	(kos-send-command cmd)))


(kos-choose-cpu 1)

;;(kos-send-command "print \"Hello\".") --> "Hello"
;;(kos-send-command "list.")





										   






