;;; live-server.el --- Run live-server for current buffer -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laluxx

;; Author: Laluxx
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools, web
;; URL: https://github.com/laluxx/live-server

;;; Commentary:

;; This package provides integration with live-server, allowing you to
;; start a live development server from the current buffer path.
;; The server will automatically cold reload connected browsers on files change.

;;; Code:

(defgroup live-server nil
  "Run live-server for current buffer."
  :group 'tools
  :prefix "live-server-")

(defcustom live-server-executable "live-server"
  "Path to the live-server executable."
  :type 'string
  :group 'live-server)

(defvar live-server--process nil
  "Hold the live-server process object.")

(defun live-server--get-buffer-directory ()
  "Get the directory of the current buffer."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun live-server-start ()
  "Start live-server for the current buffer's directory."
  (interactive)
  (if live-server--process
      (message "live-server is already running")
    (let ((default-directory (live-server--get-buffer-directory)))
      (setq live-server--process
            (make-process
             :name "live-server"
             :buffer "*live-server*"
             :command (list live-server-executable)
             :filter (lambda (proc string)
                       (when (buffer-live-p (process-buffer proc))
                         (with-current-buffer (process-buffer proc)
                           (let ((moving (= (point) (process-mark proc))))
                             (save-excursion
                               (goto-char (process-mark proc))
                               (insert string)
                               (set-marker (process-mark proc) (point)))
                             (if moving (goto-char (process-mark proc)))))))
             :sentinel (lambda (proc event)
                         (when (memq (process-status proc) '(exit signal))
                           (setq live-server--process nil)))))
      (message "Started live-server"))))

(defun live-server-stop ()
  "Stop the running live-server process."
  (interactive)
  (when live-server--process
    (delete-process live-server--process)
    (setq live-server--process nil)
    (message "Stopped live-server")))

;;;###autoload
(define-minor-mode live-server-mode
  "Toggle live-server mode.
When enabled, starts a live-server instance for the current buffer's directory."
  :lighter " LiveServer"
  :global nil
  (if live-server-mode
      (live-server-start)
    (live-server-stop)))

(provide 'live-server)

;;; live-server.el ends here
