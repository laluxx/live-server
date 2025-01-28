;;; live-server.el --- Run live-server for current buffer -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laluxx

;; Author: Laluxx
;; Version: 1.1.0
;; Package-Requires: ((emacs "25.1") (nerd-icons "0.0.1"))
;; Keywords: convenience, tools, web
;; URL: https://github.com/laluxx/live-server

;;; Commentary:

;; FIXME Open the url ony one time
;; TODO Place the icon right after the mode section
;; in the modeline. And add a space, it should be: "󰘊 "

;; This package provides integration with live-server, allowing you to
;; start a live development server from the current buffer path.
;; The server will automatically cold reload connected browsers on files change.

;; NOTE This package remembers the buffer where `live-server-start' was called
;; and call `live-server-stop' when you kill it (in any way)

;;; Code:

(require 'nerd-icons)

(defgroup live-server nil
  "Run live-server for current buffer."
  :group 'tools
  :prefix "live-server-")

(defcustom live-server-executable "live-server"
  "Path to the live-server executable."
  :type 'string
  :group 'live-server)

(defcustom live-server-modeline-icon
  (nerd-icons-mdicon "nf-md-signal_variant")
  "Icon to display in the modeline when live-server is active."
  :type 'string
  :group 'live-server)

(defcustom live-server-icon-color (face-attribute 'success :foreground nil t)
  "Color of the live-server icon in the modeline.
Inherits from the 'success' face by default."
  :type '(choice (const :tag "Inherit from success face" nil)
                 (color :tag "Custom color"))
  :group 'live-server)

(defcustom live-server-open-in-new-window t
  "Whether to open the live-server URL in a new browser window."
  :type 'boolean
  :group 'live-server)

(defvar live-server--process nil
  "Hold the live-server process object.")

(defvar live-server--buffer nil
  "Buffer where live-server was started.")

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
                           (setq live-server--process nil)
                           (setq live-server--buffer nil)
                           (live-server--update-modeline)))))
      (setq live-server--buffer (current-buffer))
      (live-server--update-modeline)
      (message "Started live-server")
      (if live-server-open-in-new-window
          (start-process "firefox" nil "firefox" "--new-window" "--kiosk" "http://localhost:8080")
        (browse-url "http://localhost:8080")))))

(defun live-server-stop ()
  "Stop the running live-server process."
  (interactive)
  (when live-server--process
    (delete-process live-server--process)
    (setq live-server--process nil)
    (setq live-server--buffer nil)
    (live-server--update-modeline)
    (message "Stopped live-server")))

(defun live-server--update-modeline ()
  "Update the modeline to reflect the current live-server status."
  (if live-server--process
      (add-to-list 'mode-line-format
                   '(:eval (when (eq (current-buffer) live-server--buffer)
                             (propertize live-server-modeline-icon
                                         'face `(:foreground ,live-server-icon-color :height 0.9)
                                         'help-echo "live-server active"
                                         'display '(raise -0.1)))); ?
                   t)
    (setq mode-line-format
          (remove '(:eval (when (eq (current-buffer) live-server--buffer)
                            (propertize live-server-modeline-icon
                                        'face `(:foreground ,live-server-icon-color :height 0.9)
                                        'help-echo "live-server active"
                                        'display '(raise -0.1)))); ?
                  mode-line-format)))
  (force-mode-line-update t))

(defun live-server--kill-buffer-hook ()
  "Hook to stop live-server when the buffer it was started from is killed."
  (when (and live-server--process
             (eq (current-buffer) live-server--buffer))
    (live-server-stop)))

(add-hook 'kill-buffer-hook #'live-server--kill-buffer-hook)

;;;###autoload
(define-minor-mode live-server-mode
  "Toggle live-server mode.
When enabled, starts a live-server instance for the current buffer's directory."
  :lighter nil
  :global nil
  (if live-server-mode
      (live-server-start)
    (live-server-stop)))

(provide 'live-server)

;;; live-server.el ends here
