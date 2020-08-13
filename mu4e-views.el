;;; mu4e-views.el --- Customize how to view emails in mu4e including viewing with xwidget-webkit -*- lexical-binding: t -*-

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: Boris Glavic <lordpretzel@gmail.com>
;; Version: 0.1
;; Package-Requires: (xwidget ivy xwidgets-reuse advice-tools ht)
;; Homepage: https://github.com/lordpretzel/mu4e-views
;; Keywords: mu4e, email, xwidgets, html


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;TODO also wrap mu4e text email viewing to get the customizable behaviour and reduction of window messing

;;; Commentary:

;; `mu4e' is great, but viewing of html emails is suboptimal.  This packages
;; enables the user to choose how to view emails.  It's main use case is to view
;; emails using an xwidgets window, but the user provided viewin methods are
;; also supported.
;;
;; Also provides methods for user defined viewing methods to access content
;; extracted from an email, e.g., urls or attachments.  This makes it easier to
;; build new views.


;;; Code:

(require 'seq)
(require 'mu4e)
(require 'ht)
(require 'ivy)
(require 'xwidgets-reuse)
(require 'cl-lib)
(require 'cl-macs)
(require 'advice-tools)

;; ********************************************************************************
;; Customize and defvars

(defcustom mu4e-views-view-commands
  '(("text" . (:viewfunc mu4e-headers-view-message)) ;; open with standard mu4e function
	("html" . (:viewfunc mu4e-views/mu4e-view-xwidget)) ;; open with xwidget
	("browser" . (:viewfunc mu4e-views/view-in-browser :no-view-window t)) ;; open with browser
    )
  "A list of commands for viewing messages in mu4e, e.g., translating html into text or opening html, e.g., with xwidgets within emacs."
  :group 'mu4e-views
  :type 'alist
  )

(defcustom mu4e-views-default-view-method
  (cdr (assoc "text" mu4e-views-view-commands))
  "Default method to use for viewing emails in mu4e."
  :group 'mu4e-views
  :type 'function
  )

(defcustom mu4e-views-inject-email-information-into-html
  t
  "If t then `mu4e-views' will inject email message headers information into the html file for the email.  This is useful for viewing emails in browsers."
  :group 'mu4e-views
  :type 'boolean
  )

(defcustom mu4e-views-next-previous-message-behaviour
  'always-switch-to-headers
  "Determines the behavior when moving to the next / previous message in the mu4e-headers view.  Default is to stay in the headers views.  Other options are staying in the current view or always moving to the `mu4e-views' window."
  :group 'mu4e-views
  :type '(radio (const :tag "Always switch to mu4e-headers window which shows the list of emails" always-switch-to-headers)
                (const :tag "Always switch to mu4e-views view window which shows the current email" always-switch-to-view)
                (const :tag "Always stay in the current window" stick-to-current-window)
                )
  )

(defcustom mu4e-views-mu4e-html-email-header-style
  "<style type=\"text/css\">
.mu4e-mu4e-views-mail-headers { font-family: Courier New; font-size:10pt; border: solid 2px;  padding: 2px; background-color: #EEEEEE; }
.mu4e-mu4e-views-header-row { display:block; padding: 1px; }
.mu4e-mu4e-views-mail-header { font-family: Courier New; font-size:10pt; display: inline-block; font-weight: normal; color: #155327; background-color: #ECFFEC; border: 1px solid; border-color: #155327; padding: 1px;}
.mu4e-mu4e-views-header-content { font-family: Courier New; font-size:10pt; display: inline-block; font-weight: normal; color: black; padding-right: 6px; }
.mu4e-mu4e-views-email { font-family: Courier New; font-size:10pt; display: inline-block; padding-right: 8px; }
.mu4e-mu4e-views-attachment { font-family: Courier New; font-size:10pt; display: inline-block; padding-right: 8px; }
</style>"
  "CSS style for displaying email header information in a mu4e-views email view."
  :group 'mu4e-views
  :type 'string
  )

(defvar mu4e-views--mu4e-select-view-msg-method-history
  nil
  "Store completion history for `mu4e-views/mu4e-select-view-msg-method'."
  )

(defvar mu4e-views--current-viewing-method
  mu4e-views-default-view-method
  "Records which method for viewing email in mu4e is currently active."
  )

(defvar mu4e-views--view-window
  nil
  "Caches the view window."
  )

;; store message object for the message currently shown in xwidgets message view
(defvar mu4e-views--current-mu4e-message
  nil
  "Store the mu4e message object for the message currently shown
in `mu4e-views' window. This enables us to provide mu4e
iunctionality in a `mu4e-views' view such as opening or storing
attachments which need this object." )

(defvar mu4e-views--header-selected
  t
  "Remember whether the user was in the header or view window when moving on to another email."
  )

(defvar mu4e-views--called-from-view
  nil
  "set if we are called from view."
  )

;; ********************************************************************************
;; functions

;; select a view method for email
;;;###autoload
(defun mu4e-views/mu4e-use-view-msg-method (method)
  "Apply this method `METHOD' for viewing emails in mu4e-headers view."
  (let
	  ((cmd (cdr (assoc method mu4e-views-view-commands)))
	   (oldmethod mu4e-views--current-viewing-method)
	   )
	;;(message "use view msg method: %s (new function: %s, old function: %s)" method (symbol-name cmd) (symbol-name oldmethod))
	;; do not update anything if the method is the same
	(unless (eq cmd oldmethod)
	  (setq mu4e-views--current-viewing-method cmd)
	  (if (eq cmd 'mu4e-headers-view-message)
		  ;; use standard mu4e method (remove all advice)
          (progn
		    (advice-tools/advice-unadvice 'mu4e~view-internal)
            (advice-tools/advice-unadvice 'mu4e-headers-view-message)
            )
		;; replace advice
        (progn
		  (advice-add 'mu4e~view-internal :override 'mu4e-views/view-msg-internal)
          (advice-add 'mu4e-headers-view-message :override 'mu4e-views/mu4e-headers-view-message)
          (advice-add 'mu4e~headers-move :after 'mu4e-views/mu4e-after-headers-mode)
          )
		)
	  )
	)
  )

;; ********************************************************************************
;; functions for displaying message in xwidgets
;; view a mu4e message using xwidgets
;; (defun mu4e-views/mu4e-xwidget-reuse-browse-url (url)
;;   "Open URL using xwidgets, but do not "
;;   (interactive)
;;   (let ((buf (car (seq-filter (lambda (x) (string-match "*xwidget webkit:" (buffer-name x))) (buffer-list)))))
;; 	(if buf
;; 		(progn (message "have xwidget buffer %s, use it!" (buffer-name buf))
;; 			   (switch-to-buffer buf)
;; 			   (xwidget-webkit-goto-url url)
;; 			   (mu4e-views/turn-off-all-xwidgets-specialization-minor-modes)
;; 			   (mu4e-view-html-xwidgets-mode 1)
;; 			   )
;; 	  (progn (xwidget-webkit-browse-url url)
;; 			 (mu4e-views/turn-off-all-xwidgets-specialization-minor-modes)
;; 			 (mu4e-view-html-xwidgets-mode 1)
;; 			 (message "don't have xwidget buffer: create one")
;; 			 )
;; 	  )
;; 	)
;;   )

;; ********************************************************************************
;; functions for viewing a mu4e message in xwidgets
(defun mu4e-views/mu4e-action-view-with-xwidget (msg)
  "View the body of the message `MSG' inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
	(mu4e-error "No xwidget support available"))
  (let* ((html (mu4e-message-field msg :body-html))
		 (txt (mu4e-message-field msg :body-txt))
		 )
	(unless (or html txt)
	  (mu4e-error "No body part for this message"))
	(setq mu4e-views--current-mu4e-message msg)
	(xwidgets-reuse/xwidget-reuse-browse-url
     (concat "file://" (mu4e-views/mu4e~write-body-and-headers-to-html msg))
     'mu4e-view-view-actions-mode
     )
	)
  )

;; open mu4e message with xwidget and then remove advice for ourselves
(defun mu4e-views/mu4e-view-xwidget (html msg win)
  "View message `MSG' with `HTML' content in xwidget using window `WIN'."
  (interactive)
  (select-window win)
  ;; show message
  (mu4e-views/mu4e-action-view-with-xwidget msg)
  ;; switch back to headers window
  (select-window (get-buffer-window (mu4e-get-headers-buffer)))
  )

;; ********************************************************************************
;; functions viewing email in a webbrowser (available as action and as a view method)

;; create file to store message in and open in browser
(defun mu4e-views/mu4e-view-in-browser-action (msg)
  "Open email `MSG' in browser using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (mu4e-views/mu4e~write-body-and-headers-to-html msg)))
  )

(defun mu4e-views/view-in-browser (html msg)
  "Open email `MSG` with content `HTML' using `browse-url'."
  (browse-url (concat "file://" html))
  )

;; access with `aV' from headers view
(add-to-list 'mu4e-headers-actions
             '("ViewInBrowser" . mu4e-views/mu4e-view-in-browser-action) t)

;; ********************************************************************************
;; functions for writing a message to HTML and making it accessible to custom views

;; create formatted html for headers like subject and from/to of an email
(defun mu4e-views/mu4e-email-headers-as-html (msg)
  "Create formatted html for headers like subject and from/to of email `MSG'."
  (interactive)
  (mu4e-views/mu4e-create-mu4e-attachment-table-if-need-by mu4e-views--current-mu4e-message)
  (cl-flet ((wrap-row (header content id) (concat "<div class=\"mu4e-mu4e-views-header-row\"><div class=\"mu4e-mu4e-views-mail-header\">" header "</div>: <div class=\"mu4e-mu4e-views-header-content\" id=\"" id "\">" content "</div></div>"))
			(wrap-row-multi (headers) (concat "<div class=\"mu4e-mu4e-views-header-row\">"
									          (mapconcat (lambda (l) (let ((header (nth 0 l))
												                           (content (nth 1 l))
												                           (id (nth 2 l)))
												                       (concat "<div class=\"mu4e-mu4e-views-mail-header\">"
													                           header "</div>: <div class=\"mu4e-mu4e-views-header-content\" id=\"" id "\">"
													                           content "</div>"))
										                   )
										                 headers "")
									          "</div>")
							)
			)
	(with-temp-buffer
	  (insert (concat "<div class=\"mu4e-mu4e-views-mail-headers\">"))
	  (insert (wrap-row "from" (mapconcat (lambda (mail) (concat "<div class=\"mu4e-mu4e-views-email\">" (car mail) " (" (cdr mail) ")</div>")) (mu4e-message-field msg :from) "") "mu4e-from"))
	  (insert (wrap-row "to" (mapconcat (lambda (mail) (concat "<div class=\"mu4e-mu4e-views-email\">" (car mail) " (" (cdr mail) ")</div>"))  (mu4e-message-field msg :to) "") "mu4e-to"))
	  (when (mu4e-message-field msg :cc)
		(insert (wrap-row "cc" (mapconcat (lambda (mail) (concat "<div class=\"mu4e-mu4e-views-email\">" (car mail) " (" (cdr mail) ")</div>"))  (mu4e-message-field msg :cc) "") "mu4e-cc")))
	  (when (mu4e-message-field msg :bcc)
		(insert (wrap-row "bcc" (mapconcat (lambda (mail) (concat "<div class=\"mu4e-mu4e-views-email\">" (car mail) " (" (cdr mail) ")</div>"))  (mu4e-message-field msg :bcc) "") "mu4e-bcc")))
	  (insert (wrap-row "subject" (mu4e-message-field msg :subject) "mu4e-subject"))
	  (insert (wrap-row-multi (list
							   (list "date" (format-time-string mu4e-view-date-format
											                    (mu4e-message-field msg :date)) "mu4e-date")
							   (list "size" (mu4e-display-size (mu4e-message-field msg :size)) "mu4e-size")
							   (list "maildir" (mu4e-message-field msg :maildir) "mu4e-maildir")
							   )
							  ))
	  (let ((attachments (mapcar (lambda (k) (mu4e~view-get-attach mu4e-views--current-mu4e-message k)) (ht-keys mu4e~view-attach-map))))
		(when attachments
		  (insert (wrap-row "attachments" (mapconcat (lambda (att) (concat "<div class=\"mu4e-mu4e-views-attachment\">" (lax-plist-get att :name) " (" (mu4e-display-size (lax-plist-get att :size)) ")</div>"))  attachments "") "mu4e-attachments"))
		  )
		)
	  (insert "</div>")
	  (buffer-string)
	  )
	)
  )

;; write a message as html but also write the header information
(defun mu4e-views/set-auto-mode-dummy (&optional keep-mode-if-same)
  "Do nothing function that replaces `set-auto-mode' when just writing to a file.  Ignores `KEEP-MODE-IF-SAME'."
  )

(defun mu4e-views/vc-refresh-state-dummy ()
  "Do nothing function that replaces `vc-refresh-state' when just writing to a file."
  )

(defun mu4e-views/vc-before-save-dummy ()
  "Do nothing function that replaces `vc-before-save' when just writing to a file."
  )

(defun mu4e-views/vc-after-save-dummy ()
  "Do nothing function that replaces `vc-before-after' when just writing to a file."
  )

(defun mu4e-views/mu4e~write-body-and-headers-to-html (msg)
  "Write the body (either html or text) and headers of `MSG' to a temporary html file.  Return the filename."
  (let* ((html (mu4e-message-field msg :body-html))
		 (txt (mu4e-message-field msg :body-txt))
		 (tmpfile (mu4e-make-temp-file "html"))
		 (attachments (cl-remove-if (lambda (part)
								      (or (null (plist-get part :attachment))
								          (null (plist-get part :cid))))
								    (mu4e-message-field msg :parts))))
	(unless (or html txt)
	  (mu4e-error "No body part for this message"))
	;; remove hooks and advices that are not needed for writing constructed content to a file, but slow us down
	(advice-tools/advice-remove-if-def 'set-visited-file-name 'doom-modeline-update-buffer-file-name)
	(advice-tools/advice-remove-if-def 'set-visited-file-name 'lsp--on-set-visited-file-name)
	(advice-tools/advice-remove-if-def 'basic-save-buffer 'polymode-with-current-base-buffer)
	(advice-tools/advice-remove-if-def 'vc-refresh-state 'doom-modeline-update-vcs-text)
	(advice-tools/advice-remove-if-def 'vc-refresh-state 'doom-modeline-update-vcs-icon)
	(advice-tools/advice-remove-if-def 'rename-buffer 'doom-modeline-update-buffer-file-name)
	(advice-tools/advice-add-if-def 'set-auto-mode :override 'mu4e-views/set-auto-mode-dummy)
	(advice-tools/advice-add-if-def 'vc-refresh-state :override 'mu4e-views/vc-refresh-state-dummy)
	(advice-tools/advice-add-if-def 'vc-before-save :override 'mu4e-views/vc-before-save-dummy)
	(advice-tools/advice-add-if-def 'vc-after-save :override 'mu4e-views/vc-after-save-dummy)
	(let ((cache-before-save-hook before-save-hook)
		  (cache-after-save-hook after-save-hook)
		  )
	  (with-temp-buffer
		(setq before-save-hook nil)
		(setq after-save-hook nil)
		(insert (concat "<head><meta charset=\"UTF-8\">" mu4e-views-mu4e-html-email-header-style  "</head>\n"))
        (when mu4e-views-inject-email-information-into-html
          (insert (mu4e-views/mu4e-email-headers-as-html msg)))
		(insert (or html (concat "<div style=\"white-space: pre-wrap;\">" txt "</div>")))
		(write-file tmpfile)
		;; rewrite attachment urls
		(mapc (lambda (attachment)
				(goto-char (point-min))
				(while (re-search-forward (format "src=\"cid:%s\""
									              (plist-get attachment :cid)) nil t)
				  (if (plist-get attachment :temp)
					  (replace-match (format "src=\"%s\""
									         (plist-get attachment :temp)))
					(replace-match (format "src=\"%s%s\"" temporary-file-directory
								           (plist-get attachment :name)))
					(let ((tmp-attachment-name
						   (format "%s%s" temporary-file-directory
							       (plist-get attachment :name))))
					  (mu4e~proc-extract 'save (mu4e-message-field msg :docid)
								         (plist-get attachment :index)
								         mu4e-decryption-policy tmp-attachment-name)
					  (mu4e-remove-file-later tmp-attachment-name)))))
			  attachments)
		(save-buffer)
		;; restore normal behaviour
		(advice-tools/advice-add-if-def 'set-visited-file-name :after 'doom-modeline-update-buffer-file-name)
		(advice-tools/advice-add-if-def 'set-visited-file-name :around 'lsp--on-set-visited-file-name)
		(advice-tools/advice-add-if-def 'basic-save-buffer :around 'polymode-with-current-base-buffer)
		(advice-tools/advice-add-if-def 'vc-refresh-state :after 'doom-modeline-update-vcs-text)
		(advice-tools/advice-add-if-def 'vc-refresh-state :after 'doom-modeline-update-vcs-icon)
		(advice-tools/advice-add-if-def 'rename-buffer :after 'doom-modeline-update-buffer-file-name)
		(advice-tools/advice-remove-if-def 'set-auto-mode 'mu4e-views/set-auto-mode-dummy)
		(advice-tools/advice-remove-if-def 'vc-refresh-state 'mu4e-views/vc-refresh-state-dummy)
		(advice-tools/advice-remove-if-def 'vc-before-save 'mu4e-views/vc-before-save-dummy)
		(advice-tools/advice-remove-if-def 'vc-after-save 'mu4e-views/vc-after-save-dummy)
		(setq before-save-hook cache-before-save-hook)
		(setq after-save-hook cache-after-save-hook)
		(lax-plist-put msg :html-file tmpfile)
		tmpfile))))

;; ********************************************************************************
;; functions that are replace mu4e functions and make sure the mu4e-views window is shown
;;TODO check that the window size ratios are ok?
(defun mu4e-views/mu4e-header-and-view-windows-p ()
  "Check whether we are already showing the mu4e-headers and (custom) mu4e-view windows."
  (let ((have-header nil)
        (have-view nil)
        (other-buf nil)
        (loading-buffer mu4e~headers-loading-buf)
        (header-buffer (mu4e-get-headers-buffer))
        )
    (unless (eq (length (window-list)) 2)
      nil
      )
    (cl-loop for w in (window-list) do
             (let* ((buf (window-buffer w))
                    ;; (bufname (buffer-name buf))
                    )
               ;;(message "FOUND WINDOW: %s nad BUF: %s IS VALID: %s" w bufname (window-valid-p w))
               (when (eq buf header-buffer)
                ;; (message "HAVE HEADER WINDOW: %s BUFFER: %s" w buf)
                 (setq have-header t)
                 )
               (when (eq buf loading-buffer)
                ;; (message "HAVE VIEW WINDOW: %s BUFFER: %s" w buf)
                 (setq have-view t)
                 )
               ;; (when (eq w mu4e-views--view-window)
               ;;   (message "found mu4e-views window")
               ;;   (setq have-view t)
               ;;   )
               (unless (or (eq buf header-buffer) (eq buf loading-buffer))
                 ;;(message "NOT HEADER NOR VIEW WINDOW: %s BUFFER: %s" w buf)
                 (setq other-buf t)
                 )
               )
             )
    (and have-header (or have-view other-buf))
    )
  )

(defun mu4e-views/get-view-win ()
  "Return window to use for mu4e-views viewing of emails."
  (let (win)
    (cl-loop for w in (window-list) do
             (let* ((buf (window-buffer w))
                    (header-buffer (mu4e-get-headers-buffer))
                    )
               (unless (eq buf header-buffer)
                 (when (window-valid-p w)
                  ;; (message "GET VIEW WINDOW IS: %s BUFFER: %s" w buf)
                   (setq win w)
                   )
                 )
               )
             )
    (if win
        win
      (error "View window not found in %s" (window-list))
      )
    )
  )

(defun mu4e-views/headers-redraw-get-view-window ()
  "Close all windows, redraw the headers buffer based on the value of `mu4e-split-view', and return a window for the message view."
  ;; if single is used then the headers buffer needs to be replaced
  (when (eq mu4e-split-view 'single-window)
    (let ((win (or (and (buffer-live-p (mu4e-get-view-buffer))
                        (get-buffer-window (mu4e-get-view-buffer)))
                   (selected-window))))
      (setq mu4e-views--view-window win)
      win
      )
    )
  ;; if we have already the right setup, then just return the mu4e-views window
  (if (mu4e-views/mu4e-header-and-view-windows-p)
      (mu4e-views/get-view-win)
    ;; create the window
    (progn
      (unless (buffer-live-p (mu4e-get-headers-buffer))
        (mu4e-error "No headers buffer available"))
      (switch-to-buffer (mu4e-get-headers-buffer))
      (delete-other-windows)
      ;; kill the existing view buffer
      (when (buffer-live-p (mu4e-get-view-buffer))
        (kill-buffer (mu4e-get-view-buffer)))
      ;; get a new view window
      (setq mu4e~headers-view-win
            (let* ((new-win-func
                    (cond
                     ((eq mu4e-split-view 'horizontal) ;; split horizontally
                      '(split-window-vertically mu4e-headers-visible-lines))
                     ((eq mu4e-split-view 'vertical) ;; split vertically
                      '(split-window-horizontally mu4e-headers-visible-columns)))))
              (cond ((with-demoted-errors "Unable to split window: %S"
                       (eval new-win-func)))
                    (t ;; no splitting; just use the currently selected one
                     (setq mu4e-views--view-window (selected-window))
                   ;;  (message "CREATED NEW VIEW WINDOW %s" (selected-window))
                     (selected-window)
                     ))))))
  )

(defun mu4e-views/mu4e-headers-view-message ()
  "View message at point.  If there's an existing window for the view, re-use that one.  If not, create a new one, depending on the value of `mu4e-split-view': if it's a symbol `horizontal' or `vertical', split the window accordingly; if it is nil, replace
the current window."
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((msg (mu4e-message-at-point))
         (docid (or (mu4e-message-field msg :docid)
                    (mu4e-warn "No message at point")))
         (decrypt (mu4e~decrypt-p msg))
         (verify  (not mu4e-view-use-gnus))
         (viewwin (if (plist-get mu4e-views--current-viewing-method :no-view-window)
                      nil
                    (mu4e-views/headers-redraw-get-view-window)))
         )
    (when viewwin
     ;; (message "VIEW BEFORE LOADING WINDOW: %s" viewwin)
      (select-window viewwin)

      ;; show some 'loading...' buffer
      (unless (buffer-live-p mu4e~headers-loading-buf)
        (setq mu4e~headers-loading-buf (get-buffer-create " *mu4e-loading*"))
        (with-current-buffer mu4e~headers-loading-buf
          (mu4e-loading-mode)))

      (switch-to-buffer mu4e~headers-loading-buf)
     ;; (message "LOADING WINDOW IN: %s" (selected-window))
      )
    (mu4e~proc-view docid mu4e-view-show-images decrypt verify)))


(defun mu4e-views/view-msg-internal (msg)
  "Replacement for mu4e-view-msg-internal."
  (let* ((viewfunc (plist-get mu4e-views--current-viewing-method :viewfunc))
         (no-window (plist-get mu4e-views--current-viewing-method :no-view-window))
         (html (mu4e-message-field msg :body-html))
	     (txt (mu4e-message-field msg :body-txt))
         ;; (currentwin (selected-window))
         htmlfile
	     )
	(unless (or html txt)
	  (mu4e-error "No body part for this message"))
    ;; (unless (and win (window-valid-p win))
    ;;   (mu4e-error "window no longer active"))
	(setq mu4e-views--current-mu4e-message msg)
	(setq htmlfile (mu4e-views/mu4e~write-body-and-headers-to-html msg))
    (if no-window
        ;; method does not use a window, do nothing
        (funcall viewfunc htmlfile msg)
      ;; method needs a window, reuse or create a new one, then switch to the headers or view window based on `mu4e-views-next-previous-message-behaviour'.
      (progn
       ;; (message "CALL VIEW FUNCTION ON WINDOW: %s WITH CURRENT BUFFER: %s IS VALID %s" (selected-window) (current-buffer) (window-valid-p (mu4e-views/headers-redraw-get-view-window)))
        (funcall viewfunc htmlfile msg (mu4e-views/headers-redraw-get-view-window))
        (select-window (mu4e-views/headers-redraw-get-view-window))
        (cl-case mu4e-views-next-previous-message-behaviour
          (stick-to-current-window (if mu4e-views--header-selected
                                       (progn (message "TO HEADER ...")
                                              (select-window (get-buffer-window (mu4e-get-headers-buffer))))
                                     (progn (select-window (mu4e-views/headers-redraw-get-view-window))
                                            (message "TO VIEW ..."))
                                     ))
          (always-switch-to-view (select-window (mu4e-views/headers-redraw-get-view-window)))
          (always-switch-to-headers (mu4e~headers-select-window))
          )
        )
      )
	)
  )

;; ********************************************************************************
;; minor mode that advices mu4e functions to use mu4e-views instead
;; (define-minor-mode mu4e-views-global-mode
;;   "Minor mode for setting up keys when viewing a mu4e email in xwidgets."
;;   ;; The initial value - Set to 1 to enable by default
;;   :init-value nil
;;   ;; The indicator for the mode line.
;;   :lighter " M4VIEW"
;;   ;; The minor mode keymap
;;   :keymap
;;   ;; Make mode global
;;   :global t
;;   )

;; ********************************************************************************
;; helpers for mu4e-headers view.
;; functions to scroll message view from header window

;;;###autoload
(defun mu4e-views/mu4e-headers-windows-only ()
  "Show only the headers window of mu4e."
  (interactive)
  ;; delete other windows because otherwise mu4e will mess up and select the header window for replacement
  (switch-to-buffer (mu4e-get-headers-buffer))
  (delete-other-windows)
  )

;;;###autoload
(defun mu4e-views/cursor-msg-view-window-down ()
  "Scroll message view down using xwidget method if we view message using xwidget-webkit."
  (interactive)
  (let* ((wind (other-window-for-scrolling))
		 (mode (with-selected-window wind major-mode)))
	(if (eq mode 'xwidget-webkit-mode)
		(with-selected-window wind
		  (xwidget-webkit-scroll-up 100))
	  (scroll-other-window 2))))

;;;###autoload
(defun mu4e-views/cursor-msg-view-window-up ()
  "Scroll message view up using xwidget method if we view message using xwidget-webkit."
  (interactive)
  (let* ((wind (other-window-for-scrolling))
		 (mode (with-selected-window wind major-mode)))
	(if (eq mode 'xwidget-webkit-mode)
		(with-selected-window wind
		  (xwidget-webkit-scroll-down 100))
	  (scroll-other-window 2))))

;;;###autoload
(defun mu4e-views/mu4e-headers-next (&optional n)
  "Move to next message in headers view, if a xwidget message
view is open then use that to show the message. With prefix
argument move `n' steps instead."
  (interactive "P")
  (let ((step (or n 1)))
	(mu4e-views/mu4e-headers-move step)
	)
  )

;;;###autoload
(defun mu4e-views/mu4e-headers-prev (&optional n)
  "Move to `n'th previous message in headers view, if a xwidget message view is open then use that to show the message.  With prefix argument move `n' steps backwards."
  (interactive "P")
  (let ((step (* -1 (or n 1))))
	(mu4e-views/mu4e-headers-move step)
	)
  )

;;;###autoload
(defun mu4e-views/mu4e-headers-move (n)
  "Move by 'n` steps in the headers view. Negative numbers move backwards.  If message view is open show message in the view."
  (interactive)
  (switch-to-buffer (mu4e-get-headers-buffer))
  (setq mu4e-views--called-from-view t)
  (setq mu4e-views--header-selected nil)
  (message "CALLED FROM VIEW")
  (mu4e~headers-move n)
  )

;;;###autoload
(defun mu4e-views/mu4e-after-headers-mode (n)
  "Called when `mu4e~headers-move' is called to record from where it was called."
  (if mu4e-views--called-from-view
      (setq mu4e-views--called-from-view nil)
    (progn
      (setq mu4e-views--header-selected t)
      (message "CALLED FROM HEADER"))
    )
  )

;; ********************************************************************************
;; helper function for accessing parts of an email. Should be bound by custom mu4e-view modes.
;; These functions wrapp `mu4e' functions and pass on our saved message

;; prepare mu4e datastructure so that urls in the message are extracted and can be opened
;;;###autoload
(defun mu4e-views/mu4e-extract-urls-from-msg (msg)
  "Prepare mu4e datastructure for `MSG' so that command view message commands like browsing urls work in our xwidget message view."
  (interactive)
  (unless (plist-member msg :body-urls)
    (let ((num 0)
          (urls nil))
      (with-temp-buffer
        (setq mu4e~view-link-map ;; buffer local
              (make-hash-table :size 32 :weakness nil))
        (insert-file-contents (lax-plist-get msg :html-file))
        (goto-char (point-min))
        (while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
          (let ((bounds (thing-at-point-bounds-of-url-at-point)))
            (when bounds
              (let* ((url (thing-at-point-url-at-point)))
                (puthash (cl-incf num) url mu4e~view-link-map)
                (push url urls)
                )
              )
            )))
      (lax-plist-put msg :body-urls urls)
      )
    )
  )

;;;###autoload
(defun mu4e-views/mu4e-select-url-from-message ()
  "Select a url included in a mu4e message."
  (interactive)
  (mu4e-views/mu4e-extract-urls-from-msg mu4e-views--current-mu4e-message)
  (ivy-read "Select url: " ;; prompt
			;;(ht-map (lambda (k v) v) mu4e~view-link-map) ;; collection to complete over
			(lax-plist-get mu4e-views--current-mu4e-message :body-urls)
			:action (lambda (x)
					  (browse-url x)
					  )
			:sort t
			:require-match t
			:caller 'mu4e-views/mu4e-select-url-from-message
			)
  )

;;;###autoload
(defun mu4e-views/mu4e-open-attachment ()
  "Select an attached from an mu4e message and open it."
  (interactive)
  (let* ((attachments (mapcar (lambda (k) (list k (mu4e~view-get-attach mu4e-views--current-mu4e-message k))) (ht-keys mu4e~view-attach-map)))
		 (names (mapcar (lambda (x) (mu4e-message-part-field (cadr x) :name)) attachments))
		 (name-to-index (mapcar (lambda (x) (cons (plist-get (cadr x) :name) (car x))) attachments))
		 )
	(ivy-read "Select url: " ;; prompt
			  names ;; collection to complete over
			  :action (lambda (x)
						(let ((index (cdr (assoc x name-to-index))))
						  (mu4e-view-open-attachment mu4e-views--current-mu4e-message index)
						  )
						)
			  :sort t
			  :require-match t
			  :caller 'mu4e-views/mu4e-open-attachment
			  )
	)
  )

;;;###autoload
(defun mu4e-views/mu4e-save-attachment ()
  "Select an attached from an mu4e message and save it."
  (interactive)
  (let* ((attachments (mapcar (lambda (k) (list k (mu4e~view-get-attach mu4e-views--current-mu4e-message k))) (ht-keys mu4e~view-attach-map)))
		 (names (mapcar (lambda (x) (mu4e-message-part-field (cadr x) :name)) attachments))
		 (name-to-index (mapcar (lambda (x) (cons (plist-get (cadr x) :name) (car x))) attachments))
		 )
	(ivy-read "Select attachment(s): " ;; prompt
			  names ;; collection to complete over
			  :action (lambda (x)
						(let ((index (cdr (assoc x name-to-index))))
						  (mu4e-view-save-attachment-single mu4e-views--current-mu4e-message index)
						  )
						)
			  :sort t
			  :require-match t
			  :caller 'mu4e-views/mu4e-save-attachment
			  )
	)
  )

;;;###autoload
(defun mu4e-views/mu4e-save-all-attachments ()
  "Save all attachements to a single directory choosen by the user."
  (interactive)
  (let* ((msg mu4e-views--current-mu4e-message)
		 (attachnums (sort (ht-keys mu4e~view-attach-map) '<))
		 (path (concat (mu4e~get-attachment-dir) "/"))
		 (attachdir (mu4e~view-request-attachments-dir path))
		 )
	(dolist (num attachnums)
	  (let* ((att (mu4e~view-get-attach msg num))
			 (fname  (plist-get att :name))
			 (index (plist-get att :index))
			 (retry t)
			 fpath)
		(while retry
		  (setq fpath (expand-file-name (concat attachdir fname) path))
		  (setq retry
				(and (file-exists-p fpath)
					 (not (y-or-n-p
						   (mu4e-format "Overwrite '%s'?" fpath))))))
		(mu4e~proc-extract
		 'save (mu4e-message-field msg :docid)
		 index mu4e-decryption-policy fpath))))
  )

(defun mu4e-views/mu4e-create-mu4e-attachment-table-if-need-by (msg)
  "Call the mu4e function to setup the attachments hash-map for `MSG' if this has not been done already."
  (unless (plist-member msg :attachment-setup)
	(mu4e~view-construct-attachments-header msg)
	(lax-plist-put msg :attachment-setup t)
	)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-open-attachment ()
  "Wraps the `mu4e-view-open-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views/mu4e-create-mu4e-attachment-table-if-need-by mu4e-views--current-mu4e-message)
  (mu4e-views/mu4e-open-attachment)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-go-to-url ()
  "Wraps the `mu4e-view-go-to-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views/mu4e-select-url-from-message)
  ;;(mu4e-view-go-to-url mu4e-views--current-mu4e-message)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-save-url ()
  "Wraps the `mu4e-view-save-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-save-url mu4e-views--current-mu4e-message)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-save-attachment ()
  "Wraps the `mu4e-save-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views/mu4e-create-mu4e-attachment-table-if-need-by mu4e-views--current-mu4e-message)
  (mu4e-views/mu4e-save-attachment)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-save-all-attachments ()
  "Wraps function to save all attachments using `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views/mu4e-create-mu4e-attachment-table-if-need-by mu4e-views--current-mu4e-message)
  (mu4e-views/mu4e-save-all-attachments)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-action ()
  "Wraps the `mu4e-view-action' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-action mu4e-views--current-mu4e-message)
  )

;;;###autoload
(defun mu4e-views/mu4e-view-fetch-url ()
  "Wraps the `mu4e-view-fetch-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-fetch-url mu4e-views--current-mu4e-message)
  )

;; ********************************************************************************
;; Minor mode that bounds keys to access mu4e email actions like saving attachments.
;; create a custom keymap for mu4e-views-view-actions-mode-map
(defvar mu4e-views-view-actions-mode-map
  (make-keymap)
  "Mu4e-views-view-actions-mode keymap.")
(define-key mu4e-views-view-actions-mode-map (kbd "q") 'mu4e-views/mu4e-headers-windows-only)
(define-key mu4e-views-view-actions-mode-map (kbd "n") 'mu4e-views/mu4e-headers-next)
(define-key mu4e-views-view-actions-mode-map (kbd "p") 'mu4e-views/mu4e-headers-prev)
(define-key mu4e-views-view-actions-mode-map (kbd "o") 'mu4e-views/mu4e-view-open-attachment)
(define-key mu4e-views-view-actions-mode-map (kbd "g") 'mu4e-views/mu4e-view-go-to-url)
(define-key mu4e-views-view-actions-mode-map (kbd "k") 'mu4e-views/mu4e-view-save-url)
(define-key mu4e-views-view-actions-mode-map (kbd "e") 'mu4e-views/mu4e-view-save-attachment)
(define-key mu4e-views-view-actions-mode-map (kbd "E") 'mu4e-views/mu4e-view-save-all-attachments)
(define-key mu4e-views-view-actions-mode-map (kbd "a") 'mu4e-views/mu4e-view-action)
(define-key mu4e-views-view-actions-mode-map (kbd "f") 'mu4e-views/mu4e-view-fetch-url)

;; create a minor mode mainly for custom keys
(define-minor-mode mu4e-view-view-actions-mode
  "Minor mode for setting up keys when viewing a mu4e email in a mu4e-views window."
  ;; The initial value - Set to 1 to enable by default
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " M4"
  ;; The minor mode keymap
  :keymap mu4e-views-view-actions-mode-map
  ;; Make mode global rather than buffer local
  :global nil
  )

;; function to select how to view emails
;;;###autoload
(defun mu4e-views/mu4e-select-view-msg-method ()
  "select the method for viewing emails in mu4e"
  (interactive)
  (ivy-read "Select method for viewing mail: " ;; prompt
			(mapcar (lambda (x) (car x)) mu4e-views-view-commands) ;; collection to complete over
			:action (lambda (x)
					  (mu4e-views/mu4e-use-view-msg-method x)
					  )
			:sort t
			:require-match t
            :history 'mu4e-views--mu4e-select-view-msg-method-history
			:caller 'mu4e-views/mu4e-select-view-msg-method
			)
  )

;;;###autoload
(defun mu4e-views-deactivate ()
  "Uninstalls the advices on mu4e functions created by mu4e-views."
  (interactive)
  (advice-tools/advice-unadvice 'mu4e~view-internal)
  (advice-tools/advice-unadvice 'mu4e-headers-view-message)
  (advice-tools/advice-unadvice 'mu4e~headers-move)
  )

(provide 'mu4e-views)
;;; mu4e-views.el ends here
