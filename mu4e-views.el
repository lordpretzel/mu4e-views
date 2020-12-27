;;; mu4e-views.el --- View emails in mu4e using xwidget-webkit -*- lexical-binding: t -*-

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: Boris Glavic <lordpretzel@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "26.1") (xwidgets-reuse "0.2") (ht "2.2"))
;; Homepage: https://github.com/lordpretzel/mu4e-views
;; Keywords: mail


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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `mu4e' is great, but viewing of html emails is suboptimal.  This packages
;; enables the user to choose how to view emails.  It's main use case is to view
;; emails using an xwidgets window, but the user provided viewing methods are
;; also supported.
;;
;; Also provides methods for user defined viewing methods to access content
;; extracted from an email, e.g., urls or attachments.  This makes it easier to
;; build new views.


;;; Code:
;;TODO also wrap mu4e text email viewing to get the customizable behaviour and reduction of window messing
(require 'seq)
(require 'mu4e)
(require 'ht)
(require 'xwidgets-reuse)
(require 'cl-lib)
(require 'thingatpt)

;; ********************************************************************************
;; Customize and defvars
(defcustom mu4e-views-view-commands
  '(("text" . (:viewfunc mu4e-headers-view-message)) ;; open with standard mu4e function
	("html" . (:viewfunc mu4e-views-mu4e-view-xwidget)) ;; open with xwidget
	("browser" . (:viewfunc mu4e-views-view-in-browser :no-view-window t))) ;; open with browser
  "A list of commands for viewing messages in mu4e.

Currently supported are translating html into text (text which is
also the `mu4e' default), opening html in xwidgets within
Emacs (html), or opening the email's html using
`browse-url' (browser)."
  :group 'mu4e-views
  :type 'alist)

(defcustom mu4e-views-default-view-method
  (cdr (assoc "text" mu4e-views-view-commands))
  "Default method to use for viewing emails in mu4e."
  :group 'mu4e-views
  :type 'function)

(defcustom mu4e-views-inject-email-information-into-html
  t
  "Show a email headers (e.g., subject) in the html view.

If t then `mu4e-views' will inject email message headers information into the
email's html file for the email.  This is useful for viewing emails in
browsers and xwidgets."
  :group 'mu4e-views
  :type 'boolean)

(defcustom mu4e-views-next-previous-message-behaviour
  'always-switch-to-headers
  "Behavior when moving to the next / previous message in the mu4e-headers view.

Default (`always-switch-to-headers') is to stay in the headers views.  Other
options are staying in the current view (`stick-to-current-window') or always
moving to the `mu4e-views' window (`always-switch-to-view')."
  :group 'mu4e-views
  :type '(radio (const :tag
                       "Always switch to mu4e-headers window which shows the list of emails"
                       always-switch-to-headers)
                (const :tag
                       "Always switch to mu4e-views view window which shows the current email"
                       always-switch-to-view)
                (const :tag
                       "Always stay in the current window"
                       stick-to-current-window)))

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
  :type 'string)

(defcustom mu4e-views-completion-method
  'default
  "The completion framework to use when letting choosing an option from a list.

The default (`default') is to just use completing read.  Other
supported options are `ivy', `helm', and `ido'."
  :group 'mu4e-views
  :type '(radio (const :tag "Use completing read." default)
                (const :tag "Use ivy." ivy)
                (const :tag "Use helm." helm)
                (const :tag "Use ido." ido)
                (function :tag "Custom function")))

(defvar mu4e-views--mu4e-select-view-msg-method-history
  nil
  "Store completion history for `mu4e-views-mu4e-select-view-msg-method'.")

(defvar mu4e-views--current-viewing-method
  mu4e-views-default-view-method
  "Records which method for viewing email in mu4e is currently active.")

(defvar mu4e-views--view-window
  nil
  "Caches the view window.")

(defvar mu4e-views--current-mu4e-message
  nil
  "Store the `mu4e' message object for the message shown in `mu4e-views' window.

This enables us to provide `mu4e' functionality in a `mu4e-views'
view such as opening or storing attachments which need this
object.")

(defvar mu4e-views--header-selected
  t
  "On moving to another email, store whether we are in the headers window.")

(defvar mu4e-views--called-from-view
  nil
  "Set if we are called from the view window.")

;; ********************************************************************************
;; helper functions for advising
(defun mu4e-views-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun mu4e-views-advice-add-if-def (f type theadvice)
  "Add advice THEADVICE as type TYPE to function F.

Only do this if the function to be advised (F) and the advising
function (THEADVICE) both exists."
  (when (and (fboundp f)  (fboundp theadvice))
    (advice-add f type theadvice)))

(defun mu4e-views-advice-remove-if-def (f theadvice)
  "Remove advice THEADVICE from function F.

Only do this if the function to be advised (F) and the advising
function (THEADVICE) both exists."
  (when (and (fboundp f)  (fboundp theadvice))
    (advice-remove f theadvice)))

;; ********************************************************************************
;; wrapper for completing read frameworks (adapted from projectile:
;; https://github.com/bbatsov/projectile/)
(cl-defun mu4e-views-completing-read (prompt
                                      choices
                                      &key initial-input
                                      action
                                      history
                                      sort
                                      caller
                                      require-match)
  "Present a PROMPT with CHOICES.

Optionally, provide INITIAL-INPUT and an ACTION to execute with
the chosen option.  If the completion framework supports it and
HISTORY is not nil, then store completion history in HISTORY.  If
the framework supports it and SORT is t, then sort CHOICES.  If
CALLER is provided and the framework supports it, provide CALLER
as a caller.  Otherwise, provide `mu4e-views-completing-read' as
a caller.  If REQUIRE-MATCH is provided, then only matching
inputs can be selected."
  (let (res)
    (setq res
          (cond
           ((eq mu4e-views-completion-method 'ido)
            (ido-completing-read prompt
                                 choices
                                 nil
                                 require-match
                                 initial-input
                                 history))
           ((eq mu4e-views-completion-method 'default)
            (completing-read prompt
                             choices
                             nil
                             require-match
                             initial-input
                             history))
           ((eq mu4e-views-completion-method 'helm)
            (if (and (fboundp 'helm)
                     (fboundp 'helm-make-source))
                (helm :sources
                      (helm-make-source "mu4e-views" 'helm-source-sync
                        :candidates choices
                        :must-match require-match
                        :action (if action
                                    (prog1 action
                                      (setq action nil))
                                  #'identity))
                      :prompt prompt
                      :input initial-input
                      :history history
                      :buffer "*helm-mu4e-views*")
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
           ((eq mu4e-views-completion-method 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :action (prog1 action
                                    (setq action nil))
                          :caller (or caller 'mu4e-views-completing-read)
                          :history history
                          :require-match require-match
                          :sort sort)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           (t (funcall mu4e-views-completion-method prompt choices))))
    (if action
        (funcall action res)
      res)))

;; ********************************************************************************

;;;###autoload
(defun mu4e-views-mu4e-use-view-msg-method (method)
  "Apply METHOD for viewing emails in mu4e-headers view."
  (let
	  ((cmd (cdr (assoc method mu4e-views-view-commands)))
	   (oldmethod mu4e-views--current-viewing-method))
	;; do not update anything if the method is the same
	(unless (eq cmd oldmethod)
	  (setq mu4e-views--current-viewing-method cmd)
	  (if (eq (plist-get cmd :viewfunc) #'mu4e-headers-view-message)
		  ;; use standard mu4e method (remove all advice)
          (mu4e-views-unload-function)
		;; replace advice
        (mu4e-views-advice-mu4e)))))

;; ********************************************************************************
;; functions for viewing a mu4e message in xwidgets
(defun mu4e-views-mu4e-view-xwidget (html msg win)
  "View message MSG with HTML content in xwidget using window WIN."
  (interactive)
  (ignore msg)
  (unless (fboundp 'xwidget-webkit-browse-url)
	(mu4e-error "No xwidget support available"))
  ;; select window
  (select-window win)
  ;; show message
  (xwidgets-reuse-xwidget-reuse-browse-url
   (concat "file://" html)
   'mu4e-views-view-actions-mode))

;; ********************************************************************************
;; functions viewing email in a webbrowser (available as action and as a view method)
(defun mu4e-views-mu4e-view-in-browser-action (msg)
  "Open email MSG in browser using `browse-url'."
  (interactive)
  (browse-url (concat "file://"
                      (mu4e-views-mu4e~write-body-and-headers-to-html msg))))

(defun mu4e-views-view-in-browser (html msg)
  "Open email `MSG` with content HTML using `browse-url'."
  (ignore msg)
  (browse-url (concat "file://" html)))

;; access with `aV' from headers view
(add-to-list 'mu4e-headers-actions
             '("ViewInBrowser" . mu4e-views-mu4e-view-in-browser-action) t)

;; ********************************************************************************
;; functions for writing a message to HTML and making it accessible to custom views
(defun mu4e-views-mu4e-email-headers-as-html (msg)
  "Create formatted html for headers like subject and from/to of email MSG."
  (interactive)
  (mu4e-views-mu4e-create-mu4e-attachment-table-if-need-by
   mu4e-views--current-mu4e-message)
  (cl-flet ((wrap-row (header content id)
                      (concat "<div class=\"mu4e-mu4e-views-header-row\">"
                              "<div class=\"mu4e-mu4e-views-mail-header\">"
                              header "</div>: <div class=\"mu4e-mu4e-views-header-content\" id=\""
                              id "\">"
                              content "</div></div>"))
			(wrap-row-multi (headers)
                            (concat "<div class=\"mu4e-mu4e-views-header-row\">"
									(mapconcat
                                     (lambda (l) (let ((header (nth 0 l))
												       (content (nth 1 l))
												       (id (nth 2 l)))
												   (concat "<div class=\"mu4e-mu4e-views-mail-header\">"
													       header "</div>: <div class=\"mu4e-mu4e-views-header-content\" id=\""
                                                           id "\">"
													       content "</div>")))
										       headers "")
									"</div>")))
	(with-temp-buffer
	  (insert (concat "<div class=\"mu4e-mu4e-views-mail-headers\">"))
	  (insert (wrap-row "from"
                        (mapconcat
                         (lambda (mail)
                           (concat "<div class=\"mu4e-mu4e-views-email\">"
                                   (car mail) " ("
                                   (cdr mail) ")</div>"))
                         (mu4e-message-field msg :from) "")
                        "mu4e-from"))
	  (insert (wrap-row "to" (mapconcat
                              (lambda (mail)
                                (concat "<div class=\"mu4e-mu4e-views-email\">"
                                        (car mail) " ("
                                        (cdr mail) ")</div>"))
                              (mu4e-message-field msg :to) "")
                        "mu4e-to"))
	  (when (mu4e-message-field msg :cc)
		(insert (wrap-row "cc" (mapconcat
                                (lambda (mail)
                                  (concat "<div class=\"mu4e-mu4e-views-email\">"
                                          (car mail) " ("
                                          (cdr mail) ")</div>"))
                                (mu4e-message-field msg :cc) "")
                          "mu4e-cc")))
	  (when (mu4e-message-field msg :bcc)
		(insert (wrap-row "bcc" (mapconcat
                                 (lambda (mail)
                                   (concat "<div class=\"mu4e-mu4e-views-email\">"
                                           (car mail) " ("
                                           (cdr mail) ")</div>"))
                                 (mu4e-message-field msg :bcc) "")
                          "mu4e-bcc")))
	  (insert (wrap-row "subject" (mu4e-message-field msg :subject) "mu4e-subject"))
	  (insert (wrap-row-multi (list
							   (list "date"
                                     (format-time-string mu4e-view-date-format
											             (mu4e-message-field msg :date))
                                     "mu4e-date")
							   (list "size"
                                     (mu4e-display-size (mu4e-message-field msg :size))
                                     "mu4e-size")
							   (list "maildir"
                                     (mu4e-message-field msg :maildir)
                                     "mu4e-maildir"))))
	  (let ((attachments (mapcar
                          (lambda (k) (mu4e~view-get-attach mu4e-views--current-mu4e-message k))
                          (ht-keys mu4e~view-attach-map))))
		(when attachments
		  (insert (wrap-row "attachments" (mapconcat
                                           (lambda (att)
                                             (concat "<div class=\"mu4e-mu4e-views-attachment\">"
                                                     (lax-plist-get att :name) " ("
                                                     (mu4e-display-size (lax-plist-get att :size)) ")</div>"))
                                           attachments "")
                            "mu4e-attachments"))))
	  (insert "</div>")
	  (buffer-string))))

(defun mu4e-views-set-auto-mode-dummy (&optional keep-mode-if-same)
  "Do nothing function to replace `set-auto-mode' when just writing to a file.
Ignore `KEEP-MODE-IF-SAME'."
  (ignore keep-mode-if-same))

(defun mu4e-views-vc-refresh-state-dummy ()
  "Do nothing function to replace `vc-refresh-state' when just writing to a file.")

(defun mu4e-views-vc-before-save-dummy ()
  "Do nothing function to replace `vc-before-save' when just writing to a file.")

(defun mu4e-views-vc-after-save-dummy ()
  "Do nothing function to replace `vc-before-after' when just writing to a file.")

(defun mu4e-views-mu4e~write-body-and-headers-to-html (msg)
  "Write the body (either html or text) and headers of MSG to a temporary file.

Return the file's name.  Text messages are converted into html."
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
	(mu4e-views-advice-remove-if-def #'set-visited-file-name 'doom-modeline-update-buffer-file-name)
	(mu4e-views-advice-remove-if-def #'set-visited-file-name 'lsp--on-set-visited-file-name)
	(mu4e-views-advice-remove-if-def #'basic-save-buffer 'polymode-with-current-base-buffer)
	(mu4e-views-advice-remove-if-def #'vc-refresh-state 'doom-modeline-update-vcs-text)
	(mu4e-views-advice-remove-if-def #'vc-refresh-state 'doom-modeline-update-vcs-icon)
	(mu4e-views-advice-remove-if-def #'rename-buffer 'doom-modeline-update-buffer-file-name)
	(mu4e-views-advice-add-if-def #'set-auto-mode :override #'mu4e-views-set-auto-mode-dummy)
	(mu4e-views-advice-add-if-def #'vc-refresh-state :override #'mu4e-views-vc-refresh-state-dummy)
	(mu4e-views-advice-add-if-def #'vc-before-save :override #'mu4e-views-vc-before-save-dummy)
	(mu4e-views-advice-add-if-def #'vc-after-save :override #'mu4e-views-vc-after-save-dummy)
	(let ((cache-before-save-hook before-save-hook)
		  (cache-after-save-hook after-save-hook))
	  (with-temp-buffer
		(setq before-save-hook nil)
		(setq after-save-hook nil)
		(insert (concat "<head><meta charset=\"UTF-8\">" mu4e-views-mu4e-html-email-header-style  "</head>\n"))
        (when mu4e-views-inject-email-information-into-html
          (insert (mu4e-views-mu4e-email-headers-as-html msg)))
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
		(mu4e-views-advice-add-if-def #'set-visited-file-name :after 'doom-modeline-update-buffer-file-name)
		(mu4e-views-advice-add-if-def #'set-visited-file-name :around 'lsp--on-set-visited-file-name)
		(mu4e-views-advice-add-if-def #'basic-save-buffer :around 'polymode-with-current-base-buffer)
		(mu4e-views-advice-add-if-def #'vc-refresh-state :after 'doom-modeline-update-vcs-text)
		(mu4e-views-advice-add-if-def #'vc-refresh-state :after 'doom-modeline-update-vcs-icon)
		(mu4e-views-advice-add-if-def #'rename-buffer :after 'doom-modeline-update-buffer-file-name)
		(mu4e-views-advice-remove-if-def #'set-auto-mode #'mu4e-views-set-auto-mode-dummy)
		(mu4e-views-advice-remove-if-def #'vc-refresh-state #'mu4e-views-vc-refresh-state-dummy)
		(mu4e-views-advice-remove-if-def #'vc-before-save #'mu4e-views-vc-before-save-dummy)
		(mu4e-views-advice-remove-if-def #'vc-after-save #'mu4e-views-vc-after-save-dummy)
		(setq before-save-hook cache-before-save-hook)
		(setq after-save-hook cache-after-save-hook)
		(lax-plist-put msg :html-file tmpfile)
		tmpfile))))

;; ********************************************************************************
;; functions that are replace mu4e functions and make sure the mu4e-views window is shown
;;TODO check that the window size ratios are ok?
(defun mu4e-views-mu4e-header-and-view-windows-p ()
  "Check whether we are already showing the `mu4e-headers' and our view windows."
  (let ((have-header nil)
        (have-view nil)
        (other-buf nil)
        (loading-buffer mu4e~headers-loading-buf)
        (header-buffer (mu4e-get-headers-buffer)))
    (if (eq (length (window-list)) 2)
        ;; we have two window check whether they are the correct ones
        (progn
          (cl-loop for w in (window-list) do
                   (let* ((buf (window-buffer w)))
                     (when (eq buf header-buffer)
                       (setq have-header t))
                     (when (eq buf loading-buffer)
                       (setq have-view t))
                     (unless (or (eq buf header-buffer) (eq buf loading-buffer))
                       (setq other-buf t))))
          (and have-header (or have-view other-buf)))
      ;; return nil if window list has not exactly 2 windows
      nil)))

(defun mu4e-views-mu4e-view-window-p (&optional window)
  "Return t if WINDOW is the mu4e-views message window.  If WINDOW is omitted, then check for the current window."
  (if (mu4e-views-mu4e-header-and-view-windows-p)
      (let ((thewindow (or window (selected-window))))
        (message "selected win: %s" thewindow)
        (eq thewindow (mu4e-views-get-view-win)))
    nil))

(defun mu4e-views-get-view-win (&optional noerror)
  "Return window to use for `mu4e-views' viewing of emails.

If optional argument NOERROR is t then do not throw an error if the window does not exist."
  (let (win)
    (cl-loop for w in (window-list) do
             (let* ((buf (window-buffer w))
                    (header-buffer (mu4e-get-headers-buffer)))
               (unless (eq buf header-buffer)
                 (when (window-valid-p w)
                   (setq win w)))))
    (if win
        win
      (unless noerror (error "View window not found in %s" (window-list))))))

(defun mu4e-views-headers-redraw-get-view-window ()
  "Unless we already have the correct window layout, rebuild it.

For that we close all windows, redraw the headers buffer based on
the value of `mu4e-split-view', and return a window for the
message view (if the current viewing method needs a window)."
  ;; if single is used then the headers buffer needs to be replaced
  (when (eq mu4e-split-view 'single-window)
    (let ((win (or (and (buffer-live-p (mu4e-get-view-buffer))
                        (get-buffer-window (mu4e-get-view-buffer)))
                   (selected-window))))
      (setq mu4e-views--view-window win)
      win))
  ;; if we have already the right setup, then just return the mu4e-views window
  (if (mu4e-views-mu4e-header-and-view-windows-p)
      (mu4e-views-get-view-win)
    ;; create the window
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
                   (selected-window)))))))

(defun mu4e-views-headers-redraw-get-view-buffer ()
  "Return the view buffer, redrawing the view window if we do not have the correct layout."
  (window-buffer (mu4e-views-headers-redraw-get-view-window)))

(defun mu4e-views-select-other-view ()
  "When the headers view is selected, then select the message view (if that has a live window), and vice versa."
  (interactive)
  (message "SWITCHING")
  (let* ((other-buf
          (cond
           ((eq major-mode 'mu4e-headers-mode)
            (window-buffer (mu4e-views-get-view-win t)))
           ((mu4e-views-mu4e-view-window-p)
            (mu4e-get-headers-buffer))))
         (other-win (and other-buf (get-buffer-window other-buf))))
    (message "other-win %s other buffer %s" other-win other-buf)
    (if (window-live-p other-win)
        (select-window other-win)
      (mu4e-message "NO window to switch to"))))

(defun mu4e-views-mu4e-headers-view-message ()
  "View message at point.

If there's an existing window for the view, re-use that one.  If
not, create a new one, depending on the value of
`mu4e-split-view': if it's a symbol `horizontal' or `vertical',
split the window accordingly; if it is nil, replace the current
window."
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((msg (mu4e-message-at-point))
         (docid (or (mu4e-message-field msg :docid)
                    (mu4e-warn "No message at point")))
         (decrypt (mu4e~decrypt-p msg))
         (verify  (not mu4e-view-use-gnus)))
    ;; (viewwin (if (plist-get mu4e-views--current-viewing-method :no-view-window)
    ;;              nil
    ;;            (mu4e-views-headers-redraw-get-view-window))))
    ;; (when viewwin
    ;;   (select-window viewwin)
    ;;   ;; show some 'loading...' buffer
    ;;   (unless (buffer-live-p mu4e~headers-loading-buf)
    ;;     (setq mu4e~headers-loading-buf (get-buffer-create " *mu4e-loading*"))
    ;;     (with-current-buffer mu4e~headers-loading-buf
    ;;       (mu4e-loading-mode)))
    ;;   (switch-to-buffer mu4e~headers-loading-buf))
    (let ((mu4e-ver (version-to-list mu4e-mu-version)))
      (if (and (eq (car mu4e-ver) 1) (eq (cadr mu4e-ver) 2))
          (mu4e~proc-view docid decrypt verify)
        (mu4e~proc-view docid mu4e-view-show-images decrypt verify)))))

(defun mu4e-views-view-msg-internal (msg)
  "Replacement for `mu4e-view-msg-internal'.

Takes `mu4e' message MSG as input."
  (let* ((viewfunc (plist-get mu4e-views--current-viewing-method :viewfunc))
         (no-window (plist-get mu4e-views--current-viewing-method :no-view-window))
         (html (mu4e-message-field msg :body-html))
	     (txt (mu4e-message-field msg :body-txt))
         ;; (currentwin (selected-window))
         htmlfile)
	(unless (or html txt)
	  (mu4e-error "No body part for this message"))
    ;; (unless (and win (window-valid-p win))
    ;;   (mu4e-error "Window no longer active"))
	(setq mu4e-views--current-mu4e-message msg)
	(setq htmlfile (mu4e-views-mu4e~write-body-and-headers-to-html msg))
    (if no-window
        ;; method does not use a window, do nothing
        (funcall viewfunc htmlfile msg)
      ;; method needs a window, reuse or create a new one, then switch to the
      ;; headers or view window based on
      ;; `mu4e-views-next-previous-message-behaviour'.
        (funcall viewfunc htmlfile msg (mu4e-views-headers-redraw-get-view-window))
        (select-window (mu4e-views-headers-redraw-get-view-window))
        (cl-case mu4e-views-next-previous-message-behaviour
          (stick-to-current-window (if mu4e-views--header-selected
                                       (select-window
                                        (get-buffer-window (mu4e-get-headers-buffer)))
                                     (select-window
                                      (mu4e-views-headers-redraw-get-view-window))))
          (always-switch-to-view (select-window
                                  (mu4e-views-headers-redraw-get-view-window)))
          (always-switch-to-headers (mu4e~headers-select-window))))))

;; ********************************************************************************
;; helpers for mu4e-headers view.

;;;###autoload
(defun mu4e-views-mu4e-headers-windows-only ()
  "Show only the headers window of mu4e."
  (interactive)
  ;; delete other windows because otherwise mu4e will mess up and select the
  ;; header window for replacement.
  (switch-to-buffer (mu4e-get-headers-buffer))
  (delete-other-windows))

;;;###autoload
(defun mu4e-views-cursor-msg-view-window-down ()
  "Scroll message view down if we are viewing the message using xwidget-webkit."
  (interactive)
  (let* ((wind (other-window-for-scrolling))
		 (mode (with-selected-window wind major-mode)))
	(if (eq mode 'xwidget-webkit-mode)
		(with-selected-window wind
          (if (>= emacs-major-version 27)
		      (xwidget-webkit-scroll-up 100)
            (xwidget-webkit-scroll-up)))
	  (scroll-other-window 2))))

;;;###autoload
(defun mu4e-views-cursor-msg-view-window-up ()
  "Scroll message view up if we are viewing the message using xwidget-webkit."
  (interactive)
  (let* ((wind (other-window-for-scrolling))
		 (mode (with-selected-window wind major-mode)))
	(if (eq mode 'xwidget-webkit-mode)
		(with-selected-window wind
          (if (>= emacs-major-version 27)
		      (xwidget-webkit-scroll-down 100)
            (xwidget-webkit-scroll-down)))
	  (scroll-other-window 2))))

;;;###autoload
(defun mu4e-views-mu4e-headers-next (&optional n)
  "Move to next message in headers view.

If a xwidget message view is open then use that to show the
message.  With prefix argument move N steps instead."
  (interactive "P")
  (let ((step (or n 1)))
	(mu4e-views-mu4e-headers-move step)))

;;;###autoload
(defun mu4e-views-mu4e-headers-prev (&optional n)
  "Move to previous message in headers view.

If a xwidget message view is open then use that to show the
message.  With prefix argument move N steps backwards instead."
  (interactive "P")
  (let ((step (* -1 (or n 1))))
	(mu4e-views-mu4e-headers-move step)))

;;;###autoload
(defun mu4e-views-mu4e-headers-move (n)
  "Move by N steps in the headers view.

Negative numbers move backwards.  If the message view is open
show message in the view."
  (interactive)
  (with-current-buffer (mu4e-get-headers-buffer)
    (setq mu4e-views--called-from-view t)
    (setq mu4e-views--header-selected nil)
    (mu4e~headers-move n)))

;;;###autoload
(defun mu4e-views-mu4e-after-headers-mode (n)
  "Called when `mu4e~headers-move' is called to record from where it was called.
Ignore N."
  (ignore n)
  (if mu4e-views--called-from-view
      (setq mu4e-views--called-from-view nil)
    (setq mu4e-views--header-selected t)))

;; ********************************************************************************
;; helper function for accessing parts of an email. Should be bound by custom
;; mu4e-view modes.  These functions wrapp `mu4e' functions and pass on our
;; saved message

;;;###autoload
(defun mu4e-views-mu4e-extract-urls-from-msg (msg)
  "Prepare mu4e message data structure for MSG.
This data structure is used to support commands like browsing
urls in `mu4e-views' xwidget message view."
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
                (push url urls))))))
      (lax-plist-put msg :body-urls urls))))

;;;###autoload
(defun mu4e-views-mu4e-select-url-from-message ()
  "Select a url from a mu4e message."
  (interactive)
  (mu4e-views-mu4e-extract-urls-from-msg mu4e-views--current-mu4e-message)
  (mu4e-views-completing-read "Select url: " ;; prompt
			                  (lax-plist-get
                               mu4e-views--current-mu4e-message
                               :body-urls)
			                  :action (lambda (x)
					                    (browse-url x))
			                  :sort t
			                  :require-match t
			                  :caller 'mu4e-views-mu4e-select-url-from-message))

;;;###autoload
(defun mu4e-views-mu4e-open-attachment ()
  "Select an attached from a mu4e message and open it."
  (interactive)
  (let* ((attachments (mapcar (lambda (k)
                                (list k
                                      (mu4e~view-get-attach
                                       mu4e-views--current-mu4e-message
                                       k)))
                              (ht-keys mu4e~view-attach-map)))
		 (names (mapcar
                 (lambda (x) (mu4e-message-part-field (cadr x) :name))
                 attachments))
		 (name-to-index (mapcar
                         (lambda (x) (cons (plist-get (cadr x) :name) (car x)))
                         attachments)))
	(mu4e-views-completing-read "Select url: " ;; prompt
			                    names ;; collection to complete over
			                    :action (lambda (x)
						                  (let ((index (cdr (assoc x name-to-index))))
						                    (mu4e-view-open-attachment
                                             mu4e-views--current-mu4e-message
                                             index)))
			                    :sort t
			                    :require-match t
			                    :caller 'mu4e-views-mu4e-open-attachment)))

;;;###autoload
(defun mu4e-views-mu4e-save-attachment ()
  "Select an attached from a mu4e message and save it."
  (interactive)
  (let* ((attachments (mapcar (lambda (k)
                                (list k
                                      (mu4e~view-get-attach
                                       mu4e-views--current-mu4e-message k)))
                              (ht-keys mu4e~view-attach-map)))
		 (names (mapcar
                 (lambda (x) (mu4e-message-part-field (cadr x) :name))
                 attachments))
		 (name-to-index (mapcar
                         (lambda (x) (cons (plist-get (cadr x) :name) (car x)))
                         attachments)))
	(mu4e-views-completing-read "Select attachment(s): " ;; prompt
			                    names ;; collection to complete over
			                    :action (lambda (x)
						                  (let ((index (cdr (assoc x name-to-index))))
						                    (mu4e-view-save-attachment-single
                                             mu4e-views--current-mu4e-message index)))
			                    :sort t
			                    :require-match t
			                    :caller 'mu4e-views-mu4e-save-attachment)))

;;;###autoload
(defun mu4e-views-mu4e-save-all-attachments ()
  "Save all attachments to a single directory chosen by the user."
  (interactive)
  (let* ((msg mu4e-views--current-mu4e-message)
		 (attachnums (sort (ht-keys mu4e~view-attach-map) '<))
		 (path (concat (mu4e~get-attachment-dir) "/"))
		 (attachdir (mu4e~view-request-attachments-dir path)))
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
		 index mu4e-decryption-policy fpath)))))

(defun mu4e-views-mu4e-create-mu4e-attachment-table-if-need-by (msg)
  "Call the `mu4e' function to setup the attachments hash-map for MSG.

The function we are using is
`mu4e~view-construct-attachments-header'. Only do this if we have
not already done this for this message."
  (unless (plist-member msg :attachment-setup)
	(mu4e~view-construct-attachments-header msg)
	(lax-plist-put msg :attachment-setup t)))

;;;###autoload
(defun mu4e-views-mu4e-view-open-attachment ()
  "Wraps the `mu4e-view-open-attachment' function.

Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views-mu4e-create-mu4e-attachment-table-if-need-by
   mu4e-views--current-mu4e-message)
  (mu4e-views-mu4e-open-attachment))

;;;###autoload
(defun mu4e-views-mu4e-view-go-to-url ()
  "Wraps the `mu4e-view-go-to-url' function.

Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views-mu4e-select-url-from-message))

;;;###autoload
(defun mu4e-views-mu4e-view-save-url ()
  "Wraps the `mu4e-view-save-url' function.

 Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-save-url mu4e-views--current-mu4e-message))

;;;###autoload
(defun mu4e-views-mu4e-view-save-attachment ()
  "Wraps the `mu4e-save-attachment' function.

 Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views-mu4e-create-mu4e-attachment-table-if-need-by
   mu4e-views--current-mu4e-message)
  (mu4e-views-mu4e-save-attachment))

;;;###autoload
(defun mu4e-views-mu4e-view-save-all-attachments ()
  "Wraps function to save all attachments using `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-views-mu4e-create-mu4e-attachment-table-if-need-by
   mu4e-views--current-mu4e-message)
  (mu4e-views-mu4e-save-all-attachments))

;;;###autoload
(defun mu4e-views-mu4e-view-action ()
  "Wraps the `mu4e-view-action' function.

Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-action mu4e-views--current-mu4e-message))

;;;###autoload
(defun mu4e-views-mu4e-view-fetch-url ()
  "Wraps the `mu4e-view-fetch-url' function.

Passes on the message stored in `mu4e-views--current-mu4e-message'."
  (interactive)
  (mu4e-view-fetch-url mu4e-views--current-mu4e-message))

;; ********************************************************************************
;; Minor mode that bounds keys to access mu4e email actions like saving attachments.
;; create a custom keymap for mu4e-views-view-actions-mode-map
(defvar mu4e-views-view-actions-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "q") #'mu4e-views-mu4e-headers-windows-only)
    (define-key km (kbd "n") #'mu4e-views-mu4e-headers-next)
    (define-key km (kbd "p") #'mu4e-views-mu4e-headers-prev)
    (define-key km (kbd "o") #'mu4e-views-mu4e-view-open-attachment)
    (define-key km (kbd "g") #'mu4e-views-mu4e-view-go-to-url)
    (define-key km (kbd "k") #'mu4e-views-mu4e-view-save-url)
    (define-key km (kbd "e") #'mu4e-views-mu4e-view-save-attachment)
    (define-key km (kbd "E") #'mu4e-views-mu4e-view-save-all-attachments)
    (define-key km (kbd "a") #'mu4e-views-mu4e-view-action)
    (define-key km (kbd "f") #'mu4e-views-mu4e-view-fetch-url)
    (define-key km (kbd "y") #'mu4e-views-select-other-view)
    km)
  "The keymap for `Mu4e-views-view-actions-mode'.")

;; create a minor mode mainly for custom keys
(define-minor-mode mu4e-views-view-actions-mode
  "Minor mode for setting up keys when viewing a mu4e email in a mu4e-views window."
  ;; The initial value - Set to 1 to enable by default
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " M4"
  ;; The minor mode keymap
  :keymap mu4e-views-view-actions-mode-map
  ;; Make mode global rather than buffer local
  :global nil)

;; Register the minor mode with `xwidgets-reuse' so that it is automatically
;; turned of if the xwidget session is reused for a different purpose.
(xwidgets-reuse-register-minor-mode 'mu4e-views-view-actions-mode)

;; function to select how to view emails
;;;###autoload
(defun mu4e-views-mu4e-select-view-msg-method ()
  "Select the method for viewing emails in `mu4e'."
  (interactive)
  (mu4e-views-completing-read "Select method for viewing mail: " ;; prompt
                              ;; collection to complete over
			                  (mapcar (lambda (x) (car x)) mu4e-views-view-commands)
			                  :action (lambda (x)
					                    (mu4e-views-mu4e-use-view-msg-method x))
			                  :sort t
			                  :require-match t
                              :history 'mu4e-views--mu4e-select-view-msg-method-history
			                  :caller 'mu4e-views-mu4e-select-view-msg-method))

;;;###autoload
(defun mu4e-views-unload-function ()
  "Uninstalls the advices on mu4e functions created by mu4e-views."
  (interactive)
  (mu4e-views-advice-unadvice 'mu4e~view-internal)
  (mu4e-views-advice-unadvice 'mu4e-headers-view-message)
  (mu4e-views-advice-unadvice 'mu4e~headers-move)
  (mu4e-views-advice-unadvice 'mu4e-select-other-view))

(defun mu4e-views-advice-mu4e ()
  "Install the advices on mu4e functions used by mu4e-views to overwrite its functionality."
  (advice-add 'mu4e~view-internal
              :override #'mu4e-views-view-msg-internal)
  (advice-add 'mu4e-headers-view-message
              :override #'mu4e-views-mu4e-headers-view-message)
  (advice-add 'mu4e~headers-move
              :after #'mu4e-views-mu4e-after-headers-mode)
  (advice-add 'mu4e-select-other-view
              :override #'mu4e-views-select-other-view))

(provide 'mu4e-views)
;;; mu4e-views.el ends here
