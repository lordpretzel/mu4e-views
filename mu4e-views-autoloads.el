;;; mu4e-views-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_mu4e-views" "flycheck_mu4e-views.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_mu4e-views.el

(autoload 'mu4e-views/mu4e-use-view-msg-method "flycheck_mu4e-views" "\
Apply this method `METHOD' for viewing emails in mu4e-headers view.

\(fn METHOD)" nil nil)

(autoload 'mu4e-views/mu4e-headers-windows-only "flycheck_mu4e-views" "\
Show only the headers window of mu4e." t nil)

(autoload 'mu4e-views/cursor-msg-view-window-down "flycheck_mu4e-views" "\
Scroll message view down using xwidget method if we view message using xwidget-webkit." t nil)

(autoload 'mu4e-views/cursor-msg-view-window-up "flycheck_mu4e-views" "\
Scroll message view up using xwidget method if we view message using xwidget-webkit." t nil)

(autoload 'mu4e-views/mu4e-headers-next "flycheck_mu4e-views" "\
Move to next message in headers view, if a xwidget message
view is open then use that to show the message. With prefix
argument move `n' steps instead.

\(fn &optional N)" t nil)

(autoload 'mu4e-views/mu4e-headers-prev "flycheck_mu4e-views" "\
Move to `n'th previous message in headers view, if a xwidget message view is open then use that to show the message.  With prefix argument move `n' steps backwards.

\(fn &optional N)" t nil)

(autoload 'mu4e-views/mu4e-headers-move "flycheck_mu4e-views" "\
Move by 'n` steps in the headers view. Negative numbers move backwards.  If message view is open show message in the view.

\(fn N)" t nil)

(autoload 'mu4e-views/mu4e-after-headers-mode "flycheck_mu4e-views" "\
Called when `mu4e~headers-move' is called to record from where it was called.

\(fn N)" nil nil)

(autoload 'mu4e-views/mu4e-extract-urls-from-msg "flycheck_mu4e-views" "\
Prepare mu4e datastructure for `MSG' so that command view message commands like browsing urls work in our xwidget message view.

\(fn MSG)" t nil)

(autoload 'mu4e-views/mu4e-select-url-from-message "flycheck_mu4e-views" "\
Select a url included in a mu4e message." t nil)

(autoload 'mu4e-views/mu4e-open-attachment "flycheck_mu4e-views" "\
Select an attached from an mu4e message and open it." t nil)

(autoload 'mu4e-views/mu4e-save-attachment "flycheck_mu4e-views" "\
Select an attached from an mu4e message and save it." t nil)

(autoload 'mu4e-views/mu4e-save-all-attachments "flycheck_mu4e-views" "\
Save all attachements to a single directory choosen by the user." t nil)

(autoload 'mu4e-views/mu4e-view-open-attachment "flycheck_mu4e-views" "\
Wraps the `mu4e-view-open-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-go-to-url "flycheck_mu4e-views" "\
Wraps the `mu4e-view-go-to-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-url "flycheck_mu4e-views" "\
Wraps the `mu4e-view-save-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-attachment "flycheck_mu4e-views" "\
Wraps the `mu4e-save-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-all-attachments "flycheck_mu4e-views" "\
Wraps function to save all attachments using `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-action "flycheck_mu4e-views" "\
Wraps the `mu4e-view-action' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-fetch-url "flycheck_mu4e-views" "\
Wraps the `mu4e-view-fetch-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-select-view-msg-method "flycheck_mu4e-views" "\
select the method for viewing emails in mu4e" t nil)

(autoload 'mu4e-views-deactivate "flycheck_mu4e-views" "\
Uninstalls the advices on mu4e functions created by mu4e-views." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck_mu4e-views" '("mu4e-view")))

;;;***

;;;### (autoloads nil "mu4e-views" "mu4e-views.el" (0 0 0 0))
;;; Generated autoloads from mu4e-views.el

(autoload 'mu4e-views/mu4e-use-view-msg-method "mu4e-views" "\
Apply this method `METHOD' for viewing emails in mu4e-headers view.

\(fn METHOD)" nil nil)

(autoload 'mu4e-views/mu4e-headers-windows-only "mu4e-views" "\
Show only the headers window of mu4e." t nil)

(autoload 'mu4e-views/cursor-msg-view-window-down "mu4e-views" "\
Scroll message view down using xwidget method if we view message using xwidget-webkit." t nil)

(autoload 'mu4e-views/cursor-msg-view-window-up "mu4e-views" "\
Scroll message view up using xwidget method if we view message using xwidget-webkit." t nil)

(autoload 'mu4e-views/mu4e-headers-next "mu4e-views" "\
Move to next message in headers view, if a xwidget message
view is open then use that to show the message. With prefix
argument move `n' steps instead.

\(fn &optional N)" t nil)

(autoload 'mu4e-views/mu4e-headers-prev "mu4e-views" "\
Move to `n'th previous message in headers view, if a xwidget message view is open then use that to show the message.  With prefix argument move `n' steps backwards.

\(fn &optional N)" t nil)

(autoload 'mu4e-views/mu4e-headers-move "mu4e-views" "\
Move by 'n` steps in the headers view. Negative numbers move backwards.  If message view is open show message in the view.

\(fn N)" t nil)

(autoload 'mu4e-views/mu4e-after-headers-mode "mu4e-views" "\
Called when `mu4e~headers-move' is called to record from where it was called.

\(fn N)" nil nil)

(autoload 'mu4e-views/mu4e-extract-urls-from-msg "mu4e-views" "\
Prepare mu4e datastructure for `MSG' so that command view message commands like browsing urls work in our xwidget message view.

\(fn MSG)" t nil)

(autoload 'mu4e-views/mu4e-select-url-from-message "mu4e-views" "\
Select a url included in a mu4e message." t nil)

(autoload 'mu4e-views/mu4e-open-attachment "mu4e-views" "\
Select an attached from an mu4e message and open it." t nil)

(autoload 'mu4e-views/mu4e-save-attachment "mu4e-views" "\
Select an attached from an mu4e message and save it." t nil)

(autoload 'mu4e-views/mu4e-save-all-attachments "mu4e-views" "\
Save all attachements to a single directory choosen by the user." t nil)

(autoload 'mu4e-views/mu4e-view-open-attachment "mu4e-views" "\
Wraps the `mu4e-view-open-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-go-to-url "mu4e-views" "\
Wraps the `mu4e-view-go-to-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-url "mu4e-views" "\
Wraps the `mu4e-view-save-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-attachment "mu4e-views" "\
Wraps the `mu4e-save-attachment' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-save-all-attachments "mu4e-views" "\
Wraps function to save all attachments using `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-action "mu4e-views" "\
Wraps the `mu4e-view-action' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-view-fetch-url "mu4e-views" "\
Wraps the `mu4e-view-fetch-url' function and passes on the message stored in `mu4e-views--current-mu4e-message'." t nil)

(autoload 'mu4e-views/mu4e-select-view-msg-method "mu4e-views" "\
select the method for viewing emails in mu4e" t nil)

(autoload 'mu4e-views-deactivate "mu4e-views" "\
Uninstalls the advices on mu4e functions created by mu4e-views." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mu4e-views" '("mu4e-view")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mu4e-views-autoloads.el ends here
