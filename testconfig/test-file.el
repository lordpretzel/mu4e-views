;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(custom-set-variables '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") )

(require 'package)
(package-initialize)

;;(require 'cl-lib)
;;(require 'cl-mac)
(defvar custom/packages
  '(ht
    esxml
    xwidgets-reuse)
  "Default packages")

(setq mu4e-views--debug t)

(defun custom/packages-installed-p ()
  (let ((in t))
  (dolist (pkg custom/packages)
    (when (not (package-installed-p pkg))
      (setq in nil)))
     in))

(when (not (custom/packages-installed-p))
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg custom/packages)
    (when (not (package-installed-p pkg))
    (package-install pkg))))

(setq mu-ver (getenv "MUVER"))
(setq mu4e-dir (concat "/mu-" mu-ver "/share/emacs/site-lisp/mu4e/"))
(setq mu-bin (concat "/mu-" mu-ver "/bin/mu"))
(setq mu-home (concat "/mu-home-" mu-ver))

(add-to-list 'load-path mu4e-dir)
(require 'mu4e)
(add-to-list 'load-path "/mu4e-views")
(require 'mu4e-views)

(setq mu4e-mu-binary mu-bin)
(setq mu4e-mu-home mu-home)

(define-key mu4e-headers-mode-map (kbd "v") 'mu4e-views-mu4e-select-view-msg-method)
(setq mu4e-views-default-view-method "html")
(mu4e-views-mu4e-use-view-msg-method "html")

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; allow for updating mail using 'U' in the main view: (use none so we do not have to setup everything)
(setq mu4e-get-mail-command nil)
;;(setq mu4e-get-mail-command "mbsync -a")

;; directory for mbsync (default settings)
(setq mu4e-maildir (expand-file-name "/Maildir/"))
(setq mu4e-drafts-folder "/lordpretzel-gmx/Drafts")
(setq mu4e-sent-folder   "/lordpretzel-gmx/SentMail")
(setq mu4e-trash-folder "/lordpretzel-gmx/Trash")

;; shortcuts
(add-to-list 'mu4e-bookmarks
             '(:name "Messages with attachments"
                     :query "flag:attach"
                     :key ?a))

;; something about ourselves
(setq user-mail-address "lord_pretzel@gmx.net")
(setq user-full-name  "Boris Glavic")

(setq mu4e-contexts
      `(,(make-mu4e-context
           :name "gmx"
           :enter-func (lambda () (mu4e-message "Enter lord_pretzel@gmx.net context"))
           :leave-func (lambda () (mu4e-message "Leave lord_pretzel@gmx.net context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "lord_pretzel@gmx.net")))
           :vars '( ( user-mail-address       . "lord_pretzel@gmx.net" )
                    ( user-full-name          . "Boris Glavic" )
                    (smtpmail-queue-dir . "~/Maildir/lordpretzel-gmx/queue/")
                    (message-send-mail-function . smtpmail-send-it)
                    (smtpmail-smtp-user . "lord_pretzel@gmx.net")
                    (smtpmail-starttls-credentials . (("mail.gmx.net" 587 nil nil)))
                    (smtpmail-auth-credentials . (expand-file-name "~/scripts/.authinfo.gpg"))
                    (smtpmail-default-smtp-server . "mail.gmx.net")
                    (smtpmail-smtp-server . "mail.gmx.net")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-debug-info . t)
                    (smtpmail-debug-verbose . t)
                    (mu4e-maildir-shortcuts . (
                                               ("/lordpretzel-gmx/INBOX"            . ?i)
                                               ("/lordpretzel-gmx/SentMail" . ?s)
                                               ("/lordpretzel-gmx/Spam"       . ?p)
                                               ))
                    (mu4e-sent-folder .  "/lordpretzel-gmx/SentMail")
                    )
           )
         )
      )

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(mu4e-context-switch t "gmx")

;; for convenience open source file and test config
(find-file "/mu4e-views/testconfig/test-file.el")
(find-file "/mu4e-views/mu4e-views.el")
(mu4e)
