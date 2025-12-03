[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt) <!-- [![Build Status](https://secure.travis-ci.org/lordpretzel/mu4e-views.png)](http://travis-ci.org/lordpretzel/mu4e-views) --> <!-- [![GitHub release](https://img.shields.io/github/release/lordpretzel/mu4e-views.svg?maxAge=86400)](https://github.com/lordpretzel/mu4e-views/releases) --> [![MELPA Stable](http://stable.melpa.org/packages/mu4e-views-badge.svg)](http://stable.melpa.org/#/mu4e-views) [![MELPA](http://melpa.org/packages/mu4e-views-badge.svg)](http://melpa.org/#/mu4e-views)

# mu4e-views

<!--toc:start-->
- [mu4e-views](#mu4e-views)
  - [Warning](#warning)
  - [Installation](#installation)
    - [MELPA](#melpa)
    - [Prerequisites](#prerequisites)
    - [Quelpa](#quelpa)
    - [straight](#straight)
    - [Source](#source)
  - [Usage](#usage)
    - [Setup](#setup)
    - [Temporarily switching view methods](#temporarily-switching-view-methods)
    - [Settings](#settings)
    - [xwidgets view (view method "html", "html-block" and "html-nonblock")](#xwidgets-view-view-method-html-html-block-and-html-nonblock)
      - [Synergy with `xwwp`](#synergy-with-xwwp)
    - [Filtering html content](#filtering-html-content)
    - [Dispatcher view method](#dispatcher-view-method)
    - [Define custom views](#define-custom-views)
    - [Exporting email](#exporting-email)
  - [Development](#development)
    - [Selecting which `mu4e` versions to include](#selecting-which-mu4e-versions-to-include)
    - [Building the docker images](#building-the-docker-images)
    - [Using the images](#using-the-images)
    - [Running emacs with a particular mu version](#running-emacs-with-a-particular-mu-version)
<!--toc:end-->

 [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) is great, but viewing of html emails is suboptimal.  This packages enables the user to choose how to view emails.  It's main use case is to view html emails using an xwidgets window, but custom viewing methods are also supported.

![viewing-html-emails](./screencasts/mu4e-views.gif)

Also provides methods to access content extracted from an email, e.g., urls or attachments. This makes it easier to build user defined viewing methods.

## Warning

**HTML emails may contain malicious content and tracking images. While `mu4e-views` by default applies a filter to remove things like external images that are used for tracking users, the current filtering is rather naive and probably misses many types of tracking.**

With `mu4e-views` you can customize the filtering rules (`mu4e-views-html-dom-filter-chain`). Also you may want to setup customized rules for which view method and filtering is used based on the senders email address are coming from (see the discussion of the `dispatcher` view method and customized views below).

## Installation

### MELPA

`mu4e-views` is available from MELPA (both
[stable](http://stable.melpa.org/#/mu4e-views) and
[unstable](http://melpa.org/#/mu4e-views)).  Assuming your
`package-archives` lists MELPA, just type

~~~sh
M-x package-install RET mu4e-views RET
~~~

to install it.

### Prerequisites

If you are not installing from [melpa](http://melpa.org/#/mu4e-views) you have to install the dependency [xwidgets-reuse](https://github.com/lordpretzel/xwidgets-reuse) first. Also this uses `xwidget`, so you can only use this package if your emacs has been compiled with support for `xwidget`. If you are unsure whether this is the case try running: `(xwidget-webkit-browse-url "https://www.gnu.org/")`.

### Quelpa

Using [use-package](https://github.com/jwiegley/use-package) with [quelpa](https://github.com/quelpa/quelpa).

~~~elisp
(use-package
  :quelpa ((mu4e-views
    :fetcher github
    :repo "lordpretzel/mu4e-views")
  :upgrade t))
~~~

### straight

Using [use-package](https://github.com/jwiegley/use-package) with [straight.el](https://github.com/raxod502/straight.el)

~~~elisp
(use-package mu4e-views
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views"))
~~~

### Source

Alternatively, install from source. First, clone the source code:

~~~sh
cd MY-PATH
git clone https://github.com/lordpretzel/mu4e-views.git
~~~

Now, from Emacs execute:

~~~
M-x package-install-file RET MY-PATH/mu4e-views
~~~

Alternatively to the second step, add this to your Symbol’s value as variable is void: \.emacs file:

~~~elisp
(add-to-list 'load-path "MY-PATH/mu4e-views")
(require 'mu4e-views)
~~~

## Usage

This package changes the way how `mu4e` shows emails when selecting an email from the `mu4e-headers` view. The main purpose of this package is to enable viewing of `html` emails in `xwidgets-webkit`, but is also possible for a user to define new custom views. Once a view is selected, you just use `mu4e` as usual and emails selected in the `headers` view are shown using the currently active view method.

### Setup

After the package is loaded, you can call `mu4e-views-mu4e-select-view-msg-method` from the `mu4e-headers` view to select the method to use for viewing. Per default `mu4e-view` supports:

- `html` - uses `xwidgets` to show the email
- `html-block` - as `html` but forces the application of filters to block external content
- `html-nonblock` - as `html` but never applies filters to block external content
- `text` - the default `mu4e` method for viewing emails that translates the email into text
- `browser` - open the email using `browse-url`, e.g., in your system browser
- `pdf` - transform email to pdf and view pdf in emacs
- `html-src` - show the source code of html messages
- `gnu` - open the meail using `mu4e`'s gnus method
- `dispatcher` - this view methods applies a set of predicates to the message to determine which view method to use

You may want to bind this to a key in `mu4e-headers-mode-map` and set the default method by customizing `mu4e-views-default-view-method`.

~~~elisp
(define-key mu4e-headers-mode-map (kbd "v") #'mu4e-views-mu4e-select-view-msg-method)
~~~

Here is an example setup:

~~~elisp
(use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	    )
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view
~~~

### Temporarily switching view methods

Sometimes it is useful to be able to view the currently email message currently selected in the headers view using a different view method without changing the view method. For instance, you may want to switch from the default html view method (`"html"`) that blocks remote content to one that shows remote content to show images in the current email (`"html-nonblock"`).

`mu4e-views` now provides a function for that: `(mu4e-views-view-current-msg-with-method NAME)`. This function causes `mu4e-views` to redisplay the current email message using the view method named `NAME`. When selecting a different email message afterwards, this email will be shown using your normal viewing method.

Since showing remote content is a common use case, there is convenience function for that: `mu4e-views-mu4e-view-as-nonblocked-html`. You may want to bind this to a key in `mu4e-headers-view`. This is already bound to `"i"` in the `mu4e-views-view-actions-mode-map` keymap used by the `html` view methods.

### Settings

- `mu4e-views-html-filter-external-content` - apply filters to remove external content from emails **(the default filters are based on my naive understanding and should not be trusted)**.
- `mu4e-views-html-dom-filter-chain` - list of filters to use to remove external content from emails
- `mu4e-views-completion-method` - framework used for completion.
- `mu4e-views-inject-email-information-into-html` - if `t`, then create a header shown on top of the html message with useful information from the email. The header uses CSS styles defined in `mu4e-views-mu4e-html-email-header-style`.
- `mu4e-views-mu4e-html-email-header-style` - CSS style for showing the header of an email (`mu4e-views` injects this header into the html text of the email). Customize to change appearance of this header.
- `mu4e-views-mu4e-email-headers-as-html-function` - if you want to change what html is injected for an email more radically, then you can supply your own function for doing this. This function should take a single parameter that is a `mu4e` message `plist`. Have a look at the `mu4e` source code to learn more about what information is stored in such a plist.
- `mu4e-views-mu4e-email-headers-as-html-function` - this can be used to use a custom function to translate email header information into html for viewing.
- `mu4e-views-next-previous-message-behaviour` - per default `mu4e` switches from the `headers` window to the `view` window once an email is opened, e.g., by pressing `n`. This option customizes this behavior:
  - `always-switch-to-headers` - always switch back the `headers` window
  - `always-switch-to-view` - always switch back the `view` window (default behavior of `mu4e`)
  - `stick-to-current-window` - stay in the current window (`headers` or `views`)
- `mu4e-views-view-commands` - the view methods supported by `mu4e-views`. Customize to add new methods.
- `mu4e-views-default-view-method` - the default method for viewing emails.
- `mu4e-views-auto-view-selected-message` - if `t` (default), then automatically show the email selected in the headers view if the view window is shown. That means when moving between emails with `n` and `p` the view window is updated to show the selected email.
- `mu4e-views-dispatcher-predicate-view-map` - predicates for selecting the view method to use per email when using the *dispatcher* view method.
- `mu4e-views-html-to-pdf-command` - command to run to translate hmtl into pdf for the *pdf* view method
- `mu4e-views-respect-mu4e-view-use-gnus` - normally `mu4e-views` determines its own settings to determine what view method to use. If this is non-nil, then `mu4e-view` respects the `mu4e-view-use-gnus` setting.
- `mu4e-views-export-alist` - alist of `(method . function)` cons cells that maps a `method` (a symbol) to the function implementing this method. These functions should take two arguments `msg` which is a `mu4e` message plist and `file` is the file the exported message should be written to

### xwidgets view (view method "html", "html-block" and "html-nonblock")

Several keys are bound in this view to store attachments, open attachments, go to urls in the email similar to the regular `mu4e` view window.

- `q`: `mu4e-views-mu4e-headers-windows-only` - quit view window
- `n`: `mu4e-views-mu4e-headers-next` - move to next message
- `p`: `mu4e-views-mu4e-headers-prev` - move to previous message
- `o`: `mu4e-views-mu4e-view-open-attachment` - open an attachment
- `g`: `mu4e-views-mu4e-view-go-to-url` - go to URL from email
- `k`: `mu4e-views-mu4e-view-save-url` - save URL
- `e`: `mu4e-views-mu4e-view-save-attachment` - save an attachment
- `E`: `mu4e-views-mu4e-view-save-all-attachment` - save all attachments
- `a`: `mu4e-views-mu4e-view-action` - call a `mu4e` view action
- `f`: `mu4e-views-mu4e-view-fetch-url` - fetch URL from email
- `y`: `mu4e-views-select-other-view` - switch to the other view (the headers view)

#### Synergy with `xwwp`

To use your keyboard to click on links in an email shown in `xwidgets`, you can use the excellent [xwwp](https://github.com/canatella/xwwp) package.

### Filtering html content

`mu4e-views` now supports filtering of `html` content to combat email tracking. However, the default filters of `mu4e-views` is quite naive and probably misses many types of tracking content (pull requests for improvement are appreciated). You can set `mu4e-views-html-filter-external-content` to control whether filters are applied or not.

- **Note**: filtering is only available when emacs is build with `libxml` support, because it uses `libxml-parse-html-region` to translate html into `dom` model.

You can customize `mu4e-views-html-dom-filter-chain` to define a list of functions that are applied in sequence to filter the email message's HTML dom. A filter function `f` should take as input ta message plist `msg` a dom node `dom` and should return the filtered DOM at node `dom`. To recursively apply `f` to the children of a node, your function `f` needs to explicitly call `(mu4e-views-apply-dom-filter-to-children msg node f)`. While inconvenient this is necessary so that your function can change the DOM structure (e.g., remove a subtree). A good starting point is to have a look at the implementation of `mu4e-views-default-dom-filter`.

### Dispatcher view method

If you do not want to implement a full view method yourself, you can let `mu4e-views` dispatch to a view method based on what email you are looking at. You need to customize `mu4e-views-dispatcher-predicate-view-map` which is an alist of `(predicate . viewmethod-name)` pairs. Each of these predicates is a function that takes as input a message plist (`mu4e`'s internal representation of a message). The dispatcher view method applies the predicates in sequence and selects the view method for the first predicate that evaluates to non-nil. For example, this is the default setting for `mu4e-views-dispatcher-predicate-view-map`:

```elisp
`((,(lambda (msg) (mu4e-message-field msg :body-html)) . "html")
  (,(lambda (msg) (ignore msg) t) . "text")
```

If you use this setting, then emails with html content are viewed using xwidgets (the `"html"` view method) and all other emails are viewed using `mu4e`'s default view (called `"text"` in `mu4e-views`). As another example, consider the usecase of blocking remote content from emails unless the sender is in a whitelist (this overrides the `mu4e-views-html-filter-external-content` setting by using view methods `"html-nonblock"` and `"html-block"`). Note that this would require you to setup the `email-whitelist` variable yourself.

```elisp
`((,(lambda (msg) (-contains-p email-whitelist (cdr (mu4e-message-field msg :from))))
   . "html-nonblock")
  (,(lambda (msg) (ignore msg) t) . "html-block")
```

### Define custom views

To define a new view, you need to write a function `(my-view-func html msg win)` that uses window `win` to show the message. `html` is the name of a file storing the `html` content of the message.
If `mu4e-views-inject-email-information-into-html` is `t` then `mu4e-views` injects a header into the html code to show some basic information about the email (e.g., sender, attachments, ...). `msg` is a `mu4e` internal message plist. You can use it to extract additional information about the email to be shown. Please refer to the  [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) and `mu4e-views` source code to see how this works.

To make `mu4e-views` aware of your new view method add it to `mu4e-views-view-commands` giving it a user-facing name. The format is `(cons name plist)`. The supported keys for `plist` are:

- `:viewfunc` [**REQUIRED**] - all view methods needs to set `:viewfunc` to a function. This function should take three arguments: `html`, `msg`, and `win` and should show the message `msg` whose content is stored in file `html` in window `win`. View methods that do not use windows (see `:no-view-window`) should have a signature `(html win)` instead. View methods that set `:view-function-only-msg` to non-nil should have a signature `(msg win)`.
- `:is-view-window-p` [**REQUIRED**] - views methods that use windows (`:no-view-window` is not set) need to set this to a function  with a single argument `window` that returns `t` if `window` is the window your method uses for showing the email
- `:no-view-window` [**OPTIONAL**] - methods that do not show the email in emacs should set `:no-view-window t` which instructs `mu4e-views` to not create a window for viewing the email. For example, the browser method does this.
- `:view-function-only-msg` [**OPTIONAL**] - if your method does not need `m4ue-views` to generate the message as an html file then use `:view-function-only-msg t`. In this case your function should have the signature `(msg win)`.
- `:create-view-window` [**OPTIONAL**] - You can set `:create-view-window` if the window you use for showing messages needs some initial setup before the asynchronous `mu4e` method for viewing an email is called (whose callback will indirectly call your `:viewfunc`). For example, the method below shows the raw html code of messages.
- `:filter-html` [**OPTIONAL**] - if this key is provided it overrides `mu4e-views-html-filter-external-content` and forces filtering / not filtering of email html content.
- `:filters` [**OPTIONAL**] - if html is filtered (the method sets `:filter-html t` or `mu4e-views-html-filter-external-content` is non-nil and the method does not set `:filter-html nil`), then use this list of filters instead of the one set in `mu4e-views-html-dom-filter-chain`.

As an example view method consider the usecase of showing the html source of an email:

~~~elisp
(defun mu4e-views-view-raw-html (html msg win)
  (let ((buf (find-file-noselect html)))
    (with-current-buffer buf
      (read-only-mode)
      (select-window win)
      (switch-to-buffer buf t t))))

(defun mu4e-views-view-raw-html-is-view-p (win)
  (let ((winbuf (window-buffer win)))
    (with-current-buffer winbuf
      (eq major-mode 'html-mode))))

(add-to-list 'mu4e-views-view-commands
             '("rawhtml" .
               (:viewfunc mu4e-views-view-raw-html
                          :is-view-window-p mu4e-views-view-raw-html-is-view-p)))
~~~

`mu4e-views` provides several helper functions for typical operations with emails such as storing attachments as described above. These functions can be used in custom views too.

### Exporting email
`mu4e-views` now has some basic functionality for exporting emails. Out of the box, export to pdf and html files is supported. This functionality is provided through a dispatcher function `mu4e-views-export-msg-action`. Per default this action is not added to the header and view actions. To bind this function as an action under key `e`, add the following to your configuration:

```elisp
(add-to-list 'mu4e-headers-actions
             '("e: export message" . mu4e-views-export-msg-action) t)
(add-to-list 'mu4e-view-actions
             '("e: export message" . mu4e-views-export-msg-action) t)
```

The export action's export formats are defined in `mu4e-views-export-alist` which is an alist mapping symbols (file extensions) to functions that are called for exporting to this type of file. For example, `(pdf . mu4e-views-export-to-pdf)` means that the function `mu4e-views-export-to-pdf` will be called for exporting to `pdf`. An export function should take two arguments `msg` and `file`. `file` is the file to which we should export to and `msg` is the email (a `mu4e` plist).

## Development

If you want to hack on `mu4e-view`, it maybe usefult to build docker containers for testing with different `mu4e` versions. There are two dockerfiles: `Dockerfile` builds a non-gui version and `Dockerfile-gui` builds a gui version for testing with xwidgets that you can connect to via `VNC`.

### Selecting which `mu4e` versions to include

To change which versions are build change
the `VERSIONS` variable in `./dockerfiles/build-mu.sh`. Note that versions are git tags, e.g., `master` or `1.4.13`.

### Building the docker images

For both docker images you need some valid `Maildir` to be included into the image which should be stored in `YOUR_MU4E_SRC_ROOT_DIR/Maildir`. The docker images will contain multiple `mu` versions. Then run from the root `mu4e` source directory:

~~~sh
docker build -t mu4e-views .
docker build -f ./Dockerfile-gui -t mu4e-views-gui .
~~~

### Using the images

To run the non-gui image and expose your version of `mu4e`. Run the following from the `mu4e-views` directory.

~~~sh
docker run -ti --rm -v $(pwd):/mu4e-views mu4e-views /bin/bash
~~~

For the the gui version you would want to run it in daemon mode and expose the VNC port:

~~~sh
docker run -d --rm -p 5900:5900 -v $(pwd):/mu4e-views mu4e-views-gui
~~~

Then use your favorite VNC viewer to connect to the container.

### Running emacs with a particular mu version

The images include scripts to start emacs with a particular mu version, e.g., `/emu-master.sh` or `/emu-1.4.13.sh`. The build will create one script for each version in `VERSIONS` (see above).


 This docker container is meant for debugging mu4e-views with different mu versions in gui environment
 A typical use case is to start a container with your local development version of mu4e-views
 E.g., from your local mu4e-views git repo:

 To build the docker image copy some valid Maildir to `./Maildir` and run `docker build -t mu4e-views-test .`
 If you need to test with a different version, then add it to `VERSIONS="1.3.10 1.4.13 master` in
