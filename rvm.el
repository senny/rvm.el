;;; rvm.el --- Emacs integration for rvm

;; Copyright (C) 2010-2011 Yves Senn

;; Author: Yves Senn <yves.senn@gmx.ch>
;; URL: http://www.emacswiki.org/emacs/RvmEl
;; Version: 1.4.0
;; Created: 5 April 2010
;; Keywords: ruby rvm
;; EmacsWiki: RvmEl

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; M-x rvm-use-default prepares the current Emacs session to use
;; the default ruby configured with rvm.

;; M-x rvm-use allows you to switch the current session to the ruby
;; implementation of your choice. You can also change the active gemset.

;;; Compiler support:

(eval-when-compile (require 'cl))
(defvar eshell-path-env)
(defvar persp-mode)
(defvar perspectives-hash)
(declare-function persp-switch "perspective" (name))

;;; Code:

(defcustom rvm-executable
  (or (executable-find "rvm")
      (and (file-readable-p "~/.rvm/bin/rvm") "~/.rvm/bin/rvm")
      (and (file-readable-p "/usr/local/rvm/bin/rvm") "/usr/local/rvm/bin/rvm"))
  "Location of RVM executable."
  :group 'rvm
  :type 'file)

(defcustom rvm-configuration-file-name
  ".rvmrc"
  "RVM configuration file name"
  :group 'rvm
  :type 'string)

(defcustom rvm-verbose t
  "If true, RVM will print messages for various tasks."
  :group 'rvm
  :type 'boolean)

(defvar rvm-configuration-ruby-version-file-name
  ".ruby-version"
  "Ruby version configuration file name")

(defvar rvm-configuration-ruby-gemset-file-name
  ".ruby-gemset"
  "Ruby version configuration file name")

(defvar rvm-configuration-gemfile-file-name
  "Gemfile"
  "Gemfile file name")

(defcustom rvm-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by rvm.el to interactivly complete user input"
  :group 'rvm
  :type 'function)

(defcustom rvm-interactive-find-file-function
  (if ido-mode 'ido-find-file 'find-file)
  "The function which is used by rvm.el to interactivly open files"
  :group 'rvm
  :type 'function)

(defvar rvm--current-ruby nil
  "Current active Ruby version.")

(defvar rvm--current-gemset nil
  "Current active gemset.")

(defvar rvm--gemset-default "global"
  "the default gemset per ruby interpreter")

(defvar rvm--gemset-separator "@"
  "character that separates the ruby version from the gemset.")

(defvar rvm--current-ruby-binary-path nil
  "reflects the path to the current 'ruby' executable.
This path gets added to the PATH variable and the exec-path list.")

(defvar rvm--current-gem-binary-path nil
  "reflects the path to the current 'rubygems' executables.
This path gets added to the PATH variable and the exec-path list.")

(defvar rvm--info-option-regexp "\s+\\(.+?\\):\s+\"\\(.+?\\)\""
  "regular expression to parse the options from rvm info")

(defvar rvm--list-ruby-regexp "\s*\\(=?[>\*]\\)?\s*\\(.+?\\)\s*\\[\\(.+\\)\\]\s*$"
  "regular expression to parse the ruby version from the 'rvm list' output")

(defvar rvm--gemset-list-filter-regexp "^\\(gemsets for\\|Gemset '\\)"
  "regular expression to filter the output of rvm gemset list")

(defvar rvm--gemset-list-regexp "\s*\\(=>\\)?\s*\\(.+?\\)\s*$"
  "regular expression to parse the gemset from the 'rvm gemset list' output")

(defvar rvm--gemfile-parse-ruby-regexp-as-comment "\\#ruby=\\(.+\\)"
  "regular expression to parse the ruby version from the Gemfile as comment")

(defvar rvm--gemfile-parse-ruby-regexp-as-directive "ruby [\'\"]\\(.+\\)[\'\"]"
  "regular expression to parse the ruby version from the Gemfile as directive")

(defvar rvm--gemfile-parse-gemset-regexp "#ruby-gemset=\\(.+\\)"
  "regular expression to parse the ruby gemset from the Gemfile")

(defvar rvm--rvmrc-parse-regexp (concat "\\(?:^rvm\s+\\(?:use\s+\\|\\)\\|environment_id=\"\\)\s*"
                                        "\\(?:--.+\s\\)*" ;; Flags
                                        "\\([^"
                                        rvm--gemset-separator
                                        "\n]+\\)\\(?:"
                                        rvm--gemset-separator
                                        "\\([^\"\s\n]+\\)\\)?\\(?:\"\\|\\)")
  "regular expression to parse the .rvmrc files inside project directories.
the first group matches the ruby-version and the second group is the gemset.
when no gemset is set, the second group is nil")

;; Support Code

;; Put with other utils
;; From http://www.emacswiki.org/emacs/ElispCookbook
(defun rvm--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun rvm--message (format-string &rest objects)
  "Like `message', but will only print if `rvm-verbose' is true."
  (when rvm-verbose
    (apply 'message (cons format-string objects))))

;; Application Code

;;;###autoload
(defun rvm-use-default ()
  "use the rvm-default ruby as the current ruby version"
  (interactive)
  (when (rvm-working-p)
    (rvm-use (rvm--ruby-default) rvm--gemset-default)))

;;;###autoload
(defun rvm-activate-corresponding-ruby ()
  "activate the corresponding ruby version for the file in the current buffer.
This function searches for an .rvmrc file and activates the configured ruby.
If no .rvmrc file is found, the default ruby is used insted."
  (interactive)

  (when (rvm-working-p)
    (let ((config-file-path nil)
          (config-gemset-file-path nil)
          (rvmrc-info (or (rvm--load-info-rvmrc) (rvm--load-info-ruby-version) (rvm--load-info-gemfile))))
      (if rvmrc-info (rvm-use (first rvmrc-info) (second rvmrc-info))
        (rvm-use-default)))))

(defun rvm--load-info-rvmrc (&optional path)
  (let ((config-file-path (rvm--locate-file rvm-configuration-file-name path)))
    (if config-file-path
        (rvm--rvmrc-read-version config-file-path)
      nil)))

(defun rvm--load-info-ruby-version (&optional path)
  (let ((config-file-path (rvm--locate-file rvm-configuration-ruby-version-file-name path))
        (gemset-file-path (rvm--locate-file rvm-configuration-ruby-gemset-file-name path)))
    (if config-file-path
        (list (rvm--chomp (rvm--get-string-from-file config-file-path))
              (if gemset-file-path
                  (rvm--chomp (rvm--get-string-from-file gemset-file-path))
                rvm--gemset-default))
      nil)))

(defun rvm--load-info-gemfile (&optional path)
  (let ((config-file-path (rvm--locate-file rvm-configuration-gemfile-file-name path)))
        (if config-file-path
            (rvm--gemfile-read-version config-file-path)
          nil)))

;;;###autoload
(defun rvm-use (new-ruby new-gemset)
  "switch the current ruby version to any ruby, which is installed with rvm"
  (interactive
   (let* ((picked-ruby (rvm--completing-read "Ruby Version: "
                                             (rvm/list)))
          (picked-gemset (rvm--completing-read "Gemset: "
                                               (rvm/gemset-list picked-ruby))))
     (list picked-ruby picked-gemset)))
  (when (rvm-working-p)
   (let* ((new-ruby-with-gemset (rvm--ruby-gemset-string new-ruby new-gemset))
          (ruby-info (rvm/info new-ruby-with-gemset))
          (new-ruby-binary (cdr (assoc "ruby" ruby-info)))
          (new-ruby-gemhome (cdr (assoc "GEM_HOME" ruby-info)))
          (new-ruby-gempath (cdr (assoc "GEM_PATH" ruby-info))))
     (setq rvm--current-ruby new-ruby)
     (setq rvm--current-gemset new-gemset)
     (rvm--set-ruby (file-name-directory new-ruby-binary))
     (rvm--set-gemhome new-ruby-gemhome new-ruby-gempath new-gemset))
   (rvm--message (concat "Ruby: " new-ruby " Gemset: " new-gemset))))

;;;###autoload
(defun rvm-open-gem (gemhome)
  (interactive (list (rvm--emacs-gemhome)))
  (when (rvm-working-p)
    (let* ((gems-dir (concat gemhome "/gems/"))
           (gem-name (rvm--completing-read "Gem: "
                                           (directory-files gems-dir nil "^[^.]")))
           (gem-dir (concat gems-dir gem-name)))
      (when (and (featurep 'perspective) persp-mode)
        (let ((initialize (not (gethash gem-name perspectives-hash))))
          (persp-switch gem-name)))
      (rvm--find-file gem-dir))))

(defun rvm-activate-ruby-for (path &optional callback)
  "Activate Ruby for PATH.

If CALLBACK is specified, active Ruby for PATH only in that
function."
  (let* ((path (directory-file-name path))
         (prev-ruby rvm--current-ruby)
         (prev-gemset rvm--current-gemset)
         (rvmrc-info
          (or
           (rvm--load-info-rvmrc path)
           (rvm--load-info-ruby-version path)
           (rvm--load-info-gemfile path))))
    (apply 'rvm-use rvmrc-info)
    (when callback
      (unwind-protect
          (funcall callback)
        (rvm-use prev-ruby prev-gemset)))))

;;;; TODO: take buffer switching into account
(defun rvm-autodetect-ruby ()
  (interactive)
  (when (rvm-working-p)
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (rvm--message "rvm.el is now autodetecting the ruby version")))

(defun rvm-autodetect-ruby-stop ()
  (interactive)
  (when (rvm-working-p)
    (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (rvm--message "stopped rvm.el from autodetecting ruby versions")))

(defun rvm/list (&optional default-ruby)
  (let ((rubies (rvm--call-process "list" (when default-ruby "default")))
        (start 0)
        (parsed-rubies '())
        (current-ruby '()))
    (while (string-match rvm--list-ruby-regexp rubies start)
      (let ((ruby-version (match-string 2 rubies))
            (ruby-platform (match-string 3 rubies))
            (ruby-current-version (match-string 1 rubies)))
        (add-to-list 'current-ruby ruby-current-version)
        (if ruby-current-version (add-to-list 'parsed-rubies ruby-version)
          (add-to-list 'parsed-rubies ruby-version t))
        (setq start (match-end 0))))
    parsed-rubies))

(defun rvm/gemset-list (ruby-version)
  (let* ((gemset-result (rvm--call-process "gemset" "list_all"))
         (gemset-lines (split-string gemset-result "\n"))
         (parsed-gemsets (list))
         (ruby-current-version nil))
    (loop for gemset in gemset-lines do
          (let ((filtered-gemset (string-match rvm--gemset-list-filter-regexp gemset)))
            (if filtered-gemset
                (if (string-match ruby-version gemset)
                    (setq ruby-current-version ruby-version)
                  (setq ruby-current-version nil)))
            (if (and (> (length gemset) 0)
                     ruby-current-version
                     (not filtered-gemset)
                     (string-match rvm--gemset-list-regexp gemset))
                ;; replace-regexp is to handle default gemset which is returned with
                ;; wrapping parens but is referred to without them in rvm use command
                (add-to-list 'parsed-gemsets (replace-regexp-in-string "^(\\|)$" "" (match-string 2 gemset)) t))))
    parsed-gemsets))

(defun rvm/info (&optional ruby-version)
  (let ((info (rvm--call-process "info" ruby-version))
        (start 0)
        (parsed-info '()))
    (when (not info) (error "The ruby version: %s is not installed" ruby-version))
    (while (string-match rvm--info-option-regexp info start)
      (let ((info-key (match-string 1 info))
            (info-value (match-string 2 info)))
        (add-to-list 'parsed-info (cons info-key info-value))
        (setq start (match-end 0))))
    parsed-info))

(defun rvm--string-trim (string)
  (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" string))

(defun rvm--ruby-gemset-string (ruby-version gemset)
  (if (rvm--default-gemset-p gemset) ruby-version
    (concat ruby-version rvm--gemset-separator gemset)))

(defun rvm--completing-read (prompt options)
  (let ((selected (funcall rvm-interactive-completion-function prompt options)))
    (rvm--string-trim selected)))

(defun rvm--find-file (directory)
  (let ((default-directory directory))
    (call-interactively rvm-interactive-find-file-function)))

(defun rvm--emacs-ruby-binary ()
  rvm--current-ruby-binary-path)

(defun rvm--emacs-gemhome ()
  (getenv "GEM_HOME"))

(defun rvm--emacs-gempath ()
  (getenv "GEM_PATH"))

(defun rvm--change-path (current-binary-var new-binaries)
  (let ((current-binaries-for-path
         (mapconcat 'identity (eval current-binary-var) ":"))
        (new-binaries-for-path (mapconcat 'identity new-binaries ":")))
    (if (and (eval current-binary-var)
             (not (string= (first (eval current-binary-var)) "/bin")))
        (progn
          (setenv "PATH" (replace-regexp-in-string
                          (regexp-quote current-binaries-for-path)
                          new-binaries-for-path
                          (getenv "PATH")))
          (dolist (binary (eval current-binary-var))
            (setq exec-path (remove binary exec-path))))
      (setenv "PATH" (concat new-binaries-for-path ":" (getenv "PATH"))))
    (dolist (binary new-binaries)
      (add-to-list 'exec-path binary))
    (setq eshell-path-env (getenv "PATH"))
    (set current-binary-var new-binaries)))

(defun rvm--set-ruby (ruby-binary)
  (rvm--change-path 'rvm--current-ruby-binary-path (list ruby-binary)))

(defun rvm--locate-file (file-name &optional path)
  "searches the directory tree for an given file. Returns nil if the file was not found."
  (let ((directory (locate-dominating-file (or path (expand-file-name (or buffer-file-name ""))) file-name)))
    (when directory (expand-file-name file-name directory))))

(defun rvm--get-string-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun rvm--rvmrc-read-version (path-to-rvmrc)
  (rvm--rvmrc-parse-version (rvm--get-string-from-file path-to-rvmrc)))

(defun rvm--gemfile-read-version (path-to-gemfile)
  (rvm--gemfile-parse-version (rvm--get-string-from-file path-to-gemfile)))

(defun rvm--rvmrc-parse-version (rvmrc-line)
  (let ((rvmrc-without-comments (replace-regexp-in-string "#.*$" "" rvmrc-line)))
    (when (string-match rvm--rvmrc-parse-regexp rvmrc-without-comments)
      (list (rvm--string-trim (match-string 1 rvmrc-without-comments))
            (rvm--string-trim (or (match-string 2 rvmrc-without-comments) rvm--gemset-default))))))

(defun rvm--gemfile-parse-version (gemfile-line)
  (let ((ruby-version (when (or (string-match rvm--gemfile-parse-ruby-regexp-as-comment gemfile-line)
				(string-match rvm--gemfile-parse-ruby-regexp-as-directive gemfile-line))
			(match-string 1 gemfile-line)))
	(ruby-gemset (when (string-match rvm--gemfile-parse-gemset-regexp gemfile-line)
		       (match-string 1 gemfile-line))))
    (if ruby-version
        (list ruby-version (or ruby-gemset rvm--gemset-default))
      nil)))

(defun rvm--gem-binary-path-from-gem-path (gempath)
  (let ((gem-paths (split-string gempath ":")))
    (mapcar (lambda (path) (concat path "/bin")) gem-paths)))

(defun rvm--set-gemhome (gemhome gempath gemset)
  (if (and gemhome gempath gemset)
      (progn
        (setenv "GEM_HOME" gemhome)
        (setenv "GEM_PATH" gempath)
        (setenv "BUNDLE_PATH" gemhome)
        (rvm--change-path 'rvm--current-gem-binary-path (rvm--gem-binary-path-from-gem-path gempath)))
    (setenv "GEM_HOME" "")
    (setenv "GEM_PATH" "")
    (setenv "BUNDLE_PATH" "")))

(defun rvm--ruby-default ()
  (car (rvm/list t)))

(defun rvm-working-p ()
  (and rvm-executable (file-exists-p rvm-executable)))

(defun rvm--default-gemset-p (gemset)
  (string= gemset rvm--gemset-default))

(defun rvm--call-process (&rest args)
  (with-temp-buffer
    (let* ((success (apply 'call-process rvm-executable nil t nil
                           (delete nil args)))
           (output (buffer-substring-no-properties
                    (point-min) (point-max))))
      (if (= 0 success)
          output
        (rvm--message output)))))

(defun rvm-gem-install (gem)
  "Install GEM into the currently active RVM Gemset."
  (interactive "SGem Install: ")
  (shell-command (format "%s install %s&" ; & executes async
                         (concat (first rvm--current-ruby-binary-path) "/gem") gem))
  (pop-to-buffer "*Async Shell Command*"))

(provide 'rvm)
;;; rvm.el ends here
