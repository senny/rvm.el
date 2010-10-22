;;; rvm.el --- Emacs integration for rvm

;; Copyright (C) 2010 Yves Senn

;; Author: Yves Senn <yves.senn@gmx.ch>
;; URL: http://www.emacswiki.org/emacs/RvmEl
;; Version: 1.1
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

;;; Code:

(defcustom rvm-executable
  (or (executable-find "rvm") "~/.rvm/bin/rvm")
  "Location of RVM executable."
  :group 'rvm
  :type 'file)

(defcustom rvm-configuration-file-name
  ".rvmrc"
  "RVM configuration file name"
  :group 'rvm
  :type 'string)

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

(defvar rvm--list-ruby-regexp "\s*\\(=>\\)?\s*\\(.+?\\)\s*\\[\\(.+\\)\\]\s*$"
  "regular expression to parse the ruby version from the 'rvm list' output")

;;;###autoload
(defun rvm-use-default ()
  "use the rvm-default ruby as the current ruby version"
  (interactive)
  (rvm-use (rvm--ruby-default) rvm--gemset-default))

;;;###autoload
(defun rvm-activate-corresponding-ruby ()
  "activate the corresponding ruby version for the file in the current buffer.
This function searches for an .rvmrc file and actiavtes the configured ruby.
If no .rvmrc file is found, the default ruby is used insted."
  (interactive)
  (let* ((rvmrc-path (rvm--rvmrc-locate))
         (rvmrc-info (if rvmrc-path (rvm--rvmrc-read-version rvmrc-path) nil)))
    (if rvmrc-info (rvm-use (first rvmrc-info) (second rvmrc-info))
      (rvm-use-default))))

;;;###autoload
(defun rvm-use (new-ruby new-gemset)
  "switch the current ruby version to any ruby, which is installed with rvm"
  (interactive
   (let* ((picked-ruby (rvm--completing-read "Ruby Version: "
                                             (rvm/list)))
          (picked-gemset (rvm--completing-read "Gemset: "
                                               (rvm/gemset-list picked-ruby))))
     (list picked-ruby picked-gemset)))
  (let* ((ruby-info (rvm/info new-ruby))
         (new-ruby-binary (cdr (assoc "ruby" ruby-info)))
         (new-ruby-gemhome (cdr (assoc "GEM_HOME" ruby-info))))
    (rvm--set-ruby (file-name-directory new-ruby-binary))
    (rvm--set-gemhome new-ruby-gemhome new-gemset))
  (message (concat "Ruby: " new-ruby " Gemset: " new-gemset)))

;;;###autoload
(defun rvm-open-gem (gemhome)
  (interactive (list (rvm--emacs-gemhome)))
  (let* ((gems-dir (concat gemhome "/gems/"))
         (gem-name (rvm--completing-read "Gem: "
                                         (directory-files gems-dir nil "^[^.]")))
         (gem-dir (concat gems-dir gem-name)))
    (when (and (featurep 'perspective) persp-mode)
      (let ((initialize (not (gethash gem-name perspectives-hash))))
        (persp-switch gem-name)))
    (rvm--find-file gem-dir)))

;;;; TODO: take buffer switching into account
(defun rvm-autodetect-ruby ()
  (interactive)
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (message "rvm.el is now autodetecting the ruby version"))

(defun rvm-autodetect-ruby-stop ()
  (interactive)
  (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (message "stopped rvm.el from autodetecting ruby versions"))

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
  (let* ((gemset-result (rvm--call-process ruby-version "gemset" "list"))
         (gemset-lines (split-string gemset-result "\n"))
         (parsed-gemsets (list)))
    (loop for i from 2 to (length gemset-lines) do
          (let ((gemset (nth i gemset-lines)))
            (when (and (> (length gemset) 0)
                       (not (string-match "info:" gemset)))
              (add-to-list 'parsed-gemsets gemset t))))
    parsed-gemsets))

(defun rvm/info (&optional ruby-version)
  (let ((info (rvm--call-process "info" ruby-version))
        (start 0)
        (parsed-info '()))
    (while (string-match "\s+\\(.+\\):\s+\"\\(.+\\)\"" info start)
      (let ((info-key (match-string 1 info))
            (info-value (match-string 2 info)))
        (add-to-list 'parsed-info (cons info-key info-value))
        (setq start (match-end 0))))
    parsed-info))

(defun rvm--completing-read (prompt options)
  (funcall rvm-interactive-completion-function prompt options))

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

(defun rvm--rvmrc-locate (&optional path)
  "searches the directory tree for an .rvmrc configuration file"
  (when (null path) (setq path default-directory))
  (cond
   ((equal (expand-file-name path) (expand-file-name "~")) nil)
   ((equal (expand-file-name path) "/") nil)
   ((member rvm-configuration-file-name (directory-files path))
    (concat (expand-file-name path) "/.rvmrc"))
   (t (rvm--rvmrc-locate (concat (file-name-as-directory path) "..")))))

(defun rvm--rvmrc-read-version (path-to-rvmrc)
  (with-temp-buffer
    (insert-file-contents path-to-rvmrc)
    (goto-char (point-min))
    (if (re-search-forward
         (concat "rvm\s+\\(.+\\)" rvm--gemset-separator "\\(.*\\)") nil t)
        (list (match-string 1) (match-string 2))
      nil)))

(defun rvm--set-gemhome (gemhome gemset)
  (if (and gemhome gemset)
      (let ((current-gemset (concat gemhome rvm--gemset-separator gemset)))
        (setenv "GEM_HOME" current-gemset)
        (setenv "GEM_PATH" (concat gemhome ":" current-gemset))
        (setenv "BUNDLE_PATH" (if (string= gemset rvm--gemset-default)
                                  gemhome current-gemset))
        (rvm--change-path 'rvm--current-gem-binary-path
                          (list (if (rvm--default-gemset-p gemset)
                                    (concat gemhome "/bin")
                                  (concat current-gemset "/bin"))
                                (concat gemhome
                                        rvm--gemset-separator "global/bin"))))
    ;; TODO: make system gems work
    (setenv "GEM_HOME" "")
    (setenv "GEM_PATH" "")
    (setenv "BUNDLE_PATH" "")))

(defun rvm--ruby-default ()
  (car (rvm/list t)))

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
        (message output)))))

(provide 'rvm)
;;; rvm.el ends here
