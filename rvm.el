;; rvm.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2010  Yves Senn <yves senn * gmx ch>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "rvm--<descriptive-name>", and name rvm command invocations
;; "rvm/command-name", like rvm/list.

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

(defvar rvm--current-ruby-binary-path nil
  "reflects the path to the current 'ruby' executable.
This path gets added to the PATH variable and the exec-path list.")

(defun rvm-use-default ()
  "use the rvm-default ruby as the current ruby version"
  (interactive)
  (rvm--set-ruby (rvm--ruby-binary-path "--default")))

(defun rvm-activate-corresponding-ruby ()
  "activate the corresponding ruby version for the file in the current buffer.
This function searches for an .rvmrc file and actiavtes the configured ruby.
If no .rvmrc file is found, the default ruby is used insted."
  (interactive)
  (let* ((rvmrc-path (rvm--rvmrc-locate))
         (rvmrc-ruby (if rvmrc-path (rvm--rvmrc-read-version rvmrc-path) nil)))
    (if rvmrc-ruby (rvm-use rvmrc-ruby)
      (rvm-use-default))))

(defun rvm-use (new-ruby)
  "switch the current ruby version to any ruby, which is installed with rvm"
  (interactive (list (ido-completing-read "Ruby Version: " (rvm/list))))
  (let* (;; (new-ruby-binary (assoc "ruby" (rvm/info new-ruby)))
         (new-ruby-binary (rvm--ruby-binary-path new-ruby)))
    (rvm--set-ruby new-ruby-binary))
  (message (concat "current Ruby: " new-ruby)))

;; TODO: take buffer switching into account
(defun rvm-autodetect-ruby ()
  (interactive)
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (message "rvm.el is now autodetecting the ruby version"))

(defun rvm-autodetect-ruby-stop ()
  (interactive)
  (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (message "stopped rvm.el from autodetecting ruby versions"))

(defun rvm/list ()
  (let ((rubies (rvm--call-process "list"))
        (start 0)
        (parsed-rubies '())
        (current-ruby '()))
    (while (string-match "\s*\\(=>\\)?\s*\\(.+?\\)\s*\\[\\(.+\\)\\]\s*$" rubies start)
      (let ((ruby-version (match-string 2 rubies))
            (ruby-platform (match-string 3 rubies))
            (ruby-current-version (match-string 1 rubies)))
        (add-to-list 'current-ruby ruby-current-version)
        (if ruby-current-version (add-to-list 'parsed-rubies ruby-version)
          (add-to-list 'parsed-rubies ruby-version t))
        (setq start (match-end 0))))
    (when (= (length (delete nil current-ruby)) 0)
      (delete "system" parsed-rubies)
      (add-to-list 'parsed-rubies "system"))
    parsed-rubies))

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

(defun rvm--set-ruby (ruby-binary)
  (if (and rvm--current-ruby-binary-path (not (string= rvm--current-ruby-binary-path "/bin")))
      (progn
        (setenv "PATH" (replace-regexp-in-string
                        (regexp-quote rvm--current-ruby-binary-path)
                        ruby-binary
                        (getenv "PATH")))
        (setq exec-path (remove rvm--current-ruby-binary-path exec-path)))
    (setenv "PATH" (concat ruby-binary ":" (getenv "PATH"))))
  (add-to-list 'exec-path ruby-binary)
  (setq rvm--current-ruby-binary-path ruby-binary))

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
    (if (re-search-forward "rvm\s+\\(.+\\)@" nil t)
        (match-string 1)
      nil)))

(defun rvm--ruby-binary-path (ruby-version)
  (let ((info (rvm--call-process "info" ruby-version)))
    (string-match "MY_RUBY_HOME:\s+\"\\(.*?\\)\"" info)
    (concat (match-string 1 info) "/bin")))

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
