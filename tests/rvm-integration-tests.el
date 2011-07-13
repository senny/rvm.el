;;; rvm-integration-tests.el --- Test Suite for rvm.el

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)

(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun get-rvm-stub (name)
  (get-string-from-file (concat
                         (file-name-directory (symbol-file 'get-rvm-stub))
                         "rvm_stubs/"
                         name)))

(defun rvm--call-process (&rest args)
  (let ((command (first args))
        (arg1 (second args)))
    (cond
     ((and (string= command "list") (string= arg1 t))
      (get-rvm-stub "rvm_list_default"))
     ((string= command "list")
      (get-rvm-stub "rvm_list"))
     ((equal args '("ruby-1.9.2-head" "gemset" "list"))
      (get-rvm-stub "ruby-1.9.2-head_rvm_gemset_list"))
     ((and (string= command "info") (string= arg1 "ruby-1.9.2-head"))
      (get-rvm-stub "ruby-1.9.2-head_rvm_info"))
     ((and (string= command "info") (string= arg1 "ruby-1.9.2-head@rails3"))
      (get-rvm-stub "ruby-1.9.2-head@rails3_rvm_info"))
     ((and (string= command "info") (string= arg1 "ruby-1.8.7-p249@experimental"))
      (get-rvm-stub "ruby-1.8.7-p249@experimental_rvm_info"))
     ((and (string= command "info") (string= arg1 "ruby-1.8.7-p249"))
      (get-rvm-stub "ruby-1.8.7-p249_rvm_info")))))

(defun should-be-rvm-environment (ruby-binaries gemhome gempath)
  (should (equal (rvm--emacs-ruby-binary) ruby-binaries))
  (dolist (binary ruby-binaries)
    (should (string-match-p binary (getenv "PATH"))))
  (should (equal (rvm--emacs-gemhome) gemhome))
  (should (equal (rvm--emacs-gempath) gempath)))

(defun rvm-test-environment (body)
  (rvm-use-default)
  (should-be-rvm-environment
   '("/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/")
   "/Users/senny/.rvm/gems/ruby-1.9.2-head"
   "/Users/senny/.rvm/gems/ruby-1.9.2-head:/Users/senny/.rvm/gems/ruby-1.9.2-head@global")
  (funcall body))

(ert-deftest rvm-test-rvm/info ()
  (let* ((result (rvm/info "ruby-1.8.7-p249"))
         (version (cdr (assoc "version" result)))
         (gem_home (cdr (assoc "GEM_HOME" result)))
         (gem_path (cdr (assoc "GEM_PATH" result)))
         (gemset (cdr (assoc "gemset" result)))
         (binary (cdr (assoc "ruby" result))))
    (should (string= version "1.8.7"))
    (should (string= gem_home "/Users/senny/.rvm/gems/ruby-1.8.7-p249"))
    (should (string= gem_path "/Users/senny/.rvm/gems/ruby-1.8.7-p249:/Users/senny/.rvm/gems/ruby-1.8.7-p249@global"))
    (should (string= gemset nil))
    (should (string= binary "/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/ruby"))))

(ert-deftest rvm-test-rvm/list ()
  (let* ((result (rvm/list)))
    (should (equal result
                   '("ruby-1.9.2-head" "ruby-1.8.7-p249" "ruby-1.9.1-p378" "ruby-1.9.2-preview1" "ruby-head")))))

(ert-deftest rvm-test-rvm/gemset-list ()
  (let* ((result (rvm/gemset-list "ruby-1.9.2-head")))
    (should (equal result '("experimental" "global" "rails3" "rails3-beta4" "rails3beta")))))

(ert-deftest rvm-test-rvm-use ()
  (rvm-test-environment (lambda ()
                          (rvm-use "ruby-1.8.7-p249" "experimental")
                          (should-be-rvm-environment
                           '("/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/")
                           "/Users/senny/.rvm/gems/ruby-1.8.7-p249@experimental"
                           "/Users/senny/.rvm/gems/ruby-1.8.7-p249@experimental:/Users/senny/.rvm/gems/ruby-1.8.7-p249@global")
                          )))

(ert-deftest rvm-test-activate-corresponding-ruby ()
  (rvm-test-environment (lambda ()
                          (cd (file-name-directory (symbol-file 'get-rvm-stub)))
                          (rvm-activate-corresponding-ruby)
                          (should-be-rvm-environment
                           '("/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/")
                           "/Users/senny/.rvm/gems/ruby-1.9.2-head@rails3"
                           "/Users/senny/.rvm/gems/ruby-1.9.2-head@rails3:/Users/senny/.rvm/gems/ruby-1.9.2-head@global")
                          )))
