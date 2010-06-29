;;; rvm-tests.el --- Test Suite for rvm.el

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

(defun rvm--call-process (&rest args)
  (let ((command (first args))
        (arg1 (second args)))
    (cond
     ((and (string= command "list") (string= arg1 t))
      "ruby-1.9.2-head")
     ((string= command "list")
      "rvm Rubies

   ruby-1.8.7-p249 [ x86_64 ]
   ruby-1.9.1-p378 [ x86_64 ]
=> ruby-1.9.2-head [ x86_64 ]
   ruby-1.9.2-preview1 [ i386 ]
   ruby-head [ x86_64 ]

Default Ruby (for new shells)

   ruby-1.9.2-head [ x86_64 ]

System Ruby

   system [ x86_64 i386 ppc ]")
     ((and (string= command "gemset list") (string= arg1 "ruby-1.9.2-head"))
      "experimental
global
rails3
rails3-beta4
rails3beta")
     ((and (string= command "info") (string= arg1 "ruby-1.9.2-head"))
      "
system:
  uname:        \"Darwin ymac.local 10.4.0 Darwin Kernel Version 10.4.0: Fri Apr 23 18:28:53 PDT 2010; root:xnu-1504.7.4~1/RELEASE_I386 i386\"
  shell:        \"zsh\"
  version:     \"4.3.9\"

rvm:
  type:         \"rvm is a shell function\"
  version:      \"rvm 0.1.27 by Wayne E. Seguin (wayneeseguin@gmail.com) [http://rvm.beginrescueend.com/]\"

ruby:
  interpreter:  \"ruby\"
  version:      \"1.9.2dev\"
  date:         \"2010-03-22\"
  platform:     \"x86_64-darwin10.2.0\"
  patchlevel:   \"2010-03-22 trunk 27005\"
  full_version: \"ruby 1.9.2dev (2010-03-22 trunk 27005) [x86_64-darwin10.2.0]\"

homes:
  gem:          \"/Users/senny/.rvm/gems/ruby-1.9.2-head\"
  ruby:         \"/Users/senny/.rvm/rubies/ruby-1.9.2-head\"

binaries:
  ruby:         \"/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/ruby\"
  irb:          \"/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/irb\"
  gem:          \"/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/gem\"
  rake:         \"/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/rake\"

environment:
  GEM_HOME:     \"/Users/senny/.rvm/gems/ruby-1.9.2-head\"
  GEM_PATH:     \"/Users/senny/.rvm/gems/ruby-1.9.2-head:/Users/senny/.rvm/gems/ruby-1.9.2-head@global\"
  BUNDLE_PATH:  \"/Users/senny/.rvm/gems/ruby-1.9.2-head\"
  MY_RUBY_HOME: \"/Users/senny/.rvm/rubies/ruby-1.9.2-head\"
  IRBRC:        \"/Users/senny/.rvm/rubies/ruby-1.9.2-head/.irbrc\"
  gemset:       \"\"

")
     ((and (string= command "info") (string= arg1 "ruby-1.8.7-p249"))
      "system:
  uname:        \"Darwin ymac.local 10.4.0 Darwin Kernel Version 10.4.0: Fri Apr 23 18:28:53 PDT 2010; root:xnu-1504.7.4~1/RELEASE_I386 i386\"
  shell:        \"zsh\"
  version:     \"4.3.9\"

rvm:
  type:         \"rvm is a shell function\"
  version:      \"rvm 0.1.27 by Wayne E. Seguin (wayneeseguin@gmail.com) [http://rvm.beginrescueend.com/]\"

ruby:
  interpreter:  \"ruby\"
  version:      \"1.8.7\"
  date:         \"2010-01-10\"
  platform:     \"i686-darwin10.2.0\"
  patchlevel:   \"2010-01-10 patchlevel 249\"
  full_version: \"ruby 1.8.7 (2010-01-10 patchlevel 249) [i686-darwin10.2.0]\"

homes:
  gem:          \"/Users/senny/.rvm/gems/ruby-1.8.7-p249\"
  ruby:         \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249\"

binaries:
  ruby:         \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/ruby\"
  irb:          \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/irb\"
  gem:          \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/gem\"
  rake:         \"/Users/senny/.rvm/gems/ruby-1.8.7-p249@global/bin/rake\"

environment:
  GEM_HOME:     \"/Users/senny/.rvm/gems/ruby-1.8.7-p249\"
  GEM_PATH:     \"/Users/senny/.rvm/gems/ruby-1.8.7-p249:/Users/senny/.rvm/gems/ruby-1.8.7-p249@global\"
  BUNDLE_PATH:  \"/Users/senny/.rvm/gems/ruby-1.8.7-p249\"
  MY_RUBY_HOME: \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249\"
  IRBRC:        \"/Users/senny/.rvm/rubies/ruby-1.8.7-p249/.irbrc\"
  gemset:       \"\""))))

(defun should-be-rvm-environment (ruby-binary gemhome gempath)
  (should (equal (rvm--emacs-ruby-binary) ruby-binary))
  (should (string-match-p ruby-binary (getenv "PATH")))
  (should (equal (rvm--emacs-gemhome) gemhome))
  (should (equal (rvm--emacs-gempath) gempath)))

(defun rvm-test-environment (body)
  (rvm-use-default)
  (should-be-rvm-environment "/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/" "/Users/senny/.rvm/gems/ruby-1.9.2-head" "/Users/senny/.rvm/gems/ruby-1.9.2-head:/Users/senny/.rvm/gems/ruby-1.9.2-head@global")
  (funcall body))

(deftest rvm-test-info ()
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

(deftest rvm-test-list ()
  (let* ((result (rvm/list)))
    (should (equal result
                   '("ruby-1.9.2-head" "ruby-1.8.7-p249" "ruby-1.9.1-p378" "ruby-1.9.2-preview1" "ruby-head" "system")))))

(deftest rvm-test-gemset-list ()
  (let* ((result (rvm/gemset-list "ruby-1.9.2-head")))
    (should (equal result '("*default*" "global" "rails3" "rails3-beta4" "rails3beta")))))

(deftest rvm-test-rvmrc-read-version ()
  (should (equal (rvm--rvmrc-read-version ".rvmrc") '("ruby-1.9.2-head" "rails3"))))

(deftest rvm-test-rvm-use ()
  (rvm-test-environment (lambda ()
                          (rvm-use "ruby-1.8.7-p249" "experimental")
                          (should-be-rvm-environment "/Users/senny/.rvm/rubies/ruby-1.8.7-p249/bin/" "/Users/senny/.rvm/gems/ruby-1.8.7-p249@experimental" "/Users/senny/.rvm/gems/ruby-1.8.7-p249@experimental:/Users/senny/.rvm/gems/ruby-1.8.7-p249@global")
                          )))

(deftest rvm-test-activate-corresponding-ruby ()
  (rvm-test-environment (lambda ()
                          (rvm-activate-corresponding-ruby)
                          (should-be-rvm-environment "/Users/senny/.rvm/rubies/ruby-1.9.2-head/bin/" "/Users/senny/.rvm/gems/ruby-1.9.2-head@rails3" "/Users/senny/.rvm/gems/ruby-1.9.2-head@rails3:/Users/senny/.rvm/gems/ruby-1.9.2-head@global")
                          )))

(ert-run-tests-interactively "rvm-.*")
