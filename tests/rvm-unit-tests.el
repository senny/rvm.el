;;; rvm-unit-tests.el --- Test Suite for rvm.el

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

(ert-deftest rvm-unit-test-rvm--string-trim ()
  (should (equal (rvm--string-trim " test123\n\n  ")
                 "test123")))

(ert-deftest rvm-unit-test-rvmrc-parse-version ()
  (should (equal (rvm--rvmrc-parse-version "rvm a_ruby@a_gemset")
                 '("a_ruby" "a_gemset"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-with-use ()
  (should (equal (rvm--rvmrc-parse-version "rvm use another_ruby@another_gemset")
                 '("another_ruby" "another_gemset"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-without-gemset ()
  (should (equal (rvm--rvmrc-parse-version "rvm another_ruby")
                 '("another_ruby" "global"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-with-single-flag ()
  (should (equal (rvm--rvmrc-parse-version "rvm --create ruby-1.8.7-p302@foo")
                 '("ruby-1.8.7-p302" "foo"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-with-multiple-single-flag ()
  (should (equal (rvm--rvmrc-parse-version "rvm --one --two --three ree-1.8.7-2010.01")
                 '("ree-1.8.7-2010.01" "global"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-with-rvm-generated-rvmrc-short ()
  (should (equal (rvm--rvmrc-parse-version "environment_id=\"ruby-1.9.2-p180-patched@something\"")
                 '("ruby-1.9.2-p180-patched" "something"))))

(ert-deftest rvm-unit-test-completing-read-all-good ()
  (let ((rvm-interactive-completion-function
         (lambda (prompt options) "global")))
    (should (equal (rvm--completing-read "Foo" "Bar") "global"))))

(ert-deftest rvm-unit-test-completing-read-all-space-in-name ()
  (let ((rvm-interactive-completion-function
         (lambda (prompt options) "   global")))
    (should (equal (rvm--completing-read "Foo" "Bar") "global"))))

(ert-deftest rvm-unit-test-rvmrc-parse-version-with-rvm-generated-rvmrc ()
  (should (equal (rvm--rvmrc-parse-version "#!/usr/bin/env bash

# This is an RVM Project .rvmrc file, used to automatically load the ruby
# development environment upon cd'ing into the directory

# First we specify our desired <ruby>[@<gemset>], the @gemset name is optional.
environment_id=\"ruby-1.9.2-p180-patched@something\"

#
# First we attempt to load the desired environment directly from the environment
# file. This is very fast and efficicent compared to running through the entire
# CLI and selector. If you want feedback on which environment was used then
# insert the word 'use' after --create as this triggers verbose mode.
#
if [[ -d \"${rvm_path:-$HOME/.rvm}/environments\" \
  && -s \"${rvm_path:-$HOME/.rvm}/environments/$environment_id\" ]]
then
  \. \"${rvm_path:-$HOME/.rvm}/environments/$environment_id\"

  if [[ -s \".rvm/hooks/after_use\" ]]
  then
    . \".rvm/hooks/after_use\"
  fi
else
  # If the environment file has not yet been created, use the RVM CLI to select.
  if ! rvm --create use  \"$environment_id\"
  then
    echo \"Failed to create RVM environment ''.\"
  fi
fi

#
# If you use an RVM gemset file to install a list of gems (*.gems), you can have
# it be automatically loaded. Uncomment the following and adjust the filename if
# necessary.
#
# filename=\".gems\"
# if [[ -s \"$filename\" ]] ; then
#   rvm gemset import \"$filename\" | grep -v already | grep -v listed | grep -v complete | sed '/^$/d'
# fi\"")
                 '("ruby-1.9.2-p180-patched" "something"))))
