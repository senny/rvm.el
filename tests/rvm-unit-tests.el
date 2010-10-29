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

(deftest rvm-unit-test-rvmrv-parse-version ()
  (should (equal (rvm--rvmrc-parse-version "rvm a_ruby@a_gemset")
                 '("a_ruby" "a_gemset")))
  )

(deftest rvm-unit-test-rvmrv-parse-version-with-use ()
  (should (equal (rvm--rvmrc-parse-version "rvm use another_ruby@another_gemset")
                 '("another_ruby" "another_gemset")))
  )

(deftest rvm-unit-test-rvmrv-parse-version-without-gemset ()
  (should (equal (rvm--rvmrc-parse-version "rvm another_ruby")
                 '("another_ruby" "global")))
  )

(ert-run-tests-interactively "rvm-unit-.*")
