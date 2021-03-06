rvm.el
======

[![Build Status](https://secure.travis-ci.org/senny/rvm.el.svg?branch=master)](https://travis-ci.org/senny/rvm.el)

This package integrates Emacs with the Ruby Version Manager ( rvm ).

Installation
------------

Just drop rvm.el in your load-path and put the following in your .emacs

```lisp
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session
```

Usage
-----
Just call the interactive function `rvm-activate-corresponding-ruby` and rvm.el will change Emacs to use the right ruby version for the file in the current buffer. Remember that the new ruby version will be used for the whole Emacs session and not just the current file. If you want to switch to another ruby version manualy you can call `rvm-use` instead. See `rvm-activate-ruby-for` to activate for a specific path.

Please note that using system-ruby is not supported in rvm.el

RVM will by default print messages when switching environment. You can disable all messages by setting `rvm-verbose` to `nil`.

Read about rvm.el
-----------------
If you want to read what people have written about rvm.el, this section links you to the relevant blog posts:

* [Emacs Reboot #14](https://avdi.codes/rvm-el-and-inf-ruby-emacs-reboot-14/) by Avdi Grimm
* [Handle Ruby versions](http://web.archive.org/web/20161018171345/http://emacsrookie.com/2011/10/01/handle-ruby-versions/) by Samuel Tonini
* [Emacs tidbits for Ruby developers](http://web.archive.org/web/20190211162443/http://blog.senny.ch/blog/2012/10/06/emacs-tidbits-for-ruby-developers/) by Yves Senn

Community
---------
### Got a question?

Just send me a message and I'll try to get to you as soon as possible.

### Found a bug?

Please register a new issue.

### Fixed something?

1. Fork rvm.el
2. Install [Cask](http://cask.github.io/installation)
3. Install dependencies - `cask install`
4. Create a topic branch - `git checkout -b my_branch`
5. Make your changes and update the History.txt file
6. Run the tests - `make test`
7. Push to your branch - `git push origin my_branch`
8. Send me a pull-request for your topic branch
9. That's it!
