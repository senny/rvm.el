# In Git

[Compare v1.2.0..master](https://github.com/senny/rvm.el/compare/1.2.0...master)

## New Features

## Bugfixes

# 1.2.0 (01.02.2012)

[Compare v1.2.0..master](https://github.com/senny/rvm.el/compare/1.1...1.2.0)

## New Features

* open sources in a gem from the current gemset
* ability to change the used completion function
* rvm-open-gem creates a new perspective when perspective.el is loaded
* Install gems from Emacs with rvm-gem-install

## Bugfixes

* Fixed weird ascii character problem in example
* rvm now works again with the global gemset
* rvm-open-gem should now work with the standard ido distribution
* the exec-path is now set properly
* gem-home and gem-path are now set using `rvm info` instead of building the paths manually
* bug-fix in rvm--rvmrc-parse-version which was incorrectly reading some rvmrc files

# 1.1 (16.05.2010)

[Compare v1.1.0..master](https://github.com/senny/rvm.el/compare/1.0...1.1)

## New Features

* switch between installed ruby implementations
* use the default rvm ruby as default ruby within emacs
* rvm.el can now detect the right version to use for the current buffer (takes .rvmrc into account)
* support for Gemsets
* add Gemsets /bin directories to PATH

## Bugfixes

* Fixed setting of gemset path and ruby path
* Fixed leading blanks in gemset name (global) using chomp
