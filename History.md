# In Git

[Compare v1.4.2..master](https://github.com/senny/rvm.el/compare/v1.4.2...master)

# 1.4.2 (12.21.2020)

[Compare v1.4.2..v1.4.1](https://github.com/senny/rvm.el/compare/v1.4.1...v1.4.2)

## Bugfixes

* Fix cl deprecation warnings; use cl-lib instead

# 1.4.1 (10.26.2020)

[Compare v1.4.1..v1.4.0](https://github.com/senny/rvm.el/compare/v1.4.0...v1.4.1)

## New Features

* Support gemset name inside .ruby-version file (#57, thanks Étienne Deparis)

## Bugfixes

* Fix setting of BUNDLE_PATH (#59, thanks Étienne Deparis)

# 1.4.0 (04.02.2015)

[Compare v1.4.0..v1.3.0](https://github.com/senny/rvm.el/compare/v1.3.0...v1.4.0)

## New Features

*  Support for .ruby-version file
*  Support for rvm/bundler integration

## Bugfixes

* Fix warning from mapcar
* Fix naming conflict with electric-pair-mode
* Fix version parsing in .rvmrc

# 1.3.0 (02.10.2013)

[Compare v1.3.0..1.2.0](https://github.com/senny/rvm.el/compare/1.2.0...v1.3.0)

## Bugfixes

* Fix gemset list bug
* Fix gemset parsing
* Fix missing usage of rvm-configuration-file-name in rvm--rvmrc-locate

# 1.2.0 (01.02.2012)

[Compare v1.2.0..1.1](https://github.com/senny/rvm.el/compare/1.1...1.2.0)

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
* support both Gemfile syntaxes for ruby-version

# 1.1 (16.05.2010)

[Compare v1.1.0..1.0](https://github.com/senny/rvm.el/compare/1.0...1.1)

## New Features

* switch between installed ruby implementations
* use the default rvm ruby as default ruby within emacs
* rvm.el can now detect the right version to use for the current buffer (takes .rvmrc into account)
* support for Gemsets
* add Gemsets /bin directories to PATH

## Bugfixes

* Fixed setting of gemset path and ruby path
* Fixed leading blanks in gemset name (global) using chomp
