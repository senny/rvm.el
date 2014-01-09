(require 'f)

(defvar rvm-test/test-path
  (f-parent (f-this-file)))

(defvar rvm-test/root-path
  (f-parent rvm-test/test-path))

(setq rvm-verbose nil)

(eval-when-compile
  (require 'cl))

(unless (require 'ert nil 'no-error)
  (require 'ert (f-expand "rvm" rvm-test/test-path)))

(require 'rvm (f-expand "rvm" rvm-test/root-path))
