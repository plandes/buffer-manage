;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of buffer-manager-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'buffer-manage)

(defclass fake-manager (buffer-manager) ())

(defcustom fake-manager-singleton
  (fake-manager "singleton")
  "The singleton fake manager."
  :group 'fake
  :type 'object)

(ert-deftest test-load ()
  "Test successful evaluation of buffer-manage"
  (should (not (null fake-manager-singleton))))

(provide 'buffer-manage-test)

;;; buffer-manage-test ends here
