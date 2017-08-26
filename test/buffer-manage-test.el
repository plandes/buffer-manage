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

(cl-defmethod config-manager-entry-default-name ((this fake-manager))
  "fake")

(defcustom fake-manager-singleton
  (fake-manager "singleton")
  "The singleton fake manager."
  :group 'fake
  :type 'object)

(ert-deftest test-load ()
  "Test successful evaluation of buffer-manage"
  (should (not (null fake-manager-singleton))))

(ert-deftest test-iterate-name ()
  (should (equal "name"
		 (config-manager-iterate-name "name" nil)))
  (should (equal "name"
		 (config-manager-iterate-name "name" '("xname"))))
  (should (equal "name"
		 (config-manager-iterate-name "name" '("namex"))))
  (should (equal "name"
		 (config-manager-iterate-name "name" '("other"))))
  (should (equal "name<2>"
		 (config-manager-iterate-name "name" '("name"))))
  (should (equal "name"
		 (config-manager-iterate-name "name" '("name<2>"))))
  (should (equal "name<3>"
		 (config-manager-iterate-name "name" '("name" "name<2>"))))
  (should (equal "name"
		 (config-manager-iterate-name "name" '("name<2>" "name<3>"))))
  (should (equal "name<4>"
		 (config-manager-iterate-name
		  "name" '("name" "name<2>" "name<3>"))))
  (should (equal "name<3>"
		 (config-manager-iterate-name
		  "name" '("name" "name<2>" "name<4>"))))
  (should (equal "name<3>"
		 (config-manager-iterate-name
		  "name" '("name" "name<4>" "name<2>"))))
  (should (equal "name<5>"
		 (config-manager-iterate-name
		  "name" '("name" "name<4>" "name<2>" "name<3>")))))

(provide 'buffer-manage-test)

;;; buffer-manage-test ends here
