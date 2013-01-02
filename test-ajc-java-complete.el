(load "./ajc-java-complete.el")
(require 'ert)
(require 'cl)

(ert-deftest test-ajc-split-pkg-item ()
  (should (equal '("java.lang" 222 333) (ajc-split-pkg-item "java.lang`222`333"))))

(ert-deftest test-ajc-split-class-item ()
  (should (equal '("SomeClass" 7 9 25) (ajc-split-class-item "SomeClass`7`9`25"))))

(ert-deftest test-ajc-split-constructor ()
  (should (equal '("SomeClass" "" "")
                 (ajc-split-constructor "  SomeClass``")))
  (should (equal '("SomeClass" ("int") "")
                 (ajc-split-constructor "  SomeClass`~int`")))
  (should (equal '("SomeClass" ("java.lang.String") "")
                 (ajc-split-constructor "  SomeClass`~java.lang.String`")))
  (should (equal '("SomeClass" ("int" "int" "int") "")
                 (ajc-split-constructor "  SomeClass`~int,~int,~int`")))
  (should (equal '("SomeClass" ("double") ("java.lang.Exception"))
                 (ajc-split-constructor "  SomeClass`~double`~java.lang.Exception"))))

(ert-deftest test-ajc-split-items ()
  (should (equal '("")
                 (ajc-split-items "")))
  (should (equal '(("int"))
                 (ajc-split-items "~int")))
  (should (equal '(("int" "int" "int"))
                 (ajc-split-items "~int,~int,~int"))))

(ert-deftest test-ajc-split-field ()
  (should (equal '("CONSTANT" "int")
                 (ajc-split-field " CONSTANT`~int"))))

(ert-deftest test-ajc-split-method ()
  (should (equal
           '("getStrField" "java.lang.String" "" "")
           (ajc-split-method "getStrField`~java.lang.String``")))
  (should
   (equal
    '("getStringArray" "java.lang.String[]" ("java.util.ArrayList") ("java.lang.Exception"))
    (ajc-split-method
     "getStringArray`~java.lang.String[]`~java.util.ArrayList`~java.lang.Exception"))))
