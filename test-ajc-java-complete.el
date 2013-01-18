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

(ert-deftest test-ajc-get-validated-stack-list-or-nil-4-method-complete ()
  (should (equal
           '("a" ".")
           (ajc-get-validated-stack-list-or-nil-4-method-complete '("a" "."))))
  (should (equal
           '("System" "." "out" ".")
           (ajc-get-validated-stack-list-or-nil-4-method-complete
            '("System" "." "out" "."))))
  (should (equal nil
                 (ajc-get-validated-stack-list-or-nil-4-method-complete '())))
  (should (equal nil
                 (ajc-get-validated-stack-list-or-nil-4-method-complete '("a" "." "-"))))))

(ert-deftest test-ajc-complete-method-candidates-1 ()
  (should
   (equal '("equals(Object)" "exit(int)" "err")
          (ajc-complete-method-candidates-1 '("System" "." "e")))))

(ert-deftest test-ajc-find-class-first-check-imported ()
  (should
   (equal '("System" 28 24120 24159)
          (ajc-find-class-first-check-imported "System"))))

(ert-deftest test-ajc-split-line-4-complete-method ()
  (should
   (equal
    '("System" "." "getProperty" "(" "str" "." "substring" "(" "3" ")" ")" "."
      "to")
    (ajc-split-line-4-complete-method "System.getProperty(str.substring(3)).to")))
  (should
   (equal '("Obj" "(" ")" "." "r")
          (ajc-split-line-4-complete-method "new Obj().r"))))

(ert-deftest test-ajc-split-string-with-separator ()
  (should
   (equal '("abc" "." "def" "." "g")
          (ajc-split-string-with-separator "abc.def.g" "\\." "."))))

(ert-deftest test-ajc-complete-method-is-available ()
  (should
   (equal t
          (ajc-complete-method-is-available "System.getProperty(str.substring(3)).to")))
  (should
   (equal nil
          (let ((ajc-complete-method-candidates-cache-stack-list
                 '("System" "." "getProperty" "." "t"))
                (ajc-complete-method-candidates-cache nil))
            (ajc-complete-method-is-available "System.getProperty(str.substring(3)).to")))))

(ert-deftest test-ajc-calculate-all-imported-class-items ()
  (should
   (equal '("File" "Test" "Vector")
          (sort (mapcar #'car (with-temp-buffer
                                (insert "import java.io.File;")
                                (newline)
                                (insert "import java.util.Vector;")
                                (newline)
                                (insert "import org.junit.Test;")
                                (insert "class ClassName {")
                                (newline)
                                (ajc-calculate-all-imported-class-items t)))
                #'string<)))
  ;; todo add testcase for import java.util.*; statement.
  )

(ert-deftest test-ajc-find-out-matched-class-item-without-package-prefix ()
  (should
   (equal '("StringBufferInputStream" "StringBuffer" "StringBuilder")
          (mapcar #'car
                  (ajc-find-out-matched-class-item-without-package-prefix "StringB"))))
  (should
   (equal '"Vector"
          (caar (ajc-find-out-matched-class-item-without-package-prefix "Vector")))))

(ert-deftest test-ajc-fqn-candidates-1 ()
  (should
   (equal "PI"
          (caar (ajc-fqn-candidates-1 "java.lang.Math.P"))))
  (should
   (null (ajc-fqn-candidates-1 "foo.bar.b"))))

(ert-deftest test-ajc-package-candidates ()
  (should
   (equal '("java.util.Vector")
          (ajc-package-candidates "java.util.V")))
  (should
   (equal '("java.lang.Math")
          (ajc-package-candidates "java.lang.M")))
  (should
   (null (ajc-package-candidates "foo.bar."))))
