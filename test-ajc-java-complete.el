(load "./ajc-java-complete.el")
(require 'ert)
(require 'cl)

(defvar test-ajc-someclass-tagfile "test/someclass.tag")
(defvar test-ajc-junit-tagfile "test/junit.ajctag")
(defvar test-ajc-javart-tagifle "test/javart.tag")

(defun test-ajc-unpropertize-text (text prop-lst)
  "Return unpropertized text."
  (remove-list-of-text-properties 0
                                  (length text)
                                  prop-lst
                                  text)
  text)

(defun test-ajc-fixture (tag-file-list body)
  (let ((ajc-is-running nil)
        (ajc-tag-file-list (mapcar (lambda (name)
                                     (file-truename
                                      (expand-file-name name)))
                                   tag-file-list))
        (ajc-tag-buffer-list nil)
        (ajc-lines-and-positions-list nil)
        (ajc-package-in-tags-cache-tbl nil)
        (ajc-two-char-tbl nil)
        (ajc-previous-class-prefix nil)
        (ajc-matched-class-items-cache nil)
        (ajc-sorted-class-buffer-name-list nil)
        (ajc-complete-method-candidates-cache-stack-list nil)
        (ajc-plain-method-tables nil)
        (ajc-method-table-cache-dir "./test/cache"))
    (unwind-protect
        (progn
          (ajc-init t)
          (funcall body))
      (mapcar #'kill-buffer ajc-tag-buffer-list))))

(ert-deftest test-ajc-split-pkg-item ()
  (should (equal '("java.lang" 222 333) (ajc-split-pkg-item "java.lang`222`333"))))

(ert-deftest test-ajc-split-class-item ()
  (should (equal '("SomeClass" 7 9 25) (ajc-split-class-item "SomeClass`7`9`25"))))

(ert-deftest test-ajc-split-constructor ()
  (should (equal '("SomeClass" "" "")
                 (ajc-split-constructor "  SomeClass``" 0)))
  (should (equal '("SomeClass" ("int") "")
                 (ajc-split-constructor "  SomeClass`~int`" 0)))
  (should (equal '("SomeClass" ("java.lang.String") "")
                 (ajc-split-constructor "  SomeClass`~java.lang.String`" 0)))
  (should (equal '("SomeClass" ("int" "int" "int") "")
                 (ajc-split-constructor "  SomeClass`~int,~int,~int`" 0)))
  (should (equal '("SomeClass" ("double") ("java.lang.Exception"))
                 (ajc-split-constructor "  SomeClass`~double`~java.lang.Exception" 0)))
    ;; TODO add some tests which use INDEX argment.
  )

(ert-deftest test-ajc-split-items ()
  (should (equal '("")
                 (ajc-split-items "" 0)))
  (should (equal '(("int"))
                 (ajc-split-items "~int" 0)))
  (should (equal '(("int" "int" "int"))
                 (ajc-split-items "~int,~int,~int" 0)))
  ;; TODO add some tests which use INDEX argment.
  )

(ert-deftest test-ajc-split-field ()
  (should (equal '("CONSTANT" "int")
                 (ajc-split-field " CONSTANT`~int" 0))))

(ert-deftest test-ajc-split-method ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should (equal
              '("getStrField" "java.lang.String" "" "")
              (ajc-split-method "getStrField`~java.lang.String``" 0)))
     (should
      (equal
       '("getStringArray" "java.lang.String[]" ("java.util.ArrayList") ("java.lang.Exception"))
       (ajc-split-method
        "getStringArray`~java.lang.String[]`~java.util.ArrayList`~java.lang.Exception" 0)))
     (should
      (equal
       '("getSomeClassObj" ("SomeClass" 0 7 21 39) "" "")
       (ajc-split-method "getSomeClassObj`9``" 0))))))

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
                 (ajc-get-validated-stack-list-or-nil-4-method-complete '("a" "." "-")))))

(ert-deftest test-ajc-complete-method-candidates-1 ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("getStrField()" "getStringArray(java.util.ArrayList)")
             (ajc-complete-method-candidates-1 '("SomeClass" "." "getStr")))))))

(ert-deftest test-ajc-find-class-first-check-imported ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("SomeClass" 0 7 21 39)
             (ajc-find-class-first-check-imported "SomeClass"))))))

(ert-deftest test-ajc-split-line-4-complete-method ()
  (should
   (equal
    '("System" "." "getProperty" "(" "str" "." "substring" "(" "3" ")" ")" "."
      "to")
    (ajc-split-line-4-complete-method "System.getProperty(str.substring(3)).to")))
  (should
   (equal '("Obj" "(" ")" "." "r")
          (ajc-split-line-4-complete-method "new Obj().r")))
  (should
   (equal '("answer" ".")
          (ajc-split-line-4-complete-method "if (answer.equals(\"Y\") || answer.")))
  (should
   (equal '("(" "answer" ".")
          (ajc-split-line-4-complete-method
           "if (answer.euqals(\"Y\")) && (answer.equals(\"N\")) || (answer.")))
  (should
   (equal '("obj" ".")
          (ajc-split-line-4-complete-method
           "++obj.")))
  (should
   (equal
    ;; '("ByteArrayInputStream" "(" "(" "String" "+" "System" "."
    ;;   "getProperty" "(" "String" ")" ")" ".")
    '("(" "String" "+" "System" "." "getProperty" "(" "String" ")" ")" ".")
    (ajc-split-line-4-complete-method
     "new ByteArrayInputStream((\"y\" + System.getProperty(\"line.separator\")).")))
  (should
   (equal '("StringBuffer" "(" ")" ".")
          (ajc-split-line-4-complete-method "new StringBuffer().")))
  (should
   (equal '("Obj" "(" "String" ")" ".")
          (ajc-split-line-4-complete-method "new Obj(\"arg\").")))
  (should
   (equal '("(" "a" "+" "b" ")" ".")
          (ajc-split-line-4-complete-method "new Obj((a + b).")))
  (should (equal
           '("Obj" "(" "(" "a" "+" "b" ")" "." "method" "(" ")" ")" ".")
           (ajc-split-line-4-complete-method "new Obj((a + b).method()).")))
  (should
   (equal '("File" "(" "String" ")" ".")
          (ajc-split-line-4-complete-method
           "new FileInputStream(new File(\"file\").")))
  (should
   (equal '("ObjC" "(" ")" ".")
          (ajc-split-line-4-complete-method
           "new ObjA(new ObjB(new ObjC().")))
  (should
   (equal '("(" "String" "+" "String" ")" ".")
          (ajc-split-line-4-complete-method
           "(\"a\" + \"b\").")))
  (should
   (equal '("String" "." "trim" "(" ")" ".")
          (ajc-split-line-4-complete-method
           "\"foo\".trim().")))
  (should
   (equal '("message" "." "getBody" "(" ")" ".")
          (ajc-split-line-4-complete-method
           "    for (String element : message.getBody().")))
  )

(ert-deftest test-ajc-parse-splited-line-4-complete-method ()
  (should
   (equal '("System" "." "getProperty")
          (ajc-parse-splited-line-4-complete-method
           "System.getProperty(str.substring(3))")))
  (should
   (equal nil
          (ajc-parse-splited-line-4-complete-method
           "new Obj((a + b).")))
  (should
   (equal '("File" ".")
          (ajc-parse-splited-line-4-complete-method
           "new File(\"file\").")))
  (should
   (equal '("File" ".")
          (ajc-parse-splited-line-4-complete-method
           "new FileInputStream(new File(\"file\").")))
  (should
   (equal '("ObjC" ".")
          (ajc-parse-splited-line-4-complete-method
           "new ObjA(new ObjB(new ObjC().")))
  (should
   (equal '("String" ".")
          (ajc-parse-splited-line-4-complete-method
           "(\"a\" + \"b\").")))
  (should
   (equal '("String" ".")
          (ajc-parse-splited-line-4-complete-method
           "(\"a\" + (\"b\" + \"c\").")))
  (should
   (equal '("String" "." "trim" ".")
          (ajc-parse-splited-line-4-complete-method
           "\"foo\".trim().")))
  (should
   (equal '("String" ".")
          (ajc-parse-splited-line-4-complete-method
           "(\"str\").")))
  )

(ert-deftest test-ajc-bug-in-ajc-parse-splited-line-4-complete-method ()
  :expected-result :failed
  (should
   (equal '("_sniperState" ".")
          (ajc-parse-splited-line-4-complete-method
           "      then (_sniperState)."))))

(ert-deftest test-ajc-extract-parenthesized-part-maybe ()
  (should
   (equal '("a" "+" "b")
          (ajc-extract-parenthesized-part-maybe '("a" "+" "b"))))
  (should
   (equal '("(" "a" "+" "b" ")" ".")
          (ajc-extract-parenthesized-part-maybe
           '("new" "Obj" "(" "(" "a" "+" "b" ")" "."))))
  (should
   (equal '("StringBuffer" "(" ")" ".")
          (ajc-extract-parenthesized-part-maybe '("StringBuffer" "(" ")" "."))))
  (should
   (equal '("File" "(" "String" ")" ".")
          (ajc-extract-parenthesized-part-maybe
           '("FileInputStream" "(" "File" "(" "String" ")" "."))))
  (should
   (equal '("ObjC" "(" ")" ".")
          (ajc-extract-parenthesized-part-maybe
           '("ObjA" "(" "ObjB" "(" "ObjC" "(" ")" "."))))
  (should
   (equal '("String" "." "trim" "(" ")" ".")
          (ajc-extract-parenthesized-part-maybe
           '("String" "." "trim" "(" ")" "."))))
  )

(ert-deftest test-ajc-remove-unnecessary-heading-part ()
  (should
   (equal '("(" "answer" ".")
          (ajc-remove-unnecessary-heading-part
           '("if" "(" "answer" "." "euqals" "(" "String" ")" ")" "&&"
             "(" "answer" "." "equals" "(" "String" ")" ")" "||"
             "(" "answer" "."))))
  (should
   (equal '("obj" ".")
          (ajc-remove-unnecessary-heading-part
           '("+" "obj" "."))))
  (should
   (equal '("(" "a" "+" "b" ")" ".")
          (ajc-remove-unnecessary-heading-part
           '("(" "a" "+" "b" ")" "."))))
  )

(ert-deftest test-ajc-guess-type-of-factor ()
  (should
   (equal "String"
          (ajc-guess-type-of-factor
           '("(" "String" "+" "String" ")" "."))))
  (should
   (equal "String"
          (ajc-guess-type-of-factor
           '("(" "String" "+" "(" "String" "+" "String" ")" ")" "."))))
  ;; (variable + variable).
  (should
   (equal nil
          (ajc-guess-type-of-factor
           '("(" "var1" "+" "var2" ")" "."))))
  (should
   (equal "String"
          (ajc-guess-type-of-factor
           '("(" "String" ")" "."))))
  (should
   (equal "varname"
          (ajc-guess-type-of-factor
           '("(" "varname" ")" "."))))
  )

(ert-deftest test-ajc-find-out-type-of-factors ()
  (should
   (equal "String"
          (ajc-find-out-type-of-factors
           '("String" "String"))))
  (should
   (equal "String"
          (ajc-find-out-type-of-factors
           '("String" "System.getProperty(String)"))))
  (should
   (equal "String"
          (ajc-find-out-type-of-factors
           '("System.getProperty(String)" "String"))))
  (should
   (equal "String"
          (ajc-find-out-type-of-factors
           '("(" "String" ")")))))

(ert-deftest test-ajc-find-out-type-of-factor ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal "String"
             (ajc-find-out-type-of-factor
              "String")))
     (should
      (equal "java.lang.String"
             (ajc-find-out-type-of-factor
              "SomeClass.getStrField")))
     (should
      (equal "varname"
             (ajc-find-out-type-of-factor
              "varname")))
     (should
      (equal "int"
             (ajc-find-out-type-of-factor
              "SomeClass.CONSTANT")))
     )))

(ert-deftest test-ajc-split-and-concat-list-by-operators ()
  (should
   (equal '("String")
          (ajc-split-and-concat-list-by-operators
           '("(" "String" ")" "."))))
  (should
   (equal '("varname")
          (ajc-split-and-concat-list-by-operators
           '("(" "varname" ")" "."))))
  (should
   (equal '("String" "String")
          (ajc-split-and-concat-list-by-operators
           '("(" "String" "+" "String" ")" "."))))
  (should
   ;; "(String + System.getProperty(String))."
   (equal '("String" "System.getProperty")
          (ajc-split-and-concat-list-by-operators
           '("(" "String" "+" "System" "." "getProperty" ")" "."))))
  (should
   (equal '("String" "String" "String")
          (ajc-split-and-concat-list-by-operators
           '("(" "String" "+" "(" "String" "+" "String" ")" ")" "."))))
  )

(ert-deftest test-ajc-split-and-concat-list-by-operators-1 ()
  (should
   (equal '("String" "String")
          (ajc-split-and-concat-list-by-operators-1
           '("(" "String" "+" "String" ")")
           nil)))
  (should
   (equal '("String" "System.getProperty")
          (ajc-split-and-concat-list-by-operators-1
           '("(" "String" "+" "System" "." "getProperty" ")")
           nil)))
  (should
   (equal '("String" "String" "String")
          (ajc-split-and-concat-list-by-operators-1
           '("(" "String" "+" "(" "String" "+" "String" ")" ")")
           nil)))
  (should
   (equal '("String" "String" "String")
          (ajc-split-and-concat-list-by-operators-1
           '("(" "(" "String" "+" "String" ")" "+" "String" ")")
           nil)))
  (should
   (equal '("String")
          (ajc-split-and-concat-list-by-operators-1
           '("(" "String" ")")
           nil)))
  )

(ert-deftest test-ajc-split-string-with-separator ()
  (should
   (equal '("abc" "." "def" "." "g")
          (ajc-split-string-with-separator "abc.def.g" "\\." "."))))

(ert-deftest test-ajc-complete-method-is-available ()
  (let ((ajc-complete-method-candidates-cache nil)
        (ajc-complete-method-candidates-cache-stack-list nil))
    (should
     (equal t
            (ajc-complete-method-is-available "System.getProperty(str.substring(3)).to")))
    (should
     (equal nil
            (let ((ajc-complete-method-candidates-cache-stack-list
                   '("System" "." "getProperty" "." "t"))
                  (ajc-complete-method-candidates-cache nil))
              (ajc-complete-method-is-available "System.getProperty(str.substring(3)).to"))))))

(ert-deftest test-ajc-calculate-all-imported-class-items ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("Parameterized" "Test")
             (sort (mapcar #'car (with-temp-buffer
                                   (insert "import org.junit.runners.Parameterized;")
                                   (newline)
                                   (insert "import org.junit.Test;")
                                   (insert "class ClassName {")
                                   (newline)
                                   (ajc-calculate-all-imported-class-items t)))
                   #'string<)))
     ;; todo add testcase for import java.util.*; statement.
  )))

(ert-deftest test-ajc-find-out-matched-class-item-without-package-prefix ()
  (should
   (equal '("StringBufferInputStream" "StringBuffer" "StringBuilder")
          (mapcar #'car
                  (ajc-find-out-matched-class-item-without-package-prefix "StringB"))))
  (should
   (equal '"Vector"
          (caar (ajc-find-out-matched-class-item-without-package-prefix "Vector")))))

(ert-deftest test-ajc-fqn-candidates-1 ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal "CONSTANT"
             (caar (ajc-fqn-candidates-1 "ajc.somepackage.SomeClass.CON"))))
     (should
      (null (ajc-fqn-candidates-1 "foo.bar.b")))))
  (test-ajc-fixture
   `(,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("org.junit.Test")
             (or (ajc-package-candidates "org.junit.T")
                 (caar (ajc-fqn-candidates-1 "org.junit.T"))))))))

(ert-deftest test-ajc-package-candidates ()
  (should
   (equal '("java.util.Vector")
          (ajc-package-candidates "java.util.V")))
  (should
   (equal '("java.lang.Math")
          (ajc-package-candidates "java.lang.M")))
  (should
   (null (ajc-package-candidates "foo.bar."))))

(ert-deftest test-ajc-complete-class-with-cache ()
  (should
   (string= "ArrayList" (caar (ajc-complete-class-with-cache "ArrayLi")))))

(ert-deftest test-ajc-create-two-char-item ()
  (let ((b (get-buffer-create "*test-ajc-build-map-4-search-class*"))
        (tag-buffer (find-file-noselect "test/someclass.tag")))
    (with-current-buffer b
      (erase-buffer)
      (goto-char (point-min))
      (insert-buffer-substring-no-properties tag-buffer
                                             159
                                             198)
      (sort-lines nil 1 (point-max))
      (goto-char (point-min))
      (should
       (equal '(0 1 21)
              (ajc-create-two-char-item "An" (buffer-name b) 1 0))))))

(ert-deftest test-ajc-sort-class ()
  (let* ((bname " *someclass.tag*")
         (lst (list (with-current-buffer
                       (find-file-noselect test-ajc-someclass-tagfile)
                     (rename-buffer bname)
                     (current-buffer))))
        (ajc-lines-and-positions-list
         (list (ajc-get-lines-and-positions (get-buffer bname))))
        (tbl nil)
        (ajc-sorted-class-buffer-name-list nil))
    (setq tbl (ajc-sort-class lst))
    (should (not (null (gethash "So" tbl))))
    (should (member " *sorted-class-someclass.tag*"
                    ajc-sorted-class-buffer-name-list))
    (mapcar #'kill-buffer ajc-sorted-class-buffer-name-list)
    (kill-buffer bname)))

(ert-deftest test-ajc-sort-class-1 ()
  (let* ((class-buffer-name "*test-ajc-build-map-4-search-class*")
         (tag-buffer (find-file-noselect test-ajc-someclass-tagfile))
         (tbl (make-hash-table :test #'equal))
         (ajc-lines-and-positions-list (list (ajc-get-lines-and-positions tag-buffer))))
    (ajc-sort-class-1 0 class-buffer-name tag-buffer tbl)
    (loop for k being the hash-keys in tbl
          do (message "%s => %s" k (gethash k tbl)))
    (should (not (null (gethash "So" tbl))))
    ))

(ert-deftest test-ajc-get-package-line-and-position ()
  (let ((cell (ajc-get-package-line-and-position
               (find-file-noselect test-ajc-someclass-tagfile))))
    (should (= 7 (car cell)))
    (should (= 138 (cdr cell)))))

(ert-deftest test-ajc-get-class-line-and-position ()
  (let ((cell (ajc-get-class-line-and-position
               (find-file-noselect test-ajc-someclass-tagfile))))
    (should (= 8 (car cell)))
    (should (= 159 (cdr cell)))))

(ert-deftest test-ajc-get-member-lines-and-positions ()
  (let ((lst (ajc-get-member-lines-and-positions
              (find-file-noselect "test/someclass.tag"))))
    (should (= 10 (caar lst)))
    (should (= 198 (cdar lst)))
    (should (= 39 (cadr lst)))
    (should (= 1090 (cddr lst)))))

(ert-deftest test-ajc-get-class-part ()
  (let ((ajc-lines-and-positions-list
         (list (ajc-get-lines-and-positions (get-buffer "someclass.tag")))))
    (string=
     "AnotherClass`7`10`21\nSomeClass`7`21`39"
     (ajc-get-class-part 0 (get-buffer "someclass.tag")))))

(ert-deftest test-ajc-get-lines-and-positions ()
  (let ((lst (list (ajc-get-lines-and-positions (get-buffer "someclass.tag")))))
    (should (= 7
               (ajc-get-package-first-line 0 lst)))
    (should (= 138
               (ajc-get-package-first-line-position 0 lst)))
    (should (= 8
               (ajc-get-class-first-line 0 lst)))
    (should (= 159
               (ajc-get-class-first-line-position 0 lst)))
    (should (= 10
               (ajc-get-member-first-line 0 lst)))
    (should (= 198
               (ajc-get-member-first-line-position 0 lst)))
    (should (= 39
               (ajc-get-member-end-line 0 lst)))
    (should (= 1090
               (ajc-get-member-end-line-position 0 lst)))))

(ert-deftest test-ajc-init ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should t)
     (message "ajc-sorted-class-buffer-name-list=%s"
              ajc-sorted-class-buffer-name-list))))

(ert-deftest test-ajc-get-position-by-line ()
  (should (string= " *someclass.tag*"
                   (ajc-gen-tag-buffer-name test-ajc-someclass-tagfile))))

(ert-deftest test-ajc-build-package-in-tags-cache-tbl ()
  (let* ((tbl nil)
         (buf (find-file-noselect test-ajc-someclass-tagfile))
         (ajc-lines-and-positions-list
          (list (ajc-get-lines-and-positions buf))))
    (setq tbl (ajc-build-package-in-tags-cache-tbl (list buf)))
    (should (gethash "ajc.somepackage" tbl nil))))

(ert-deftest test-ajc-make-package-item ()
  (should
   (equal '("ajc.somepackage" 0 8 10)
          (ajc-make-package-item "ajc.somepackage`8`10" 0))))

(ert-deftest test-ajc-find-out-matched-pkg-item ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should (equal '(("ajc.somepackage" 0 8 10))
                    (ajc-find-out-matched-pkg-item "ajc")))
     (should (equal '(("ajc.somepackage" 0 8 10))
                    (ajc-find-out-matched-pkg-item "ajc.somepackage" t)))))
  (test-ajc-fixture
   `(,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '(("org.junit" 0 82 93))
             (ajc-find-out-matched-pkg-item "org.junit" t)))))
  )

(ert-deftest test-ajc-shrunk-matched-pkgs ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal '("ajc.somepackage")
             (ajc-shrunk-matched-pkgs "ajc.")))))
  (test-ajc-fixture
   `(,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("org.junit.runner" "org.junit.runners")
             (ajc-shrunk-matched-pkgs "org.junit.runn"))))))

(ert-deftest test-ajc-find-out-matched-class-item ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal
       '(("SomeClass" 0 7 21 39))
       (ajc-find-out-matched-class-item "ajc.somepackage"
                                        "So")))
     (should
      (equal
       '(("AnotherClass" 0 7 10 21) ("SomeClass" 0 7 21 39))
       (ajc-find-out-matched-class-item "ajc.somepackage"
                                        nil)))
     (should
      (equal
       '(("SomeClass" 0 7 21 39))
       (ajc-find-out-matched-class-item "ajc.somepackage"
                                        "SomeClass"
                                        t)))
     (should
      (equal
       '(("Test" 1 14 1199 1205))
       (ajc-find-out-matched-class-item "org.junit"
                                        "Test"
                                        t)))
     (should
      (null (ajc-find-out-matched-class-item nil nil))))))

(ert-deftest test-ajc-complete-constructor ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal
       '("SomeClass()" "SomeClass(double)" "SomeClass(int)" "SomeClass(java.lang.String)")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "SomeClass"))))
     (should
      (equal
       '("ajc.somepackage.SomeClass()" "ajc.somepackage.SomeClass(double)"
         "ajc.somepackage.SomeClass(int)" "ajc.somepackage.SomeClass(java.lang.String)")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "SomeClass" "ajc.somepackage"))))
     )))

(ert-deftest test-ajc-complete-class-with-cache ()
  (let ((ajc-previous-class-prefix nil)
        (ajc-matched-class-items-cache nil))
    (test-ajc-fixture
     `(,test-ajc-someclass-tagfile)
     (lambda ()
       (should (equal
                '(("SomeClass" 0 7 21 39))
                (ajc-complete-class-with-cache "So")))
       (should (string= "So" ajc-previous-class-prefix))
       (should (equal '(("SomeClass" 0 7 21 39))
                       ajc-matched-class-items-cache))))))

(ert-deftest test-ajc-find-out-matched-class-item-without-package-prefix-1 ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal '(("SomeClass" 0 7 21 39))
             (ajc-find-out-matched-class-item-without-package-prefix-1
              "So" 0))))))

(ert-deftest test-ajc-find-out-matched-class-item-without-package-prefix ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal '(("SomeClass" 0 7 21 39))
             (ajc-find-out-matched-class-item-without-package-prefix "Some"))))))

(ert-deftest test-ajc-get-two-char-item ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (equal '((0 22 39))
             (ajc-get-two-char-item "So"))))))

(ert-deftest test-ajc-find-members ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '(("toString" "java.lang.String" "" ""))
             (ajc-find-members '("SomeClass" 0 7 21 39) "toSt")))
     (should
      (equal '(("getSomeClassObj" ("SomeClass" 0 7 21 39) "" ""))
             (ajc-find-members '("AnotherClass" 0 7 10 21) "getSomeClassO")))
     (should
      (equal '(("toString" "java.lang.String" "" ""))
             (ajc-find-members
              (car (ajc-find-out-matched-class-item-without-package-prefix
                    "JUnit4" t))
              "toString"
              t)))
     (should
      (equal nil
             (ajc-find-members
              "double"
              nil)))
     )))

(ert-deftest test-ajc-complete-class-candidates ()
  (let ((ajc-previous-class-prefix nil)
        (ajc-matched-class-items-cache nil))
    (test-ajc-fixture
     `(,test-ajc-someclass-tagfile)
     (lambda ()
       (with-temp-buffer
         (insert "Some")
         (should
          (equal '("SomeClass")
                 (mapcar (lambda (e)
                           (test-ajc-unpropertize-text e
                                                       '(view)))
                         (ajc-complete-class-candidates)))))))))

(ert-deftest test-ajc-concat-list-as-string ()
  (should
   (string= "ab"
            (ajc-concat-list-as-string '("a" "b")))))


(ert-deftest test-ajc-find-out-matched-class-item-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '(("SomeClass" 0 7 21 39))
             (ajc-find-out-matched-class-item "ajc.somepackage"
                                              "Some")))
     (should
      (equal '(("Test" 1 14 1199 1205))
             (ajc-find-out-matched-class-item "org.junit"
                                              "Tes")))
     (should
      (equal '(("Test" 1 14 1199 1205))
             (ajc-find-out-matched-class-item "org.junit"
                                              "Test"
                                              t))))))

(ert-deftest test-ajc-find-out-matched-class-item-without-package-prefix-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '(("SomeClass" 0 7 21 39))
             (ajc-find-out-matched-class-item-without-package-prefix "Some")))
     (should
      (equal '(("AssertionFailedError" 1 8 433 456))
             (ajc-find-out-matched-class-item-without-package-prefix "Assertio"))))))


(ert-deftest test-ajc-make-sorted-class-buffer-name ()
  (should
   (string= " *sorted-class-someclass.tag*"
            (ajc-make-sorted-class-buffer-name " *someclass.tag*"))))

(ert-deftest test-ajc-package-candidates-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("ajc.somepackage")
             (ajc-package-candidates "ajc.s")))
     (should
      (equal '("org.junit")
             (ajc-package-candidates "org.ju")))
     (should
      (null (ajc-package-candidates "foo.bar.b"))))))


(ert-deftest test-ajc-fqn-candidates-1-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal "assertThat"
             (caar (ajc-fqn-candidates-1 "org.junit.Assert.assertTh"))))
     (should
      (equal "CONSTANT"
             (caar (ajc-fqn-candidates-1 "ajc.somepackage.SomeClass.CON")))))))

(ert-deftest test-ajc-complete-constructor-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal
       '("SomeClass()" "SomeClass(double)" "SomeClass(int)" "SomeClass(java.lang.String)")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "SomeClass"))))
     (should
      (equal
       '("ajc.somepackage.SomeClass()" "ajc.somepackage.SomeClass(double)"
         "ajc.somepackage.SomeClass(int)" "ajc.somepackage.SomeClass(java.lang.String)")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "SomeClass" "ajc.somepackage"))))
     (should
      (equal
       '("org.junit.matchers.JUnitMatchers()")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "JUnitMatchers" "org.junit.matchers"))))
     )))

(ert-deftest test-ajc-complete-class-with-cache-multitag ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '(("SomeClass" 0 7 21 39))
             (ajc-complete-class-with-cache "SomeC")))
     (should
      (equal '(("TestCase" 1 8 551 606))
             (ajc-complete-class-with-cache "TestCa")))
     )))

(ert-deftest test-ajc-load-tag-file ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     ;; Before loading
     (should
      (= 1 (length ajc-plain-method-tables)))
     (should
      (null (let ((ac-prefix "assertT"))
              (ajc-plain-method-candidates))))
     ;; After loading
     (ajc-load-tag-file test-ajc-junit-tagfile)
     (should
      (= 2 (length ajc-tag-buffer-list)))
     (should
      (= 2 (length ajc-tag-file-list)))
     (should
      (= 2 (length ajc-plain-method-tables)))
     (should
      (equal
       '("org.junit.matchers.JUnitMatchers()")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "JUnitMatchers" "org.junit.matchers"))))
     (should
      (not (null (let ((ac-prefix "assertT"))
                   (ajc-plain-method-candidates)))))
     )))

(ert-deftest test-ajc-unload-tag-file ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     ;; Before unloading junit tag, should be completed
     (should
      (equal
       '("org.junit.matchers.JUnitMatchers()")
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "JUnitMatchers" "org.junit.matchers"))))
     (ajc-unload-tag-file (file-truename
                           (expand-file-name test-ajc-junit-tagfile)))
     ;; After unloading it, shoud not be completed
     (should (= 1 (length ajc-tag-buffer-list)))
     (should (= 1 (length ajc-tag-file-list)))
     (should (= 1 (length ajc-plain-method-tables)))
     (should
      (null
       (mapcar (lambda (e)
                 (test-ajc-unpropertize-text e '(view template-type template)))
               (ajc-complete-constructor "JUnitMatchers" "org.junit.matchers")))))))

(ert-deftest test-ajc-line-has-typeinfo-p ()
  (should
   (ajc-line-has-typeinfo-p
    "_jars"
    "private ArrayList<File> _jars = new ArrayList<>(); // absolute paths of jar files"))
  (should
   (ajc-line-has-typeinfo-p
    "cls"
    "  public ClassItem(Class cls) {"))
  (should
   (ajc-line-has-typeinfo-p
    "pair"
    "  String[] pair = elements.split(\";\");")))

(ert-deftest test-ajc-rindex ()
  (should (= 5 (ajc-rindex "abcdef" ?f)))
  (should (= 1 (ajc-rindex "abcdef" ?b)))
  (should (null (ajc-rindex "abcdef" ?g))))

(ert-deftest test-ajc-get-class-item-by-fqn ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (equal '("Test" 1 14 1199 1205)
             (ajc-get-class-item-by-fqn "org.junit.Test")))
     (should
      (null (ajc-get-class-item-by-fqn "foo.bar.buzz")))
     (should
      (null (ajc-get-class-item-by-fqn "foo"))))))

(ert-deftest test-ajc-parse-variable-line-string ()
  (should
   (string= "HashMap"
            (ajc-parse-variable-line-string
             "event"
             "    HashMap<String, String> event = new HashMap<String, String>();")))
  (should
   (string= "AuctionEventListener"
            (ajc-parse-variable-line-string
             "_listener"
             "  private AuctionEventListener _listener;")))
  (should
   (string= "Chat"
            (ajc-parse-variable-line-string
             "_notToBeGCd"
             "  @SuppressWarnings(\"unused\") private Chat _notToBeGCd;")))
  (should
   (string= "int"
            (ajc-parse-variable-line-string
             "i"
             "int i")))
  (should
   (string= "String[]"
            (ajc-parse-variable-line-string
             "pair"
             "String[] pair = element.split(\":\");"))))


(ert-deftest test-ajc-build-plain-method-table-1 ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (let ((table (make-hash-table :test #'equal)))
       (ajc-build-plain-method-table-1 table (car ajc-tag-buffer-list) 0)
       (should
        (not (null (gethash "equ" table))))
       (should
        (string= "equals(java.lang.Object)"
                 (car (mapcar (lambda (e)
                                (substring-no-properties e 0))
                              (gethash "equ" table)))))
       ))))

(ert-deftest test-ajc-build-plain-method-table-1-check-all-methods ()
  (test-ajc-fixture
   `(,test-ajc-junit-tagfile)
   (lambda ()
     (loop with methods = (test-ajc-collect-all-methods test-ajc-junit-tagfile)
           with table = (car ajc-plain-method-tables)
           for method in methods
           for key = (and (>= (length method) 3) (substring method 0 3))
           for value = (and key (gethash key table))
           for value-lst = (and value (mapcar #'substring-no-properties value))
           when key
           do (progn
                (should (not (null value)))
                (should (member-if (lambda (e)
                                     (string-match (concat method "(") e))
                                   value-lst)))))))

(defun test-ajc-collect-all-methods (tagfile)
  (let ((ret nil))
    (with-temp-buffer
      (insert-file-contents-literally (expand-file-name tagfile))
      (goto-char (point-min))
      (forward-line
       (string-to-number
        (save-excursion
          (goto-char (point-min))
          (forward-line 4)
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))))
      (beginning-of-line)
      (while (re-search-forward "\\(^[a-z][^`]+\\)`.*$" nil t)
        (push (match-string-no-properties 1) ret)))
    ret))

(defvar test-ajc-plain-method-tables nil)

(ert-deftest test-ajc-build-plain-method-table ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (let ((tables (ajc-build-plain-method-table ajc-tag-buffer-list)))
       (should
        (= 2 (length tables)))
       (should
        (not (null (gethash "equ" (cadr tables)))))
       (should
        (not (null (all-completions
                    "wai"
                    (gethash "wai" (car tables))))))
       (setq test-ajc-plain-method-tables tables)
       ))))

(setq test-ajc-plain-method-tables nil)

(ert-deftest test-ajc-plain-method-candidates-1 ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (let ((tables (ajc-build-plain-method-table ajc-tag-buffer-list)))
       (should
        (not (null (ajc-plain-method-candidates-1 "wai" (car tables) 50))))
       (should
        (not (null (ajc-plain-method-candidates-1 "assertT" (cadr tables) 50))))
       ))))

(ert-deftest test-ajc-save-and-load-meothod-table-cache ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (let ((table1 (car ajc-plain-method-tables))
           (table2 nil))
       (should (hash-table-p table1))
       (should
        (gethash "equ" table1))
       (should
        (gethash "wai" table1))
       (should
        (gethash "get" table1))
       (ajc-save-method-table-cache (car ajc-tag-file-list) 0)
       (setq table2 (ajc-load-method-table-cache (car ajc-tag-file-list) 0))
       (should (hash-table-p table2))
       (should
        (gethash "equ" table2))
       (should
        (gethash "wai" table2))
       (should
        (gethash "get" table2))))))

(ert-deftest test-ajc-get-cache-filename ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile)
   (lambda ()
     (should
      (string= "someclass.ajc.cache"
               (ajc-get-cache-filename (car ajc-tag-file-list)))))))

(ert-deftest test-ajc-construct-cache-filepath ()
  (let* ((ajc-method-table-cache-dir "./test/cache")
         (cache-fullpath
          (expand-file-name
           (concat (directory-file-name ajc-method-table-cache-dir)
                   "/"
                   "someclass.ajc.cache"))))
    (multiple-value-bind (filename full-pathname)
        (ajc-construct-cache-filepath test-ajc-someclass-tagfile)
      (should
       (string= "someclass.ajc.cache"
                filename))
      (should
       (string= cache-fullpath
                full-pathname)))))

(ert-deftest test-ajc-plain-method-candidates-1-from-cache ()
  (test-ajc-fixture
   `(,test-ajc-someclass-tagfile
     ,test-ajc-junit-tagfile)
   (lambda ()
     (should
      (not (null (ajc-plain-method-candidates-1 "wai"
                                                (car ajc-plain-method-tables)
                                                50))))
     (should
      (not (null (ajc-plain-method-candidates-1 "assertT"
                                                (cadr ajc-plain-method-tables)
                                                50))))
     )))