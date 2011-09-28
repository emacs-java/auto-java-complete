;;; ajc-java-complete.el --- Auto Java Completion for GNU Emacs

;; This file is NOT part of GNU Emacs
;; plesase send Bug reports and suggestions to 'Joseph at <jixiuf@gmail.com>

;;  License

;; Copyright (C) 2011  Joseph <jixiuf@gmail.com> Limited

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;;;this is "Auto Java Complete".

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `ajc-reload'
;;    restart Auto Java Complete ,when your tag file changed,
;;  `ajc-import-all-unimported-class'
;;    import all unimported class .
;;  `ajc-import-class-under-point'
;;    import class under point.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `ajc-tag-file'
;;    the tag file is  used for java complete ,it  is generate by a Tags.java ,
;;    default = "~/.java_base.tag"
;;  `ajc-default-length-of-class'
;;    the length of class name at dropdown-menu ,if the class
;;    default = 36
;;  `ajc-return-type-char'
;;    the char  before return type when
;;    default = ":"
;;  `ajc-throws-char'
;;    the char  before Exceptions  when completing
;;    default = "   #"

;;1. it depends on auto complete ,so it would complete
;;   everything by dropdowning a menu.

;;2. it is tag based . so before you used it on emacs ,you
;;   should generate a tag file by using Tags.java .
;;   about how to use it ,see the Install section.

;;3. it depends on yasnippet . when completing method and
;;   constructor it would generate a templete dynamically
;;   so that you can jump from a paramter to another one .

;;4. when completing method ,it can show return type behind
;;   each candidate on dropdown menu.
;;   I want to show
;;       toString() :String         on menu ,but just insert
;;       toString()                 in buffer ,
;;   but there is a problem:
;;   auto complete 1.3 now doesn't support it .
;;   I patched  popup.el in auto-complete-1.3/  ,
;;   the patched file is ajc-java-complete/popup.el, please
;;   put this file into auto-complete-1.3/ ,or use
;;      ajc-java-complete/popup-patch.diff
;;      cp  ajc-java-complete/popup-patch.diff auto-complete-1.3/
;;      cd auto-complete-1.3/
;;      patch -p0 < popup-patch.diff
;;
;;     don't forget to byte-compile it


;;; Features

;; 1. support importing.
;;    when you type in  import javax.s-|-
;;    it would drop down a menu like this
;;             import  javax.s
;;
;;                     javax.sql
;;                     javax.swing
;;                     javax.sound

;; 2. support import class with keybindings (even in jsp file)
;;         auto import all Class in source file
;;    (local-set-key (kbd "C-c i") (quote ajc-import-all-unimported-class))
;;         import Class where under point
;;    (local-set-key (kbd "C-c m") (quote ajc-import-class-under-point))
;;   included in ajc-complete-config.el

;; 3. support completing class name ,you just need  typing
;;    in a Word beginning with [A-Z] ,then it would auto find
;;    matched class and list it with dropdown menu.


;; 4. support complete method.
;;    for example
;;    List<Map<String,Object>> list = new ArrayList<Map<String,Object>>();
;;         list.

;;    it would list all method like this
;;         list.
;;              equals(Object)
;;              add(Object)
;;              clear()

;;    it can do more
;;         list.listIterator().next().
;;                                      toString()
;;                                      getClass()
;;                                      notify()

;; 5. support complete constructor
;;    after keyword 'new' it would try to complete constructor


;; 6. support completing in jsp files.

;;    If your want to enable  ajc-java-complete-mode when openning
;;    a jsp file. you can
;;        (add-hook 'jsp-mode 'ajc-java-complete-mode)
;;    if you has a jsp-mode;
;;    if not ,you can do it like this
;;        (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;;    now it can complete class name,method ,constructor.
;;    it also support complete importing ,but it isn't auto completed,
;;    you must trigger it by a key binding
;;    for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;    <%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
;;    now you can  press M-1 to show the menu.
;;

;;; Install
;;   see Install file in this directory

;;; Code.


(defgroup auto-java-complete nil
  "Auto Java Completion."
  :group 'convenience
  :prefix "auto-java-complete")

(defcustom ajc-tag-file "~/.java_base.tag"
  "the tag file is  used for java complete ,it  is generate by a Tags.java ,
so before you use this tool ,try to compile Tags.java
          javac Tags.java
and  use this class like this
         java  Tags
 it will tag all jars in classpath to tag file , or
         java Tags   com.whatever.*
 just tag class under com.whatever packages "
  :type 'string
  :group 'auto-java-complete)


(defcustom ajc-default-length-of-class 36
  "the length of class name at dropdown-menu ,if the class
name is shorter than this value ,then empty string are append
.and return type are at position 37 "
  :type 'integer
  :group 'auto-java-complete)

(defcustom ajc-return-type-char ":"
  "the char  before return type when
  completing methods."
  :type 'string
  :group 'auto-java-complete
  )
(defcustom ajc-throws-char "   #"
  "the char  before Exceptions  when completing
  method"
  :type 'string
  :group 'auto-java-complete
  )

;;private variables
(defvar ajc-is-running nil "after (ajc-init) it will become true")
(defvar ajc-all-sorted-class-items nil "this is a list,
all the element are sorted class-item
this variable should work with ajc-two-char-list,
then search class is faster ")

(defvar ajc-two-char-list nil
  "in this list ,it looks like '((Ab 1 3 ) (Ac 4 15))that means
    all class those name starts with Ab are in the position of
  0~2 (because index from 0 not 1) in ajc-all-sorted-class-items " )
(defvar ajc-tmp-sorted-class-buffer-name " *ajc-tmp-sorted-class*")

(defvar ajc-tag-buffer nil "this is the buffer of .java_base.tag" )
(defvar ajc-package-first-ln 0
  "the first line number of the package section in tag file")
(defvar ajc-class-first-ln 0
"the first line number of the class section in tag file,
it is the end of package section line number too. " )
(defvar ajc-member-first-ln 0
"the first line number of the member section in tag file ,
actually it is the end of package section line number too" )
(defvar ajc-member-end-ln 0
  "the end line number of the member section in tag file ,
it is the last line number in tag file" )

(defvar ajc-position-of-package-first-line 1)
(defvar ajc-position-of-class-first-line 1)
(defvar ajc-position-of-member-first-line 1)
(defvar ajc-position-of-member-end-line 1)

(defvar ajc-matched-class-items-cache nil
  "when search class-prefix without package-name ,
  it will search thoudsands of lines in tags files ,
  so this will cache for next match maybe  ")
(defvar ajc-previous-class-prefix nil "cache last class-prefix ")

(defvar ajc-matched-import-cache-list nil
  "when complete a import ,sometimes we can use
 the last completed items for next complete  ")
(defvar ajc-previous-matched-import-prefix nil
 "previous matched prefix for import Class at head of source")

(defvar ajc-current-class-prefix-4-complete-class nil
 "when (ajc-is-available-4-complete-class-p ) return true,
 it will save current class-prefix in this variable ,so
 (ajc-complete-class-candidates) can reuse it . ")

(defun ajc-goto-line ( line-num &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (numberp line-num)
    (goto-char (point-min))
    (forward-line (1- line-num )))))

(defun ajc-read-line(line-number  &optional buffer)
  "read a line  and return the string"
  (with-current-buffer (or buffer (current-buffer))
    (ajc-goto-line line-number)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun ajc-split-string-with-separator(str regexp &optional replacement omit-nulls)
  "this function is a tool like split-string,
  but it treat separator as an element of returned list
  for example (ajc-split-string-with-separator abc.def.g \"\\.\" \".\")
  will return '(\"abc\" \".\" \"def\" \".\" \"g\" )"
  (when str
    (let (split-list  substr match-end)
      (if  (string-match regexp str)
          (progn
            (while (string-match regexp  str)
              (setq match-end (match-end 0))
              (setq  substr (substring-no-properties str 0 (- match-end 1)))
              (when (or (not omit-nulls) (> (length substr ) 0))
                (setq split-list (append split-list (list  substr))) )
              (setq split-list (append split-list (list (or replacement regexp))))
              (setq str (substring-no-properties str  match-end)))
            (when (or (not omit-nulls) (> (length str ) 0))
              (setq split-list (append split-list (list str)))))
        (setq split-list (list str)))
      split-list)))

(defun ajc-split-pkg-item ( pkg-line-string )
 "the format pkg-line-string is  str`num`num
  this function translate it to a list ,the num will be string2number
  give me  `java.lang`222`333 ,return '(\"java.lang\" 222 333 ) "
  (let (( pkg-item (split-string pkg-line-string "`" t)))
    (setcar (nthcdr 1 pkg-item ) (string-to-number (nth 1 pkg-item )))
    (setcar (nthcdr 2 pkg-item ) (string-to-number (nth 2 pkg-item )))
     pkg-item))

(defun ajc-split-pkg-item-by-pkg-ln ( pkg-line-number  &optional buffer )
  "the format pkg-line-string is str`num`num
   this function translate it to a list ,the num will be
   string2number return a list of pkg info of line-number "
  (ajc-split-pkg-item
   (ajc-read-line pkg-line-number
                  (or buffer (ajc-reload-tag-buffer-maybe)))))

(defun ajc-split-class-item ( class-line-string )
 "the format of class-line-string is
  classname`packageLineNum`memberStartLineNum`memberEndLineNum
  this function translate it to a list ,the num will be convert to number "
  (let (( class-item (split-string class-line-string "`" t)))
    (setcar (nthcdr 1 class-item ) (string-to-number (nth 1 class-item )))
    (setcar (nthcdr 2 class-item) (string-to-number (nth 2 class-item )))
    (setcar (nthcdr 3 class-item) (string-to-number (nth 3 class-item )))
     class-item))

(defun ajc-split-class-item-by-class-ln
  ( class-line-number  &optional buffer )
  (ajc-split-class-item
   (ajc-read-line class-line-number
                  (or buffer (ajc-reload-tag-buffer-maybe)))))

(defun ajc-split-constructor-by-line-num ( constructor-line-num )
   (ajc-split-constructor
    (ajc-read-line constructor-line-num
                   (ajc-reload-tag-buffer-maybe))))

(defun ajc-split-field (field-line-string)
  (when field-line-string
    (let* ((field-item)
           (field-line-string (substring-no-properties field-line-string 1))
           (split-list (split-string  field-line-string "`"))
           (return-type (nth 1 split-list)))
      ;;handle field name
      (add-to-list  'field-item  (car split-list) t)
      (if (string-match  "^~" return-type )
          (add-to-list 'field-item (substring-no-properties  return-type 1) t)
        (add-to-list 'field-item (ajc-split-class-item-by-class-ln
                                  (string-to-number return-type)) t))
      field-item )))

(defun append-space-to-item(str)
  (let ((len (length str)));; insert whitespace between classname and return type
    (if (< len (- ajc-default-length-of-class 3))
        (setq str
              (concat str
                      (make-string (- (- ajc-default-length-of-class 3) len ) 32 )));;32 mean whitespace
      (setq str (concat str "     "))))
  )

;; (ajc-field-to-string (ajc-split-field " out`654 " ) )
;; (ajc-field-to-string (ajc-split-field " out`654 " ) t)
(defun ajc-field-to-string (field-item &optional with-return-type)
  (when field-item
    (if  with-return-type
        (let ((field-string  (car field-item)) (return-type (nth 1 field-item)))
          (setq field-string (append-space-to-item field-string))
          (setq field-string (concat field-string ajc-return-type-char))
          (when (stringp return-type)
            (setq field-string (concat field-string return-type )   ))
          (when (listp return-type)
            (setq field-string (concat field-string   (car return-type))))
          field-string)
      (car field-item))))


;; (ajc-method-to-string (ajc-split-method "skippedEntity`~void`784`4012"))
;; (ajc-method-to-string (ajc-split-method "skippedEntity`~void`784`4012") t)

(defun ajc-method-to-string (method-item &optional  with-return-type-and-throws )
  "this is a toString() like function .
   when param with-detail is not null, it will include
  return type and exceptions, default it only include method name
  and params"
  (when method-item
    (let ((method-string  (car method-item))
          (return-type (nth 1 method-item)   )
          (params (nth 2 method-item)   )
          (exceptions (nth 3 method-item)))
      (if (stringp params )
          (setq method-string (concat method-string "()"))
        (setq method-string (concat method-string "("))
        (dolist (param  params )
          (when (stringp param )
            (setq method-string (concat method-string param " , " )))
          (when (listp param)
            (setq method-string (concat method-string  (car param)  " , " ))
            ))
        (setq method-string
              (replace-regexp-in-string  " , $" ")" method-string )))
      (when with-return-type-and-throws
        (setq method-string (append-space-to-item method-string))
        (cond ((stringp return-type)
               (setq method-string (concat method-string ajc-return-type-char  return-type)))
              ((listp return-type)
               (setq method-string (concat method-string ajc-return-type-char  (car return-type))))
              )
        (when (listp exceptions )
          (setq method-string (concat method-string ajc-throws-char))
          (dolist (exception  exceptions )
            (when (stringp exception ) (setq method-string (concat method-string exception " , " )))
            (when (listp exception)
              (setq method-string (concat method-string  (car exception)  " , " ))))
          (setq method-string  (replace-regexp-in-string  ", $" "" method-string )))
        )
      method-string )))

;; (ajc-class-to-string (ajc-split-class-item  "RasterOp`18`15551`15556"))
;; (ajc-class-to-string (ajc-split-class-item  "RasterOp`18`15551`15556") t)
(defun ajc-class-to-string(class-item &optional  with-package-name-append)
  (when class-item
    (let* ((class-string (car class-item)))
      (when with-package-name-append
        (setq class-string
              (concat  (append-space-to-item class-string) ajc-return-type-char
                       (car (ajc-split-pkg-item-by-pkg-ln (nth 1 class-item))))))
      class-string
      )))


;; (yas/expand-snippet(ajc-method-to-yasnippet-templete    (car
;; (ajc-find-members (car  (ajc-find-out-matched-class-item-without-package-prefix "FileWriter" t )) "write" ))))
;; (ajc-method-to-string    (car
;; (ajc-find-members (car  (ajc-find-out-matched-class-item-without-package-prefix "String" t )) "split" )))
;; "split(String , int)"
;(yas/expand-snippet "split(${1:String} , ${2:int})"
(defun ajc-method-to-yasnippet-templete (method-item)
  (when method-item
    (let ((method-string  (car method-item))
          (params (nth 2 method-item)   )
          (exceptions (nth 3 method-item))
          (index 0))
      (if (stringp params ) (setq method-string (concat method-string "()"))
        (setq method-string (concat method-string "("))
        (dolist(param params)
          (when (stringp param)
            (setq method-string
                  (concat  method-string "${" (number-to-string (+ index 1)) ":"
                           param "} , " )))
          (when (listp param)
            (setq method-string
                  (concat method-string "${" (number-to-string (+ 1 index )) ":"
                          (car param)  "} , " )))
          (setq index (1+ index)))
        (setq method-string  (replace-regexp-in-string  " , $" ")$0" method-string )))
      method-string)))

(defun ajc-method-item-to-candidate(field-or-method-item)
  "translate `field-or-method-item' to candidate ,`field-or-method-item'
can be a method item ,or a field item"
  (let ((candidate))
    (if (= 2   (length field-or-method-item ));; lenth of field is 2 (only field and returntype )
        (let ((field-full-string (ajc-field-to-string field-or-method-item t))
              (field-short-string (ajc-field-to-string field-or-method-item nil)))
          (setplist 'props nil ) (put 'props 'view field-full-string)
          (add-text-properties 0 (length field-short-string)
                               (symbol-plist  'props)  field-short-string)
          (setq candidate field-short-string)
          )
      (let((method-full-string  (ajc-method-to-string field-or-method-item t))
           (method-short-string (ajc-method-to-string field-or-method-item nil)))
        (setplist 'props nil ) (put 'props 'view method-full-string)
        (put 'props 'templete (ajc-method-to-yasnippet-templete field-or-method-item))
        (add-text-properties 0 (length method-short-string)
                             (symbol-plist  'props)  method-short-string)
        (setq candidate method-short-string)))
    candidate))

(defun ajc-split-method ( method-line-string )
  (when method-line-string
    (let ((method-item) (split-list)(return-type))
      (setq split-list (split-string  method-line-string "`"))
      ;;handle method name
      (add-to-list  'method-item  (car split-list) t)
      (setq return-type (nth 1 split-list))
      (if (string-match  "^~" return-type )
          (add-to-list 'method-item (substring-no-properties  return-type 1) t)
        (add-to-list 'method-item  (ajc-split-class-item-by-class-ln
                                    (string-to-number return-type)) t))
      ;;handle params if exists
      (if (not  (string-equal "" (nth 2 split-list)))
          (let ((params)(param-split-list))
            (setq param-split-list (split-string (nth 2 split-list)  "," t))
            (dolist (param param-split-list)
              (if (string-match  "^~" param )
                  (setq params  (append  params  (list (substring-no-properties param 1 ))))
                (setq params (append params (list (ajc-split-class-item-by-class-ln
                                                   (string-to-number param)))))))
            (setq method-item (append method-item (list params))))
        (setq method-item (append method-item  (list ""))))
      (if (not  (string-equal "" (nth 3 split-list)))
          (let ((exceptions)(exception-split-list))
            (setq exception-split-list (split-string (nth 3 split-list)  "," t))
            (dolist (exception exception-split-list)
              (if (string-match  "^~" exception )
                  (setq exceptions  (append  exceptions  (list (substring-no-properties exception 1 ))))
                (setq exceptions (append exceptions (list (ajc-split-class-item-by-class-ln
                                                           (string-to-number exception)))))))
            (setq method-item (append method-item (list exceptions))))
        (setq method-item (append method-item  (list ""))))
      method-item)))

(defun ajc-split-constructor(constructor-line-string)
  (when constructor-line-string
    (let ((constructor-item) (split-list))
      (setq constructor-line-string (substring-no-properties constructor-line-string 2))
      (setq split-list (split-string  constructor-line-string "`"))
      ;;handle constructor name
      (add-to-list  'constructor-item  (car split-list) t)
      ;;handle params if exists
      (if (not  (string-equal "" (nth 1 split-list)))
          (let ((params)(param-split-list))
            (setq param-split-list (split-string (nth 1 split-list)  "," t))
            (dolist (param param-split-list)
              (if (string-match  "^~" param )
                  (setq params  (append  params  (list (substring-no-properties param 1 ))))
                (setq params (append params (list (ajc-split-class-item-by-class-ln
                                                   (string-to-number param)))))))
            (setq constructor-item (append constructor-item (list params))))
        (setq constructor-item (append constructor-item  (list ""))))
      (if (not  (string-equal "" (nth 2 split-list)))
          (let ((exceptions)(exception-split-list))
            (setq exception-split-list (split-string (nth 2 split-list)  "," t))
            (dolist (exception exception-split-list)
              (if (string-match  "^~" exception )
                  (setq exceptions  (append  exceptions  (list (substring-no-properties exception 1 ))))
                (progn
                  (setq exceptions (append exceptions (list (ajc-split-class-item-by-class-ln
                                                             (string-to-number exception))))))))
            (setq constructor-item (append constructor-item (list exceptions))))
        (setq constructor-item (append constructor-item  (list ""))))
      constructor-item)))

(defun ajc-constructor-to-string (constructor-item &optional is-with-exceptions)
  (when constructor-item
    (let((constructor-string  (car constructor-item))
         (params (nth 1 constructor-item)   )
         (exceptions (nth 2 constructor-item)))
      (if (stringp params ) (setq constructor-string (concat constructor-string "()"))
        (progn
          (setq constructor-string (concat constructor-string "("))
          (dolist (param  params )
            (when (stringp param ) (setq constructor-string (concat constructor-string param " , " )))
            (when (listp param)
              (setq constructor-string (concat constructor-string  (car param)  " , " ))))
          (setq constructor-string  (replace-regexp-in-string  " , $" ")" constructor-string ))))
      (when is-with-exceptions
        (when (listp exceptions )
          (setq constructor-string (concat constructor-string ajc-throws-char))
          (dolist (exception  exceptions )
            (when (stringp exception ) (setq constructor-string (concat constructor-string exception " , " )))
            (when (listp exception)
              (setq constructor-string (concat constructor-string  (car exception)  " , " ))))
          (setq constructor-string  (replace-regexp-in-string  ", $" "" constructor-string ))))
      constructor-string)))

(defun ajc-constructor-to-yasnippet-templete (constructor-item)
  (when constructor-item
    (let((constructor-string  (car constructor-item))
         (params (nth 1 constructor-item)   )
         (exceptions (nth 2 constructor-item))
         (index 0))
      (if (stringp params ) (setq constructor-string (concat constructor-string "()"))
        (setq constructor-string (concat constructor-string "("))
        (dolist (param params)
          (when (stringp param ) (setq constructor-string
                                       (concat  constructor-string "${" (number-to-string (+ index 1)) ":"
                                                param "} , " )))
          (when (listp param)
            (setq constructor-string (concat constructor-string "${" (number-to-string (+ 1 index )) ":"
                                             (car param)  "} , " )))
          (setq index (+ 1 index ))
          )
        (setq constructor-string  (replace-regexp-in-string  " , $" ")$0" constructor-string )))
      (setq constructor-string constructor-string))))

;; find tag file
(defun ajc-init()
  "find java tag file and do some initial works, like  populate some variables "
  (unless ajc-is-running
    (setq ajc-tag-file (file-truename (expand-file-name ajc-tag-file  )))
    (if (file-exists-p  ajc-tag-file)
        (with-current-buffer (find-file-noselect ajc-tag-file )
          ;; a buffer name starts with empth string,means hidden this buffer
          (rename-buffer " *java-base.tag*")
          (setq ajc-tag-buffer " *java-base.tag*")
          (buffer-disable-undo)
          (setq buffer-read-only t)
          (fundamental-mode)
          (setq case-fold-search nil)
          (setq ajc-package-first-ln  (string-to-number (ajc-read-line 3)))
          (setq ajc-position-of-package-first-line (progn   (ajc-goto-line  ajc-package-first-ln) (point)) )
          (setq ajc-class-first-ln    (string-to-number  (ajc-read-line 4)))
          (setq  ajc-position-of-class-first-line (progn   (ajc-goto-line  ajc-class-first-ln) (point)) )
          (setq ajc-member-first-ln   (string-to-number (ajc-read-line 5)))
          (setq  ajc-position-of-member-first-line (progn   (ajc-goto-line  ajc-member-first-ln) (point)) )
          (setq ajc-member-end-ln     (string-to-number (ajc-read-line 6)))
          (setq  ajc-position-of-member-end-line (progn   (ajc-goto-line  ajc-member-end-ln) (point)) )
;;          (ajc-load-all-sorted-class-items-to-memory)
          (ajc-sort-class)
          )
      (message  ( concat ajc-tag-file " doesn't exists !!!")))
    (setq ajc-is-running t)
    ))
;;;###autoload
(defun ajc-reload()
  "restart Auto Java Complete ,when your tag file changed,
you can use this function restart AutoJavaComplete "
  (interactive)
  (setq ajc-is-running nil)
  (ajc-init)
  )
;;;###autoload
(defalias 'auto-java-complete-reload 'ajc-reload)

(defun ajc-reload-tag-buffer-maybe( )
  "check if the ajc-tag-buffer is still live ,if not reload it "
  (unless ajc-tag-buffer
    (ajc-init))
  ajc-tag-buffer)

;; (ajc-find-out-matched-pkg-item "java.awt")
;; (ajc-find-out-matched-pkg-item "java.awt" t)
(defun ajc-find-out-matched-pkg-item
  (pkg-prefix &optional exactly_match  buffer)
  "this function is used to find out all matched packages whose prefix is `pkg-prefix'
  for example: suppose  pkg-prefix=javax.xm  then it will return
   '( (\"javax.xml.bind\" 2741 2767 ) (\"javax.xml.bind.attachment\" 2776 2778 ))
if exactly_match is not nil then pkg-prefix will be seen as full package name , and
we will suppose you are searching package name = pkg-prefix , if exactly_match is given
then only 1 or 0 item will returned so we will try to
 convert '((packageName 12 33 )) to '(packageName 12 33 ) "
  (with-current-buffer (or buffer  (ajc-reload-tag-buffer-maybe))
    (let ((regexp-pkg-prefix (concat "^" (regexp-quote pkg-prefix)))
          matched-package)
      (when exactly_match ;;I use ` char as the separator in tag file
        (setq regexp-pkg-prefix (concat "^"  (regexp-quote  pkg-prefix )  "`" )))
      (goto-char ajc-position-of-package-first-line)
      (while (re-search-forward regexp-pkg-prefix ajc-position-of-class-first-line t)
        (add-to-list 'matched-package
                     (ajc-split-pkg-item
                      (buffer-substring-no-properties (line-beginning-position)(line-end-position)))))
      (when exactly_match (setq matched-package (car matched-package)))
      matched-package )))

;; (ajc-shrunk-matched-pkgs "java.aw") == java.awt
(defun ajc-shrunk-matched-pkgs (pkg-prefix)
  "this function is used for list matched package.
when you import a package in head of your java file,
when you typed in 'jav-|-', then it will list 'java javax'
instead of 'java.lang java.lang.rel javax.xml javax.xml.ws'"
  (let ((matched-pkg-items (ajc-find-out-matched-pkg-item pkg-prefix))
         (index-of-first-dot 0) (return-list)
         (length-of-pkg-prefix (length pkg-prefix)))
    (dolist (current-item matched-pkg-items)
      (if (setq index-of-first-dot
                (string-match "\\." (car current-item) length-of-pkg-prefix))
          (add-to-list 'return-list (substring-no-properties
                                     (car current-item) 0 index-of-first-dot))
        (add-to-list 'return-list (car current-item))))
    return-list))

(defun ajc-find-class-first-check-imported(class-name)
  "this function will find class from imported classes,
if doesn't exists,find from the tag file,
if more than one class item matched class-name in tag file,
then imported one of them first"
  (let* ((imported-class (ajc-caculate-all-imported-class-items))
         (matched-class-item
          (catch 'found
            (dolist (item imported-class)
              (when (string-equal class-name (car item))
                (throw 'found item))))))
    (unless matched-class-item;;if not found from imported section
      (let ((matched-class-items
             (ajc-find-out-matched-class-item-without-package-prefix class-name t)))
        (if (= (length matched-class-items) 1)
            (setq matched-class-item (car matched-class-items))
          (setq matched-class-item
                (car (ajc-insert-import-at-head-of-source-file matched-class-items))))))
    matched-class-item))

;; (ajc-find-out-matched-class-item "java.io" "Fil")
;; (ajc-find-out-matched-class-item "java.io" "")
;; (ajc-find-out-matched-class-item "java.io" nil)
;; (ajc-find-out-matched-class-item "java.io" "File" t)
;; (ajc-find-out-matched-class-item nil "File" )
;; (print (length (ajc-find-out-matched-class-item nil nil )))
(defun ajc-find-out-matched-class-item
  (package-name class-prefix &optional exactly_match  buffer)
  "this function is use to find out all Class whose package name is
`package-name' and ClassName  starts with `class-prefix' if package-name
is nil, then try to find out all Class whose ClassName starts with
`class-prefix' if `class-prefix' is nil or empty string ,it will try to
find out all Class in package `package-name' if both  `package-name'
and class-prefix are nil, then it will return all Class in all package
the param `exactly_match' ,means only class name exactly equals
 to `class-prefix' will be return"
  (let* ((class-prefix (or class-prefix ""))
        (regexp-class-prefix
         (if exactly_match (concat "^" (regexp-quote class-prefix) "`" )
           (concat "^" (regexp-quote class-prefix))))
        (matched-pkg-item (when package-name (ajc-find-out-matched-pkg-item package-name t)))
        (line-num    ajc-class-first-ln)
        (end-position ajc-member-first-ln)
        return-list current-line-string)
    (with-current-buffer (or buffer (ajc-reload-tag-buffer-maybe))
      (when matched-pkg-item
        (setq line-num (nth 1 matched-pkg-item)
              end-position (nth 2 matched-pkg-item)))
      (while (< line-num end-position)
        (setq current-line-string (ajc-read-line line-num))
        (when (string-match regexp-class-prefix current-line-string)
          (add-to-list 'return-list (ajc-split-class-item current-line-string)))
        (setq line-num (1+ line-num)))
      return-list)))

;;(ajc-sort-class)
;;(ajc-get-two-char-item "Sy")
(defun ajc-get-two-char-item(two-char-string)
  (catch 'found
    (dolist (item  ajc-two-char-list)
      (when (string= (car item)  two-char-string)
        (throw 'found item)))))

;;(ajc-find-out-matched-class-item-without-package-prefix "System" t)
;; (ajc-find-out-matched-class-item-without-package-prefix "_ServantLocatorStub")
(defun ajc-find-out-matched-class-item-without-package-prefix
  (class-prefix &optional exactly_match)
  "actully you can use ajc-find-out-matched-class-item to do
the same thing ,just let package-prefix nil but it is very slowly ,
it need to search all the line in tag file just to find out one class item .
so this function use ajc-load-all-sorted-clas-items-to-memory
to sort the class section and load it in memory and build a index for it,
limit : length of class-prefix must larger than 2"
  (with-current-buffer (get-buffer ajc-tmp-sorted-class-buffer-name)
    (let ((matched-class-items) (case-fold-search nil)
          (regexp-class-prefix (if exactly_match
                                   (concat "^" class-prefix "`")
                                 (concat "^" class-prefix ) ))
          (two-char-item  (or  (ajc-get-two-char-item (substring-no-properties class-prefix 0 2) )
                               (list nil (point-min) (point-max)) )))
      (goto-char (nth 1 two-char-item))
      (while (re-search-forward regexp-class-prefix (nth 2 two-char-item) t )
        (add-to-list 'matched-class-items
                     (ajc-split-class-item
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))) t))
      matched-class-items
      )))


(defun ajc-sort-class ()
  "sort class for search ,we build a table for example ((Ab 1 3) (Ac 4 6))
then we search AbstractC ,we just need to search line number from 1 3 "
  (let ((case-fold-search nil))
    (with-current-buffer (get-buffer-create ajc-tmp-sorted-class-buffer-name)
      (erase-buffer)
      (insert-buffer-substring (ajc-reload-tag-buffer-maybe)
                               ajc-position-of-class-first-line ajc-position-of-member-first-line)
      (sort-lines nil 1 (point-max))
      (let ((end ?Z) (index ?A) (index2 ?A)  (two-char)
            (two-char-item)(next-start-search-postion))
        (setq ajc-two-char-list nil)
        (while  (<= index end) (setq index2 ?A)
                (while ( <= index2 ?z)
                  (setq two-char (concat (char-to-string index) (char-to-string index2)))
                  (setq two-char-item
                        (ajc-build-map-4-search-class
                         two-char ajc-tmp-sorted-class-buffer-name (or next-start-search-postion 1)))
                  (if two-char-item
                      (add-to-list 'ajc-two-char-list  two-char-item  t)
                    (setq next-start-search-postion (nth 2 two-char-item)))
                  (if (= index2 ?Z) (setq index2 ?a) (setq index2 (+ index2 1))))
                (setq index (+ index 1)))))))

(defun ajc-build-map-4-search-class (two-char-prefix ajc-tmp-sorted-class-buffer-name  start-search-postion)
  "suppose two-char-prefix is 'Ab' and ajc-tmp-sorted-class-buffer-name is the buffer
 ,all lines in it is the classname has been sorted by classname
it is cut from tag file between ajc-class-first-ln and ajc-member-first-ln ,and sorted by (sort-lines)
then this function is try to find out className begin with two-char-prefix ,and got the start position
and end position ,record in a list ,when search class name begin with two-char-prefix ,we just need to
find it from the start position to the end position ,it is faster than directly searching the unsorted
tag buffer file "
  (with-current-buffer  ajc-tmp-sorted-class-buffer-name
    (goto-char start-search-postion)
    (let ((char1 (string-to-char (substring-no-properties two-char-prefix 0 1)))
          (char2 (string-to-char (substring-no-properties two-char-prefix 1 2)))
          start end has-found-first return-item end-position end-prefix-regexp  case-fold-search)
      (if (or  (= char1 ?Z)  (= char2 ?z) (= char2 ?Z))
          (setq end-position (line-number-at-pos (point-max)))
        (progn
          (if (< char2 ?a)
              (setq end-prefix-regexp (concat  "^" (char-to-string char1)
                                               "[a-z" (char-to-string (+ 1 char2)) "-Z]\\|^" (char-to-string (+ char1 1)) "[a-zA-Z]"  ))
            (setq end-prefix-regexp (concat "^" (char-to-string char1)
                                            "[" (char-to-string (+ 1 char2)) "-z]\\|^" (char-to-string (+ char1 1)) "[a-zA-Z]"  )))
          (goto-char start-search-postion)
          (if (re-search-forward end-prefix-regexp  (point-max) t)
              (setq end-position (point))
            (setq end-position (point-max)))))
      (goto-char start-search-postion)
      (while (re-search-forward (concat "^" two-char-prefix ) end-position t )
        (when (not has-found-first)
          (setq has-found-first t)
          (setq start (line-beginning-position)))
        (setq end (line-end-position)))
      (when (numberp start) (setq return-item (list two-char-prefix start end)))
      return-item)))

;; (defun ajc-load-all-sorted-class-items-to-memory()
;;   (ajc-sort-class);;first sort the class ,and populate ajc-two-char-list variable
;;   (with-current-buffer  ajc-tmp-sorted-class-buffer-name
;;     (goto-char (point-min))
;;     (let ((max-line-num (line-number-at-pos (point-max)))(line-num 1))
;;       (while  (< line-num max-line-num)
;;         (add-to-list 'ajc-all-sorted-class-items
;;                      (ajc-split-class-item (ajc-read-line line-num)) t)
;;         (setq line-num (+ line-num 1)))))
;;   (kill-buffer (get-buffer  ajc-tmp-sorted-class-buffer-name)))

(defun ajc-import-package-candidates()
  "this function is the candidates , so you can bind it with a key sequence
  it will return a list, for example `( java.lang ,java.ref)"
  (save-excursion
    (let ((prefix-string) (matched-pkg-strings))
      (setq case-fold-search nil)
      (when
          (re-search-backward ;;search import string in java file or jsp file ,now support jsp
           "\\(?:\\(?:import=\"\\(?:.*[ \t\n]*,[ \t\n]*\\)*\\)\\|\\(?:import[ \t]+\\)\\)\\([a-zA-Z0-9_\\.]*\\)"
           nil t)
        (setq prefix-string (match-string-no-properties 1 ))
        (when (and ajc-matched-import-cache-list  ;;first try completion from cache
                   (string-match (concat "^"  ajc-previous-matched-import-prefix   ) prefix-string ))
          (setq matched-pkg-strings (all-completions prefix-string ajc-matched-import-cache-list)))
        (when (= (length matched-pkg-strings ) 0 ) ;;if there are 0 matched in cache then find it out from tag files
          (setq matched-pkg-strings ;;add pkgs
                (append matched-pkg-strings
                        (ajc-shrunk-matched-pkgs prefix-string)))
          (let ((index_of_last_dot (string-match "\\.[a-zA-Z_0-9]*$" prefix-string));;add classes
                 (package-prefix)(class-prefix))
            (when index_of_last_dot
              (setq package-prefix (substring-no-properties prefix-string 0 index_of_last_dot))
              (setq class-prefix (substring-no-properties prefix-string (+ 1 index_of_last_dot)))
              (dolist (element (ajc-find-out-matched-class-item package-prefix class-prefix))
                (add-to-list 'matched-pkg-strings (concat package-prefix "." (car element)))))))
;;        (setq ajc-is-importing-packages-p t)
        (setq ajc-previous-matched-import-prefix prefix-string) ;;
        (setq  ajc-matched-import-cache-list matched-pkg-strings)))))

(defun ajc-find-out-class-by-parse-source ()
  "find out class in current  java source file, then will import  them if they haven't been imported   "
  (save-excursion
    (save-match-data
      (let ((matched-class-strings)
            (return-type-regexp  "\\(\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\| *\t*\\[ *\t*]\\)+\\)" )
            (split-char-regexp "\\(,\\|<\\|>\\|]\\|\\[\\| \\|\t\\|\n\\)"));;a list of split char like ", \t<>[]"
       (goto-char (point-min))  (setq case-fold-search nil)
        (while (search-forward-regexp (concat "\\bnew[ \t]+" return-type-regexp)(point-max) 't)
          (setq matched-class-strings (append matched-class-strings  (split-string (match-string-no-properties 1 ) split-char-regexp t))))
       (goto-char (point-min))
        (while (search-forward-regexp "\\b\\([A-Z][a-zA-Z0-9_]*\\)\\.[a-zA-Z0-9_]+[ \t]*(" (point-max) 't)
          (setq matched-class-strings (append matched-class-strings  (list (match-string-no-properties 1)))))
        (goto-char (point-min))
        (while (search-forward-regexp "\\([a-zA-Z0-9_]+\\)\\.getInstance[ \t]*(" (point-max) 't)
          (add-to-list 'matched-class-strings (match-string-no-properties 1)))
       (goto-char (point-min))
        ;;find out all statement of variable ,for example
        ;; String name;      Map<String,<String,Ojbect>>[] map=
        (while (search-forward-regexp "^[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*\\([A-Z]\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\| *\t*\\[ *\t*]\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*[;=]"  (point-max) 't)
          (setq matched-class-strings
                (append matched-class-strings  (split-string (match-string-no-properties 2 ) split-char-regexp  t))))
       (goto-char (point-min));; find ClassName after "catch" keywords  for example :catch(IOException e )
        (while   (search-forward-regexp "catch[ \t]*(\\([a-zA-Z0-9_]+\\)[ \t]+"  (point-max) 't)
          (add-to-list 'matched-class-strings (match-string-no-properties 1)))
        (goto-char (point-min)) ;;find method statement
        (while   (search-forward-regexp "^[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{"  (point-max) 't)
          (let ((exception (match-string-no-properties 11))
                (returns (match-string-no-properties 2))
                (params (match-string-no-properties 9)))
            ;;handle return type
            (setq matched-class-strings (append matched-class-strings  (split-string  returns "\\(,\\|<\\|>\\|]\\|\\[\\| \\|\t\\)"  t)))
;;;;handle methods parameters  ;;find out 'Map String Ojbect User' from "Map<String,Object> map,User user"
            (while (and params (> (length params) 0))
              (if (string-match "\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*>\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+" params)
                  (progn (setq matched-class-strings
                          (append matched-class-strings (split-string (match-string-no-properties 0 params ) split-char-regexp t)))
                    (string-match "[ \t]*[a-zA-Z0-9_]+[ \t,]?" params  (match-end 0 ))
                    (setq params (substring-no-properties params  (match-end 0 ))))
                (setq params nil)))
            ;;handle throws Exception1,Exception2, we will exatract Exception1 Exception2 from throws sentence
       (when exception
            (setq matched-class-strings (append matched-class-strings (split-string  exception split-char-regexp t))))))
         ;;remove primitive type and remove duplicate item
        (delete-dups matched-class-strings) (delete "" matched-class-strings)
        (dolist (ele matched-class-strings)
          (if (string-match  "\\(int\\|float\\|double\\|long\\|short\\|char\\|byte\\|void\\|boolean\\|return\\|public\\|static\\|private\\|protected\\|abstract\\|final\\|native\\|package\\|new\\)" ele)
              (delete ele matched-class-strings)))
       matched-class-strings
        ))))

(defun ajc-caculate-all-unimported-class-items()
  "this function will find out all unimported Class itmes , it just do a subtration
   (ajc-find-out-class-by-parse-source) -(ajc-caculate-all-imported-class-items)
what you need to do next, is just import the unimported class  "
  (let ((imported-class-names (mapcar 'car (ajc-caculate-all-imported-class-items)))
        (class-names-in-source (ajc-find-out-class-by-parse-source))
        (unimported-class-items))
    (print class-names-in-source)
    (dolist (ele class-names-in-source)
      (unless (member ele imported-class-names)
        (setq unimported-class-items
              (append unimported-class-items
                      (ajc-find-out-matched-class-item-without-package-prefix ele t)))))
    unimported-class-items))

(defun ajc-import-all-unimported-class()
  "import all unimported class ."
  (interactive)
    (ajc-insert-import-at-head-of-source-file
     (ajc-caculate-all-unimported-class-items)))

(defun ajc-import-class-under-point ()
  "import class under point."
  (interactive)
  (let ((cur-word (current-word)))
    (when (and cur-word  (> (length cur-word) 0))
      (when (string-match "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)$" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word )))
      (when (string-match "^\\([a-zA-Z0-9_]+\\)[^a-zA-Z0-9_]" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word)))
    (ajc-insert-import-at-head-of-source-file
     (ajc-find-out-matched-class-item-without-package-prefix cur-word t)))))

(defun ajc-insert-import-at-head-of-source-file (import-class-items-list)
  "insert 'import sentence' at head of java source file,
before that it will use y-or-n-p ask user to confirm "
  (let ((import-class-buffer "*ajc-import-java-class*")
        (import-class-window) (user-confirmed-class-items-list)
        (java-buffer (current-buffer))(java-window))
    (setq case-fold-search nil)
    (if (and import-class-items-list (> (length import-class-items-list) 0))
        (progn
          (setq import-class-buffer (switch-to-buffer-other-window  import-class-buffer t))
          (setq java-window (get-buffer-window java-buffer))
          (setq import-class-window (get-buffer-window import-class-buffer))
          (with-current-buffer    import-class-buffer  ;;show maybe imported Class in a new buffer
            (delete-region (point-min)(point-max))
            (dolist (ele import-class-items-list)
              (insert (concat "[ ]  "  (car (ajc-split-pkg-item-by-pkg-ln  (nth 1 ele ))) "." (car ele)  "\n")))
            (insert "  ");;insert empty line at end of buffer
            (goto-char (1+(point-min)))
            (dolist (ele import-class-items-list ) ;;ask user whether to import the Class
              (beginning-of-line)(forward-char 1)
              (when (y-or-n-p (concat "import " (car ele)  "? "))
                (add-to-list 'user-confirmed-class-items-list ele)
                (delete-char 1) (insert "*"))
              (forward-line 1)(forward-char 1)))
          ;;delete *import-java-class* buffer and window
          (delete-window import-class-window)(kill-buffer import-class-buffer)
          (with-current-buffer java-buffer
            (ajc-insert-import-at-head-of-source-file-without-confirm user-confirmed-class-items-list))
          (message "Finished importing.")
          user-confirmed-class-items-list)
      (message "No class need import."))))


(defun ajc-insert-import-at-head-of-source-file-without-confirm (class-items)
(save-match-data  ;;insert  at head of java source
      (setq case-fold-search nil)
  (save-excursion   (goto-char (point-min))
    (let* ((class-start (save-excursion
                (re-search-forward
                 "\\(\\b\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)"  nil 't))))
      (if (not class-start);; then this is a jsp file
          (let ((all-class-strings ""))
            (dolist (class-item class-items)
              (setq all-class-strings
                    (concat all-class-strings
                            (car (ajc-split-pkg-item-by-pkg-ln  (nth 1 class-item ))) "." (car class-item)
                            ",")))
            (unless (string-equal "" all-class-strings);;delete last char ","
                (setq all-class-strings (substring all-class-strings 0 (1- (string-width all-class-strings)))))
            (goto-char (point-min))
            (insert (concat "<%@ page import=\"" all-class-strings "\" %>\n"  )))
        (if(re-search-forward "^[ \t]*import[ \t]+[a-zA-Z0-9_\\.\\*]+[ \t]*;" class-start 't)
          ;;if find 'import' insert before it
          (progn (beginning-of-line )(insert "\n")(forward-line -1)
             (dolist (ele class-items)(insert
                                      (concat "import "
                                             (car (ajc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                             "." (car  ele) ";\n"))))
        ;; if hasn't found 'import; then insert after 'package ' statement
        (progn (goto-char (point-min))
               (if (re-search-forward "^[ \t]*package[ \t]+[a-z0-9_\\.]+[ \t]*;" class-start 't)
                   (progn (forward-line 1) (beginning-of-line)(newline)
                    (dolist (ele class-items)
                            (insert (concat "import "
                                             (car (ajc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                             "." (car  ele) ";\n"))))
                 (progn ;;if hasn't found 'import' and 'package' then insert at head of buffer
                   (goto-char (point-min))
                (dolist (ele class-items)
                         (insert (concat "import "
                                           (car (ajc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                            "." (car  ele) ";\n")))))))
           )))))

(defun ajc-find-out-import-line ()
 "make a regex to match the packages in the import statements ,
return a list of each line string (exclude keyword 'import') "
  (let ((imported-lines))
    (save-match-data (save-excursion
        (goto-char (point-min))
        (setq case-fold-search nil)
        (let ((class-start (save-excursion
           (re-search-forward
            "\\(\\b\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[\n \t]*\\({\\|extends\\|implements\\)\\)" nil 't))))
          (if class-start ;;if found class or interface key words ,then this is a java file  ,if not  it is a jsp file
              (while (re-search-forward "^[ \t]*import[ \t]+\\([a-zA-Z0-9_\\.\\*]+\\)[ \t]*;" class-start 't)
                (add-to-list 'imported-lines  (match-string-no-properties 1))
                (end-of-line))
            ;;may be this is a jsp file
            (while (re-search-forward "\\bimport=\"\\(.*?\\)[ \t]*\"[ \t]+"  (point-max) 't)
              (setq imported-lines (append imported-lines  (split-string (match-string-no-properties 1) "[ \t,]" t)))
              (end-of-line))))))
    imported-lines ))

(defun ajc-caculate-all-imported-class-items (&optional exclude_java_lang)
  "find out all imported class  ,default include class in java.lang.*"
  (let ((imported-line (ajc-find-out-import-line))(element)(index)  (return-class-items))
    (setq case-fold-search nil)
    (dolist ( element imported-line )
      (setq index   (string-match "\\.\\*$"  element))
      (if index   ;;import a package
         (setq return-class-items (append return-class-items
                     (ajc-find-out-matched-class-item (substring-no-properties element 0 index) nil)))
        (progn  ;;import a class
          (string-match "^\\(.+\\)\\.\\([a-zA-Z0-9_]+\\)$" element)
          (setq return-class-items (append return-class-items
                (ajc-find-out-matched-class-item
                      (match-string-no-properties 1 element ) (match-string-no-properties 2 element )  t ))))))
    (if exclude_java_lang
        (setq return-class-items return-class-items )
      (setq return-class-items  (append return-class-items  (ajc-find-out-matched-class-item "java.lang" nil )))
      )))
(defun ajc-complete-constructor-candidates ()
  (let (candidates class-items);;if find keyword:new ,then do constructor complete ,if not do class complete
    (setq case-fold-search nil)
    (when (looking-back "\\bnew[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)[ \t]*(?[ \t]*"  (line-beginning-position))
      (setq class-items (ajc-complete-class-with-cache (match-string-no-properties 1)))
      (dolist (class-item class-items)
        (setq candidates (append candidates (ajc-complete-constructor (car class-item))))
        ))
    candidates
    ))

(defun ajc-complete-constructor (class-prefix)
  (let ((matched-class-items (ajc-find-out-matched-class-item-without-package-prefix class-prefix t))
        (matched-constructor-items) (return-complete-list))
    ;;when matched class > 1 ,then ask user to import one of them ,
    ;;then we can got the imported class item , we complete its constructor
    (dolist (matched-class-item matched-class-items)
      (let ((line-num     (nth 2  matched-class-item))  (matching-constructor t)
            (end-position (nth 3  matched-class-item)) (current-line))
        (while (and matching-constructor  (< line-num end-position ))
          (setq current-line (ajc-read-line line-num (ajc-reload-tag-buffer-maybe)))
          (if (string-match "^  "  current-line)
              (add-to-list 'matched-constructor-items (ajc-split-constructor current-line))
            (setq matching-constructor nil))
          (setq line-num (+ line-num 1)))
        (dolist (constructor matched-constructor-items)
          (let ((constructor-full-string (ajc-constructor-to-string constructor t))
                (constructor-short-string (ajc-constructor-to-string constructor nil)))
            (add-to-list 'return-complete-list  constructor-short-string t)
            (setplist 'props nil )
            (put 'props 'view constructor-full-string)
            (put 'props 'templete (ajc-constructor-to-yasnippet-templete constructor))
            (add-text-properties 0 (length constructor-short-string)
                                 (symbol-plist  'props)  constructor-short-string)
            ))))
    return-complete-list))

(defun ajc-is-available-4-complete-class-p ()
  "only when this function return t ,then ajc-complete-class-candidates
    will try to find out candidates  "
  (let ((class-prefix (current-word)) (is-available))
    (setq case-fold-search nil)
        (when  (and class-prefix  (> (length class-prefix ) 0) (string-match "^[A-Z][a-zA-Z0-9_]*$" class-prefix))
           (setq ajc-current-class-prefix-4-complete-class class-prefix)
           (setq is-available t))
    is-available
    ))


(defun ajc-complete-class-candidates ()
  "complete class name with (current-word) as class-prefix"
  (when (ajc-is-available-4-complete-class-p)
    (let ((candidate)(candidates)
          (class-items (ajc-complete-class-with-cache ajc-current-class-prefix-4-complete-class)))
      (dolist (class-item class-items)
        (setq candidate  (car class-item))
        (setplist 'props nil )
        (put 'props 'view (ajc-class-to-string class-item t))
        (add-text-properties 0 (length candidate) (symbol-plist  'props)  candidate)
        (add-to-list 'candidates candidate t)
     ) candidates
      )
    ))

(defun ajc-complete-class-with-cache ( class-prefix )
  "find out class name starts with class-prefix ,before search tag file ,it first
check out ajc-matched-class-items-cache to find out if ant matched class exists "
  (let ((return-list))
    (setq case-fold-search nil)
    (when  (and class-prefix   (string-match "[A-Z][a-zA-Z0-9_]*" class-prefix))
      (if (and ajc-previous-class-prefix   (string-match (concat "^" ajc-previous-class-prefix ) class-prefix ))
          (dolist (class-item ajc-matched-class-items-cache )
            (if (string-match (concat "^" class-prefix) (car class-item)) (add-to-list 'return-list class-item t)))
        (setq return-list  (ajc-find-out-matched-class-item-without-package-prefix class-prefix)))
      (when (> (length return-list) 0);; if find matched ,update cache ,or not
        (setq ajc-previous-class-prefix class-prefix)
        (setq ajc-matched-class-items-cache return-list )))
    return-list;; return
))

(defun ajc-build-list-with-nth-on-each-element (list index  )
  "if params : list= '((1 11 111) (2 22 222)) index=1 then return '(11 22 ) "
  (let ((return-list))
    (dolist (ele list)
      (add-to-list 'return-list (nth index ele) t))
     return-list;;return
 ))

(defun ajc-find-members (class-item  &optional member-prefix &optional exactly_match)
  "find members(field method) under class-item which member name match member-prefix ,
if member-prefix is nil or empty string it will return all members under class-item"
  (let ((line-num (nth 2 class-item))  (end-position (nth 3 class-item)) (return-member-items)
       (regexp-method-prefix)(regexp-field-prefix) (current-line-string ))
       (if exactly_match
            (setq regexp-method-prefix (concat "^" member-prefix "`")
                  regexp-field-prefix (concat "^ " member-prefix "`"))
         (if (or (not member-prefix)  (string-equal "" member-prefix))
            (setq regexp-method-prefix "^[a-zA-Z0-9_]" regexp-field-prefix "^ [^ ]" )
            (setq regexp-method-prefix (concat "^" member-prefix )
                  regexp-field-prefix (concat "^ " member-prefix ))))
       (with-current-buffer (ajc-reload-tag-buffer-maybe)
         (while (< line-num  end-position)
           (setq current-line-string (ajc-read-line line-num))
           (if (string-match regexp-method-prefix current-line-string)
               (add-to-list 'return-member-items (ajc-split-method current-line-string ) t)
             (when (string-match regexp-field-prefix current-line-string)
                 (add-to-list 'return-member-items (ajc-split-field current-line-string ) t)))
           (setq line-num (+ line-num 1))))
        return-member-items))

(defun ajc-caculate-class-name-by-variable(variable-name)
  "this function is used to find Class name depend on a varibale name ,for example
 the varibale-name is str ,then if exists 'String str' in source file , String will be returned "
  (let ((matched-class-name) (variable-line-string) (index-of-var-in-line) (var-stack))
    (setq case-fold-search nil)
    (save-excursion
      (if (search-backward-regexp  (concat "\\b\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\)*[ \t]+"   variable-name "\\b")   (point-min) t )
          (setq variable-line-string (ajc-read-line  ( line-number-at-pos (point))))
        (when (search-forward-regexp
               (concat "\\b\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\)*[ \t]+"   variable-name "\\b")
               (point-max) t)
          (setq variable-line-string (ajc-read-line  ( line-number-at-pos (point))))
    )))
    (when variable-line-string
      (setq index-of-var-in-line  (string-match  (concat "[ \t]+" variable-name "\\b")  variable-line-string))
      (setq variable-line-string (substring-no-properties  variable-line-string 0  index-of-var-in-line   ))
      (setq var-stack (split-string variable-line-string "[( \t]" t))
      (let ((tmp-list))
        (dolist (ele var-stack)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "<"  "<"  t))))
        (setq var-stack tmp-list))
      (let ((tmp-list))
        (dolist (ele var-stack)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele ">"  ">"  t))))
        (setq var-stack tmp-list))
      (setq var-stack (nreverse var-stack ))
      (let ((top (pop var-stack)) (parse-finished ))
        (while  (and top (not parse-finished))
          (when (string-match "[A-Z][a-zA-Z0-9_]*" top )
            (setq matched-class-name top)   (setq parse-finished t));; parse finished ,exit the  loop
          (when (string-equal ">" top)
            (let ((e)(right-stack))
              (push top  right-stack)
              (setq e (pop var-stack))
              (while (and e (  > (length right-stack) 0))
                (if (string-equal "<" e ) (pop right-stack))
                (if (string-equal ">" e ) (push e right-stack))
                (setq e (pop var-stack)))
              (if e (push e var-stack))))
          (setq top (pop var-stack))))
      ) matched-class-name))

;;TODO: add cache support for method candidates
;; if it failed ,then don't try, to waste time.

;;(ajc-concat-list-as-string '("a" "b"))=="ab"
(defun ajc-concat-list-as-string(list)
  "(ajc-concat-list-as-string '(\"a\" \"b\"))==\"ab\""
  (let((str ""))
    (dolist (ele list)
      (setq str (concat str ele)))
    str))


(defvar ajc-complete-method-candidates-cache nil)
(defvar ajc-complete-method-candidates-cache-stack-list nil)

(defun ajc-complete-method-is-available()
  "check whether method completion is available or not ,
suppose previous (current-line)==\"Systema.aaa\"
but it failed to get any candidates,
and now (current-line)==\"Systema.aaab\" It would
 not get any candidates too ,we needn't try to complete it ."
  (let ((stack-list ( ajc-get-validated-stack-list-or-nil-4-method-complete
                     (ajc-parse-splited-line-4-complete-method)))
        (is-available t)
        )
    (when (and  ajc-complete-method-candidates-cache-stack-list
                (string-match (concat "^" (regexp-quote  (ajc-concat-list-as-string ajc-complete-method-candidates-cache-stack-list)))
                              (ajc-concat-list-as-string stack-list))
                (not  ajc-complete-method-candidates-cache))
      (setq is-available nil))
    (setq ajc-complete-method-candidates-cache-stack-list stack-list)
    is-available))

(defun ajc-complete-method-candidates()
  "Get method completion candidates."
  (when (ajc-complete-method-is-available)
    (setq ajc-complete-method-candidates-cache
          (ajc-complete-method-candidates-1 ajc-complete-method-candidates-cache-stack-list))))

;;
;; (defun ajc-complete-method-candidates-with-cache()
;;  "this function works ,but I found it useless"
;;   (let ((candidates)
;;         (stack-list (ajc-get-validated-stack-listd-stack-or-nil-4-method-complete
;;                      (ajc-parse-splited-line-4-complete-method)))
;;         )
;;     (if ajc-complete-method-candidates-cache-stack-list
;;         (if (string-equal "." (car (last stack-list)))
;;             (setq candidates (ajc-complete-method-candidates-without-cache));;complete without cache
;;           (if (and (= (length stack-list) (length ajc-complete-method-candidates-cache-stack-list))
;;                      (string-match (concat "^" (regexp-quote  (ajc-concat-list-as-string ajc-complete-method-candidates-cache-stack-list))) (ajc-concat-list-as-string stack-list)))
;;                      (setq candidates ajc-complete-method-candidates-cache)
;;                      (setq candidates (ajc-complete-method-candidates-without-cache));;complete without cache
;;                  ))
;;       (setq candidates (ajc-complete-method-candidates-without-cache));;else complete without cache
;;       )
;;       (setq ajc-complete-method-candidates-cache-stack-list stack-list)
;;       (setq ajc-complete-method-candidates-cache candidates)
;;       ))


(defun ajc-complete-method-candidates-1(stack-list)
  "get method candidates depend on stack-list, about
 what stack-list it is,check out
 `ajc-parse-splited-line-4-complete-method'"
  (when stack-list
    (let( (is-dot-last  (= (% (length stack-list ) 2 ) 0))
          top return-list return-string-list)
      (setq stack-list (remove "." stack-list ))
      (setq  top (pop stack-list))
      (let ((class-item ))
        (if (string-match "^[A-Z][a-zA-Z0-9_]*$" top)
            (setq class-item (ajc-find-class-first-check-imported  top))
          (setq class-item (ajc-find-class-first-check-imported (ajc-caculate-class-name-by-variable top))))
        (while  (and class-item (> (length stack-list ) 1))
          (setq class-item  (nth 1 (car  (ajc-find-members class-item (pop stack-list) t))))
          )
        (if is-dot-last (let ((member-string (pop stack-list)))
                          (if member-string
                              (setq class-item  (nth 1 (car  (ajc-find-members class-item member-string t)))))
                          (setq return-list (ajc-find-members class-item   )))
          (setq return-list (ajc-find-members class-item   (pop stack-list)))))
      (mapcar  'ajc-method-item-to-candidate return-list)
      )))


(defun ajc-get-validated-stack-list-or-nil-4-method-complete(stack-list)
  "if stack-list is validated ,return itself ,else return nil."
  (when (and stack-list (> (length stack-list) 0))
    (let ((current-item (car stack-list)) (validated-stack-list stack-list) (index 1)(next-item))
      (if (and (> (length stack-list) 1)  (string-match "^[a-zA-Z0-9_]+$" current-item ))
          (while (and validated-stack-list current-item)
            (setq next-item (nth index stack-list))
            (if (string-match "^[a-zA-Z0-9_]+$" current-item )
                (if next-item (if (not (string-equal "." next-item)) (setq validated-stack-list nil)   )))
            (if (string-equal "." current-item )
                (if next-item (if (not (string-match "^[a-zA-Z0-9_]+$" next-item)) (setq validated-stack-list nil)   )))
            (setq current-item next-item)
            (setq index (+ 1 index)))
        (setq validated-stack-list nil))
      validated-stack-list)))

(defun ajc-parse-splited-line-4-complete-method ()
  " parse current line  for complete method  ,suppose current line is
System.getProperty(str.substring(3)).to
first ajc-split-line-4-complete-method will split this line to
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to'
ajc-remove-unnecessary-items-4-complete-method will remove anything between ( and )  ,so only
'System'  '.' 'getProperty'  '.'  'to'  is left "
  (let* ((line-string (buffer-substring-no-properties (line-beginning-position) (point)))
          (splited-line-items (ajc-split-line-4-complete-method line-string)))
    (ajc-remove-unnecessary-items-4-complete-method splited-line-items )))

(defun ajc-remove-unnecessary-items-4-complete-method (splited-line-items)
" System.getProperty(str.substring(3)).to
first ajc-split-line-4-complete-method will split this line to
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to'
this function will remove anything between ( and )  ,so only
'System'  '.' 'getProperty'  '.'  'to'  is left "
  (let* (  (stack-list)(ele) (reverse-current-line-split-list  (reverse splited-line-items)) (parse-finished))
    (setq ele (pop reverse-current-line-split-list))
    (while  (and ele (not parse-finished))
      (if  (or (string-equal ";" ele) (string-equal "(" ele )) (setq parse-finished t);; parse finished ,exit the  loop
          (if (string-equal ")" ele)
              (let ((e)(right-stack))
                (push ele  right-stack)
                (setq e (pop reverse-current-line-split-list))
                (while (and e (  > (length right-stack) 0))
                  (if (string-equal "(" e ) (pop right-stack))
                  (if (string-equal ")" e ) (push e right-stack))
                  (setq e (pop reverse-current-line-split-list)))
                (if e    (push e reverse-current-line-split-list)))
              (push ele stack-list)
            ))
      (setq ele (pop reverse-current-line-split-list)))
    (setq stack-list stack-list)
      ))
;; (defun ajc-replace-keyword-with-its-class-name()
;;   (save-excursion
;;     (let ((class-name)))
;;     (setq case-fold-search nil)
;;     (if (search-backward-regexp  "\\bclass[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)\\b"   (point-min) t )
;;         (setq class-name (match-string-no-properties 1 ))
;;       ))
;;   )
(defun ajc-split-line-4-complete-method(line-string  )
  "this function is used to complete method ,first this function will split line-string to small items
for example : suppose line-string is
System.getProperty(str.substring(3)).to
then this function split it to
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to' "
  (save-excursion
    (let* (  (stack-list nil))
      (setq case-fold-search nil)
        (setq line-string  (replace-regexp-in-string   "\\\\\"" "'"       line-string))
        (setq line-string  (replace-regexp-in-string   "\".*?\"" "String" line-string))
        (setq line-string  (replace-regexp-in-string   "\\bnew\\b"    ""  line-string))
        (setq line-string  (replace-regexp-in-string   "\\breturn\\b" ""  line-string))
        (setq line-string  (replace-regexp-in-string   "\\this\\b" ""  line-string))
        (while (string-match "=\\(.*\\)" line-string)
          (setq line-string (match-string-no-properties 1 line-string)))
       ;;split line-string with "." ,but add "." as an element at its position in list
      (setq stack-list (ajc-split-string-with-separator  line-string "[ \t]*\\.[ \t]*"  "." t))
       ;;split each element  with "(" ,but add "(" as an element at its position in list
      ;;and merge all the list in a list
      (let ((ele)(tmp-list))
           (dolist (ele stack-list)
            (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "("  "("  t))))
           (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele ")"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "\\["  "("  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "]"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "{"  "("  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "}"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele "<"  "("  t))))
        (setq stack-list tmp-list))
      (let((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele ">"  ")"  t))))
        (setq stack-list tmp-list))
            (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele ","  ";"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ajc-split-string-with-separator ele ";"  ";"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (split-string ele "[ \t]+"  t))))
        (setq stack-list tmp-list))
      (setq stack-list stack-list )
      ))
  )

(defun ajc-java-keywords-candidates ()
  (let ((keywords))
            (setq keywords (list "public" "protected"  "private" "native" "final" "synchronized" "transient" "abstract"  "static" "import" "this" "if" "else" "else if" "break" "case" "switch"  "continue" "class" "interface" "package" "new" "try" "catch" "finally" "super" "void"  "int" "float" "double" "short" "char" "byte" "long" "boolean" "enum" "instanceof"  "for" "while" "throw" "throws"  "extends" "implements" ))
    ))
(provide 'ajc-java-complete)

;; End.
