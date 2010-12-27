;;;this is "Auto Java Complete". 

;;; Summary

;;1. it depends on auto complete ,so it would complete
;;   everything by dropdowning a menu.

;;2. it is tag based . so before you used it on emacs ,you
;;   should generate a tag file by using Tags.java .
;;   about how to use it ,see the Install section.

;;3. it depends on yasnippet . when completing method and
;;   constructor it would generate a templete dynamically
;;   so that you can jump from a paramter to another one .

;;4. when completing method ,it can show return type behind
;;   each candidate on dropdown menu. but there is a problem:
;;   because auto complete 1.3 now would treat all the string
;;   on the dropdown-menu row as the candidates ,so I must
;;   find a way to delete the return type string . I use the
;;   'action' property of ac-source to do it. so you need
;;   always  press <RET> to active the 'action function'
;;   you can choose do not showing return type by customing
;;   ajc-show-more-info-when-complete-class-and-method  .



;;; Features

;; 1. support importing.
;;    when you type in  import javax.s
;;    it would drop down a menu like this
;;             import  javax.s

                       javax.sql
                       javax.swing
                       javax.sound
                       
;; 2. support import class with keybindings
;;         auto import all Class in source file    
;;    (local-set-key (kbd "C-c i") (quote ajc-import-all-unimported-class))
;;         import Class where under point 
;;    (local-set-key (kbd "C-c m") (quote ajc-import-class-under-point))

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
                                      toString()
                                      getClass()
                                      notify()
                                      
;; 5. support complete constructor
;;    after keyword 'new' it would try to complete constructor


;;; Install

;; 1. generate the tag file .
;;
;;     Tags.java use JDK reflection to generate a tag file by
;;     loading all class in classpath ,what you need to do is
;;     just add your jars to $CLASSPATH. don't drop it in
;;     JRE_HOME/lib/ext/ , the suggestion is
;;     export CLASSPATH=$CLASSPATH:your-jar-path
;;     it need about 3~10 min depending on your jars
;;     during it ,you may see some exceptions ,if it don't kill
;;     the program ,just ignore it .
;;     run 
;;                 javac Tags.java 
;;                 java  Tags
;;
;;     to generate the tag file ~/.java_base.tag 
;;     or
;;                 java  Tags com.yourcompanyname.*
;;
;;     it would only tag those class whose name is starts with
;;     com.yourcompanyname.
;;
;;     if it can't work on you computer ,use my tag file
;;     java_base.tag.bz2, just uncompress and rename it to
;;     .java_base.tag and put it in your home directory.
;;     of course you can change the name by customing
;;                 `ajc-tag-file'


;;  2. you should have installed  auto-complete and yasnippet.
;;     about how to install and config them ,you should find
;;     it on the net.

;;  3. then  add this lines  in .emacs

       (add-to-list 'load-path (   "~/.emacs.d/ajc-java-complete/") )
       (require 'ajc-java-complete-config)

;;     read ajc-java-complete-config.el  for more info .

;;     restart your emacs ,and enjoy.

;;---------------------------------------------
;; you download a Video demo (2.8M) 
 wget --no-check-certificate https://github.com/jixiuf/screencast-repos/raw/master/auto-java-complete-demo-2010-12-25.mp4.bz2    
