;;; ajc-java-complete.el --- Auto Java Completion fo GNU Emacs
;; This file is NOT part of GNU Emacs
;; plesase send Bug reports and suggestions to 'Joseph at <jixiuf@gmail.com>

;;{{{ License 

;;  License
        
;; Copyright (C) 2010  joseph <jixiuf@gmail.com> Limited

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

;; Firstly I am not an English Speaker ,so forgive my bad English .
;;;this is "Auto Java Complete". 

;;}}}

;;{{{ Summary
;;;this is "Auto Java Complete".
;;; read README.txt  first .

;; you can download a Video demo (2.8M) 

;; actually the default config is in ajc-java-complete.el file ,just load it ,or write 
;; your own config file if you don't want to  use auto-complete.


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
;;
;;      cp  ajc-java-complete/popup-patch.diff auto-complete-1.3/
;;      cd auto-complete-1.3/
;;      patch -p0 < popup-patch.diff
;;}}} 

;;{{{ Features
;; 1. support importing.
;;    when you type in  import javax.s
;;    it would drop down a menu like this
;;             import  javax.s

;;                     javax.sql
;;                     javax.swing
;;                     javax.sound
                       
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
;;                                      toString()
;;                                      getClass()
;;                                      notify()
                                      
;; 5. support complete constructor
;;    after keyword 'new' it would try to complete constructor


;; 6. support completing in jsp files.
      
;;    if you want Auto Java Complete works  when you edit
;;    jsp file ,you just need to do something like this
      
;;      (add-hook 'nxml-mode-hook 'ajc-java-complete-hook t)
      
;;    now it can complete class name,method ,constructor
;;    it also support complete importing ,but it isn't auto completed,
;;    you must trigger it by a key binding
;;    for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;    <%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
;;    now you can  press M-1 to show the menu.
;;    it does not support importing Class(importing Class under point
;;    ,and importing all Class in buffer) by keybinding ,I will try to make
;;    it work later.
;;}}} 

;;{{{ Install

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
;;     after installed auto-complete ,you should do some
;;       patch on auto-complete-1.3/popup.el
;;       tow choice :
;;        1. put the popup.el into auto-complete-1.3/ (recommand)
;;        2. cd auto-complete-1.3/
;;           patch -p0 <popup-patch.diff
            

;;  3. then  add this lines  in .emacs

;;       (add-to-list 'load-path (   "~/.emacs.d/ajc-java-complete/") )
;;       (require 'ajc-java-complete-config)

;;     read ajc-java-complete-config.el  for more info .

;;     restart your emacs ,and enjoy.
;;}}}

;;{{{ History
;; tag  0.2.4
;;      a litter change of tag file.
;;      replace toString`25:784`` to   toString`784`` in tag file
;;      package line 25 is not needn't now .
;;      so the old tag file doesn't work with this version.
;;      you need regenerate it by using Tags.java


;; tag 0.2.3
;;     support importing class under point ,and importing
;;     all class in buffer when editing jsp files 
;;
;; tag 0.2.2
;;     support completion in jsp files.
    
;;     if you want Auto Java Complete works  when you edit
;;     jsp file ,just need to do something like this
    
;;       (add-hook 'nxml-mode-hook 'ajc-java-complete-hook t)
      
;;     now it can complete class name,method ,constructor
;;     it also support complete importing ,but it isn't auto completed,
;;     you must trigger it by a key binding
;;     for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;     <%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
;;     now you can  press M-1 to show the menu.
    
;;     it does not support importing Class(importing Class under point
;;     ,and importing all Class in buffer) by keybinding ,I will try to make
;;     it works later.


;; tag 0.2.1  same to tag 0.2 ,just add comments in README .

;; tag 0.2
;;    add popup.el and popup-patch.diff
;;    support of showing return type behind each method candidate ,
;;    by make a patch on auto-complete-1.3/popup.el
   

;; tag 0.1.1
;;    add support of showing return type behind each method candidate,
;;    by define an advice on  (ac-expand-common) and (ac-selected-candidates)
;;    but if auto-complete.el is byte-compiled ,this advice doesn't work
;;}}}   
;;---------------------------------------------
