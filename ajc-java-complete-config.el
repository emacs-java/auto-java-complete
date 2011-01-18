;;; ajc-java-complete-config.el --- Auto Java Completion  for GNU Emacs
        
(require 'auto-complete)
(require 'yasnippet)
(require 'ajc-java-complete)
;; conflect with 
;; (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; when complete constructor 

(ajc-init)
;;hooks 
(defun ajc-java-complete-hook ()
  (ajc-init-when-load-first-java-file)
    (setq ac-sources (append 
                      '(ac-source-ajc-import
                        ac-source-ajc-constructor 
                        ac-source-ajc-class
                        ac-source-ajc-method
                        ac-source-ajc-keywords ) ac-sources))
;; auto import all Class in source file    
(local-set-key (kbd "C-c i") (quote ajc-import-all-unimported-class))
;; import Class where under point 
(local-set-key (kbd "C-c m") (quote ajc-import-class-under-point))
    )

(add-hook 'java-mode-hook 'ajc-java-complete-hook t)
;(add-hook 'emacs-lisp-mode-hook 'ajc-java-complete-hook)

;;    if you want Auto Java Complete works  when you edit
;;    jsp file ,you just need to do something like this
;;   (add-hook 'nxml-mode-hook 'ajc-java-complete-hook t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;add support for jsp when import ,but you should trigger it by key-binding
;;for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;<%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
(defun prefix-support-jsp-importing ()
  (when   (re-search-backward "\\(import=\"\\(.*[ \t\n]*,[ \t\n]*\\)*\\)\\|\\(import[ \t]+\\)"  nil t)
    (match-end 0))
  )
;; sources for auto complete
(ac-define-source ajc-import
  '((candidates . (ajc-import-package-candidates))
;;    (prefix . "\\bimport[ \t]+\\(.*\\)")
    (prefix . prefix-support-jsp-importing )
    ))

(ac-define-source ajc-class
  '((candidates . (ajc-complete-class-candidates ))
   (prefix . "\\b\\([A-Z][a-zA-Z0-9_]*\\)")
   (cache)
))

(ac-define-source ajc-constructor
  '((candidates . (ajc-complete-constructor-candidates ))
   (cache)
   (prefix . "\\bnew[ \t]+\\([A-Z][a-zA-Z0-9_]*[ \t]*(?\\)")
   (action . ajc-expand-yasnippet-templete-with-ac)
))

(ac-define-source ajc-method
  '((candidates . (ajc-complete-method-candidates ))
  (cache)
  (requires . 0)
  (prefix . "\\.\\(.*\\)") 
  (action .  ajc-expand-yasnippet-templete-with-ac)
))

(ac-define-source ajc-keywords
  '((candidates . (ajc-java-keywords-candidates))
) )


;; end of sources

(defun ajc-expand-yasnippet-templete-with-ac ()
  (let* ((last-complete-string (cdr ac-last-completion))
         (yasnippet-templete (get-text-property 0 'templete last-complete-string)))
    (when  yasnippet-templete
      (delete-backward-char (length last-complete-string))
      (yas/expand-snippet yasnippet-templete))))

(provide 'ajc-java-complete-config)


