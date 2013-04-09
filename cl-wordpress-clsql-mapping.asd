;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem cl-wordpress-clsql-mapping
  :author "Ben Hyde <bhyde@pobox.com>"
  :license "Apache 2.0"
  :description "clsql:def-view-class definitions for wordpress"
  :serial t
  :depends-on (alexandria cl-ppcre clsql)
  :components ((:system clsql-mysql 
                        :around-compile
                        #'(lambda (thunk) ;; see Note A
                            #+ccl(eval
                                  (read-from-string
                                   "(clsql-sys:push-library-path 
                                      (probe-file \"/opt/local/lib/mysql5/mysql/\"))"))
                            (funcall thunk)))
               (:file "packages")
               (:file "db-mapping")))

;; Note A: too clever, and probably disfunctional, scheme to inform
;; cl-mysql where to find it's libraries.  Note there is 2nd call
;; to push-libary-path in the source code, so it can be found, again, 
;; when clsql loads it at runtime.

