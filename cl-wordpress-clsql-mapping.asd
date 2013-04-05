;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;(in-package "ASDF")

(defsystem cl-wordpress-clsql-mapping
  :author "Ben Hyde <bhyde@pobox.com>"
  :license "Apache 2.0"
  :description "clsql:def-view-class definitions for wordpress"
  :serial t
  :depends-on (alexandria clsql clsql-mssql)
  :components ((:file "packages")
               (:file "wp-db-mapping")))

;; Note.  clsql-mysql needs to load the mysql library when it builds. But to do
;; that you need to inform clsql where the library resides.  You do that with
;; clsql-sys:push-library-path.  You need to do that after clsql loads, and befor
;; attempting to load clsql-mysql.  ASDF does not currently provide a easy way
;; to do that.  So for now I'm doing that by hand.

; (clsql-sys:push-library-path (probe-file \"/opt/local/lib/mysql5/mysql/\"))
