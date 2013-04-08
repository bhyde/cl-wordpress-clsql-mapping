(in-package "WORDPRESS")

#+ccl
(clsql-sys:push-library-path (probe-file "/opt/local/lib/mysql5/mysql/"))

(defun trimmed-text (text &optional (max 15))
  (if (< max (length text))
      (format nil "~A..." (subseq text 0 (- max 3)))
      text))

;;;; Some globals that record the blog who's database we are
;;;; messing with

(defvar *mysql-user*)
(defvar *mysql-password*)
(defvar *mysql-database*)
(defvar *mysql-host* "127.0.0.1") ; note that localhost would need to be known to the database.
(defvar *mysql-port* 3306)
(defvar *wp-prefix* "")

;;; Cache some info.
(defvar *table-names* nil)
(defvar *option-names* nil)

(defmacro with-blog ((&key (recording t)) &body body)
  (once-only (recording)
    `(let ((*default-database*
            (connect
             (list *mysql-host* *mysql-database* *mysql-user* *mysql-password* *mysql-port*)
             :database-type :mysql
             :pool t)))
       (unwind-protect
            (progn 
              (when ,recording
                (start-sql-recording))
              ,@body)
         (when ,recording
           (stop-sql-recording))
         (disconnect)))))

; (with-blog () (list-tables))
; (with-blog () (server-version))
; (mapcar #'first (caar (with-blog () (query "SELECT option_name FROM wp_lckt07_options;"))))

(defun switch-to-another-blog (&key user password database prefix (host *mysql-host*))
  (assert user)
  (assert password)
  (assert database)
  (assert prefix)
  (disconnect-pooled)
  (setf *mysql-user* user
        *mysql-password* password 
        *mysql-database* database 
        *mysql-host* host 
        *wp-prefix* prefix 
        *option-names* nil
        *table-names* nil)
  (with-blog () (list-tables)))

(defun sql2lisp (name-string)
  (intern (string-upcase (substitute #\- #\_ name-string))))

(defun lisp2sql (token)
  (etypecase token
    (string token)
    (symbol
     (nstring-downcase (substitute #\_ #\- (symbol-name token))))))

(defun table-name (table)
  (format nil "~A~A" *wp-prefix* (lisp2sql table)))

(defun tables-names ()
  (or *table-names*
      (loop
         as prefix-length = (1+ (length *wp-prefix*))
         for (actual-table-name) in (caar (with-blog () (list-tables)))
         collect (sql2lisp (subseq actual-table-name prefix-length)))))

;;;; Options

(defun list-options ()
  (or *option-names*
      (setf *option-names*
            (loop for (option-name) in (caar (squery 'options :fields '(option-name)))
               collect (sql2lisp option-name)))))

(defun fetch-option (name)
  (caaaar
   (fquery "SELECT option_value FROM ~A WHERE option_name = ~S" 
           (table-name :options)
           (lisp2sql name))))

(defun show-options (n)
  (list-options)
  (loop
     for i below n
     for k in (nthcdr n *option-names*)
     do (format t "~&~30a ~a" k (fetch-option k))))

;;; consider https://github.com/nixeagle/php-serialization


;;;; Reader/writers for various types found in some of the wordpress table columns.

(defun parse-iso-8601-time-foo (x)
  (if (string= x "0000-00-00 00:00:00")
      nil
      (clsql-sys::parse-iso-8601-time x)))

(defun write-iso-8601-time-foo (x)
  (if x
      (clsql-sys::write-time x)
      "0000-00-00 00:00:00"))




;;; make-class-desc

#+nil
(defun make-class-desc (table-name)
  ;; based on http://lists.b9.com/pipermail/clsql/2009-October/001770.html
 (let ((desc (clsql:query (format nil "describe ~a;" table-name)))
       (types (clsql:list-attribute-types table-name)))
   (flet 
       ((convert-mysql-type (type)
          ;; based on http://lists.b9.com/pipermail/clsql/2009-October/001770.html
          (ecase type
            ((decimal numeric) 'ratio)
            ((int tinyint smallint mediumint bigint year) 'integer)
            ((float real double precision ) 'double-float)
            ((date datetime timestamp) 'integer)	
            ((time) 'integer)
            (enum 'keyword)
            ((char varchar text tinytext mediumtext longtext) 'string)
            ((bit blob mediumblob longblob tinyblob geometry) 'simple-array)))
        (keyp (name) (equalp "PRI" (fourth (find name desc :key
                                                 'first :test 'equalp))))
        (slot-name (name) (intern (string-upcase (substitute #\- #\_ name
                                                             :test 'char-equal)))))
     `(clsql:def-view-class ,(slot-name table-name) ()
	,(loop for (name type scale null) in types
	    collect `(,(slot-name name)
                       :accessor ,(slot-name name)
		       :column ,name
		       :type ,(convert-mysql-type (intern (symbol-name type)))
		       :db-kind ,(if (keyp name) :key :base)
		       :initarg ,(intern (symbol-name (slot-name name)) :keyword)))
	(:base-table ,table-name)))))

;;;; Options

(def-view-class wp-option ()
  ((option-id :accessor option-id
              :column "option_id"
              :type integer
              :db-kind :key
              :initarg :option-id)
   (option-name :accessor option-name
                :column "option_name"
                :type string
                :db-kind :base
                :initarg :option-name)
   (option-value :accessor option-value
                 :column "option_value"
                 :type string
                 :db-kind :base
                 :initarg :option-value)
   (autoload :accessor autoload 
     :column "autoload" 
     :type string 
     :db-kind :base 
     :initarg :autoload))
  (:base-table "wp_lckt07_options"))

(defmethod print-object ((option wp-option) stream)
  (print-unreadable-object (option stream :type t :identity nil)
    (format stream "#~D ~s ~s"
            (option-id option)
            (trimmed-text (option-name option) 25)
            (trimmed-text (option-value option)))))


;;;; Post and their Metadata

(def-view-class wp-post ()
  ((id :column "ID" :type integer :db-kind :key :initarg :id :accessor id)
   (post-author :column "post_author" :type integer :db-kind :base
    :initarg :post-author :accessor post-author)
   (post-date :column "post_date" :type wall-time :db-reader parse-iso-8601-time-foo :db-kind :base :initarg
    :post-date)
   (post-date-gmt :column "post_date_gmt" :type wall-time  :db-reader parse-iso-8601-time-foo :db-kind :base
    :initarg :post-date-gmt)
   (post-content :column "post_content" :type string :db-kind :base
    :initarg :post-content :accessor post-content)
   (post-title :column "post_title" :type string :db-kind :base :initarg
    :post-title :accessor post-title)
   (post-category :column "post_category" :type integer :db-kind :base
    :initarg :post-category :accessor post-category)
   (post-excerpt :column "post_excerpt" :type string :db-kind :base
    :initarg :post-excerpt)
   (post-status :column "post_status" :type keyword :db-kind :base :initarg
    :post-status :accessor post-status)
   (comment-status :column "comment_status" :type keyword :db-kind :base
    :initarg :comment-status)
   (ping-status :column "ping_status" :type keyword :db-kind :base :initarg
    :ping-status)
   (post-password :column "post_password" :type string :db-kind :base
    :initarg :post-password)
   (post-name :column "post_name" :type string :db-kind :base :initarg
    :post-name :accessor post-name)
   (to-ping :column "to_ping" :type string :db-kind :base :initarg
    :to-ping)
   (pinged :column "pinged" :type string :db-kind :base :initarg :pinged)
   (post-modified :column "post_modified" :type wall-time  :db-reader parse-iso-8601-time-foo :db-kind :base
    :initarg :post-modified)
   (post-modified-gmt :column "post_modified_gmt"
                      :type wall-time
                      :db-reader parse-iso-8601-time-foo
                      :db-kind :base
                      :initarg :post-modified-gmt)
   (post-content-filtered :column "post_content_filtered" :type string
    :db-kind :base :initarg :post-content-filtered)
   (post-parent :column "post_parent" :type integer :db-kind :base
    :initarg :post-parent)
   (guid :column "guid" :type string :db-kind :base :initarg :guid)
   (menu-order :column "menu_order" :type integer :db-kind :base :initarg
    :menu-order)
   (post-type :column "post_type" :type keyword :db-kind :base :initarg
    :post-type :accessor post-type)
   (post-mime-type :column "post_mime_type" :type string :db-kind :base
    :initarg :post-mime-type)
   (comment-count :column "comment_count" :type integer :db-kind :base
    :initarg :comment-count :accessor comment-count)
   ;; info we garner by queries.
   (post-user :accessor post-user
              :db-kind :join
              :db-info (:join-class wp-user
                                    :home-key post-author
                                    :foreign-key id
                                    :set nil))
   (post-metadata :reader post-metadata
                  :db-kind :join
                  :db-info (:join-class wp-postmeta
                                        :home-key id
                                        :foreign-key post-id
                                        :set t))
   (term-relationships :accessor term-relationships
                       :db-kind :join
                       :db-info (:join-class wp-term-relationship
                                 :home-key id
                                 :foreign-key object-id
                                 :set t))
   (comments :accessor comments
                       :db-kind :join
                       :db-info (:join-class wp-comment
                                 :home-key id
                                 :foreign-key comment-post-id
                                 :set t)))
  (:base-table "wp_lckt07_posts"))

; (select 'wp-post (sql-operation '= (sql-expression :attribute "ID") 11))
; (pprint (select 'wp-post :offset 2 :limit 10))

(defmethod print-object ((post wp-post) stream)
  (print-unreadable-object (post stream :type t :identity nil)
    (format stream "~a/~d:~s"
            (when (slot-boundp post 'id)
              (id post))
            (when (slot-boundp post 'post-status)
              (let ((s (post-status post)))
                (cond
                  ((string= s "draft") "d")
                  ((string= s "publish") "p")
                  ((string= s "inherit") "i")
                  (t s))))
            (when (slot-boundp post 'post-title)
              (post-title post)))))

(defun fetch-posts (which)
  (select 'wp-post :where
          (etypecase which
            (fixnum 
             (sql-operation '= (sql-expression :attribute "ID") which))
            (string 
             (sql-operation 'like (sql-expression :attribute "post_title") which)))))

(defmacro do-posts ((post &optional (where-clause "true")) &body body)
  (once-only (where-clause)
    `(loop 
        with batch-size = 100
        for offset from 0 by batch-size
        as posts = (select 'wp-post 
                           :offset offset :limit batch-size
                           :where ,where-clause)
        while posts
        do
          (loop 
             for (,post) in posts 
             unless (<= 1881 (id ,post) 1884)
             do ,@body))))

(def-view-class wp-postmeta ()
  ((meta-id :accessor meta-id
            :column "meta_id"
            :type integer
            :db-kind :key
            :initarg :meta-id)
   (post-id :accessor post-id
            :column "post_id"
            :type integer
            :db-kind :base
            :initarg :post-id)
   (meta-key :accessor meta-key
             :column "meta_key"
             :type string
             :db-kind :base
             :initarg :meta-key)
   (meta-value :accessor meta-value
               :column "meta_value"
               :type string
               :db-kind :base
               :initarg :meta-value)
   ;; computed
   (post
      :accessor post
      :db-kind :join
      :db-info (:join-class wp-post
	        :home-key post-id
		:foreign-key id
		:set nil)))
  (:base-table "wp_lckt07_postmeta"))

(defmethod print-object ((pm wp-postmeta) stream)
  (print-unreadable-object (pm stream :type t :identity nil)
    (format stream "#~D post#~S/~a/~a"
            (meta-id pm)
            (when (slot-boundp pm 'post-id)
              (post-id pm))
            (when (slot-boundp pm 'meta-key)
              (meta-key pm))
            (when (slot-boundp pm 'meta-value)
              (trimmed-text (meta-value pm))))))

;;; Comments and their Metadata

(def-view-class wp-comment ()
  ((comment-id :accessor comment-id
               :column "comment_ID"
               :type integer
               :db-kind :key
               :initarg :comment-id)
   (comment-post-id :accessor comment-post-id
                    :column "comment_post_ID"
                    :type integer
                    :db-kind :base
                    :initarg :comment-post-id)
   (comment-author :accessor comment-author
                   :column "comment_author"
                   :type string
                   :db-kind :base
                   :initarg :comment-author)
   (comment-author-email :accessor comment-author-email
                         :column "comment_author_email"
                         :type string
                         :db-kind :base
                         :initarg :comment-author-email)
   (comment-author-url :accessor comment-author-url
                       :column "comment_author_url"
                       :type string
                       :db-kind :base
                       :initarg :comment-author-url)
   (comment-author-ip :accessor comment-author-ip
                      :column "comment_author_IP"
                      :type string
                      :db-kind :base
                      :initarg :comment-author-ip)
   (comment-date :accessor comment-date
                 :column "comment_date"
                 :type wall-time
                 :db-kind :base
                 :initarg :comment-date)
   (comment-date-gmt :accessor comment-date-gmt
                     :column "comment_date_gmt"
                     :type wall-time
                     :db-kind :base
                     :initarg :comment-date-gmt)
   (comment-content :accessor comment-content
                    :column "comment_content"
                    :type string
                    :db-kind :base
                    :initarg :comment-content)
   (comment-karma :accessor comment-karma
                  :column "comment_karma"
                  :type integer
                  :db-kind :base
                  :initarg :comment-karma)
   (comment-approved :accessor comment-approved
                     :column "comment_approved"
                     :type string
                     :db-kind :base
                     :initarg :comment-approved)
   (comment-agent :accessor comment-agent
                  :column "comment_agent"
                  :type string
                  :db-kind :base
                  :initarg :comment-agent)
   (comment-type :accessor comment-type
                 :column "comment_type"
                 :type string
                 :db-kind :base
                 :initarg :comment-type)
   (comment-parent :accessor comment-parent
                   :column "comment_parent"
                   :type integer
                   :db-kind :base
                   :initarg :comment-parent)
   (user-id :accessor user-id
            :column "user_id"
            :type integer
            :db-kind :base
            :initarg :user-id)
   (comment-subscribe :accessor comment-subscribe
                      :column "comment_subscribe"
                      :type keyword
                      :db-kind :base
                      :initarg :comment-subscribe))
  ;; computed
  (post :accessor post
        :db-kind :join
        :db-info (:join-class wp-post
                  :home-key post-id
                  :foreign-key id
                  :set nil))
  (comment-metadata :reader comment-metadata
                    :db-kind :join
                    :db-info (:join-class wp-commentmeta
                                          :home-key id
                                          :foreign-key comment-id
                                          :set t))

  (:base-table "wp_lckt07_comments"))

(def-view-class wp-commentmeta ()
  ((meta-id :accessor meta-id
            :column "meta_id"
            :type integer
            :db-kind :key
            :initarg :meta-id)
   (comment-id :accessor comment-id
               :column "comment_id"
               :type integer
               :db-kind :base
               :initarg :comment-id)
   (meta-key :accessor meta-key
             :column "meta_key"
             :type string
             :db-kind :base
             :initarg :meta-key)
   (meta-value :accessor meta-value
               :column "meta_value"
               :type string
               :db-kind :base
               :initarg :meta-value)
   ;; computed
  (comment :accessor comment
        :db-kind :join
        :db-info (:join-class wp-comment
                  :home-key comment-id
                  :foreign-key comment-id
                  :set nil)))
  (:base-table "wp_lckt07_commentmeta"))

;;;; Links

(def-view-class wp-links ()
  ((link-id :accessor link-id
            :column "link_id"
            :type integer
            :db-kind :key
            :initarg :link-id)
   (link-url :accessor link-url
             :column "link_url"
             :type string
             :db-kind :base
             :initarg :link-url)
   (link-name :accessor link-name
              :column "link_name"
              :type string
              :db-kind :base
              :initarg :link-name)
   (link-image :accessor link-image
               :column "link_image"
               :type string
               :db-kind :base
               :initarg :link-image)
   (link-target :accessor link-target
                :column "link_target"
                :type string
                :db-kind :base
                :initarg :link-target)
   (link-category :accessor link-category
                  :column "link_category"
                  :type integer
                  :db-kind :base
                  :initarg :link-category)
   (link-description :accessor link-description
                     :column "link_description"
                     :type string
                     :db-kind :base
                     :initarg :link-description)
   (link-visible :accessor link-visible
                 :column "link_visible"
                 :type string
                 :db-kind :base
                 :initarg :link-visible)
   (link-owner :accessor link-owner
               :column "link_owner"
               :type integer
               :db-kind :base
               :initarg :link-owner)
   (link-rating :accessor link-rating
                :column "link_rating"
                :type integer
                :db-kind :base
                :initarg :link-rating)
   (link-updated :accessor link-updated
                 :column "link_updated"
                 :type integer
                 :db-kind :base
                 :initarg :link-updated)
   (link-rel :accessor link-rel
             :column "link_rel"
             :type string
             :db-kind :base
             :initarg :link-rel)
   (link-notes :accessor link-notes
               :column "link_notes"
               :type string
               :db-kind :base
               :initarg :link-notes)
   (link-rss :accessor link-rss
             :column "link_rss"
             :type string
             :db-kind :base
             :initarg :link-rss)
   ;; Computed
   (term-relationships :accessor term-relationships
                       :db-kind :join
                       :db-info (:join-class wp-term-relationship
                                 :home-key link-id
                                 :foreign-key object-id
                                 :set t)))
  (:base-table "wp_lckt07_links"))


;;;  term-relationships, taxonomy, terms

;;; Posts and links can have an ordered set of tags.  Each such tagging
;;; gets a row in wp-term-relationships which bridges between a node in
;;; the taxomony and the link or post.  The taxonomy is a tree.  Each
;;; node in that tree has a description, and in a seperate table a
;;; term 


(def-view-class wp-term ()
  ((term-id :accessor term-id
            :column "term_id"
            :type integer
            :db-kind :key
            :initarg :term-id)
   (name :accessor name
         :column "name"
         :type string
         :db-kind :base
         :initarg :name)
   (slug :accessor slug
         :column "slug"
         :type string
         :db-kind :base
         :initarg :slug)
   (term-group :accessor term-group
               :column "term_group"
               :type integer
               :db-kind :base
               :initarg :term-group)
   ;;
   (taxonomies :accessor taxonomies
               :db-kind :join
               :db-info (:join-class wp-term-taxonomy
                         :home-key term-id
                         :foreign-key term-id
                         :set t)))
  (:base-table "wp_lckt07_terms"))

(defmethod print-object ((term wp-term) stream)
  (print-unreadable-object (term stream :type t :identity nil)
    (format stream "#~D ~A" (term-id term) (name term))))

(def-view-class wp-term-relationship ()
  ((object-id :accessor object-id ;; a post or a link
              :column "object_id"
              :type integer
              :db-kind :key
              :initarg :object-id)
   (term-taxonomy-id :accessor term-taxonomy-id
                     :column "term_taxonomy_id"
                     :type integer
                     :db-kind :key
                     :initarg :term-taxonomy-id)
   (term-taxonomy
      :accessor term-taxonomy
      :db-kind :join
      :db-info (:join-class wp-term-taxonomy
	        :home-key term-taxonomy-id
		:foreign-key term-taxonomy-id
		:set nil))
   (term-order :accessor term-order
               :column "term_order"
               :type integer
               :db-kind :base
               :initarg :term-order))
  (:base-table "wp_lckt07_term_relationships"))

(def-view-class wp-term-taxonomy ()
  ((term-taxonomy-id :accesor term-taxonomy-id
                     :column "term_taxonomy_id"
                     :type integer
                     :db-kind :key
                     :initarg :term-taxonomy-id)
   (term-id :accesor term-id
            :column "term_id" 
            :type integer 
            :db-kind :base 
            :initarg :term-id)
   (term :accessor term
         :db-kind :join
         :db-info (:join-class wp-term
	 :home-key term-id
	 :foreign-key term-id
	 :set nil))
   (taxonomy :column "taxonomy" 
             :type string 
             :db-kind :base 
             :initarg :taxonomy
             :accessor taxonomy)
   (description :column "description" 
                :type string 
                :db-kind :base 
                :initarg :description
                :accesor description)
   (parent :accessor parent
           :column "parent" 
           :type integer 
           :db-kind :base 
           :initarg :parent)
   (count :accessor term-taxonomy-count
          :column "count"
          :type integer
          :db-kind :base
          :initarg :count))
  (:base-table "wp_lckt07_term_taxonomy"))

(defmethod print-object ((tt wp-term-taxonomy) stream)
  (print-unreadable-object (tt stream :type t :identity nil)
    (format stream "#~D ~A ~A"
            (slot-value tt 'term-taxonomy-id)
            (taxonomy tt)
            (cond
              ((slot-boundp tt 'term)
               (foo (term tt)))
              (t (format nil "term#~d" (slot-value tt 'term-id)))))))

;;; Users

(def-view-class wp-user ()
  ((id :accessor id
       :column "ID"
       :type integer
       :db-kind :key
       :initarg :id)
   (user-login :accessor user-login
               :column "user_login"
               :type string
               :db-kind :base
               :initarg :user-login)
   (user-pass :accessor user-pass
              :column "user_pass"
              :type string
              :db-kind :base
              :initarg :user-pass)
   (user-nicename :accessor user-nicename
                  :column "user_nicename"
                  :type string
                  :db-kind :base
                  :initarg :user-nicename)
   (user-email :accessor user-email
               :column "user_email"
               :type string
               :db-kind :base
               :initarg :user-email)
   (user-url :accessor user-url
             :column "user_url"
             :type string
             :db-kind :base
             :initarg :user-url)
   (user-registered :accessor user-registered
                    :column "user_registered"
                    :type wall-time
                    :db-reader parse-iso-8601-time-foo
                    :db-kind :base
                    :initarg :user-registered)
   (user-activation-key :accessor user-activation-key
                        :column "user_activation_key"
                        :type string
                        :db-kind :base
                        :initarg :user-activation-key)
   (user-status :accessor user-status
                :column "user_status"
                :type integer
                :db-kind :base
                :initarg :user-status)
   (display-name :accessor display-name
                 :column "display_name"
                 :type string
                 :db-kind :base
                 :initarg :display-name))
  (:base-table "wp_lckt07_users"))

(defmethod print-object ((post wp-user) stream)
  (print-unreadable-object (post stream :type t :identity nil)
    (format stream "#~a ~a"
            (when (slot-boundp post 'id) (id post))
            (when (slot-boundp post 'user-login) (user-login post)))))

;; User meta
(def-view-class wp-usermeta ()
  ((umeta-id :accessor umeta-id
             :column "umeta_id"
             :type integer
             :db-kind :key
             :initarg :umeta-id)
   (user-id :accessor user-id
            :column "user_id"
            :type integer
            :db-kind :base
            :initarg :user-id)
   (user
      :accessor user
      :db-kind :join
      :db-info (:join-class wp-user
	        :home-key user-id
		:foreign-key id
		:set nil))
   (meta-key :accessor meta-key
             :column "meta_key"
             :type keyword
             :db-kind :base
             :initarg :meta-key)
   (meta-value :accessor meta-value
               :column "meta_value"
               :type string
               :db-kind :base
               :initarg :meta-value))
  (:base-table "wp_lckt07_usermeta"))

;; "wp_lckt07_commentmeta"
;; "wp_lckt07_comments"
;; "wp_lckt07_links"
;; "wp_lckt07_options"
;; "wp_lckt07_pollin_answer"
;; "wp_lckt07_pollin_question"
;; "wp_lckt07_postie_config"

;;; Others
;;  "wp_lckt07_surveys_answer"
;;  "wp_lckt07_surveys_question"
;;  "wp_lckt07_surveys_result"
;;  "wp_lckt07_surveys_result_answer"
;;  "wp_lckt07_surveys_survey"

