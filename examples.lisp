(in-package "WORDPRESS")

;;; Options

(defun list-option-names () ;; -> a long list of strings
  (loop for (wp-option) in (select 'wp-option)
     collect (option-name wp-option)))

(defun get-autoload-options ()
  (mapcar #'car 
          (select 'wp-option
                  :where (sql-operation 'like 
                                        (sql-expression :attribute "autoload")
                                        "yes"))))

(defun fetch-option (name) ;; -> a wp-option
  (caar
   (select 'wp-option :where
           (sql-operation 'like (sql-expression :attribute "option_name") name))))

(defun fetch-option-value (name) ;; -> a string
  (option-value
   (caar
    (select 'wp-option :where
            (sql-operation 'like (sql-expression :attribute "option_name") name)))))

(defun option-apropos (regex)
  "Return the wp-option objects for options who's names match the regex given"
  (loop
     for (wp-option) in (select 'wp-option)
     when (cl-ppcre:scan regex (option-name wp-option))
     collect wp-option))


;;; Posts

(defun outline-posting (wp-post)
  (flet ((f (a b)
           (format t "~&~A: ~S" a b)))
    (f "title" (post-title wp-post))
    (f "author" (user-login (post-user wp-post)))
    (format t "~&status: ~S, ~d comments and comment are currently ~s"
            (post-status wp-post)
            (comment-count wp-post)
            (comment-status wp-post))
    (f "content" (trimmed-text (post-content zzz) 30))
    (loop
       with metadata = (post-metadata wp-post)
       initially (format t "~&~D \"metadata\" entries" (length metadata))
       for md in metadata
         do (format t "~&  key: ~S, value: ~S"
                    (trimmed-text (meta-key md))
                    (trimmed-text (meta-value md))))
    (loop
       with term-relationships = (sort (term-relationships wp-post)  #'<
                                       :key #'term-taxonomy-id)
       with last-taxonomy = "THISISUNLIKELYTOBEATAXONONMY"
       initially (format t "~&~D \"term-relationships\" entries" 
                         (length term-relationships))
       for tr in term-relationships
       for term-taxonomy = (term-taxonomy tr)
       for taxonomy = (taxonomy term-taxonomy)
       unless (string= taxonomy last-taxonomy)
       do
         (setf last-taxonomy taxonomy)
         (format t "~&  ~S" last-taxonomy)
       do
         (format t "~&    ~S" (name (term term-taxonomy))))))

