# Introduction

Wordpress is a widely adopted blogging platform.  It stores the accumlated blog postings in a hybrid of mysql database tables and the file system.  This repository provides Common Lisp code for interacting with the mysql database.  This is based on CLSQL, and in particular it stands on CLSQL's object relational database mapping scheme via a set of def-view-classes, one per mysql table.

This currently has numerous shortcommings, but I find it quite useful.
* I dont' understand how to load CLSQL automatically.
* There are no test cases and I'm confident that implies there are bugs.
* I've used this only to clean up text encodings and browse the databases.
* I haven't puzzed out how to make this play well with wordpress's $table_prefix scheme, so you have to hand modify the tail of each def-view-class.
* php's serialization is used on some database cells, but this doesn't.

# Usage

1. load clsql (ql:quickload "clsql")
2. tell it were to find the mysql libraries, for example; (clsql-sys:push-library-path (probe-file \"/opt/local/lib/mysql5/mysql/\"))
3. load this: (ql:quickload "cl-wordpress-database-mapping")
4. assure the wordpress mysql database listener is visible (see Appendix A)
5. write a function like this using your blog's cl-config.php for guidance
```common-lisp
(defun switch-to-tmp ()
  (wp:switch-to-another-blog :user "userinmysql"
                          :password "realpassword"
                          :database "databasename"
                          :prefix "wp_cltk07_"
                          :host "127.0.0.1"))
```
6. Invoke the function to switch your attention to your blog.  If it's all working you
will get a list of the tables in you wordpress blog's mysql database.

# Some Examples

Fetch all the authors: `(select 'wp-users)`

Fetch a single post: `(fetch-post "title")` or `(fetch-post 3)`

Fetch the comments on a post: `(comments (fetch-post 3))`

# Appendix A: forwarding a remote mysql server's listener

Security demands that outsiders should not be able to access the wordpress database directly.  If your running this code from outside you'll need to work around that.  SSH port forwarding can help  For example you might put something like this in you ~/.ssh/config file.

```
Host dh-my-blog-host
     ForwardAgent no
     ForwardX11 no
     Hostname hosting.example.net
     User me_me_me
     LocalForward 3306 mysql.example.com:3306
```

You can find the actual database host in your blog's wp-config.php file.

You'll be happier if you also use ~/.ssh/authorized_keys etc.; but this is not the place to go into the details of that.
