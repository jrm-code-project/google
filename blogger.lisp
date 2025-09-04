;;; -*- Lisp -*-

(in-package "GOOGLE")

(defun blogger-get (blog-id &optional next-page-token)
  (google-get (format nil "https://www.googleapis.com/blogger/v3/blogs/~d/posts~@[?pageToken=~a~]"
                      blog-id next-page-token)
              (blogger-api-key)))

;;; How this works:
;;; The inner scan-fn returns a list of posts followed by the next page token.
;;; On each iteration, the CDR of the previous iteration is returned.

;;; Eventually, the next page token will be only thing in the list.  When this happens, blogger-get
;;; is called to generate the next page of posts.

;;; Over this stream of lists, we map car to get the first element in each list.  It will be either
;;; a post or the next page token.

;;; Over this stream of objects, we choose only those that are hash-tables (i.e. posts).
(defun scan-blogger-posts (blog-id)
  (declare (optimizable-series-function))
  (choose-if #'hash-table-p
    (map-fn t #'car
      (scan-fn t
        (lambda () (let ((page (blogger-get blog-id)))
                     (append (coerce (gethash :items page) 'list)
                             (list (gethash :next-page-token page)))))
        (lambda (stack)
          (if (stringp (car stack))
              (let ((page (blogger-get blog-id (car stack))))
                (append (coerce (gethash :items page) 'list)
                        (list (gethash :next-page-token page))))
              (cdr stack)))
        (lambda (stack) (null stack))))))
