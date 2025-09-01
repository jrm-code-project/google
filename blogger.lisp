;;; -*- Lisp -*-

(in-package "GOOGLE")

(defun blogger-get (blog-id &optional next-page-token)
  (google-get (format nil "https://www.googleapis.com/blogger/v3/blogs/~d/posts~@[?pageToken=~a~]"
                      blog-id next-page-token)
              (blogger-api-key)))

(defun scan-blogger-posts (blog-id)
  (declare (optimizable-series-function))
  (map-fn 'string
          (lambda (post) (gethash :content post))
          (choose-if #'hash-table-p
                     (map-fn 't #'car
                             (scan-fn t
                                      (lambda () (let ((page (blogger-get blog-id)))
                                                   (append (coerce (gethash :items page) 'list)
                                                           (list  (gethash :next-page-token page)))))
                                      (lambda (stack)
                                        (if (stringp (car stack))
                                            (let ((page (blogger-get blog-id (car stack))))
                                              (append (coerce (gethash :items page) 'list)
                                                      (list (gethash :next-page-token page))))
                                            (cdr stack)))
                                      (lambda (stack) (null stack)))))))
