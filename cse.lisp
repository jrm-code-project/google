;;; -*- Lisp -*-

(in-package "GOOGLE")

(defun custom-search (query &key custom-search-engine-id)
  (google-get (format nil "https://www.googleapis.com/customsearch/v1?cx=~a&q=~a"
                      custom-search-engine-id
                      query)
              (search-engine-api-key)))

(defun web-search (query)
  (custom-search query :custom-search-engine-id (google-search-engine-id)))

(defun hyperspec-search (query)
  (custom-search query :custom-search-engine-id (hyperspec-search-engine-id)))
