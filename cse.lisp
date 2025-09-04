;;; -*- Lisp -*-

(in-package "GOOGLE")

(defun custom-search (query &key custom-search-engine-id)
  "Perform a Google custom search for QUERY using the specified CUSTOM-SEARCH-ENGINE-ID."
  (google-get (format nil "https://www.googleapis.com/customsearch/v1?cx=~a&q=~a"
                      (quri:url-encode custom-search-engine-id)
                      (quri:url-encode query))
              (search-engine-api-key)))

(defun web-search (query)
  "Perform a Google web search for QUERY using the configured custom search engine ID."
  (custom-search query :custom-search-engine-id (google-search-engine-id)))

(defun hyperspec-search (query)
  "Perform a search of the Common Lisp Hyperspec for QUERY using the configured custom search engine ID."
  (custom-search query :custom-search-engine-id (hyperspec-search-engine-id)))
