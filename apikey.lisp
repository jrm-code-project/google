;;; -*- Lisp -*-

;;; Fetch the API keys.

;;; We need an API key for Gemini and optionally a Custom Search
;;; Engine API key if we want to have access to web search
;;; capabilities.

;;; The API keys can be scoped to a specific Google Cloud project.

;;; The API keys can be stored in files in the user's XDG config
;;; directory tree, or set in environment variables.

;;; It costs nothing to set up a Google Cloud account and create a
;;; project, but you will be charged for usage of the Gemini API and
;;; custom search engine if you exceed the free tier.  The costs as of
;;; 2025 appear to be modest (a few dollars per month) for a hobbyist.
;;; When you sign up, you get a $300 credit for the first 90 days, so
;;; you can judge for yourself if the costs are acceptable.

;;; For the fancy setup, I assume you have a Google Cloud account and
;;; have created a project.  I assume your XDG config directory is ~/.config

;;; The file ~/.config/googleapis/default-project will contain the
;;; name of your project.

;;; The file ~/.config/googleapis/{project}/apikey will contain your
;;; api key for the Gemini API.  A Gemini API key is required for this
;;; to work.

;;; The directory ~/.config/googleapis/{project}/Blogger will contain
;;; credentials for accessing your Blogger blog if you have one.  If
;;; these optional files are not present, you will not have access to
;;; your blog.
;;;
;;;  - ~/.config/googleapis/{project}/Blogger/apikey contains the
;;;    apikey for the Blogger API
;;;  - ~/.config/googleapis/{project}/Blogger/blog-id contains the
;;;    blog ID for your blog

;;; The directory ~/.config/googleapis/{project}/CustomSearchEngine
;;; will contain credentials for accessing the Google Search engine.
;;; If these optional files are not present, the system will not be
;;; able to perform Google searches.
;;;
;;; - ~/.config/googleapis/{project}/CustomSearchEngine/apikey
;;;   contains the apikey for the Google Custom Search Engine API
;;; - ~/.config/googleapis/{project}/CustomSearchEngine/hyperspec-id
;;;   contains the Google Custom Search Engine Id for searching the
;;;   Common Lisp hyperspec.  Configure one yourself, or use
;;;   `008072110934663485714:6gce0ybe318`  If you do not configure
;;;   this, you will not be able to use the hyperspec tool.
;;; - ~/.config/googleapis/{project}/CustomSearchEngine/id contains
;;;   the Google Custom Search Engine Id for a vanilla Google Search.

;;; As an alternative to laying out the above directory structure, you
;;; can place the credentials in these environment variables:
;;; 
;;; BLOGGER_API_KEY - API Key for Blogger API
;;; BLOG_ID - ID of your blog.
;;; GEMINI_API_KEY - API Key for Google Gemini
;;; CSE_API_KEY - API Key for the Google Custom Search Engine API
;;; GOOGLE_CSE_ID - Custom search engine ID for a Google Search
;;; HYPERSPEC_CSE_ID - Custom search engine ID for a Hyperspec search

(in-package "GOOGLE")

(defun googleapis-pathname ()
  "Returns the base pathname for Google APIs configuration files,
   typically located in the user's XDG config directory."
  (uiop/configuration:xdg-config-pathname "googleapis/"))

(defun default-apikey-pathname ()
  "Returns the default pathname for the API key file within the
   Google APIs configuration directory."
  (merge-pathnames "apikey" (googleapis-pathname)))

(defun default-project-pathname ()
  "Returns the default pathname for the default project file within the
   Google APIs configuration directory."
  (merge-pathnames "default-project" (googleapis-pathname)))

(defun default-blogger-apikey-pathname ()
  "Returns the default pathname for the Google Custom Search Engine API key file
   within the Google APIs configuration directory."
  (merge-pathnames "blogger-apikey" (googleapis-pathname)))

(defun default-blogger-blog-id-pathname ()
  "Returns the default pathname for the Google Custom Search Engine ID file
   within the Google APIs configuration directory."
  (merge-pathnames "blog-id" (googleapis-pathname)))

(defun default-custom-search-engine-apikey-pathname ()
  "Returns the default pathname for the Google Custom Search Engine API key file
   within the Google APIs configuration directory."
  (merge-pathnames "cse-apikey" (googleapis-pathname)))

(defun default-custom-search-engine-id-pathname ()
  "Returns the default pathname for the Google Custom Search Engine ID file
   within the Google APIs configuration directory."
  (merge-pathnames "cse-id" (googleapis-pathname)))

(defun default-hyperspec-custom-search-engine-id-pathname ()
  "Returns the default pathname for the Google Custom Search Engine ID file
   within the Google APIs configuration directory."
  (merge-pathnames "hyperspec-id" (googleapis-pathname)))

(defun default-project ()
  "Reads and returns the default Google Cloud project name from its
   designated configuration file. Returns NIL if the file does not exist
   or is empty."
  (let ((pathname (default-project-pathname)))
    (when (probe-file pathname)
      (with-open-file (stream pathname :direction :input)
        (let ((line (read-line stream nil)))
          (when line
            (str:trim line)))))))

(defun project-apikey-pathname (project)
  "Constructs the pathname for the API key file specific to a given
   Google Cloud PROJECT within the Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project)
                                  :name "apikey")
                   (googleapis-pathname)))

(defun project-blogger-pathname (project)
  "Constructs the pathname for the Blogger configuration
   directory specific to a given Google Cloud PROJECT within the
   Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project "Blogger"))
                   (googleapis-pathname)))

(defun project-cse-pathname (project)
  "Constructs the pathname for the Custom Search Engine configuration
   directory specific to a given Google Cloud PROJECT within the
   Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project "CustomSearchEngine"))
                   (googleapis-pathname)))

(defun project-blogger-apikey-pathname (project)
  "Constructs the pathname for the Blogger API key file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-blogger-pathname project) "apikey"))

(defun project-blogger-blog-id-pathname (project)
  "Constructs the pathname for the Blogger API key file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-blogger-pathname project) "blog-id"))

(defun project-custom-search-engine-apikey-pathname (project)
  "Constructs the pathname for the Custom Search Engine API key file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-cse-pathname project) "apikey"))

(defun project-custom-search-engine-id-pathname (project)
  "Constructs the pathname for the Custom Search Engine ID file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-cse-pathname project) "id"))

(defun project-hyperspec-custom-search-engine-id-pathname (project)
  "Constructs the pathname for the Hyperspec Search Engine ID file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-cse-pathname project) "hyperspec-id"))

(defun apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-apikey-pathname project))))
      (probe-file (default-apikey-pathname))))

(defun blogger-apikey-pathname ()
  "Determines the effective pathname for the Blogger API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-blogger-apikey-pathname project))))
      (probe-file (default-blogger-apikey-pathname))))

(defun blogger-blog-id-pathname ()
  "Determines the effective pathname for the Blogger Blog Id key.
   It first checks for a project-specific Blog Id (if a default project
   is set), then falls back to the default Blog Id pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-blogger-blog-id-pathname project))))
      (probe-file (default-blogger-blog-id-pathname))))

(defun custom-search-engine-apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-custom-search-engine-apikey-pathname project))))
      (probe-file (default-custom-search-engine-apikey-pathname))))

(defun custom-search-engine-id-pathname ()
  "Determines the effective pathname for the Google Custom Search Engine ID.
   It first checks for a project-specific ID (if a default project
   is set), then falls back to the default ID pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-custom-search-engine-id-pathname project))))
      (probe-file (default-custom-search-engine-id-pathname))))

(defun hyperspec-custom-search-engine-id-pathname ()
  "Determines the effective pathname for the Hyperspec Custom Search Engine ID.
   It first checks for a project-specific ID (if a default project
   is set), then falls back to the default ID pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-hyperspec-custom-search-engine-id-pathname project))))
      (probe-file (default-hyperspec-custom-search-engine-id-pathname))))

(defun blogger-api-key ()
  "Retrieves the Blogger API key. It first attempts to read it from
   the API key file (either project-specific or default), then falls back
   to the BLOGGER_API_KEY environment variable.
   Signals an error if no API key is found."
  (or (let ((pathname (blogger-apikey-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "BLOGGER_API_KEY")
      (error "No Blogger API key found. Set the environment variable BLOGGER_API_KEY or create a file at ~a."
             (namestring (apikey-pathname)))))

(defun gemini-api-key ()
  "Retrieves the Google API key. It first attempts to read it from
   the API key file (either project-specific or default), then falls back
   to the GOOGLE_API_KEY environment variable.
   Signals an error if no API key is found."
  (or (let ((pathname (apikey-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_API_KEY")
      (error "No Google API key found. Set the environment variable GOOGLE_API_KEY or create a file at ~a."
             (namestring (apikey-pathname)))))

(defun search-engine-api-key ()
  "Retrieves the Google Custom Search Engine API key. It first attempts to read it from
   the CSE API key file (either project-specific or default), then falls back
   to the GOOGLE_CSE_API_KEY environment variable.
   Signals an error if no CSE API key is found."
  (or (let ((pathname (custom-search-engine-apikey-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_CSE_API_KEY")
      (error "No Google Custom Search Engine API key found.  Set the environment variable GOOGLE_CSE_API_KEY or create file at ~a." (namestring (custom-search-engine-apikey-pathname)))))

(defun blogger-blog-id ()
  "Retrieves the Blogger blog ID. It first attempts to read it from
   the blog ID file (either project-specific or default), then falls back
   to the BLOG_ID environment variable.
   Signals an error if no blog ID is found."
  (or (let ((pathname (blogger-blog-id-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "BLOG_ID")
      (error "No Blogger blog ID found.  Set the environment variable BLOG_ID or create file at ~a." (namestring (blogger-blog-id-pathname)))))

(defun google-search-engine-id ()
  "Retrieves the Google Custom Search Engine ID. It first attempts to read it from
   the CSE ID file (either project-specific or default), then falls back
   to the GOOGLE_CSE_ID environment variable.
   Signals an error if no CSE ID is found."
  (or (let ((pathname (custom-search-engine-id-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_CSE_ID")
      (error "No Google Custom Search Engine ID found.  Set the environment variable GOOGLE_CSE_ID or cerate file at ~a." (namestring (custom-search-engine-id-pathname)))))

(defun hyperspec-search-engine-id ()
  "Retrieves the Google Custom Search Engine ID. It first attempts to read it from
   the Hyperspec CSE ID file (either project-specific or default), then falls back
   to the HYPERSPEC_CSE_ID environment variable.
   Signals an error if no Hyperspec CSE ID is found."
  (or (let ((pathname (hyperspec-custom-search-engine-id-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "HYPERSPEC_CSE_ID")
      (error "No Hyperspec Custom Search Engine ID found.  Set the environment variable HYPERSPEC_CSE_ID or cerate file at ~a." (namestring (hyperspec-custom-search-engine-id-pathname)))))
