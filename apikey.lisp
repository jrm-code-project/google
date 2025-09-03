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

(macrolet ((define-default-pathname (name file)
             `(DEFUN ,name ()
                (MERGE-PATHNAMES ,file (GOOGLEAPIS-PATHNAME)))))

  ;; Define functions to get the default pathnames for various configuration files.
  (define-default-pathname default-blogger-apikey-pathname "blogger-apikey")
  (define-default-pathname default-blogger-blog-id-pathname "blog-id")
  (define-default-pathname default-custom-search-engine-apikey-pathname "cse-apikey")
  (define-default-pathname default-custom-search-engine-id-pathname "cse-id")
  (define-default-pathname default-gemini-apikey-pathname "gemini-apikey")
  (define-default-pathname default-hyperspec-custom-search-engine-id-pathname "hyperspec-id")
  (define-default-pathname default-project-pathname "default-project"))

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

(macrolet ((define-project-service-pathname (service)
             `(DEFUN ,(intern (concatenate 'string "PROJECT-" (symbol-name service) "-PATHNAME")
                              (find-package "GOOGLE"))
                  (PROJECT)
                (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY
                                                (LIST :RELATIVE PROJECT
                                                      ,(str:join ""
                                                                (map 'list
                                                                     #'str:capitalize
                                                                     (str:split "-" (symbol-name service))))))
                                 (GOOGLEAPIS-PATHNAME)))))

  ;; Define functions to get the pathnames for various services within a project.
  (define-project-service-pathname :blogger)
  (define-project-service-pathname :custom-search-engine)
  (define-project-service-pathname :gemini))

(macrolet ((define-project-service-config (service file)
             `(DEFUN ,(intern (concatenate 'string "PROJECT-"
                                           (symbol-name service)
                                           "-"
                                           (string-upcase file)
                                           "-PATHNAME")
                              (find-package "GOOGLE"))
                  (PROJECT)
                (MERGE-PATHNAMES ,file (,(intern (concatenate 'string "PROJECT-" (symbol-name service) "-PATHNAME")
                                                 (find-package "GOOGLE"))
                                        PROJECT)))))

  ;; Define functions to get the pathnames for various configuration files within a project.
  (define-project-service-config :blogger "apikey")
  (define-project-service-config :blogger "blog-id")
  (define-project-service-config :custom-search-engine "apikey")
  (define-project-service-config :custom-search-engine "id")
  (define-project-service-config :gemini "apikey")
  (define-project-service-config :custom-search-engine "hyperspec-id"))

(macrolet ((define-effective-pathname (name)
             `(DEFUN ,name ()
                (OR (LET ((PROJECT (DEFAULT-PROJECT)))
                      (WHEN PROJECT
                        (PROBE-FILE (,(intern (concatenate 'string "PROJECT-" (symbol-name name))
                                              (find-package "GOOGLE"))
                                     PROJECT))))
                    (PROBE-FILE (,(intern (concatenate 'string "DEFAULT-" (symbol-name name))
                                          (find-package "GOOGLE"))))))))

  ;; Define functions to get the effective pathnames for various keys and IDs.
  (define-effective-pathname blogger-apikey-pathname)
  (define-effective-pathname blogger-blog-id-pathname)
  (define-effective-pathname custom-search-engine-apikey-pathname)
  (define-effective-pathname custom-search-engine-id-pathname)
  (define-effective-pathname gemini-apikey-pathname)
  (define-effective-pathname hyperspec-custom-search-engine-id-pathname))

(macrolet ((define-configuration (name effective-pathname environment-varible)
             `(DEFUN ,name ()
                (OR (LET ((PATHNAME (,effective-pathname)))
                      (AND PATHNAME
                           (WITH-OPEN-FILE (STREAM PATHNAME :DIRECTION :INPUT)
                             (LET ((LINE (READ-LINE STREAM NIL)))
                               (WHEN LINE
                                 (STR:TRIM LINE))))))
                    (UIOP:GETENV ,environment-varible)
                    (ERROR "No ~a found.  Set the environment variable ~a or create file at ~a."
                           ,(string-upcase (symbol-name name))
                           ,environment-varible
                           (NAMESTRING (,effective-pathname)))))))

  ;; Define functions to get the various API keys and IDs.
  (define-configuration blogger-api-key blogger-apikey-pathname "BLOGGER_API_KEY")
  (define-configuration blogger-blog-id blogger-blog-id-pathname "BLOG_ID")
  (define-configuration gemini-api-key gemini-apikey-pathname "GEMINI_API_KEY")
  (define-configuration google-search-engine-id custom-search-engine-id-pathname "GOOGLE_CSE_ID")
  (define-configuration hyperspec-search-engine-id hyperspec-custom-search-engine-id-pathname "HYPERSPEC_CSE_ID")
  (define-configuration search-engine-api-key custom-search-engine-apikey-pathname "GOOGLE_CSE_API_KEY"))
