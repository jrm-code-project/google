;;; -*- Lisp -*-

(in-package "GOOGLE")

(defun google-get (uri api-key)
  (let ((response (dexador:get uri
                               :headers `(("Accept" . "application/json")
                                          ("x-goog-api-key" . ,api-key)))))
    (if (stringp response)
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string response))
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string
           (flex:octets-to-string response :external-format :utf-8))))))

(defun google-post (uri api-key payload)
  (let ((response (dex:post uri
                            :headers `(("Accept" . "application/json")
                                       ("Content-Type" . "application/json")
                                       ("x-goog-api-key" . ,api-key))
                            :content (cl-json:encode-json-to-string payload))))
    (if (stringp response)
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string response))
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string
           (flex:octets-to-string response :external-format :utf-8))))))
