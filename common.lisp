;;; -*- Lisp -*-

(in-package "GOOGLE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper function for decoding JSON responses.
;; This centralizes the logic and makes it easier to add error handling.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun decode-google-json-response (response)
  "Decodes a Google API JSON response, handling both string and octet inputs.
   It attempts to decode the JSON. If the input is not a string, it assumes
   it's a byte array (octets) and converts it to a UTF-8 string before decoding.
   Uses WITH-DECODER-JRM-SEMANTICS for specific decoding behavior."
  (with-decoder-jrm-semantics
    (cl-json:decode-json-from-string
     (if (stringp response)
         response
         ;; Convert octets to string if not already a string
         (flexi-streams:octets-to-string response :external-format :utf-8)))))

(defun google-get (uri api-key)
  "Perform an HTTP GET of a JSON object from the Google API."
  (decode-google-json-response
   (dexador:get uri
                :headers `(("Accept" . "application/json")
                           ("x-goog-api-key" . ,api-key)))))

(defun google-post (uri api-key payload)
  "Perform an HTTP POST of a JSON object to the Google API."
  (decode-google-json-response
   (dexador:post uri
                 :headers `(("Accept" . "application/json")
                            ("Content-Type" . "application/json")
                            ("x-goog-api-key" . ,api-key))
                 :content (cl-json:encode-json-to-string payload))))
