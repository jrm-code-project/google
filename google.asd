(defsystem "google"
  :description "API to Google"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-json"
               "dexador"
               "fold"
               "function"
               "jsonx"
               "named-let"
               "series"
               "str"
               )
  :components ((:file "apikey"  :depends-on ("package"))
               (:file "blogger" :depends-on ("apikey" "common" "package"))
               (:file "common"  :depends-on ("package"))
               (:file "cse"     :depends-on ("apikey" "common" "package"))
               (:file "package")))
