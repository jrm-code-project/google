(defsystem "google"
  :description "API to Google"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-json"
               "dexador"
               "flexi-streams"
               "fold"
               "function"
               "jsonx"
               "named-let"
               "promise"
               "series"
               "str"
               )
  :components ((:file "apikey"  :depends-on ("package"))
               (:file "blogger" :depends-on ("apikey" "common" "package"))
               (:file "common"  :depends-on ("package"))
               (:file "cse"     :depends-on ("apikey" "common" "package"))
               (:file "package")))
