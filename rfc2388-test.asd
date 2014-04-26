(asdf:defsystem :rfc2388-test
  :author "Janis Dzerins <lisp@jonis.lv>"
  :version "0.1"
  :licence "Simplified BSD"
  :description "Tests for RFC2388"
  :long-description "Uses Drakma and Hunchentoot to test RFC2388."
  :depends-on ("rfc2388" "hunchentoot" "drakma" "alexandria")
  :components ((:module "test"
                :components ((:file "support")
                             (:file "tests"
                              :depends-on ("support")
                              :encoding :utf-8)))))
