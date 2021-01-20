(defproject clj-arduino "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
;;                 [clodiuno "0.0.4-SNAPSHOT"]
                 [com.github.billwinkler/clodiuno "0.0.4-with-port-id-fix"]]
  :repl-options {:init-ns clj-arduino.core}
  :repositories [["jitpack" "https://jitpack.io"]])
