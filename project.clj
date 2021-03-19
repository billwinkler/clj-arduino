(defproject clj-arduino "0.1.4-SNAPSHOT"
  :description "A Clojure Arduino prototyping library"
  :url "https://com.github.billwinkler/clj-arduino"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 ;; [clodiuno "0.0.4-SNAPSHOT"]
                 [com.github.billwinkler/clodiuno "0.0.4-with-port-id-fix"]
                 [org.clojure/core.async "1.3.610"]]
  :repl-options {:init-ns clj-arduino.core}
  :repositories [["jitpack" "https://jitpack.io"]])
