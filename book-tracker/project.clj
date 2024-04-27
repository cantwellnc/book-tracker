(defproject book-tracker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [cheshire "5.12.0"]
                 [hiccup "2.0.0-RC3"]
                 [clj-http "3.12.3"]
                 [org.clj-commons/hickory "0.7.4"]
                 [ring "1.11.0-RC1"]
                 [com.draines/postal "2.0.5"]]
  :repl-options {:init-ns book-tracker.core}
  :ring {:handler book-tracker.core/handler}
  :plugins [[lein-ring "0.12.5"]])
