(defproject markov-lean-shades "0.1.0-SNAPSHOT"
  :description "Generates Fifty Shades of Lean Startup"
  :url "https://twitter.com/ShadesOfLean"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [twitter-api "0.7.8"]
                 [environ "1.0.0"]]
  :main markov-lean-shades.publisher
  :plugins [[lein-environ "1.0.0"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.7.0"]]}})
