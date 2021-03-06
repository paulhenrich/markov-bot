(ns markov-lean-shades.publisher
  (:require [markov-lean-shades.generator :refer [gen-random]]
            [twitter.api.restful :as twitter]
            [twitter.oauth :as twitter-oauth]
            [environ.core :refer [env]]))

(def my-creds (twitter-oauth/make-oauth-creds
               (env :app-consumer-key)
               (env :app-consumer-secret)
               (env :user-access-token)
               (env :user-access-secret)))

(defn hashtag [phrase]
  ( -> phrase
       (clojure.string/replace #"Lean Startup" "#LeanStartup")
       (clojure.string/replace #"startup" "#startup")))

(defn tweet! []
  (let [tweet (hashtag (gen-random))]
    (println "Tweeting: " tweet)
    (try (twitter/statuses-update :oauth-creds my-creds
                                  :params {:status tweet})
         (catch Exception e (println "Error: " (.getMessage e))))))

(defn -main [& args]
  ;; tweet once & die (calling w/ heroku scheduler)
  (tweet!))
