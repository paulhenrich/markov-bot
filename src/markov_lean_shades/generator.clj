(ns markov-lean-shades.generator
  (:require clojure.set))

(def lean-words
  (let [ words #{"Ries" "Lean" "LSM" "adopting" "practices" "funnel" "growth" "hacking"
                 "hacker" "VC" "teams" "cross-functional" "strategic" "strategy" "failure"
                 "startup" "branding" "entrepreneur" "founder" "CTO" "disrupt" "MVP"
                 "experiment" "experimental" "validate" "experiments"}]
    (clojure.set/union words (map clojure.string/capitalize words))))

(defn word-transitions [sample]
  (let [words (clojure.string/split sample #"[\s|\n]")]
    (partition-all 3 1 words)))

(defn word-chain [partitions]
  (reduce (fn [r t] (merge-with clojure.set/union r
                                 (let [[a b c] t]
                                   {[a b] (if c #{c} #{})})))
           {}
           partitions))

(defn text->word-chain [text]
  (-> text
      word-transitions
      word-chain))

(defn chain->text [[first-word & remaining]]
  (apply str (clojure.string/capitalize first-word) " " (interpose " " remaining)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (> new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn generate-text [prefix chain]
  (let [prefix (clojure.string/split prefix #" ")
        result-chain (walk-chain prefix chain prefix)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file [filename]
  (-> filename
      clojure.java.io/resource
      slurp
      text->word-chain))

(def lean-corpus
  (merge-with {}
              (process-file "lean.txt")
              (process-file "blog-titles.txt")))

(def corpus
  (merge-with {}
              lean-corpus
              (process-file "fifty-shades.txt")))


(defn rand-prefix []
  (rand-nth (keys (filter
                   (fn [el]
                     (= 0 (count (clojure.set/intersection lean-words (set (first el))))))
                   lean-corpus))))

(defn gen-random []
  (generate-text (chain->text (rand-prefix)) corpus))

(defn up-to-n-random-phrases [n]
  (dotimes [i n]
    (let [phrase (gen-random)
          score (count (clojure.set/intersection lean-words (-> (clojure.string/split phrase #"\s")
                                                                 rest
                                                                 rest
                                                                 set)))]
      (when (> score 0)
        (println phrase)))))
(up-to-n-random-phrases 1000)
