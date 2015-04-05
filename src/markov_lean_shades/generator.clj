(ns markov-lean-shades.generator
  (:require clojure.set))

(def lean-words
  "Frequent words in lean corpus"
  (let [words #{"Lean" "LSM" "adopting" "practices" "funnel" "hacking" "pivot" "hypotheses" "traditional"
                "hacker" "hackers" "makers" "VC" "teams" "cross-functional" "strategic" "strategy"
                "startup" "startups" "start-ups" "start-up" "branding" "founder" "founders" "CTO" "disrupt" "methodology"
                "MVP" "assumptions" "riskiest" "experimental" "validate" "validation" "experiments"
                "software" "download" "IMVU"}]
    (clojure.set/union words (map clojure.string/capitalize words))))

(def shades-words
  "Frequent &/ salacious words in Fifty Shades"
  (let [words #{"Oh" "pulls" "Subject:" "contract" "don't" "Christian" "Ana" "Anastasia" "dominant" "submissive"
                "gently" "hard" "me" "safe" "sex" "leather" "mouth" "murmurs" "cable" "tie" "biting" "hips"
                "breathe" "breath" "eyes" "staring" "fingers" "bath" "Perspex" "black" "red" "legs"
                "behind" "smiles"}]
    (clojure.set/union words (map clojure.string/capitalize words))))

(defn word-transitions [sample]
  "Transform text into trigrams"
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
  (apply str first-word " " (interpose " " remaining)))

(defn walk-chain [prefix chain result]
  "Build a chain until the text version would hit 140 characters"
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
  (merge (process-file "lean.txt")
         (process-file "inc-lean.txt")
         (process-file "hbs-lean.txt")))

(def shades-corpus
  (process-file "fifty-shades.txt"))

(def corpus
  (merge lean-corpus shades-corpus))

;; ; frequency counting of source texts
;; (def shades-text
;;   (-> "inc-lean.txt" clojure.java.io/resource slurp))
;; (def shades-freq
;;   (frequencies (clojure.string/split shades-text #"\s")))
;; (->> shades-freq
;;      (sort-by val)
;;      reverse
;;      (take 250)
;;      println)

(def branching-prefixes
  "Find potential starting point for the generator"
  (keys (filter (fn [[prefix suffixes]]
                  (and (re-find #"[A-Z0-9]" (first prefix)) 
                       (> (count suffixes) 0)))
                lean-corpus)))

(defn gen-random []
  (let [rand-prefix (-> branching-prefixes rand-nth chain->text)]
    (generate-text rand-prefix corpus)))

(gen-random)
(defn score [phrase targets]
  "Scores phrases by coincidence with words in targets (set)"
  (count (clojure.set/intersection targets
                                   (-> phrase
                                       (clojure.string/replace #"[^a-zA-Z\s]" "") ; strip punctuation
                                       (clojure.string/split #"\s") ; tokenize
                                       set)))) ;
(defn up-to-n-random-phrases [n]
  "A placeholder for a smarter front-end"
  (dotimes [i n]
    (let [phrase (gen-random)]
      (when (and (>= (score phrase lean-words) 1)
                 (>= (score phrase shades-words) 1))
        (println phrase)))))

(up-to-n-random-phrases 1000)
