(ns markov-lean-shades.generator
  (:require clojure.set))

(def lean-words
  "Frequent words in lean corpus"
  (let [words #{"CTO" "Lean" "MVP" "VC" "adopting" "assumptions" "audience" "branding" "conversion"
                "cross-functional" "disrupt" "download" "experimental" "experiments" "founder" "founders"
                "funnel" "hacker" "hackers" "hacking" "hypotheses" "launch" "launching" "makers" "measure"
                "method" "methodology" "model" "pivot" "practices" "riskiest" "software" "start-up"
                "start-ups" "startup" "startups" "strategic" "strategy" "teams" "traditional" "validate"
                "validation"}]
    (clojure.set/union words (map clojure.string/capitalize words))))


(def shades-words
  "Frequent &/ salacious words in Fifty Shades"
  (let [words #{"Ana" "Anastasia" "Christian" "Oh" "Perspex" "back" "bath" "behind" "biting"
                "black" "breath" "breathe" "cable" "contract" "dominant" "don't" "eyes" "fingers"
                "gently" "hair" "hard" "hips" "kisses" "leather" "legs" "me" "mouth" "murmurs" "pulls"
                "red" "safe" "says" "sex" "smiles" "staring" "submissive" "tie"}]
    (clojure.set/union words (map clojure.string/capitalize words))))

;; frequency counting of source texts
(defn freq-words [filename]
  "Returns frequent words that aren't the *most* frequent"
  (let [file-text (-> filename clojure.java.io/resource slurp)
        freqs (frequencies (clojure.string/split file-text #"\s"))]
    (->> freqs
         (sort-by val)
         reverse
         (take 200)
         (drop 50)
         println)))

;; (freq-words "lean.txt")
;; (freq-words "inc-lean.txt")
;; (freq-words "hbs-lean.txt")
;; (freq-words "fifty-shades.txt")

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

(def branching-prefixes
  "Find potential starting point for the generator"
  (keys (filter (fn [[prefix suffixes]]
                  (and (re-find #"^[A-Z][a-z]+[^\.,!\(\)]$" (first prefix))
                       (re-find #"^[a-z0-9]+[^\.,!\(\)]$" (second prefix)))) ; avoid starting with the end
                lean-corpus)))

(defn score [phrase targets]
  "Scores phrases by coincidence with words in targets (set)"
  (count (clojure.set/intersection targets
                                   (-> phrase
                                       (clojure.string/replace #"[^a-zA-Z\s]" "") ; ignore punctuation
                                       (clojure.string/split #"\s")
                                       set))))

(defn trim-phrase [phrase]
  (clojure.string/replace phrase #"[.,][^\.,]*$" "."))

(defn gen-random []
  "Generate a random 50 Shades of Lean phrase"
  (let [prefix (-> branching-prefixes rand-nth chain->text)
        phrase (trim-phrase (generate-text prefix corpus))
        valid? (and (>= (score phrase lean-words) 2)
                    (>= (score phrase shades-words) (inc (rand-int 2))))]
    (if valid?
      phrase
      (recur))))

(gen-random)
