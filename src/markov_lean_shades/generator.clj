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
    (partition-all 2 1 words)))

(defn word-chain [partitions]
  (reduce (fn [r t] (merge-with clojure.set/union r
                                 (let [[a b] t]
                                   {[a] (if b #{b} #{})})))
           {}
           partitions))

(defn text->word-chain [text]
  (-> text
      word-transitions
      word-chain))

(defn chain->text [[first-word & remaining]]
  (apply str first-word " " (interpose " " remaining)))

(defn walk-chain [chain result]
  "Build a chain until the text version would hit 140 characters"
  (let [prefix (vector (last result))
        suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [prefix suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (> new-result-char-count 140)
          result
          (recur chain (conj result suffix)))))))


(defn generate-text [prefix chain]
  (let [;prefix (clojure.string/split prefix #" ")
        result-chain (walk-chain chain prefix)
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
  "All potential starting points for the generator"
  (keys (filter (fn [[prefix suffixes]]
                  (and (not (empty? suffixes))
                       (re-find #"^[A-Z][a-z]+[^\.,!\(\)]$" (first prefix))  ; avoid starting
                       ;(re-find #"^[a-z0-9]+[^\.,!\(\)]$" (second prefix))
                       )) ; with punctuation
                corpus)))

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
  (let [prefix (-> branching-prefixes rand-nth)
        phrase (trim-phrase (generate-text prefix corpus))
        lean-min (+ 1 (rand-int 2)) ; 1-2
        shades-min (- 4 lean-min)   ; 2-3
        valid? (and (>= (score phrase lean-words) lean-min)
                    (>= (score phrase shades-words) shades-min))]
    (if valid?
      phrase
      (recur))))

