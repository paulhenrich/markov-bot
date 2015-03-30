(ns markov-elear.generator)

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

(defn chain->text [chain]
  (apply str (interpose " " chain)))

(chain->text ["and" "the" "pobble"])

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

(generate-text "want to" (process-file "lean.txt"))
(generate-text "it is" (process-file "lean.txt"))
(generate-text "to be" (process-file "lean.txt"))

(generate-text "if you" (process-file "paul.txt"))
(generate-text "for the" (process-file "paul.txt"))

(generate-text "How to" (process-file "blog-titles.txt"))

(filter (fn [el] (> (count (second el)) 2)) (process-file "lean.txt"))
(filter (fn [el] (> (count (second el)) 4)) (process-file "paul.txt"))
(filter (fn [el] (> (count (second el)) 2)) (process-file "blog-titles.txt"))
