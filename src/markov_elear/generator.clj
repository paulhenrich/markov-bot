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
(def corpus (merge-with {}
                        (process-file "lean.txt")
                        (process-file "fifty-shades.txt")))
(generate-text "The Lean" corpus)
(generate-text "powerful tool" corpus)
(generate-text "The challenge" corpus)
(generate-text "deliver value" corpus)
(generate-text "Think goals" corpus)
(generate-text "To validate" corpus)
(generate-text "Brant Cooper," corpus)
(generate-text "Powerful new" corpus)
(generate-text "For example," corpus)
(generate-text "The learning" corpus)
(generate-text "No matter" corpus)
(generate-text "experts may" corpus)
(generate-text "best practices" corpus)
(generate-text "for Pinterest," corpus)
(generate-text "Lean Startup," corpus)


;;(filter (fn [el] (> (count (second el)) 2)) (process-file "lean.txt"))
;;(filter (fn [el] (> (count (second el)) 2)) (process-file "fifty-shades.txt"))

