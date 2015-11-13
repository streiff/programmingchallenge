(import '(java.io BufferedReader))
(import '(java.util Collections))

(def word-cmp #(compare (.toLowerCase %1) (.toLowerCase %2)))

(def dict-word-list (sort word-cmp (clojure.string/split (slurp "W") #"\n")))

(def simple-sub-map (array-map 
    #"(?i)10100111001"      '("leet")
    #"(?i)(^|[^0])b"        '("$1bee" "$1be")
    #"(?i)n00b"             '("newbie")
    #"(?i)pwnd"             '("pwned")
    #"(?i)pwnt"             '("pwned")
    #"(?i)j00"              '("you")
    #"(?i)joo"              '("you")
    #"(?i)vv"               '("w")
    #"(?i)tomoz"            '("tomorrow")
    #"(?i)(^|\W)teh(\W|$)"  '("$1the$2")
    #"(?i)'d"               '("ed")
    #"(?i)d00d"             '("dude")
    #"(?i)<3"               '("love")
    #"(?i)txt"              '("text")
    #"(?i)(^|\W)2(\W|$)"    '("$1to$2")
    #"(?i)(^|\W)4(\W|$)"    '("$1for$2")
    #"(?i)(^|\W)kk?(\W|$)"  '("$1OK$2")
    #"(?i)(^|\W)y(\W|$)"    '("$1why$2")
    #"(?i)(^|\W)ur(\W|$)"   '("$1you're$2")
    #"(?i)(^|\W)u(\W|$)"    '("$1you$2")
    #"(?i)(\w)8|8(\w)"      '("$1ate$2")
    #"(?i)c"                '("see" "c" "sea")
    #"(?i)&"                '("and" "anned" "ant")
    #"(?i)7"                '("t" "and" "anned" "ant")
    #"(?i)\$"               '("s")
    #"(?i)\("               '("c")
    #"(?i)z"                '("s")
    #"(?i)5"                '("s")
    #"(?i)@"                '("a")
    #"(?i)4"                '("a")
    #"(?i)3"                '("e")
    #"(?i)\+"               '("t")
    #"(?i)\#"               '("h")
    #"(?i)0"                '("o")
    #"(?i)\\/\\/"           '("w")
    #"(?i)/\\"              '("a")
    #"(?i)\^"               '("a")
    #"(?i)\\/"              '("v")
    #"(?i)8"                '("b")
    #"(?i)\|_\|"            '("u")
    #"(?i)\|-\|"            '("h")
    #"(?i)Я"                '("r")
    #"(?i)\|<"              '("k")
    #"(?i)\[\)"             '("d")
    #"(?i)\|\)"             '("d")
    #"(?i)><"               '("x")
    #"(?i)x"                '("ks" "cks")
    #"(?i)1|!"              '("i" "l")
    #"(?i)dafuq"            '("what the fuck")
))


(defn preprocess-punctuation [lines]
    (map #(clojure.string/replace % #"(\.|,|\?)" " $1 ") lines))

(defn split-words [lines]
    (map #(clojure.string/split % #"\s+" ) lines))

(defn word-regex-replace [word regex replacement]
    (clojure.string/replace word regex replacement))

(defn word-regex-replace-multiple [word regex replacements]
    (map #(word-regex-replace word regex %) replacements))

(defn word-simple-sub [word]
    (cons word (reduce (fn [word-list key]
        (def replacement-list (get simple-sub-map key))
        (distinct (flatten (map #(word-regex-replace-multiple % key replacement-list) word-list)))
        ) (list word) (keys simple-sub-map)))
)

(defn process-word-simple-sub [words num-passes]
    (if (= num-passes 0) words
        (recur (distinct (flatten (map word-simple-sub words))) (- num-passes 1))))

(defn simple-subs [word-lists]
    (map word-simple-sub word-lists))

(defn remove-accents [word-lists]
    (map (fn [words]
        (map (fn [word]
            (let [nfd-normalized-word (java.text.Normalizer/normalize word java.text.Normalizer$Form/NFD)]
                (clojure.string/replace nfd-normalized-word #"\p{InCombiningDiacriticalMarks}+" "")))
            words))
        word-lists))

(defn lookup-word [word]
    (def filtered-word-idx (Collections/binarySearch dict-word-list word word-cmp))
    (if (< filtered-word-idx 0) nil (nth dict-word-list filtered-word-idx)))

(defn filter-words [words]
    (filter lookup-word words))

(defn last-or-default [l default]
    (list (if (empty? l) default (last l))))

(defn filter-choices [word-lists]
    (map #(last-or-default (filter-words %) (last %)) word-lists))

(defn sentencfy [word-lists]
    (-> (flatten word-lists)
        (->> (clojure.string/join " "))
        (clojure.string/split #"\s*\.\s*")))

(defn wordify [word-lists]
    (apply str (interpose ". " 
        (map #(str (.toUpperCase (subs % 0 1)) (subs % 1)) (sentencfy word-lists)))))

(defn exclamationify [s]
    (clojure.string/replace (clojure.string/replace s #"(?i)!1|1!|!one|one!", "!!")
                            #"(?i)!eleven|eleven!" "!!!"))


(println (->> (line-seq (BufferedReader. *in*))
              (preprocess-punctuation)
              (split-words)
              (flatten)
              (simple-subs)
              (filter-choices)
              (remove-accents)
              (wordify)
              (exclamationify)))
