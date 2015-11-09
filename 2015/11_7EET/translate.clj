(import '(java.io BufferedReader))

(def dict-word-list (clojure.string/split (slurp "/usr/share/dict/words") #"\n"))

(def simple-sub-map (array-map 
    #"(?i)10100111001"      '("leet")
    #"(?i)n00b"             '("newbie")
    #"(?i)pwnd"             '("pwned")
    #"(?i)pwnt"             '("pwned")
    #"(?i)dafuq"            '("what the f**k")
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
    #"(?i)\$"               '("s")
    #"(?i)\("               '("c")
    #"(?i)z"                '("s")
    #"(?i)5"                '("s")
    #"(?i)@"                '("a")
    #"(?i)4"                '("a")
    #"(?i)3"                '("e")
    #"(?i)7"                '("t" "and" "anned" "ant")
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
    #"(?i)c"                '("see" "c" "sea")
    #"(?i)b"                '("be" "B" "bee")
    #"(?i)&"                '("and" "anned" "ant")
))


(defn preprocess-punctuation [lines]
    (map #(clojure.string/replace % #"(\.|,|\?)" " $1 ") lines))

(defn split-words [lines]
    (map #(clojure.string/split % #"\s+" ) lines))

(defn prepare-list [words]
    (map list words))

(defn word-regex-replace [word regex replacement]
    (clojure.string/replace word regex replacement))

(defn word-regex-replace-multiple [word regex replacements]
    (map #(word-regex-replace word regex %) replacements))

(defn word-simple-sub [word]
    (distinct (flatten (map #(word-regex-replace-multiple word % (get simple-sub-map %)) (keys simple-sub-map)))))

(defn process-word-simple-sub [words num-passes]
    (if (= num-passes 0) words
        (recur (distinct (flatten (map word-simple-sub words))) (- num-passes 1))))

(defn simple-subs [word-lists]
    (map #(process-word-simple-sub % (count (first %))) word-lists))

(defn remove-accents [word-lists]
    (map (fn [words]
        (map (fn [word]
            (let [nfd-normalized-word (java.text.Normalizer/normalize word java.text.Normalizer$Form/NFD)]
                (clojure.string/replace nfd-normalized-word #"\p{InCombiningDiacriticalMarks}+" "")))
            words))
        word-lists))

(defn filter-choices [word-lists]
    (map (fn [words]
        (let [first-word (first words)
              filtered-words (filter some? (map (fn [word] (some (fn [dict-word] (if (.equalsIgnoreCase word dict-word) dict-word)) dict-word-list)) words))]
            (list (if (> (count filtered-words) 0) (first filtered-words) first-word)))) word-lists))

(defn sentencfy [word-lists]
    (clojure.string/split
        (clojure.string/replace 
            (clojure.string/join " " (flatten word-lists))
            #"\s*\." ".")
        #"\. "))

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
              (prepare-list)
              (simple-subs)
              (filter-choices)
              (remove-accents)
              (wordify)
              (exclamationify)))
