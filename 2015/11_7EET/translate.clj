(import '(java.io BufferedReader))

(def simple-sub-map (array-map 
    #"(?i)10100111001"      "leet"
    #"(?i)n00b"             "newbie"
    #"(?i)pwnd"             "pwned"
    #"(?i)pwnt"             "pwned"
    #"(?i)dafuq"            "what the f**k"
    #"(?i)j00"              "you"
    #"(?i)joo"              "you"
    #"(?i)vv"               "w"
    #"(?i)tomoz"            "tomorrow"
    #"(?i)(^|\W)teh(\W|$)"  "$1the$2"
    #"(?i)'d"               "ed"
    #"(?i)d00d"             "dude"
    #"(?i)<3"               "love"
    #"(?i)txt"              "text"
    #"(?i)(^|\W)2(\W|$)"    "$1to$2"
    #"(?i)(^|\W)4(\W|$)"    "$1for$2"
    #"(?i)(^|\W)kk?(\W|$)"  "$1OK$2"
    #"(?i)(^|\W)y(\W|$)"    "$1why$2"
    #"(?i)(^|\W)ur(\W|$)"   "$1you're$2"
    #"(?i)(^|\W)u(\W|$)"    "$1you$2"
    #"(?i)(\w)8|8(\w)"      "$1ate$2"
    #"(?i)\$"               "s"
    #"(?i)\("               "c"
    #"(?i)z"                "s"
    #"(?i)5"                "s"
    #"(?i)@"                "a"
    #"(?i)4"                "a"
    #"(?i)3"                "e"
    #"(?i)7"                "t"
    #"(?i)\+"               "t"
    #"(?i)\#"               "h"
    #"(?i)0"                "o"
    #"(?i)\\/\\/"           "w"
    #"(?i)/\\"              "a"
    #"(?i)\^"               "a"
    #"(?i)\\/"              "v"
    #"(?i)8"                "b"
    #"(?i)\|_\|"            "u"
    #"(?i)\|-\|"            "h"
    #"(?i)Я"                "r"
    #"(?i)\|<"              "k"
    #"(?i)\[\)"             "d"
    #"(?i)\|\)"             "d"
    #"(?i)><"               "x"
))

(def dict-word-list (clojure.string/split (slurp "/usr/share/dict/words") #"\n"))

(defn preprocess-punctuation [lines]
    (map (fn [line] (clojure.string/replace line #"(\.|,|\?)" " $1 ")) lines))

(defn split-words [lines]
    (map (fn [line] (clojure.string/split line #"\s+" )) lines))

(defn prepare-list [words]
    (map (fn [word] (list word)) words))

(defn word-simple-sub [word]
    (distinct (map (fn [key] (clojure.string/replace word key (get simple-sub-map key))) (keys simple-sub-map))))

(defn process-word-simple-sub [words num-passes]
    (if (= num-passes 0) words
        (process-word-simple-sub (distinct (flatten (map word-simple-sub words))) (- num-passes 1))))

(defn simple-subs [word-lists]
    (map (fn [words] (process-word-simple-sub words (count (first words)))) word-lists))

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


(println
         (->> (line-seq (BufferedReader. *in*))
              (preprocess-punctuation)
              (split-words)
              (flatten)
              (prepare-list)
              (simple-subs)
              (filter-choices)
              (remove-accents)
))

; not covered by simple
; x,X    -> -ks-,-cks-
; 1      -> i,I,l,L
; !      -> i,I,!
; c,C    -> see,C,sea
; b,B    -> be,B,bee
; &,7    -> and,anned,ant (may be used in the middle of a word)
