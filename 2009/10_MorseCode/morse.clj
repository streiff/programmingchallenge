(use 'clojure.set)

(def letters 
    {".-"   "a" "-..." "b" "-.-." "c" "-.."  "d" "."   "e" "..-." "f" "--."  "g" "...." "h" ".."  "i"
     ".---" "j" "-.-"  "k" ".-.." "l" "--"   "m" "-."  "n" "---"  "o" ".--." "p" "--.-" "q" ".-." "r"
     "..."  "s" "-"    "t" "..-"  "u" "...-" "v" ".--" "w" "-..-" "x" "-.--" "y" "--.." "z"} 
)
(defn matching-letters [morse] (filter #(.startsWith morse %) (keys letters)))
(defn flatten [x] (filter (complement sequential?) (rest (tree-seq sequential? seq x))))
(defn prepend [prefix list] (map #(str prefix %) list))

(defn decode [morse]
    (if (= 0 (count morse))
        '(nil)
        (flatten (map #(prepend (get letters %) (decode (subs morse (count %)))) (matching-letters morse)))
    )
)

(dorun (map #(print % " ") (decode (first *command-line-args*))))
(println "\nDictionary words")
(dorun (map #(print % " ")
    (intersection
         (set (map #(.toLowerCase %) (re-seq #"[^\n]+" (slurp "words"))))
         (set (decode (first *command-line-args*)))
    )
))
(println "\n")
