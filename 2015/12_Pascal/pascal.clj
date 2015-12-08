(defn calculate-next-row [previous-row]
    (map #(+ (bigint %1) (bigint %2)) (cons 0 previous-row) (concat previous-row '(0))))

(defn append-rows [rows number-of-rows]
    (if (== number-of-rows 0)
        rows
        (recur (concat rows (vector (calculate-next-row (last rows)))) 
               (- number-of-rows 1))))

(defn calculate-pascal [number-of-rows]
    (cond (<= number-of-rows 0) []
          (== number-of-rows 1) '[[1]]
          :else (append-rows '[[1]] (- number-of-rows 1))))

(defn xbm-export-line [pascal-row padding-str stilted bits]
    (let [raw-bytes (clojure.string/join "" (concat
                         padding-str
                         (map #(if (= (mod % 2) 1) bits "00") pascal-row)
                         padding-str))
          aligned-bytes (if stilted (clojure.string/replace raw-bytes #"(.)(.)" "$2$1") 
                                    raw-bytes)]
        (str aligned-bytes "\n" aligned-bytes)))

(defn xbm-export-row [pascal-row pascal-rows-size]
    (def padding (* 4 (- pascal-rows-size (count pascal-row))))
    (def padding-str (repeat (/ padding 4) "0"))
    (def stilted (== (mod padding 8) 4))
    (def bits (if stilted '("81" "C3" "E7" "FF")
                          '("18" "3C" "7E" "FF")))

    (def all-bytes (clojure.string/join "\n" (map #(xbm-export-line pascal-row padding-str stilted %) bits)))

    (clojure.string/replace all-bytes #"(..)" "0x$1, "))

(defn xbm-export [pascal-rows]
    (def pascal-rows-size (count pascal-rows))

    (str "#define test_width  " (* pascal-rows-size 8) "\n"
         "#define test_height " (* pascal-rows-size 8) "\n"
         "static char test_bits[] = {\n"
         (clojure.string/replace 
             (clojure.string/join "\n" (map #(xbm-export-row % pascal-rows-size) pascal-rows))
             #", $", "")
         "};\n"))

(let [pascal (calculate-pascal (Integer/valueOf (first *command-line-args*)))]
  (println (xbm-export pascal))
)
