(defn calculate-next-row [previous-row]
  (if previous-row
    (map #(+ (bigint %1) (bigint %2)) (cons 0 previous-row) (concat previous-row '(0)))
    '[1]))

(defn calculate-pascal
  ([number-of-rows] (calculate-pascal number-of-rows '[]))
  ([number-of-rows rows]
    (if (== number-of-rows 0)
      rows
      (recur (dec number-of-rows) (concat rows (vector (calculate-next-row (last rows))))))))

(defn xbm-export-line [pascal-row padding-str stilted bits]
  (let [raw-bytes (str padding-str
                       (clojure.string/join (map #(if (odd? %) bits "00") pascal-row))
                       padding-str)]
        (if stilted 
            (clojure.string/replace raw-bytes #"(.)(.)" "$2$1") 
            raw-bytes)))

(defn xbm-export-row [pascal-row pascal-rows-size]
  (let [padding (* 4 (- pascal-rows-size (count pascal-row)))
        padding-str (apply str (repeat (/ padding 4) "0"))
        stilted (== (mod padding 8) 4)
        bits (if stilted 
                '("81" "81" "C3" "C3" "E7" "E7" "FF" "FF")
                '("18" "18" "3C" "3C" "7E" "7E" "FF" "FF"))
        all-bytes (clojure.string/join "\n" (map #(xbm-export-line pascal-row padding-str stilted %) bits))]
    (clojure.string/replace all-bytes #"(..)" "0x$1, ")))

(defn xbm-export [pascal-rows]
  (let [pascal-rows-size (count pascal-rows)]
    (str "#define test_width  " (* pascal-rows-size 8) "\n"
         "#define test_height " (* pascal-rows-size 8) "\n"
         "static char test_bits[] = {\n"
         (clojure.string/replace 
             (clojure.string/join "\n" (map #(xbm-export-row % pascal-rows-size) pascal-rows))
             #", $", "")
         "};\n")))

(println (xbm-export (calculate-pascal(Integer/valueOf (first *command-line-args*)))))
