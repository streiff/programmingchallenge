(defn pascal-triangle-next-row [curr-row]
  (map #(+' %1 %2) (cons 0 curr-row) (concat curr-row '(0))))

(defn pascal-triangle
  ([] (pascal-triangle '(1)))
  ([n] (lazy-seq (cons n (pascal-triangle (pascal-triangle-next-row n))))))

(defn xbm-export-line [pascal-row padding-str stilted bits]
  (let [raw-bytes (str padding-str
                       (clojure.string/join (map #(if (odd? %) bits "00") pascal-row))
                       padding-str)]
    (if stilted 
      (clojure.string/replace raw-bytes #"(.)(.)" "$2$1") 
      raw-bytes)))

(defn xbm-export-row [pascal-row pascal-rows-size]
  (let [padding (- pascal-rows-size (count pascal-row))
        padding-str (apply str (repeat padding "0"))
        stilted (odd? padding)
        bits (if stilted 
                '("81" "81" "C3" "C3" "E7" "E7" "FF" "FF")
                '("18" "18" "3C" "3C" "7E" "7E" "FF" "FF"))
        all-bytes (clojure.string/join "\n" (map #(xbm-export-line pascal-row padding-str stilted %) bits))]
    (clojure.string/replace all-bytes #"(..)" "0x$1, ")))

 
(defn xbm-export [num-rows]
  (println "#define test_width  " (* num-rows 8))
  (println "#define test_height " (* num-rows 8))
  (println "static char test_bits[] = {")
  (dorun (map #(println (xbm-export-row % num-rows)) (take num-rows (pascal-triangle))))
  (println "};"))

(xbm-export (Integer/valueOf (first *command-line-args*)))
