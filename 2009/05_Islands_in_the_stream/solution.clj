(defn remove-all [coords points] (remove #(some #{%} points) coords))
(defn abs [num] (if (< num 0) (* num -1) num))
(defn adj? [p1 p2] (and (<= (abs (- (first p1) (first p2))) 1)
                        (<= (abs (- (last p1) (last p2))) 1)))
(defn get-adj [coords point] (filter #(adj? point %) coords))

(defn get-adj-deep [coords points]
    (loop [p points c coords r points]
        (if (> (count p) 0)
           (recur (into (rest p) (get-adj c (first p)))
                  (remove-all c (get-adj c (first p))) 
                  (into r (get-adj c (first p))))
            r
        )
    )
)

(defn find-+-from-line 
    ([line] (find-+-from-line line 0))
    ([line index]
        (if (= 1 (count line))
            (if (= "+" line) (list index))
    
            (concat (find-+-from-line (str (first line)) index) 
                  (find-+-from-line (rest line) (inc index))
            )
        )
    )
)

(defn get-coords 
    ([lines] (get-coords lines 0))
    ([lines index]
        (if (= (count lines) 1)
            (map #(list % index) (first lines))
            (concat (get-coords (list (first lines)) index)
                    (get-coords (rest lines) (inc index)))
        )
    )
)

(defn collate 
    ([coords] (collate coords '()))
    ([coords sorted-coords] 
        (if (= (count coords) 0)
            sorted-coords
            (let [points (get-adj-deep (rest coords) (list (first coords)))]
                (collate (remove-all coords points) (conj sorted-coords points))
            )
        )
    )
)

(defn get-landmass-count [str-map]
    (map #(count %) (collate (get-coords (map find-+-from-line (.split str-map "\n")))))
)

(println (slurp (first *command-line-args*)))
(let [results (get-landmass-count (slurp (first *command-line-args*)))]
    (print (count results) "continent")
    (if (= (count results) 1) (println) (println "s"))
    (dorun (map #(println %) results))
)
