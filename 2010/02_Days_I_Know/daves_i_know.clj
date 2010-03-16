(require 'drinking_functions)

(def MAP
  (hash-map
    '1 'Mon
    '2 'Tue
    '3 'Wed
    '4 'Thu
    '5 'Fri
    '6 'Sat
    '7 'Sun))

(defn prompt-user [prompt]
  (print (str prompt " "))
  (flush)

  (let [user-input (re-seq #"^[1-7]{0,1}(,[1-7])*$" (read-line))]
    (if (= (count user-input) 0)
      (recur prompt)
      (ffirst user-input))))

(defn parse-input [user-input]
  (if (> (.length user-input) 0)
    (map #(Integer/parseInt %) (set (.split user-input ",")))
    (list)))


(defn find-end-range-index [sorted-seq]
  (loop [item (first sorted-seq) 
         seq (rest sorted-seq) 
         acc 0]
    (let [is-empty-seq (zero? (count seq))
          is-next-part-of-range (and (not is-empty-seq) (= (+ 1 item) (first seq)))]

      (cond 
        (and (not is-next-part-of-range) (> acc 1)) 
          acc
        (not is-next-part-of-range) 
          0
        :else 
          (recur (first seq) (rest seq) (+ acc 1))))))

(defn construct-ranges [sorted-seq]
  (cond 
    (= (count sorted-seq) 0) 
      ()
    :else 
      (let [end-range-index (find-end-range-index sorted-seq)]
        (if (= end-range-index 0) 
          (cons (list (first sorted-seq)) (construct-ranges (rest sorted-seq)))
          (cons (list (first sorted-seq) (nth sorted-seq end-range-index)) 
                (construct-ranges (drop (+ 1 end-range-index) sorted-seq)))))))

(defn create-output [range-seq]
  (cond
    (zero? (count range-seq))
      ""
    (= (count range-seq) 1)
      (if (= (count (first range-seq)) 1)
        (get MAP (ffirst range-seq))
        (str (get MAP (ffirst range-seq)) "-" (get MAP (second (first range-seq)))))
    :else
      (str (create-output (list (first range-seq))) "," (create-output (rest range-seq)))))

(let [user-input (prompt-user "Days:")
      parsed-input (parse-input user-input)
      sorted-input (sort parsed-input)
      ranged-seq   (construct-ranges sorted-input)
      output (create-output ranged-seq)]

  (println "Drinking Days: " output)
  (create-display sorted-input))
