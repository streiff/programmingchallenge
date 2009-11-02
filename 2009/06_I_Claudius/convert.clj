(def numerals 
    {1000 'M 900 'CM 500 'D 400 'CD 100 'C 90 'XC 
     50 'L 40 'XL 10 'X 9 'IX 5 'V 4 'IV 1 'I }
) 

(defn error [message]
    (println message)
    (System/exit 1)
)

(defn get-command-line-arg [] 
    (try (let [i (Integer/parseInt (first *command-line-args*))] 
          (if (< i 1) (error "Number cannot be smaller than one"))
          (if (> i 3999) (error "Number must be smaller than 4000"))
          i)
    (catch NumberFormatException e (error "Not a number")))
)

(defn get-highest-symbol [number]
    (first (sort (fn [x y] (- y x)) (filter #(>= number %) (keys numerals))))
)

(defn convert [number]
    (if (> number 0)
        (str (get numerals (get-highest-symbol number))
             (convert (- number (get-highest-symbol number))))
    )
)

(println (convert (get-command-line-arg)))
