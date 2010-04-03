(use 'clojure.contrib.duck-streams)

(defn expand [string]
  (if (= (.length string) 3)
    (apply str (repeat (- (int (.charAt string 1)) 64) (.charAt string 2)))
    string))

(spit 
  (str (first *command-line-args*) ".decoded")
  (apply str 
    (map expand (re-seq #"(?s)~..|." (slurp (first *command-line-args*))))))
