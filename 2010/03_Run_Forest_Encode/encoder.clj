(use 'clojure.contrib.duck-streams)

(defn compress [regex-group]
  (let [string (first regex-group)]
    (if (or (> (.length string) 3) (= (.charAt string 0) \~))
      (str \~ (char (+ 64 (.length string))) (.charAt string 0))
      string)))

(spit (str (first *command-line-args*) ".encoded")
  (apply str 
    (map compress (re-seq #"(?s)(.)(\1){0,25}" (slurp (first *command-line-args*))))))
