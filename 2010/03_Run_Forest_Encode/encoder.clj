(use 'clojure.contrib.duck-streams)

(defn compress [regex-group] 
  (let [string (first regex-group)]
    (if (or (> (.length string) 3) (= (.charAt string 0) \~))
      (str \~ (char (+ 64 (.length string))) (.charAt string 0))
      string)))

(defn expand [string]
  (if (= (.length string) 3)
    (apply str (repeat (- (int (.charAt string 1)) 64) (.charAt string 2))) 
    string))

(let [mode (first *command-line-args*)
      in-file (nth *command-line-args* 1)
      out-file (nth *command-line-args* 2)
      function (if (= mode "encode") 'compress 'expand)
      regex (if (= mode "encode") '#"(?s)(.)(\1){0,25}" '#"(?s)~..|.")]

  (spit out-file
    (apply str (eval (list 'map function (list 're-seq regex (slurp in-file)))))))
