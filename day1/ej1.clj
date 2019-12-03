(defn fuel_per_module[n] (- (Math/floor (/ n 3)) 2))
(println (->> (slurp "day1/input")
    clojure.string/split-lines
    (map asInt)
    (map fuel_per_module)
    (reduce +)))