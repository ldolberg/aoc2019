;; (defn asInt [n] (Integer/parseInt n))

;; (defn fuel_per_module[n] (- (Math/floor (/ n 3)) 2))
(require '[clojure.java.io :as io])

;; (println (mapcat #(clojure.string/split % #"\n") (lines-reducible (io/reader "day1/input"))))

(defn calculate_fuel [x]
  (loop [n x tot 0]
    (let [fuel_per_n (fuel_per_module n)]
      (if (<= fuel_per_n 0)
        tot
        (recur fuel_per_n (+ tot fuel_per_n))))))
        
(def xf (comp    
                 (map asInt)
                 (map calculate_fuel)))
                 
(def input (->> (slurp "day1/input")
                 clojure.string/split-lines
                 ))

(println (transduce xf + input))