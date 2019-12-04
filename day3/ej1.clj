(def test_one (slurp "day3/input"))
(def input (->> 
     (str/split-lines test_one)
     (map #(str/split %1 #","))))

;;(println input)

(defn expand_direction [coord idx]
  (let [n (inc (asInt (str/join (rest coord))))]
    (case (first coord)
      \R (map #(list (+ %1 (first idx)) (second idx)) (range 1 n))
      \L (map #(list (- (first idx) %1) (second idx)) (range 1 n))
      \U (map #(list (first idx) (+ %1 (second idx))) (range 1 n))
      \D (map #(list (first idx) (- (second idx) %1)) (range 1 n))
      []
    )))

;; (println (last (expand_direction "R988" [1 1])))    
;; (println (expand_direction "R988" [1 1]))

(defn contiene? [i col]
  (some #(= i %1) col)
)

(defn expand_wire [wire_code]
  (let [b (list 0 0)
      coords (list b)]
    (loop [code wire_code idx b res coords]
      (if (empty? code)
        res
        (recur (rest code) 
               (last (expand_direction (first code) idx)) 
               (concat res (expand_direction (first code) idx)))))))

(def distance #(+ (Math/abs (second %)) (Math/abs (first %))))

(def mesh (map expand_wire input))

(def cross_points (sort-by distance (clojure.set/intersection (set (first mesh)) (set (second mesh)))))

(println (distance (second cross_points)))
(println (take 2 (sort (map #(+ (.indexOf (vec (first mesh)) %1) (.indexOf (vec (second mesh)) %1)) cross_points))))