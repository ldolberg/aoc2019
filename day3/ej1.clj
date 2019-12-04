(def test_one (slurp "day3/input"))
(def input (->> 
     (str/split-lines test_one)
     (map #(str/split %1 #","))))

(defn expand_direction [coord idx]
  (let [n (inc (asInt (str/join (rest coord))))]
    (case (first coord)
      \R (map #(list (+ %1 (first idx)) (second idx)) (range 1 n))
      \L (map #(list (- (first idx) %1) (second idx)) (range 1 n))
      \U (map #(list (first idx) (+ %1 (second idx))) (range 1 n))
      \D (map #(list (first idx) (- (second idx) %1)) (range 1 n))
      []
    )))

(defn expand_wire [wire_code]
  (let [b (list 0 0)
      coords (list b)]
    (loop [code wire_code idx b res coords]
      (let [expanded (if (not-empty code) (expand_direction (first code) idx) nil)]
      (if (empty? code)
        res
        (recur (rest code) 
               (last expanded) 
               (concat res expanded)))))))

(def distance #(+ (Math/abs (second %)) (Math/abs (first %))))

(def mesh (map expand_wire input))

(def cross_points (rest (sort-by distance (clojure.set/intersection (set (first mesh)) (set (second mesh))))))

(println (distance (first cross_points)))
(println (reduce min (map #(+ (.indexOf (vec (first mesh)) %1) (.indexOf (vec (second mesh)) %1)) cross_points)))