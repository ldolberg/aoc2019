(defn digits [n]
 (map asInt (map str (seq (format "%05d" n)))))

(def input 
  (->> (str/split (slurp "day8/input") #"")
       (partition (* 25 6))))

(defn fetch[r k]
  (some #(when (= (first %1) k) (second %1)) r))

(->> input
    (map frequencies)
    (sort-by #(get %1 "0"))
    first
    (apply (fn[& r] (* (fetch r "1") (fetch r "2") )))
    println)

(defn col[img i]
  (map #(nth %1 i) img))

(def int_input (map #(map asInt %1)  input))

(defn rgb [img i]
  (let [row (col img i)]
    (first (filter #(not= 2 %1 ) row))))

(def output (partition 25 (map (partial rgb int_input) (range (* 25 6)))))

(defn draw_row[row]
  (str/join "" (map #(if (zero? %1) "." "X") row)))

(defn draw[img]
  (str/join "\n" (map draw_row img))
)

(println (draw output))