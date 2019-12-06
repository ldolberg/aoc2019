(defn fetch [mem idx mode]
  (if (zero? mode)
    (nth mem (nth mem idx))
    (nth mem idx)))

(defn fetch_n [mem idx mode n]
  (map #(fetch mem (+ idx %1) (nth mode (dec %1))) (range 1 (inc n))))

(defn digits [n]
 (map asInt (map str (seq (format "%05d" n)))))

(def pgm (map asInt (str/split (slurp "day5/input") #"," )))
;; (println pgm)

(defn compute[input] (loop [mem (vec pgm) idx 0 output (list)]
  ;; (println idx)
           (let [current (digits (nth mem idx))
                 opcode (take-last 2 current)
                 mode (reverse (drop-last 2 current))]
            ;;  (println current output)
             (if (= (last opcode) 9)
               output
               (if (= (last opcode) 1)
                 (recur (update mem (nth mem (+ 3 idx)) (constantly (apply + (fetch_n mem idx mode 2)))) (+ idx 4) output)
                 (if (= (last opcode) 2)
                   (recur (update mem (nth mem (+ 3 idx)) (constantly (apply * (fetch_n mem idx mode 2)))) (+ idx 4) output)
                   (if (= (last opcode) 3)
                     (recur (update mem (nth mem (+ 1 idx)) (constantly input)) (+ idx 2) output)
                     (if (= (last opcode) 4)
                       (recur mem (+ 2 idx) (conj output (fetch mem (inc idx) (first mode))))
                       (if (= (last opcode) 5)
                         (recur mem (if (not= 0 (fetch mem (inc idx) (first mode))) (fetch mem (+ 2 idx) (second mode)) (+ idx 3)) output)
                         (if (= (last opcode) 6)
                           (recur mem (if (= 0 (fetch mem (inc idx) (first mode))) (fetch mem (+ 2 idx) (second mode)) (+ idx 3)) output)
                           (if (= (last opcode) 7)
                             (recur (update mem (nth mem (+ 3 idx)) (constantly (if (< (fetch mem (inc idx) (first mode)) (fetch mem (+ 2 idx) (second mode))) 1 0))) (+ idx 4) output)
                             (if (= (last opcode) 8)
                             (recur (update mem (nth mem (+ 3 idx)) (constantly (if (= (fetch mem (inc idx) (first mode)) (fetch mem (+ 2 idx) (second mode))) 1 0))) (+ idx 4) output)
                             ))))))))))))


(println (compute 1))
(println (compute 5))