(def input (->> 
  (str/split (slurp "day2/input") #",")
  (map asInt)
  vec))

(defn step [idx context f]
  (update context (nth context (+ idx 3)) (constantly (f (nth context (nth context (+ idx 2))) (nth context (nth context (+ idx 1)))))))

(println 
(step 0 (vec '(1 9 10 3 2 3 11 0 99 30 40 50)) #(+ %1 %2)))

(defn init [context values]
  (loop [[f s & r] values
        res context]
    (if (nil? f)
        res
        (recur r (update res f (constantly s))))))

(defn compute[input]
  (loop [idx 0
        context input]
    (let [dx (nth context idx)]
      (if (= dx 99)
        context
        (recur (+ idx 4) (step idx context #(if (= dx 1) (+ %1 %2) (* %1 %2))))))))
    
   
(println (compute (vec '(1 9 10 3 2 3 11 0 99 30 40 50))))
(println (compute (vec '(1 1 1 4 99 5 6 0 99))))

;; (println (take 3 (compute (init input [1 12 2 2]))))

(println (->> (range 100)
  (map #(map (fn[x] (list 1 % 2 x)) (range 100)))
  flatten
  (partition 4)
  (map vec)
  (some #(when (= (first (compute (init input %))) 19690720)
   (compute (init input %))))
  (take 3)
))


;; (def xf (comp 
;;   (map #(map (fn[x] (list 1 % 2 x)) (range 100)))
;;   flatten
;;   (partition-all 4)
;;   (map vec)
;;   (map #(compute (init input %)))
;;   (take 3)))

;; (println (eduction xf (range 100)))

;; (some #(when (= (first %) 19690720)
;;    (take 3 %)) (eduction xf (range 100)))



