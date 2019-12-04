(defn expand [s]
  (let [n (last s)]
  (map #(conj s %1) (range n 10))))

(defn cmp [s n] (>= (Integer/parseInt (str/join "" s)) n))

(def input (filter #(cmp %1 24) (apply concat (map #(expand [%1]) (range 0 10)))))

(defn at_least_two [s]
  (not (= (count s) (count (set s))))
)

(defn done? [s]
  (cmp s 746315))

(defn only_two[is]
  (= (loop [r is n 1]
      (let [f (first r) s (second r)]
        (if (empty? (rest r))
          n
          (if (= f s) (recur (rest r) (inc n))
            (if (= n 2) n (recur (rest r) 1))))))
      2))

(def output 
      (filter #(cmp %1 248345) 
        (loop [pending input res []]
          (if (or (empty? pending) (done? (first pending)))
            res
            (if (= 6 (count (first pending)))
              (recur (rest pending) (conj res (first pending)))
              (recur (concat (rest pending) (expand (first pending))) res))))))
;; (println output)
(println (count (filter at_least_two output)))

(println (count (filter only_two (filter at_least_two output))))