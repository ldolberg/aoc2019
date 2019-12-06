
(defn expand [s]
  (let [n (last s)]
    (map #(conj s %1) (range n 10))))

(defn cmp [s n] (>= (Integer/parseInt (str/join "" s)) n))

(def input (filter #(cmp %1 24) (apply concat (map #(expand [%1]) (range 0 10)))))

(defn at_least_two [s]
  (not (= (count s) (count (set s)))))

(defn done? [s]
  (cmp s 746315))

(defn only_two [is]
  (= (loop [r is n 1]
       (let [f (first r) s (second r) rs (rest r)]
         (if (empty? rs)
           n
           (if (= f s) (recur rs (inc n))
               (if (= n 2) n (recur rs 1))))))
     2))

(def output
  (filter #(cmp %1 248345)
          (loop [pending input res []]
            (let [fp (first pending)]
              (if (or (empty? pending) (done? fp))
                res
                (if (= 6 (count fp))
                  (recur (rest pending) (conj res fp))
                  (recur (concat (rest pending) (expand fp)) res)))))))

(time (count (filter at_least_two output)))

(time (count (filter only_two (filter at_least_two output))))