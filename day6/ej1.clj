(def input (->> (slurp "day6/input")
                str/split-lines
                (map #(str/split %1 #"\)"))))
                ;; {:node vecinos}
(def nodos (into () (set (flatten input))))
(defn adj[x] 
  (->> input
  (filter #(= x (first %1)))
  (map second)
  ))

(defn fx[x visited]
  (some #{x} (map first visited)))


(def output 
  (loop [pending (list ["COM" 0]) visited ()]
    (let [current (first pending)]
      (if (empty? pending)
        visited
        (recur 
          (concat (map #(list %1 (inc (second current))) (filter #(not (fx %1 visited)) (adj (first current))))
          (rest pending)) (conj visited current))))))

(->> output
  (map second)
  (apply +)
  println)

;; (println output)


(def ramas 
  (loop [pending (list ["COM" []]) visited ()]
    (let [current (first pending)]
      (if (empty? pending)
        visited
        (recur 
          (concat (map #(list %1 (conj (second current) %1)) (filter #(not (fx %1 visited)) (adj (first current))))
          (rest pending)) (conj visited current))))))

;; (println ramas)

(def prama (some #(when (= (first %1) "YOU") (second %1)) ramas))
(def srama (some #(when (= (first %1) "SAN") (second %1)) ramas))

(println (+ (- (count prama) (count (clojure.set/intersection (set prama) (set srama))))
(- (count srama) (count (clojure.set/intersection (set prama) (set srama))))))

(println prama)
(println srama)
