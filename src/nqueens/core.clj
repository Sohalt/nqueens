(ns nqueens.core)

(defn- diag? [[x1 y1] [x2 y2]]
  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn- conflicts? [[x1 y1] [x2 y2]]
  (or (= x1 x2) (= y1 y2) (diag? [x1 y1] [x2 y2])))

(defn nqueens [n]
  (letfn [(helper [n choosen possible]
            (if (> n (count possible))
              false
              (if (= n 0)
                choosen
                (let [c (first possible)
                      posleft (filter (comp not (partial conflicts? c)) possible)]
                  (or (helper (dec n) (conj choosen c) posleft) (helper n choosen (remove #{c} possible)))))))]
    (helper n [] (vec (for [x (range n) y (range n)] [x y])))))

(defn render-locs [n locs]
  (let [empty-grid (vec (repeat n (vec (repeat n "-"))))
        grid (reduce (fn [a l] (assoc-in a l "x")) empty-grid locs)]
    (doseq [line grid] (println line))))

(defn render-nqueens [n]
  (render-locs n (nqueens n)))

(defn -main [n]
  (render-nqueens (Integer. n)))
