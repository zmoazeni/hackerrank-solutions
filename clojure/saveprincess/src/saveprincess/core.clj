(ns saveprincess.core
  (:gen-class))

(defrecord Cell [position value])

(defn is-mario? [char] (= char \m))
(defn is-princess? [char] (= char \p))

(defn find-in-matrix [condition matrix]
  (let [finder (fn [{position :position value :value}] (condition value))]
    (first (map :position (filter finder matrix)))))

(defn get-mario-coords [matrix] (find-in-matrix is-mario? matrix))
(defn get-princess-coords [matrix] (find-in-matrix is-princess? matrix))

(defn repeat-step [steps direction]
  (let [abs-steps (Math/abs steps)
        infinite-directions (repeat direction)]
    (take abs-steps infinite-directions)))

(defn get-path-between-coords [[my mx & _] [py px & _]]
  (let [left-or-right (if (> mx px) "LEFT" "RIGHT")
        up-or-down (if (> my py) "UP" "DOWN")
        steps-horizontal (repeat-step (- mx px) left-or-right)
        steps-vertical (repeat-step (- my py) up-or-down)]
        (vec (concat steps-horizontal steps-vertical))))

(defn displayPathToPrincess
  [n matrix]
  (let [mario-coords (get-mario-coords matrix)
        princess-coords (get-princess-coords matrix)
        path (get-path-between-coords mario-coords princess-coords)]
    (doseq [direction path] (println direction))))

(defn -main []
  (let [dimension (read-string (read-line))
        grid (doall (take dimension (map #(seq (.toCharArray %)) (repeatedly #(read-line)))))
        list-of-cells (apply concat (map-indexed
          (fn [y chars] (map-indexed (fn [x char] (Cell. [y x] char) ) chars)) grid))
        matrix (vec list-of-cells)]
    (display-path-to-princess dimensions matrix)))
