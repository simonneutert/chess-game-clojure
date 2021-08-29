(ns chess-clojure.core
  (:gen-class))
(require '[clojure.string :as str])

(def board (atom {}))
(def shadow-board (atom {}))
;; struct/map with magic -> a record
(defrecord Figurine [color type position-x position-y])

(defn show-move [^Figurine f direction]
  (println "Move" (:type f) (:color f) "to" direction))

(defmulti make-keyword (fn [x] [(type x)]))
(defmethod make-keyword [Figurine] [x]
  (make-keyword [(:position-x x) (:position-y x)]))
(defmethod make-keyword [clojure.lang.PersistentVector] [x]
  (-> x (str/join) (keyword)))

(defn make-nil-hash
  [val]
  {(make-keyword val) nil})

(defn create-shadow-board
  []
  (into
   (sorted-map)
   (for
    [x '[a b c d e f g h]
     y [1 2 3 4 5 6 7 8]]
     (make-nil-hash [x y]))))

(defn create-board
  []
  (into
   (sorted-map)
   (for [x (range 1 9) y (range 1 9)]
     (make-nil-hash [x y]))))

(defn setup-board
  []
  (swap! board merge (create-board)))

(defn setup-shadow-board
  []
  (swap! shadow-board merge (create-shadow-board)))

(defn place-figurine-on-board
  [^Figurine f]
  (swap! board merge {(make-keyword f) f}))

(defn update-board-positions
  [^Figurine f new-position]
  (show-move f new-position)
  (swap! board merge {(make-keyword f) nil})
  (swap! board merge {new-position f})
  (println f))

(defn fight
  [^Figurine _f _new-position]
  (println "Cannot fight, yet!"))

(defn determine-next-field
  [f way steps x y]
  (cond (and (= 'y way) (> y 0)) (make-keyword [(:position-x f) (+ steps (:position-y f))])
        (and (= 'y way) (< y 0)) (make-keyword [(:position-x f) (- steps (:position-y f))])
        (and (= 'x way) (> x 0)) (make-keyword [(+ steps (:position-x f)) (:position-y f)])
        (and (= 'x way) (< x 0)) (make-keyword [(- steps (:position-x f)) (:position-y f)])
        :else 'error))

;; multi method magix
(defmulti move (fn [^Figurine f direction] [(:type f) direction]))
(defmethod move ["pawn" "up"] [^Figurine f _]
  (let [next-field (determine-next-field f 'y 1 0 1)]
    (println "calcu next field" next-field)
    (cond (= next-field 'error) "error"
          (nil? (get @board next-field)) (update-board-positions f next-field)
          (not (nil? (get @board next-field))) (fight f next-field)
          :else 'error)))

(defmethod move "king"  [^Figurine f direction]
  (println "Move king")
  (show-move f direction))

;; place the figurines
(def white-pawn (->Figurine "white" "pawn" 1 2))
(def black-pawn (->Figurine "black" "pawn" 1 7))

(defn -main
  [& args]
  (setup-board)
  (setup-shadow-board)
  (place-figurine-on-board white-pawn)
  (place-figurine-on-board black-pawn)
  (move white-pawn "up" 1)
  (println "finished")
  (println @board))