(ns chess-clojure.core
  (:gen-class))
(require '[clojure.string :as str])

(def board (atom {:round 0}))
(def shadow-board (atom {}))
(def board-killed-figurines (atom []))

;; struct/map with magic -> a record
(defrecord Figurine [color type position-x position-y])

;; place the figurines
(def white-pawn (atom (->Figurine "white" "pawn" 1 2)))
(def black-pawn (atom (->Figurine "black" "pawn" 1 7)))

(defn show-move [^Figurine f direction]
  (println "Move" (:type f) (:color f) "to" direction))

(defmulti make-keyword (fn [x] [(type x)]))
(defmethod make-keyword [Figurine] [x]
  (let [x (make-keyword [(:position-x x) (:position-y x)])]
    (println x)
    x))
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
  [f]
  (swap! board merge {(make-keyword (deref f)) (deref f)}))

(defn extract-x
  [pos]
  (Integer/parseInt (str (first (rest (str pos))))))

(defn extract-y
  [pos]
  (Integer/parseInt (str (last (rest (str pos))))))

(defn next-round
  []
  (swap! board merge {:round (inc (get @board :round))}))
(defn update-board-positions
  [f new-position]
  ;; (show-move (deref f) new-position)
  (swap! board merge {(make-keyword (deref f)) nil})
  (swap! f merge {:position-x (extract-x new-position)})
  (swap! f merge {:position-y (extract-y new-position)})
  (swap! board merge {new-position (deref f)})
  (next-round)
  (println (deref f)))

(defn fight
  [f new-position enemy]
  (println "Fighting:" enemy)
  (swap! board-killed-figurines conj enemy)
  (println "killed figurines:" @board-killed-figurines)
  (update-board-positions f new-position))


(defn valid-field
  [next-field]
  (let
   [x (extract-x next-field)
    y (extract-y next-field)]
    (println "" next-field "" x " " y)
    (and (> x 0) (> y 0) (< x 9) (< y 9))))

(defn determine-next-field
  [f way steps x y]
  (let [next-field (cond (and (= 'y way) (> y 0)) (make-keyword [(:position-x (deref f)) (+ steps (:position-y (deref f)))])
                         (and (= 'y way) (< y 0)) (make-keyword [(:position-x (deref f)) (- (:position-y (deref f)) steps)])
                         (and (= 'x way) (> x 0)) (make-keyword [(+ steps (:position-x (deref f))) (:position-y (deref f))])
                         (and (= 'x way) (< x 0)) (make-keyword [(- (:position-x (deref f)) steps) (:position-y (deref f))])
                         :else 'error)]
    (if (valid-field next-field)
      (cond (= next-field 'error) 'error
            (nil? (get @board next-field)) (update-board-positions f next-field)
            (not (nil? (get @board next-field))) (fight f next-field (get @board next-field))
            :else 'error)
      ())))

;; multi method magix
(defmulti move (fn [f direction steps] [(:type (deref f)) (:color (deref f)) direction (type steps)]))
(defmethod move ["pawn" "white" "down" Long] [f _ steps]
  (determine-next-field f 'y steps 0 -1))
(defmethod move ["pawn" "white" "up" Long] [f _ steps]
  (determine-next-field f 'y steps 0 1))
(defmethod move ["pawn" "white" "left" Long] [f _ steps]
  (determine-next-field f 'x steps -1 0))
(defmethod move ["pawn" "white" "right" Long] [f _ steps]
  (determine-next-field f 'x steps 1 0))

(defmethod move ["pawn" "black" "down" Long] [f _ steps]
  (determine-next-field f 'y steps 0 1))
(defmethod move ["pawn" "black" "up" Long] [f _ steps]
  (determine-next-field f 'y steps 0 -1))
(defmethod move ["pawn" "black" "left" Long] [f _ steps]
  (determine-next-field f 'x steps 1 0))
(defmethod move ["pawn" "black" "right" Long] [f _ steps]
  (determine-next-field f 'x steps -1 0))

(defmethod move "king"  [f direction]
  (println "Move king")
  (show-move f direction))

(defn -main
  [& args]
  (setup-board)
  (setup-shadow-board)
  (place-figurine-on-board white-pawn)
  (place-figurine-on-board black-pawn)
  (println @board)
  (println (:type (deref white-pawn)))
  (move white-pawn "up" 1)
  (move white-pawn "up" 1)
  (move white-pawn "right" 1)
  (move white-pawn "down" 1)
  (move black-pawn "up" 1)
  (move black-pawn "up" 1)
  (move black-pawn "left" 1)
  (move black-pawn "up" 1)
  (move black-pawn "up" 1)

  (println "finished")
  (println @board))