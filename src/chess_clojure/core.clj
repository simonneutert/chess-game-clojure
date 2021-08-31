(ns chess-clojure.core
  (:gen-class))
(require '[clojure.string :as str])
(require '[chess-clojure.pawn :as pawn])
;; (require '[chess-clojure.figurines :as figurines])

;; please, regard java convention => "underscores"
(import '[chess_clojure.figurines Figurine])

(defn str->int
  [s]
  (-> s
      str
      Integer/parseInt))

(defn keyword->str
  [k]
  (-> k
      str
      rest))

(defn extract-x
  [pos]
  (-> pos
      keyword->str
      first
      str->int))

(defn extract-y
  [pos]
  (-> pos
      keyword->str
      last
      str->int))

(def board (atom {:round 0}))
(def shadow-board (atom {}))
(def board-killed-figurines (atom []))

;; place the figurines
(def white-pawn (atom (pawn/make-record "white" 1 2)))
(def black-pawn (atom (pawn/make-record "black" 1 7)))

(defn show-move [f direction]
  (println "Move" (:type f) (:color f) "to" direction))

(defmulti make-keyword (fn [x] [(type x)]))
(defmethod make-keyword [Figurine] [x]
  (make-keyword [(:position-x x) (:position-y x)]))
(defmethod make-keyword [clojure.lang.PersistentVector] [x]
  (-> x (str/join) (keyword)))

(defn make-nil-hash [val] {(make-keyword val) nil})

(defn create-shadow-board
  []
  (into (sorted-map)
        (for [x '[a b c d e f g h]
              y [1 2 3 4 5 6 7 8]]
          (make-nil-hash [x y]))))

(defn create-board
  []
  (into
   (sorted-map)
   (for [x (range 1 9)
         y (range 1 9)]
     (make-nil-hash [x y]))))

(defn setup-board []
  (swap! board merge (create-board)))

(defn setup-shadow-board []
  (swap! shadow-board merge (create-shadow-board)))

(defn place-figurine-on-board
  [f]
  (swap! board merge {(make-keyword (deref f)) (deref f)}))

(defn next-round []
  (swap! board merge {:round (inc (get @board :round))}))

(defn update-board-positions
  [f new-position]
  ;; (show-move (deref f) new-position)
  (swap! board merge {(make-keyword (deref f)) nil})
  (swap! f merge {:position-x (extract-x new-position)})
  (swap! f merge {:position-y (extract-y new-position)})
  (swap! board merge {new-position (deref f)})
  (next-round))

(defn fight
  [f new-position enemy]
  (update-board-positions f new-position)
  (swap! board-killed-figurines conj {:round (get @board :round) :enemy enemy}))

(defn valid-field-p
  [next-field]
  (let [x (extract-x next-field)
        y (extract-y next-field)]
    (and (> x 0) (> y 0) (< x 9) (< y 9))))

(defn move-to-field
  [f next-field]
  (if (valid-field-p next-field)
    (cond (= next-field 'error) 'error
          (nil? (get @board next-field)) (update-board-positions f next-field)
          (not (nil? (get @board next-field))) (fight f next-field (get @board next-field))
          :else 'error)
    'error))


(defn move-y
  [f _ steps-y]
  (let [x (:position-x (deref f))
        y (+ steps-y (:position-y (deref f)))]
    (-> [x y] make-keyword)))

(defn move-x
  [f steps-x _]
  (let [x (+ steps-x (:position-x (deref f)))
        y (:position-y (deref f))]
    (-> [x y] make-keyword)))

(defn move-xy
  [f steps-x steps-y]
  (let [x (+ steps-x (:position-x (deref f)))
        y (+ steps-y (:position-y (deref f)))]
    (-> [x y] make-keyword)))

(defn determine-next-field
  [f way steps-x steps-y]
  (let [next-field (cond (and (= 'y way) (> steps-y 0)) (move-y f steps-x steps-y)
                         (and (= 'y way) (< steps-y 0)) (move-y f steps-x steps-y)
                         (and (= 'x way) (> steps-x 0)) (move-x f steps-x steps-y)
                         (and (= 'x way) (< steps-x 0)) (move-x f steps-x steps-y)
                         (and (= 'xy way) (not= steps-x 0) (not= steps-y 0)) (move-xy f steps-x steps-y)
                         :else 'error)]
    (move-to-field f next-field)))

;; multi method magix
(defmulti move (fn [f direction] [(:type (deref f)) (type direction)]))
(defmethod move ["pawn" String] [f direction]
  (let [[f way steps-x steps-y] (pawn/move f direction)]
    (determine-next-field f way steps-x steps-y)))
(defmethod move "king"  [f direction]
  (println "Move king"))
(defmethod move :default
  []
  (throw (IllegalArgumentException. (str "do not know arguments"))))


(defn -main
  [& args]
  (setup-board)
  (setup-shadow-board)
  (place-figurine-on-board white-pawn)
  (place-figurine-on-board black-pawn)
  (println @board)
  (move white-pawn "up")
  (move white-pawn "up")
  (move white-pawn "right")
  (move white-pawn "down")
  (move white-pawn "up")
  (move white-pawn "down")


  (move black-pawn "up")
  (move black-pawn "up")
  (move black-pawn "left")
  (move black-pawn "up")
  (move black-pawn "up")


  (println "finished")
  (println @board)
  (println @board-killed-figurines))