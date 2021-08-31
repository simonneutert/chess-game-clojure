(ns chess-clojure.pawn (:gen-class))
(require '[chess-clojure.figurines :as figurines])

;; please, regard java convention => "underscores"
;; (import '[chess_clojure.figurines Figurine])

(defmulti move (fn [f direction] [(:type (deref f)) (:color (deref f)) direction]))
(defmethod move ["pawn" "white" "down"] [f _]
  (list f 'y 0 -1))
(defmethod move ["pawn" "white" "up"] [f _]
  (list f 'y 0 1))
(defmethod move ["pawn" "white" "left"] [f _]
  (list f 'x -1 0))
(defmethod move ["pawn" "white" "right"] [f _]
  (list f 'x 1 0))
(defmethod move ["pawn" "black" "down"] [f _]
  (list f 'y 0 1))
(defmethod move ["pawn" "black" "up"] [f _]
  (list f 'y 0 -1))
(defmethod move ["pawn" "black" "left"] [f _]
  (list f 'x 1 0))
(defmethod move ["pawn" "black" "right"] [f _]
  (list f 'x -1 0))
(defmethod move :default
  []
  (throw (IllegalArgumentException. (str "do not know arguments"))))

(defn make-record
  [color position-x position-y]
  (figurines/->Figurine "pawn" color position-x position-y))