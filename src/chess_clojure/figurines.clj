(ns chess-clojure.figurines (:gen-class))

;; struct/map with magic -> a record
;; please, regard java file/class naming convention 
;; "underscores" when "importing"

(defrecord Figurine [type color position-x position-y])
