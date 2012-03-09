(ns clj-ginbot.locations)

(def locations (atom {:offset [-1642 185] ;;[-1642 185] ff ;;[-1642 98] chrome
                      :opp-cards [6 6]
                      :deck [171 171]
                      :discard [320 171]
                      :our-cards [6 296] 
                      :holding [6 335]
                      :swap [250 294]
                      :card-width 71
                      :card-height 96
                      :next-card-offset-x 53
                      :next-card-offset-y 4
                      :card-center-x 35
                      :card-center-y 28}))

(defn location [key]
  (map +
       (:offset @locations)
       (key @locations)))

(defn card-center [[x y]]
  "Returns the location of the center of the card, from the topleft location"
  [(+ x (:card-center-x @locations)) (+ y (:card-center-y @locations))])

(defn next-card-position [[x y]]
  "Returns the location of the next card if properly layed out"
  [(+ x (:next-card-offset-x @locations)) (+ y (:next-card-offset-y @locations))])
