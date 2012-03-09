(ns clj-ginbot.core
  (:use [clj-ginbot.util :only (when-let-all try-every-until try-every index-of)]
        [clj-ginbot.locations])
  (:require [clj-ginbot.game :as game]
            [clj-ginbot.card :as card]
            [clj-ginbot.gui :as gui]
            [clj-ginbot.robot :as robot])
  (:import [java.awt.image BufferedImage]))


(def table (agent nil))

(def captured-img (let [bi (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB)] 
                    (agent {:img bi :card nil})))

(defn capture-card [[x y]]
  "Scans the region at x,y and analyzes this to return a card. 
   Returns nil if the region is not recognized as a card."
  (when-let-all [padding 5 delta 10
                 scan (robot/screenshot (- x padding) (- y padding))
                 _ (send-off captured-img (fn [_] {:card nil :img scan}))
                 [cardx _] (card/find-border scan (for [x (range delta 0 -1)]
                                               [[x 40] [x 50]])) 
                 [_ cardy] (card/find-border scan (for [y (range delta 0 -1)]
                                               [[40 y] [50 y]]))
                 croppedscan (.getSubimage scan cardx cardy (- (.getWidth scan) cardx) (- (.getHeight scan) cardy))
                 card (card/analyze-card croppedscan)]
                (send-off captured-img (fn [_] {:card card :img croppedscan}))
                (select-keys card [:rank :suit])))

(defn capture-table []
  (when-let-all [first-discard (capture-card (location :discard))
                 ours (vec (take-while identity (map capture-card (take 10 (iterate next-card-position (location :our-cards))))))]
                (when (= (count ours) 10)
                  {:discard first-discard :our-hand ours})))

(defn capture-opponent-hand []
  (let [cards (vec (take-while identity (map capture-card (take 10 (iterate next-card-position (location :opp-cards))))))]
    (when (= (count cards) 10)
      cards)))

(defn mainloop []
  (while true
    ;;(Thread/sleep 3500)
    (let [old-table @table
          old-discard (:discard old-table)
          new-discard (try-every-until 3000 (fn [new-card]
                                              (not= new-card old-discard))
                                       #(capture-card (location :discard)))
          observed-table (try-every 3000 capture-table)
          our-gin-size (game/gin-size (observed-table :our-hand))
          opponent-hand (capture-opponent-hand)
          new-table (merge observed-table
                          {:our-gin-size our-gin-size}
                          (when opponent-hand
                            {:opponent-gin-size (game/gin-size opponent-hand)
                             :opponent-hand opponent-hand}))
          ;; hidden cards are the ones no longer visible on the table
          ;; either below discard or taken by opponent
          hidden (if (or (nil? old-table)
                         (and (= 10 (:opponent-gin-size old-table))
                              (not= 10 (:opponent-gin-size new-table)))
                         (and (= 10 (:our-gin-size old-table))
                              (not= 10 (:our-gin-size new-table))))
                   #{} ; new game started, swipe old hidden cards
                   (let [old-visible-cards (conj (:our-hand old-table) (:discard old-table))
                         new-visible-cards (into (conj (:our-hand observed-table) (:discard observed-table)) opponent-hand)]
                     (let [old-hidden (into #{} (:hidden old-table))]
                       (apply disj (into old-hidden old-visible-cards) new-visible-cards))))
          our-advice (game/take-discard-or-deck (:our-hand new-table) (:discard new-table) hidden)
          our-discard (game/choose-discard (conj (:our-hand new-table) (:discard new-table)) hidden)
          ]
      (send table (constantly (merge new-table
                                     {:hidden hidden
                                      :our-advice our-advice
                                      :our-discard our-discard})))
      (when (and (> 10 (:our-gin-size new-table))
                 (> 10 (:opponent-gin-size new-table 0)))
        (cond
         (= our-advice :discard) (robot/swap-discard (index-of our-discard (:our-hand new-table)))
         (= our-advice :deck)
         (do (robot/take-from-deck);; places a card from deck on
             ;; home region, turns it face up and drags it
             ;; above the discard location for scanning
             (let [from-deck (capture-card (location :discard))
                   next-table (merge new-table {:hidden (conj hidden (:discard new-table))
                                                :discard from-deck})
                   next-advice (game/take-discard-or-deck (:our-hand next-table) (:discard next-table) (:hidden next-table))
                   choice (game/choose-discard (conj (:our-hand next-table) (:discard next-table)) (:hidden next-table))]
               (send table (constantly (merge next-table {:our-discard choice
                                                          :our-advice next-advice})))
               (cond
                (= next-advice :deck) ;; taking another card from the
                ;; deck is a better move than taking this card from
                ;; the deck, therefore don't use it
                (robot/left-release-mouse)
                (= next-advice :discard) ;; card from deck hovers on
                ;; discard, this means taking it and swapping it for a
                ;; card we already have
                (robot/swap-discard (index-of choice (:our-hand next-table)) true)))))
        ))))

(defn m []
  (gui/launch table captured-img)
  ;;(def ml (future (mainloop)))
  )
