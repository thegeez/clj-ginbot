(ns clj-ginbot.game
  (:use [clj-ginbot.util]))

(defn all-cards []
  (for [suit [:heart :club :diamond :spade]
        rank [:2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A]] 
    {:rank rank :suit suit}))

(defn card->str [card]
  "{:rank :8 :suit :club} -> 8c"
  (if (and (:rank card) (:suit card)) 
    (str (name (:rank card)) ({:heart "h" :club "c" :spade "s" :diamond "d"} (:suit card)))
    ""))

(defn str->card [s]
  "8c -> {:rank :8 :suit :club}"
  (when-let-all [istwochars (= 2 (count s))
                 srank (nth s 0)
                 isrank (#{\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A} srank)
                 rank (keyword (str srank))
                 ssuit (nth s 1)
                 issuit (#{\h \c \d \s} ssuit)
                 suit ({"h" :heart "d" :diamond "c" :club "s" :spade} (str issuit))]
                {:rank rank :suit suit}))

;; ;;;;;;;;;;;;; Finding Rummy and The Machine Player Strategy
;; ;;;;;;;;;;;;; ;;;;;;;;
(def rank->value {:2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9
              :T 10 :J 11 :Q 12 :K 13 :A 1})

(def value->rank (zipmap (vals rank->value) (keys rank->value)))

(def suit->value {:heart 0 :club 1 :spade 2 :diamond 3})

(def value->suit (zipmap (vals suit->value) (keys suit->value)))

(defn add-rank [rank n]
  (let [v (+ (rank->value rank) n)]
    (if (= v 14)
      :A
      (value->rank v))))

(defn value-sorted [cards] (sort-by (comp rank->value :rank) cards))

(def dec-rank {:2 :A, :3 :2, :4 :3,:5 :4,:6 :5,:7 :6,:8 :7,:9 :8,:T :9, :J :T, :Q :J, :K :Q,:A :K})

(defn remove-when-card [cards suit rank] ;; must be max 9 cards in the hand
  (let [[before after] (split-with #(not (and (= (:suit %) suit)
                                            (= (:rank %) rank))) cards)]
    (when (first after) ;; was the needle card in cards?
      (concat before (rest after)))))

(defn remove-when-straight [cards end-suit end-rank]
  (when-let [found-middle (remove-when-card cards end-suit (dec-rank end-rank))]
    (remove-when-card found-middle end-suit end-rank)))

(defn gin-hand-size
  [ginhand]
  (if (< (count ginhand) 3)
    0 ;; need atleast 3 cards to form a set
    (let [pivot (first ginhand)
          postpivot (rest ginhand)
          ;; case 1: don't use the pivot in a set (to find best scores when the gin-size is less than 10)
          skipscore (+ 0 ;; the pivot is not used in a set
                       (gin-hand-size postpivot))
          ;; case 2: is there a trips using the pivot
          c1 (nth ginhand 1)
          c2 (nth ginhand 2)
          c3 (nth ginhand 3 nil) ;; might be considering the last 3 cards
          samescore (if (and (= (:rank pivot) (:rank c1))
                             (= (:rank c1) (:rank c2)))
                       (max (+ 3
                               (gin-hand-size (rest (rest postpivot))))
                            (if (and c3
                                 (= (:rank pivot) (:rank c3)))
                              ;; when 4 of the same, there are 2 extra
                              ;; trip possibilities
                              (let [notsame (rest (rest (rest postpivot)))]
                                (max
                                 (+ 3
                                    (gin-hand-size (conj notsame c1)))
                                 (+ 3
                                    (gin-hand-size (conj notsame c2)))
                                 (+ 4	
                                    (gin-hand-size notsame))))
                              0))
                       0) ;; no trips possible
          ;; now try to find all the sets where the pivot is used in a straight flush
          ;; the pivot is always the lowest ranking remaining card in cards and therefore
          ;; always the first card of a straight 
          
          ;; a card possibly makes a straight with the pivot as the lowest card if it is the same suit and
          ;; within window points of rank
          ;; case 4: find a straight flush with 3 cards with
          ;; pivot as the lowest (doesn't find Q-K-A, see
          ;; case 4a)
          wostraight (remove-when-straight postpivot (:suit pivot) (add-rank (:rank pivot) 2))
          straightscore (if wostraight
                          (max (+ 3
                                  (gin-hand-size wostraight))
                               (if-let [wo4straight (remove-when-card wostraight (:suit pivot) (add-rank (:rank pivot) 3))]
                                 (+ 4	
                                    (gin-hand-size wo4straight))
                                 0))
                          0)
          ;; special case 4: find Q-K-A and J-Q-K-A
          acescore (if-not (= :A (:rank pivot))
                     0 ;; pivot is not an Ace, case does not apply
                     (if-let [wostraight (remove-when-straight postpivot (:suit pivot) :K)]
                       (max
                        (+ 3 (gin-hand-size wostraight))
                        (if-let [woj (remove-when-card wostraight (:suit pivot) :J)]
                          (+ 4 (gin-hand-size woj))
                          0))
                       0))]
      (max skipscore
           samescore
           straightscore
           acescore)
      )))

(defn gin-size [cards]
  "Finds the highest number of cards that can be put into sets."
  (gin-hand-size (value-sorted cards)))

;; advices
;; count-gone checks how many of a given value are known to be permanently
;; discarded
(defn count-gone [rank gone-cards]
  (count (filter #(= (:rank %) rank) gone-cards)))

;; count-avail checks whether a given value/suit is
;;  known to be discarded (returns 0) or not (returns 1)
(defn count-avail [value suit gone-cards]
  (let [wanted {:suit suit :rank (value->rank value)}]
    (if (some #{wanted} gone-cards)
      0
      1)))

;; rates the possibility for forming a straight given two card values in a
;; particular suit, and taking into account cards known to be discarded; the
;; rating is the number of non-discarded cards that would form a straight with
;; the given values
(defn rate-straight [suit value value2 gone-cards]
  ;; @TODO does not yet consider two gap straights
  (let [v1 (if (= value 1) ;; use ace as top or bottom
             (if (> value2 6) 14 1)
             value)
        v2 (if (= value2 1)
             (if (> value 6) 14 1)
             value2)]
    (let [delta (- (max v1 v2) (min v1 v2))]
      (cond 
       (= delta 1)
       (cond (or (= v1 1) (= v2 1))
             ;; Might get the 3?
             (count-avail 3 suit gone-cards)
             (or (= v1 14) (= v2 14))
             ;; Might get the queen?
             (count-avail 12 suit gone-cards)
             (or (= v1 13) (= v2 13))
             ;; Might get the jack or ace?
             (+ (count-avail 11 suit gone-cards)
                (count-avail 1 suit gone-cards))
             :else
             ;; Might get top or bottom?
             (+ (count-avail (dec (min v1 v2)) suit gone-cards)
                (count-avail (inc (max v1 v2)) suit gone-cards)))
       (= delta 2)
       ;; Might get the middle one?
       (let [middle (quot (+ v1 v2) 2)]
         (count-avail middle suit gone-cards))
       :else 0))))

;; This procedure is the second part of the machine's strategy. If the machine
;; sees two choices that are equally good according to gin-size, then it
;; computes a rating based on pairs, i.e., cards that might eventually go
;; together in a set.
(defn pair-rating [cards gone-cards]
  (loop [rating 0
         cards cards]
    (if (= (count cards) 1)
      (+ 20 (* 2 rating)) ;; to conform to orig pair rating algo
      (let [card (first cards)
            others (rest cards)
            suit (:suit card)
            rank (:rank card)
            card-score (reduce +
                               (map (fn [card2]
                                      (let [suit2 (:suit card2)
                                            rank2 (:rank card2)]
                                        (cond 
                                         (= rank rank2) 
                                         (- 2 (count-gone rank gone-cards))
                                         (= suit suit2)
                                         (rate-straight suit (rank->value rank) (rank->value rank2) gone-cards)
                                         :else 0)))
                                    others))]
        (recur (+ rating card-score)
               others)))))

;; The procedure implements the discard choice
;; hand contains eleven cards, our hand plus the discard or new card
;; from the deck  
(defn choose-discard [hand gone-cards]
  "Discard the card that leaves the hand with the largest gin-size.  If
   multiple cards leave the same largest gin size, pick card leaving the best
   pair rating."
  ;; @TODO "in case of a tie involving the current discard, prefer that one"
  (let [sorted-hand (value-sorted hand)]
    (loop [best []
           best-gin-size 0
           hands (map #(vector (remove #{%} sorted-hand) %) sorted-hand)]
      (if-let [h (first hands)]
        (let [gs (gin-size (first h))]
          (cond
           (> gs best-gin-size)
           (recur [h]
                  gs
                  (rest hands))
           (= gs best-gin-size)
           (recur (conj best h)
                  best-gin-size
                  (rest hands))
           :else
           (recur best
                  best-gin-size
                  (rest hands))))
        ;; found all gin sizes
        ;; find best hand based on pair rating
        (if (= (count best) 1)
          (second (first best)) ;; discard for best gin-size
          (second (apply max-key (comp #(pair-rating % gone-cards) first) best)))
        )))) 


(defn take-discard-or-deck [in-hand-cards discard gone-discards]
  "Simple strategy: we want the card if taking it will make the
   gin-size of our hand increase, or if taking it will not make the gin-size
   decrease but will increase the pair rating."
  (let [orig-size (gin-size in-hand-cards)
        hand-with-discard (conj in-hand-cards discard)
        trade-card (choose-discard hand-with-discard gone-discards)
        new-gin-cards (remove #(= trade-card %) hand-with-discard)
        new-size (gin-size new-gin-cards)]
    (if (or (> new-size orig-size)
            (and (= new-size orig-size)
                 (> (pair-rating new-gin-cards gone-discards) 
                    (pair-rating in-hand-cards gone-discards))))
      :discard
      :deck)))
