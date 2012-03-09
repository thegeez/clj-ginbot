(ns clj-ginbot.robot
  (:use [clj-ginbot.locations])
  (:import [java.awt Robot Rectangle]
           [java.awt.event InputEvent]))

(def robot (Robot.))
(def robot-delay 200)

;;; screenshot
(defn screenshot
  ([screenx screeny] (screenshot screenx screeny (:card-width @locations) (:card-height @locations)))
  ([screenx screeny w h]
     (let [rect (Rectangle. screenx screeny w h)]
       (. robot createScreenCapture rect))))

;;; mouse control
  (defn move-mouse [[x y]]
    (doto robot
      (.mouseMove x y)
      (.delay robot-delay)))
  (defn move-mouse-stepped [from-loc to-loc]
    (let [steps 5
          [from-x from-y] from-loc
          [to-x to-y] to-loc
          dx (/ (- to-x from-x) steps)
          dy (/ (- to-y from-y) steps)
          step (fn [[x y]] [(+ dx x) (+ dy y)])] 
      (doseq [step-loc (conj (vec (take steps (iterate step from-loc))) to-loc)]
        (move-mouse step-loc))))
  (defn left-press-mouse []
    (doto robot
      (.delay robot-delay)
      (.mousePress InputEvent/BUTTON1_MASK)
      (.delay robot-delay)))
  (defn left-release-mouse []
    (doto robot
      (.mouseRelease InputEvent/BUTTON1_MASK)
      (.delay robot-delay)))

;;; mouse actions
(defn swap-discard 
  "Take the card from the discard pile, put it in the holding location.
		 Move the card to be discarded onto the discard pile.
		 (Wait for the computer to make its move.)
		 Move card from the holding location to the free location in our hand.
		 Bring the cards that have been overlayed back to the foreground."
  ([for-card-n] (swap-discard for-card-n false))
  ([for-card-n skip-press]
     (let [discard-loc (card-center (location :discard))
           holding-loc (card-center (location :holding))
           to-switch-loc (nth (iterate next-card-position (card-center (location :our-cards))) for-card-n)]
       (move-mouse discard-loc)
       (when-not skip-press 
         (left-press-mouse))
       (move-mouse-stepped discard-loc holding-loc)
       (left-release-mouse)
       (move-mouse to-switch-loc)
       (left-press-mouse)
       (move-mouse-stepped to-switch-loc discard-loc)
       (left-release-mouse)
       ;; wait while the computer makes its move
       (Thread/sleep 2000)
       (move-mouse holding-loc)
       (left-press-mouse)
       (move-mouse-stepped holding-loc to-switch-loc)
       (left-release-mouse)
       ;; click every card in n..10 to foreground
       (doseq [loc (take (- 9 for-card-n) (iterate next-card-position (next-card-position to-switch-loc)))]
         (move-mouse loc)
         (left-press-mouse)
         (left-release-mouse)))))

(defn take-from-deck []
  "Take a card from the deck, let it turn face upward in our region and move it above the discard pile."
  (let [discard-loc (card-center (location :discard))
        deck-loc (card-center (location :deck))
        swap-loc (card-center (location :swap))]
    (move-mouse deck-loc)
    (left-press-mouse)
    (move-mouse-stepped deck-loc swap-loc)
    (left-release-mouse)
    (left-press-mouse)
    (move-mouse-stepped swap-loc discard-loc)))
