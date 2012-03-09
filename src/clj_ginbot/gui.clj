(ns clj-ginbot.gui
  (:require [clj-ginbot.game :as game])
  (:import [javax.swing JFrame JLabel JPanel JTextField JTable JScrollPane ImageIcon SwingUtilities]
           [java.awt GridLayout BorderLayout Color]
           [javax.swing.table DefaultTableModel]))

(defn make-advicepanel [table]
  (let [takefromdeckfield (JTextField. "Take from deck")
        takediscardfield (JTextField. "Take discard")
        todiscardfield 	(JTextField.)
        panel (doto (JPanel. (GridLayout. 2 2)) 
                (.add takefromdeckfield)
                (.add takediscardfield)
                (.add (JLabel. "Remove from hand:"))
                (.add todiscardfield))]
    (add-watch table ::advice-watch
               (fn [_ _ _ new]
                                        ;reset panel	
                 (let [new-advice (:our-advice new)
                       to-discard (:our-discard new)]
                   (.setBackground takefromdeckfield Color/WHITE)
                   (.setBackground takediscardfield Color/WHITE)
                   (condp = new-advice
                     :deck (.setBackground takefromdeckfield Color/GREEN)
                     :discard (.setBackground takediscardfield Color/GREEN))
                   (.setText todiscardfield "")
                   (when to-discard
                     (.setText todiscardfield (game/card->str to-discard))))))
    panel))

(defn make-deckpanel [table]
  (let [cards (game/all-cards)
        color-firstdiscard (Color. 255 165 0) ;; orange 
        color-hidden  	   (Color. 255 255 0) ;; yellow
        color-ours	   (Color. 0 128 0) ;; dark green
        color-opponent	   (Color. 0 0 255) ;; blue 
        ourginfield (JTextField. "Our gin size:")
        opponentginfield (JTextField. "Opponent gin size:")
        hasginpanel (doto (JPanel. (GridLayout. 1 2)) (.add ourginfield) (.add opponentginfield))
        cardspanel (doto (JPanel.) (.setLayout (GridLayout. 4 13)))
        mapping (reduce (fn [map card]
                          (let [textfield (JTextField. (game/card->str card))]
                            (.add cardspanel textfield)
                            (into map {card textfield}))) 
                        {} cards)]
    (add-watch table ::table-watch
               (fn [_ _ _ new]
                 (doseq [cell (vals mapping)] (.setBackground cell Color/WHITE)) ; reset the panel
                 (.setBackground ourginfield Color/WHITE)	
                 (.setBackground opponentginfield Color/WHITE)
                 (.setBackground (mapping (:discard new)) color-firstdiscard)
                 (doseq [ours (seq (:our-hand new))] 
                   (.setBackground (mapping ours) color-ours))
                 (doseq [opponentcard (seq (:opponent-hand new))] 
                   (.setBackground (mapping opponentcard) color-opponent))
                 (if-let [our-gin-size (:our-gin-size new)]
                   (do (when (= our-gin-size 10)
                         (.setBackground ourginfield color-ours))
                       (.setText ourginfield (str "Our gin size: " our-gin-size)))
                   (doto ourginfield
                     (.setBackground Color/WHITE)
                     (.setText "Our gin size:")))
                 (if-let [opponentginsize (:opponent-gin-size new)]
                   (do (when (= opponentginsize 10)
                         (.setBackground opponentginfield color-opponent))
                       (.setText opponentginfield (str "Opponent gin size: " opponentginsize)))
                   (doto opponentginfield
                     (.setBackground Color/WHITE)
                     (.setText "Opponent gin size:")))
                 (doseq [h (:hidden new)] 
                   (.setBackground (mapping h) color-hidden))))
    (doto (JPanel. (BorderLayout.))
      (.add hasginpanel BorderLayout/NORTH)
      (.add cardspanel BorderLayout/CENTER))))

(defn make-scanspanel [captured-img]
  (let [panel (doto (JPanel. (GridLayout. 1 1))
                (.setMinimumSize (java.awt.Dimension. 500 600)))
        defaulttablemodel (let [imageiconclass (class (ImageIcon.))
                                stringclass (class (String.))]
                            (proxy [DefaultTableModel] []
                              (getColumnClass [i]
                                              (if (= 0 i)
                                                imageiconclass
                                                stringclass))))
        tablemodel (doto defaulttablemodel 
                     (.addColumn "Scan") 
                     (.addColumn "Classification") 
                     (.addColumn "Data"))
        jtable (doto (JTable. tablemodel)
                 (.setBackground Color/LIGHT_GRAY)
                 (.setRowHeight 100))
        scrollpane (JScrollPane. jtable)]
    (add-watch captured-img ::observation-watch 
               (fn [_ _ _ new]
                 (let [card-str (game/card->str (:card new)) 
                       scan (ImageIcon. (:img new))]
                   (.insertRow tablemodel 0 (to-array [scan card-str (:card new)])))
                 (let [rc (.getRowCount defaulttablemodel)]
                   (when (> rc 40)
                     (.removeRow defaulttablemodel (dec rc))))))
    (doto panel
      (.add scrollpane))))

(defn frame [table captured-img]
  (let [deckpanel (make-deckpanel table)
        advicepanel (make-advicepanel table)
        scanspanel (make-scanspanel captured-img)]
    (doto (JFrame. "Gin Bot")
      (.setLayout (BorderLayout.))
      (.setAlwaysOnTop true)
      (.setLocation 630 180)
      (.add (doto (JPanel. (BorderLayout.))
              (.add deckpanel BorderLayout/NORTH)
              (.add advicepanel BorderLayout/CENTER)) 
            BorderLayout/NORTH)
      (.add scanspanel BorderLayout/CENTER)
      .pack .show)))

(defn launch [table captured-img]
  (SwingUtilities/invokeLater #(frame table captured-img)))
