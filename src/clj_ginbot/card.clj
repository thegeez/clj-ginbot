(ns clj-ginbot.card
  (:import [javax.imageio ImageIO]
           [java.io File]
           [javax.swing JFrame]
           [java.awt Component Dimension Color Rectangle]
           [java.awt.image BufferedImage]))

(defn is-border? [^BufferedImage bi points]
  "Determines if all the points in bi have the color of a border of a card."
  (let [is-border-color? (fn [[x y]]
                             (let [sample (Color. (.getRGB bi x y))
                                   red (.getRed sample)
                                   green (.getGreen sample)
                                   blue (.getBlue sample)]
                               (= 0 red green blue) ;; black
                               ))]
    (every? is-border-color? points)))

(defn find-border [^BufferedImage bi lines]
  "Returns a point from a line in lines that is a border.
   Returns nil if there is no such line."
  (some #(when (is-border? bi %) (first %)) lines))

(defn analyze-rank [^BufferedImage bi]
  "Finds rank of card based on pixels in the rank icon.
   Requires the bi to be cropped to the top and left border of a card."
  (let [mapping {[[7 5] [6 7] [8 7] [5 9] [9 9] [4 11] [10 11] [7 11] [4 14] [10 14]] :A
                 [[7 5] [5 6] [9 6] [8 9] [10 12] [6 11] [4 14] [10 14]] :2
                 [[7 5] [4 4] [9 6] [6 9] [10 11] [8 10] [5 14] [9 14]] :3
                 [[7 6] [9 4] [4 9] [11 11] [8 10] [9 14]] :4
                 [[7 5] [4 4] [10 4] [4 9] [10 11] [9 9] [4 12] [9 14]] :5
                 [[7 5] [5 6] [9 4] [4 10] [10 11] [9 9] [4 9] [4 11] [9 14]] :6
                 [[7 5] [4 6] [10 4] [9 7] [8 9] [7 11] [6 15] [7 15]] :7
                 [[7 4] [4 7] [10 7] [5 9] [9 9] [4 11] [10 11] [7 10] [5 14] [9 14]] :8
                 [[7 5] [4 7] [10 7] [5 9] [9 9] [10 11] [7 11] [5 15] [8 14]] :9
                 [[7 5] [6 7] [11 7] [3 4] [10 9]  [4 11] [6 12] [4 14] [10 14]] :T
                 [[7 5] [6 4] [9 4] [8 9] [3 12] [7 11] [4 14] [7 14]] :J
                 [[7 4] [3 5] [9 7] [3 9] [9 9] [3 11] [9 11] [4 14] [9 17]] :Q
                 [[10 4] [2 4] [7 7] [5 9] [3 11] [7 11] [2 15] [10 15]] :K}
        to-sample (into #{} (reduce concat (keys mapping)))
        non-white? (fn [[x y]]
                     (not= (.getRGB bi x y) -1)) ;; -1 = white
        samples (zipmap to-sample (map non-white? to-sample))
        rank (some (fn [[pixels rank]]
                     (when (every? #{true} (vals (select-keys samples pixels)))
                       rank)) mapping)]
    rank))

(defn analyze-suit [#^BufferedImage bi]
  "Determines the suit of a card by the sampling 3 pixels in the suit icon.
  Requires the bi to be cropped to the top and left border of a card."
  (let [mid-pixel (.getRGB bi 7 23)]
    (when-not (= mid-pixel -13847040) ;; green on the back of a card
      (cond
       (= mid-pixel -16777216) ;; black
       (let [upper-pixel (.getRGB bi 8 21)
             lower-pixel (.getRGB bi 9 22)]
         (if (= -16777216 upper-pixel lower-pixel) ;; black
           :spade
           :club))
       (#{-589805 -65528} mid-pixel) ;;red 
       (let [upper-pixel (.getRGB bi 10 20)] ;; mid-pixel is red
         (if (= upper-pixel -1)              ;; white
           :diamond
           :heart))
       :else nil ;; back of a card
       )
      )))

(defn analyze-card [#^BufferedImage bi]
  "Returns the suit and rank of the card in bi, nil if no judgement.
   Requires the bi to be cropped to the top and left border of a card."
  (when-let [suit (analyze-suit bi)]
    (when-let [rank (analyze-rank bi)]
      {:rank rank :suit suit})))

(comment
  ;; card ocr dev
  (defn copy-bi [bi]
    (BufferedImage. (.getColorModel bi) (.copyData bi nil) (.. bi getColorModel isAlphaPremultiplied) nil))
  
  (defn sample-rank [bi [offset-x offset-y]]
    ;; all none white is Ace
    #_(doseq [[x y] [[7 5] [6 7] [8 7] [5 9] [9 9] [4 11] [10 11] [7 11] [4 14] [10 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 2
    #_(doseq [[x y] [[7 5] [5 6] [9 6] [8 9] [10 12] [6 11] [4 14] [10 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 3
    #_(doseq [[x y] [[7 5] [4 4] [9 6] [6 9] [10 11] [8 10] [5 14] [9 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 4
    #_(doseq [[x y] [[7 6] [9 4] [4 9] [11 11] [8 10] [9 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 5
    #_(doseq [[x y] [[7 5] [4 4] [10 4] [4 9] [10 11] [9 9] [4 12] [9 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 6
    #_(doseq [[x y] [[7 5] [5 6] [9 4] [4 10] [10 11] [9 9] [4 9] [4 11] [9 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 7
    #_(doseq [[x y] [[7 5] [4 6] [10 4] [9 7] [8 9] [7 11] [6 15] [7 15]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 8
    #_(doseq [[x y] [[7 4] [4 7] [10 7] [5 9] [9 9] [4 11] [10 11] [7 10] [5 14] [9 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is 9
    #_(doseq [[x y] [[7 5] [4 7] [10 7] [5 9] [9 9] [10 11] [7 11] [5 15] [8 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is T
    #_(doseq [[x y] [[7 5] [6 7] [11 7] [3 4] [10 9]  [4 11] [6 12] [4 14] [10 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is J
    #_(doseq [[x y] [[7 5] [6 4] [9 4] [8 9] [3 12] [7 11] [4 14] [7 14]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is Q
    #_(doseq [[x y] [[7 4] [3 5] [9 7] [3 9] [9 9] [3 11] [9 11] [4 14] [9 17]]]
        (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    ;; all none white is K
    (doseq [[x y] [[10 4] [2 4] [7 7] [5 9] [3 11] [7 11] [2 15] [10 15]]]
      (.setRGB bi (+ offset-x x) (+ offset-y y) (.getRGB Color/CYAN)))
    )

  (defn sample-suit [bi [offset-x offset-y]]
    (.setRGB bi (+ offset-x 10) (+ offset-y 20) (.getRGB Color/BLUE))
    (.setRGB bi (+ offset-x 7) (+ offset-y 23) (.getRGB Color/BLUE))
    (doall (for [x (map #(+ offset-x %) [4 3 2 #_6 #_9])
                 y (map #(+ offset-y %) [19 20 22 24 26])]
             (let [white? (= (.getRGB bi x y) (.getRGB Color/WHITE))]
               (.setRGB bi x y (.getRGB Color/CYAN))
               (if white? 1 0)))))

  (defn sample-whole-rank [bi [offset-x offset-y]]
      (doall (for [x (map #(+ offset-x %) (range 2 11))
                   y (map #(+ offset-y %) (range 3 16))]
               (let [white? (= (.getRGB bi x y) (.getRGB Color/WHITE))]
                 (.setRGB bi x y (.getRGB Color/CYAN))
                 (if white? 1 0)))))
  
  (defn analyze-whole-rank [bi]
    ;; x 2 - 10 y 3 - 15
    (doall (for [x (range 2 11)
                 y (range 3 16)]
             (let [white? (= (.getRGB bi x y) (.getRGB Color/WHITE))]
               (.setRGB bi x y (.getRGB Color/CYAN))
               (if white? 1 0)))))

  
  (defn find-and-draw-border [img-ref [offset-x offset-y]]
    (let [bi @img-ref
          delta 7
          [cardx _] (find-border bi (for [x (range (+ offset-x delta) offset-x -1)]
                                      [[x (+ offset-y 40)] [x (+ offset-y 50)]])) 
          [_ cardy] (find-border bi (for [y (range (+ offset-y delta) offset-y -1)]
                                    [[(+ cardx 40) y] [(+ cardx 50) y]]))]
      (println "cardx" cardx "cardy" cardy)
      (when (and cardx cardy)
        (dotimes [i 50]
          (.setRGB bi (+ cardx i) cardy (.getRGB Color/RED))
          (.setRGB bi cardx (+ cardy i) (.getRGB Color/RED)))
        (let [cbi (copy-bi bi)
              cut (.getSubimage cbi cardx cardy 71 96)]
          (sample-suit bi [cardx cardy])
          (println (analyze-suit cut))
          (sample-rank bi [cardx cardy])
          (println (analyze-rank cut))
          #_(sample-whole-rank bi [cardx cardy])
          (println (analyze-whole-rank cut)))
        (reset! img-ref bi)
        nil)))
  
  (defn punch-point [img-ref [x y]]
    (let [bi @img-ref]
      (.setRGB bi x y (.getRGB Color/CYAN))
      (reset! img-ref bi)))
  
  (doall (for [x (map #(* 73 %) (range 13))
               y (map #(* 98 %) (range 4))]
           (do (println [x y])
               (find-and-draw-border img [x y]))))
  
  (defn view-img [img-ref]
    (let [frame (JFrame. "img-viewer")
          picture (proxy [Component] []
                    (paint [g]
                           (. g drawImage @img-ref 0 0 nil))
                    (getPreferredSize []
                                      (if-let [i @img-ref]
                                        (Dimension. (.getWidth i) (.getHeight i))
                                        (Dimension. 400 400))))]
      (doto frame
        (.add picture)
        .pack .show)
      (add-watch img-ref :img-watch
                 (fn [ref key old new]
                   (.repaint frame 0 0 0 (.getWidth new) (.getHeight new))))))
  
  (def img (atom nil))
  (defn init-img []  (reset! img (ImageIO/read (File. "classic-playing-cards.png"))))
  (init-img)
  (view-img img)
  )


