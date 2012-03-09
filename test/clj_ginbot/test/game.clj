(ns clj-ginbot.test.game
  (:use [clj-ginbot.game] :reload)
  (:use [clojure.test]))

(deftest gin-size-test
  (are [hand size] (= size (gin-size (map str->card hand)))
       '("3c" "3s" "3d" "8h" "8c" "8s" "Jh" "Qh" "Kh" "Ah") 10
       '("3c" "3s" "3d" "8h" "8c" "8s") 6
       ["2h" "2c" "3h"] 0
       '("3c" "3s" "3d") 3
       '("8h" "8c" "8s") 3
       '("2h" "3h" "4h") 3
       '("2h" "3h" "4h" "5h") 4
       '("Jh" "Qh" "Kh" "Ah")4
       '("7s" "7h" "3s" "2s" "Kh" "Kc" "4s" "7d" "As" "Ks") 10
       ["6h" "7h" "8h" "9h" "Ts" "Th" "Tc" "Ac"] 7
       '("Ac" "Ts" "6h" "8h" "Th" "3c" "9h" "7h" "2c" "Tc") 10
       ["Ac" "As" "Ad" "Ah"] 4
       ["3c" "3s" "3d" "3h"] 4
       ["Ac" "Ad" "Ah"] 3
       ["Jh" "Qh" "Kh" "Ah"] 4
       ["Qh" "Kh" "Ah"] 3
       ["2h" "4h" "3h" "Ah"] 4
       ["2h" "3h" "Ah"] 3
       ["Jh" "Qh" "Js" "Jd" "Kh" "Jc" "Ah"] 7
       ["Jh" "Qh" "Js" "Jd" "Kh" "Jc" "Ad"] 6
       ["2h" "4h" "3h" "Ah" "Ac" "Qc" "Kc"] 7
       ["2h" "4h" "3h" "Ah" "Ac" "As" "Ad"] 7
       ["Jh" "Qh" "Ah" "Jc" "Qc" "Ac"] 0
       ["Jh" "Qh" "Jc" "Qc"] 0
       ["Jh" "Qh" "Jc"] 0
       ["Kh" "Ah"] 0
       ["3d" "3s" "3c" "3h" "4d" "4s" "5s"] 6
       ["2h" "2c" "2s" "2d" "8h" "8s" "8d" "Js" "Jd" "Jh"] 10
       ["9h" "9c" "9d" "9s" "Th" "Td" "Ts" "Tc" "Jc" "Qc"] 10
       ["3h" "4h" "5h" "3c" "3d" "3s" "Ts" "Tc" "Td" "Th"]  10
       ["4h" "5h" "6h" "4c" "6c" "4d" "8s" "9s" "Ts" "Js"] 7
       ["4h" "5h" "6h" "4c" "6c" "8c" "4d" "8s" "9s" "Ts" ] 6
       ["2h" "3h" "4h" "5h" "Ah" "5c" "5s" "6s" "7s" "8s"] 10
       ["2h" "3h" "4h" "5h"] 4
       ["3h" "3c" "3d" "3s" "5d" "8c" "8d" "8s" "Ac" "As"] 7
       ["3h" "3c" "3d" "3s"] 4
       ["8c" "8d" "8s"] 3))

(deftest choose-discard-test
  (are [hand card] (= (choose-discard (map str->card hand) #{}) card)
         ["5s" "6s" "7d" "9h" "9c" "Qh" "Qc" "Qd" "Ah" "As" "Qs"] {:rank :7 :suit :diamond}
         ["2h" "3h" "4h" "5h" "Ah" "5c" "3s" "5s" "6s" "7s" "8s"] {:rank :3 :suit :spade}))

(deftest count-gone-test
  (are [rank cards gone] (= (count-gone rank (set (map str->card cards))) gone)
       :3 [] 0
       :3 ["3h"] 1
       :3 ["3h" "4h"] 1
       :3 ["3h" "4h" "3c"] 2
       :3 ["3h" "4h" "3c" "3d"] 3
       :3 ["3h" "4h" "3c" "3d" "3s"] 4))

(deftest rate-straight-test
  (is (= 2 (rate-straight :heart (rank->value :3) (rank->value :4) #{})))
  (is (= 1 (rate-straight :heart (rank->value :3) (rank->value :4) #{{:suit :heart :rank :2}})))
  (is (= 0 (rate-straight :heart (rank->value :3) (rank->value :4) #{{:suit :heart :rank :2} {:suit :heart :rank :5}})))
  (is (= 1 (rate-straight :heart (rank->value :3) (rank->value :5) #{})))
  (is (= 0 (rate-straight :heart (rank->value :3) (rank->value :5) #{{:suit :heart :rank :4}}))))

(deftest pair-rating-test
  (are [hand score] (= (pair-rating (map str->card hand) #{}) score)
       ["5s" "7d" "9h" "9c" "Qh" "Qc" "Qd" "Ah" "As" "Qs"] 56
       ["5s" "6s"  "9h" "9c" "Qh" "Qc" "Qd" "Ah" "As" "Qs"] 60))
