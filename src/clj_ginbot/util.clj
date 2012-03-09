(ns clj-ginbot.util
  (:use [clojure.algo.monads :only (domonad maybe-m)]))

(defmacro when-let-all [bindings & body]
  "Like when-let, but allows for multiple bindings, shortcutting to nil when any of those is nil.
	 Like -?> but with each step as a named binding"
  `(domonad maybe-m ~bindings (do ~@body)))

(defn try-every-until [interval pred f]
  "Returns the first non-nil pred value from executing f and sleeping interval in between"
  (some pred
        (remove #{::threadsleep} ;the ::threadsleep token to prevents the nil result from Thread/sleep to interfere with the some
                (map #(%)
                     (interpose #(do (Thread/sleep interval) ::threadsleep)
                                (repeat f)))))) 

(defn try-every [interval f]
  "Returns the first non-nil value from executing fn and sleeping interval in between"
  (try-every-until interval identity f))

(defn index-of [item coll]
  (some (fn [[val index]] 
          (when (= val item)
            index))
        (map vector coll (iterate inc 0))))
