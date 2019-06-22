;; Library of miscellaneous useful functions by mnicky
;; Under EPL License, the same as Clojure.

(defn on-signal
  "Registers function as a handler of a signal (specified by name) and
  returns created SignalHandler. Function f must be a function of one
  parameter - a caught signal.

  See also:

        http://is.gd/sun_misc_signal
        http://is.gd/signal_h

  Example:

        (on-signal \"INT\" (fn [sig] (println \"Signal\" sig \"detected!\")))
  "
  [signal f]
  (sun.misc.Signal/handle
    (sun.misc.Signal. signal)
    (proxy [sun.misc.SignalHandler] []
      (handle [sig] (f sig)))))

(defn on-shutdown
  "Registers function (of zero arguments) as a shutdown hook
  of current Runtime and returns nil. See also: http://is.gd/shutdown_hook"
  [f]
  (.addShutdownHook (Runtime/getRuntime) (Thread. f)))

(defn rand-n
  "Returns vector of n random samples from sequence xs,
  with equal probability of occurence for each item.

  See also: http://en.wikipedia.org/wiki/Reservoir_sampling"
  [n xs]
  (loop [ret (vec (take n xs)) todo (drop n xs) iter (inc (count ret))]
    (if (empty? todo)
      ret
      (let [rnd (rand-int iter)]
        (if (< rnd n)
          (recur (assoc ret rnd (first todo)) (rest todo) (inc iter))
          (recur ret (rest todo) (inc iter)))))))

(defn mk-vec
   "If coll is not a vector, returns a new vector containing
   the contents of coll. Otherwise returns the coll itself."
  [coll]
  (if (vector? coll) coll (into [] coll)))

(defn retry
  "If the check (failed?-fn (f)) is true, runs the check again, for maximum
  'retries' times. If the check is false, returns the value of (f).
  If maximum number of retries was reached and the check fails, returns
  the fail-val (default is nil). The f should be free of side effects."
  ([f retries failed?-fn]
    (retry f retries failed?-fn nil))
  ([f retries failed?-fn fail-val]
    (loop [retries retries]
      (let [ret (f)]
        (cond
          (not (failed?-fn ret)) ret
          (zero? retries) fail-val
          :else (recur (dec retries)))))))

(defn persist
  "Persists value to the given file."
  [file value]
  (spit file value))

(defn read-back
  "Reads persisted value from the given file and returns it."
  [file]
  (binding [*read-eval* false]
    (read-string (slurp file))))

(defn contextual-eval [ctx expr]
  "Returns result of evaluation of expr with passed contextual map.
  Taken from 'Joy of Clojure' book written by Michael Fogus."
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defn choice
  "Returns true with the given probability, otherwise returns false."
  [p]
  (if (< (rand-int 100) (int (* 100 p))) true false))

(defn interwind
  "Like interleave, but preserving rest of the collections also."
  [& colls]
  (loop [ret [] todo colls]
    (if (every? empty? todo)
      ret
      (recur (into ret (keep first todo)) (map rest todo)))))

(defn unpack
  "Applies identity to the collection, thus unpacking it."
  [coll]
  (apply identity coll))
