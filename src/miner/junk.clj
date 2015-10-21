
;; more like conventional with n=0, but I don't like it
(defn my-take-nth0
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
   (if (zero? n)
     (fn [rf]
       (let [vv (volatile! ::none)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
             (when (identical? @vv ::none) (vreset! vv input))
             (rf result @vv)))))
     (fn [rf]
       (let [iv (volatile! 1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv dec)]
                (if (zero? i)
                  (do (vreset! iv n)
                      (rf result input))
                  result))))))))
  ([n coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (first s) (my-take-nth n (drop n s)))))))




;; range-down not really necessary if you use previous avl -- persistent data structures FTW!

(defn range-down
  "Returns a seq of integers from HIGH (exclusive) down to LOW (inclusive).
   LOW defaults to 0. STEP is a positve decrement, defaults to 1.  Like
   `(reverse (range low high step))' but a bit faster."
  ([high] (range (dec high) -1 -1))
  ([high low] (range (dec high) (dec low) -1))
  ([high low step]
     ;; calculate nearest multiple of step + offset using mod
     (range (- (dec high) (mod (- (dec high) low) step)) (dec low) (- step))))

(defn lucky7
  ([max] (lucky7 1 (apply avl/sorted-set (range 1 max 2))))
  ([i avl]
   (let [n (nth avl i nil)]
     (if (and n (<= n (count avl)))
       (recur (inc i) (reduce (fn [sss m] (disj sss (nth sss m)))
                              avl
                              ;; run hi down to lo to avoid disturbing nth order
                              (range-down (count avl) (dec n) n)))
       (sequence avl)))))




;; not much gained with transient per round
;; transient doesn't support nth so you have to go back and forth per round
(defn lucky7-trans
  ([max] (lucky7-trans 1 (apply avl/sorted-set (range 1 max 2))))
  ([i avl]
   (let [n (nth avl i nil)]
     (if (and n (<= n (count avl)))
       (recur (inc i) (persistent! (reduce (fn [sss m] (disj! sss (nth avl m)))
                                           (transient avl)
                                           ;; run hi down to lo to avoid disturbing nth order
                                           (range-down (count avl) (dec n) n))))
       (sequence avl)))))

(defn lucky7t
  ([max] (lucky7t 1 (apply avl/sorted-set (range 1 max 2))))
  ([i avl]
   (let [n (nth avl i nil)]
     (if (and n (<= n (count avl)))
       (recur (inc i) (persistent! (reduce (fn [sss m] (disj! sss (nth avl m)))
                                           (transient avl)
                                           ;; run hi down to lo to avoid disturbing nth order
                                           (range-down (count avl) (dec n) n))))
       (sequence avl)))))





(defn my-drop-nth
  "Returns a lazy seq dropping the first and every nth item thereafter in coll.  Returns a stateful
  transducer when no collection is provided.  N=0 returns the coll."
  {:static true}
  ([n]
   (if (zero? n)
     conj
     (fn [rf]
       (let [iv (volatile! 1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv dec)]
                (if (zero? i)
                  (do (vreset! iv n)
                      result)
                  (rf result input)))))))))
  ([n coll]
   (if (zero? n)
     coll
     (lazy-drop-nth n (rest coll)))))

