
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



;; won't work for lucky-avl because the step-fn typically also needs the original avl as a
;; free variable.  Maybe could make it work with the range-down, which would let you disj
;; from the end without screwing up indices.
(defn generic-lucky
  ([init-sorted-set step-fn] (generic-lucky init-sorted-set step-fn 1))
  ([init-sorted-set step-fn start]
   (let [lucky (fn [i sorteds]
                 (let [n (nth sorteds i Long/MAX_VALUE)]
                   (if (<= n (count sorteds))
                     (recur (inc i) (reduce step-fn sorteds (range (dec n) (count sorteds) n)))
                     (sequence sorteds))))]
     (fn [max] (lucky 1 (into init-sorted-set (range start max 2)))))))







;; hack to get type hints into vswap!
;; NEEDS REVIEW

;; might be wrong about how the tag gets transmitted long vs 'long (quote????)
(defmacro tvswap!
  "SEM but tag comes as first arg.  Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  [tag vol f & args]
  (let [v (with-meta vol {:tag 'clojure.lang.Volatile})
        tagged (with-meta (gensym) {:tag tag})]
     `(let [~tagged (.deref ~v)] (.reset ~v (~f ~tagged ~@args)))))

;; NOT SURE ABOUT THIS
(defmacro vlswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in.  Requires that volatile always holds a long."
  [vol f & args]
  (let [v (with-meta vol {:tag 'clojure.lang.Volatile})
        tagged (with-meta (gensym) {:tag 'long})]
     `(let [~tagged (.deref ~v)] (long (.reset ~v (~f ~tagged ~@args))))))
  



;; https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetNaive

;; WRONG  probably messed up unsigned int
(defn bitpop [v]
  (let [v (int v)
        v (int (- v (bit-and (bit-shift-right v 1) (int 0x55555555))))
        v (int (+ (bit-and v (int 0x33333333)) (bit-and (bit-shift-right v 2) (int 0x33333333))))]
    (bit-shift-right (* (int 0x1010101) (bit-and (int 0xF0F0F0F) (+ v (bit-shift-right v 4))))
                     24)))


;; func bkpop(n: Int) -> Int {
;;     var v: UInt32 = UInt32(n)
;;     var c: UInt32 = 0
;;     for (c = 0; v != 0; c++) {
;;         v &= v - 1
;;     }
;;     return Int(c)
;; }

;; originally for 32-bit unsigned
(defn bkpop [n]
  (loop [c (int 0) v (int n)]
    (if (= v 0)
      c
      (recur (inc c) (bit-and v (dec v))))))
