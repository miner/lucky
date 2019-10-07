(ns miner.lucky
  (:require [clojure.core.rrb-vector :as fv]
            [clojure.data.avl :as avl]
            [clojure.data.int-map :as im]))

;; Someone asked how to do this on the mailing list.  Led to a bit of discussion where I
;; finally came up with the lucky-avl solution (very fast!) after a couple of misfires.
;;
;; http://en.wikipedia.org/wiki/Lucky_number

;; More references
;; https://oeis.org/A000959

;; Variant: Even lucky numbers
;; like Lucky numbers but starting with even numbers
;; https://oeis.org/A045954

;; More information:
;; http://archive.wikiwix.com/cache/?url=http://www.sciencenews.org/sn_arc97/9_6_97/mathland.htm&title=


;; SEM : need to try with Java array mutable




;; SEM: I decided I didn't like the semantics of this.  Prefer something more like
;; complement of take-nth.  See my-drop-nth for better version.  Also, this one blows up for
;; big numbers, like 1e6
(defn drop-nth-slow [n coll] 
  (lazy-seq 
    (when-let [s (seq coll)] 
      (concat (take (dec n) s) (drop-nth-slow n (drop n s))))))


;; slow because it calls drop-nth too many times when the N is bigger than the remaining
;; survivor count.
(defn lucky-slow
  ([max] (if (pos? max) (lucky-slow 1 (range 1 max 2)) ()))
  ([i acc]
   (if-let [n (nth acc i nil)]
     (recur (inc i) (drop-nth-slow n acc))
     acc)))

;; SEM: I don't like the rem being used here.  The keep-indexed is faster than lazy
;; recursive one.
(defn drop-nth1 [n coll]
  (keep-indexed (fn [i x] (when-not (zero? (rem (inc i) n)) x)) coll))

;; it's worth keeping track of the count to avoid unnecessary drop-nth calls
(defn lucky1
  ([max] (sequence (when (pos? max) (lucky1 1 (quot max 2) (range 1 max 2)))))
  ([i cnt acc]
   (let [n (long (nth acc i Long/MAX_VALUE))]
     (if (<= n cnt)
       (recur (inc i) (- cnt (quot cnt n)) (drop-nth1 n acc))
       acc))))

(def lucky100 [1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
               87, 93, 99])


(def lucky300 [1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
               87, 93, 99, 105, 111, 115, 127, 129, 133, 135, 141, 151, 159, 163, 169, 171,
               189, 193, 195, 201, 205, 211, 219, 223, 231, 235, 237, 241, 259, 261, 267,
               273, 283, 285, 289, 297])


;; there's a bug somewhere in the limits on the RRB for big numbers

(defn dis-nth-RRB
  ([n rrb] (dis-nth-RRB (dec (* n (quot (count rrb) n))) n rrb))
  ([n step rrb]
   (if (pos? n)
     (recur (- n step) step (fv/catvec (fv/subvec rrb 0 n) (fv/subvec rrb (inc n))))
     rrb)))

(defn catvec [v1 v2]
  (vec (concat v1 v2)))

(defn dis-nth
  ([n rrb] (dis-nth (dec (* n (quot (count rrb) n))) n rrb))
  ([n step rrb]
   (if (pos? n)
     (recur (- n step) step (catvec (subvec rrb 0 n) (subvec rrb (inc n))))
     rrb)))


;; maxes out btw 7000 and 8000 with RRB
(defn lucky-no-good
  ([max] (when (pos? max) (lucky-no-good 1 (vec (range 1 max 2)))))
  ([i rrb]
   (let [n (long (nth rrb i Long/MAX_VALUE))]
     (if (<= n (count rrb))
       (recur (inc i) (dis-nth n rrb))
       (seq rrb)))))


;; SEM is int-set inherently sorted?  I would guess so.

(defn disj-nth-BACKWARDS
  ([n v iset] (disj-nth-BACKWARDS (dec (* n (quot (count iset) n))) n v iset))
  ([n step v iset]
   (if-let [x (when (pos? n) (nth v n nil))]
     (let [iset (disj iset x)]
       ;; keep the same v since we're working backwards
       (recur (- n step) step v iset))
     iset)))

(defn iset-disj-nth
  ([n iset] (iset-disj-nth (dec n) n (vec (seq iset)) iset))
  ([n v iset] (iset-disj-nth (dec n) n v iset))
  ([n step v iset]
   (if-let [x (when (< n (count v)) (nth v n nil))]
     (recur (+ n step) step v (disj iset x))
     iset)))

(defn lucky-iset
  ([max] (when (pos? max) (lucky-iset 1 (im/dense-int-set (range 1 max 2)))))
  ([i iset]
   ;; WARNING: assuming that iset seq is automagically sorted (seems to be stable at least)
   (let [v (vec (seq iset))
         n (nth v i nil)]
     (if (< n (count v))
       (recur (inc i) (iset-disj-nth n v iset))
       (seq iset)))))


(defn lucky-iset2
  ([max] (when (pos? max) (lucky-iset 1 (im/dense-int-set (range 1 max 2)))))
  ([i iset]
   ;; WARNING: assuming that iset seq is automagically sorted (seems to be stable at least)
   (let [v (vector-of :long (seq iset))
         n (nth v i Long/MAX_VALUE)]
     (if (< n (count v))
       (recur (inc i) (iset-disj-nth n v iset))
       (seq iset)))))





;; only marginally faster, not worth the extra code
(defn disj-nth2
  ([n v iset] (disj-nth2 (dec n) n v (transient iset)))
  ([n step v iset]
   (if-let [x (when (< n (count v)) (nth v n nil))]
     (recur (+ n step) step v (disj! iset x))
     (persistent! iset))))

(defn lucky2
  ([max] (when (pos? max) (lucky2 1 (im/dense-int-set (range 1 max 2)))))
  ([i iset]
   ;; assuming that iset is inherently orderded so the seq is sorted for free
   (let [v (vec (seq iset))
         n (nth v i)]
     (if (< n (count v))
       (recur (inc i) (disj-nth2 n v iset))
       (seq iset)))))
  




;; another idea: reduce with disj instead of recur
;; need to generate a range of indexes using step
;; might be good with transducers


(defn iset-disj-nth
  ([n iset] (iset-disj-nth (dec n) n (vec (seq iset)) iset))
  ([n v iset] (iset-disj-nth (dec n) n v iset))
  ([n step v iset]
   (if-let [x (when (< n (count v)) (nth v n nil))]
     (recur (+ n step) step v (disj iset x))
     iset)))


;; about the same but simper so good

(defn lucky3
  ([max] (lucky3 1 (im/dense-int-set (range 1 max 2))))
  ([i iset]
   ;; WARNING: assuming that iset seq is automagically sorted, seems to be stable at least
   (let [v (vec (seq iset))
         n (long (nth v i Long/MAX_VALUE))]
     (if (<= n (count v))
       (recur (inc i) (reduce (fn [sss m] (disj sss (nth v m)))
                              iset
                              (range (dec n) (count v) n)))
       (sequence iset)))))



;; lucky4 was lucky3 with regular int-set, actually slower with regular int-set


;; another idea map range of removals into a set and then set/diff in one shot
;; not faster

(defn lucky4
  ([max] (lucky4 1 (im/dense-int-set (range 1 max 2))))
  ([i iset]
   ;; WARNING: assuming that iset seq is automagically sorted, seems to be stable at least
   (let [sss (seq iset)
         v (vec sss)
         n (long (nth v i Long/MAX_VALUE))]
     (if (<= n (count v))
       (recur (inc i)
              (im/difference iset
                             (im/dense-int-set (take-nth n (drop (dec n) sss)))))
       (sequence iset)))))


;; even slower with take/drop to build removal set

;; begging for transducers!


;;; Requires Clojure 1.7.0-alpha5 or greater

;; don't like the rem in BAD-drop-nth
(comment
  (defn BAD-drop-nth
  ([n] (keep-indexed (fn [i x] (when-not (zero? (rem (inc i) n)) x))))
  ([n coll]
   (keep-indexed (fn [i x] (when-not (zero? (rem (inc i) n)) x)) coll))))


;; need to look up the item to decide the step N
;; so it's hard to compose as one stream of values
;; can we package the state so we have the index and survivors?

;; empty would make a convenient transducer that does nothing (ultimately returns empty list)
(defn my-empty
  "Like empty, but no-arg yields degenerate transducer that alway returns empty list"
  ([] (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result)))))
  ([coll] (empty coll)))
  

;; I think (take-nth 0) should always be the empty list
(defn my-take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided.  N=0 returns empty list."
  {:added "1.0"
   :static true}
  ([n]
   (if (zero? n)
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result))))
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
   (if (zero? n)
     ()
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (first s) (my-take-nth n (drop n s))))))))


(defn lazy-drop-nth [n coll]
  ;; assumes (pos? n)
  ;; Notice drops first (aiming to be complement of take-nth)
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take (dec n) (rest s)) (lazy-drop-nth n (drop n s))))))

(defn NOT-BETTER-lazy-drop-nth [n coll]
  (mapcat (fn [i item] (when-not (zero? i) (list item))) (cycle (range n)) coll))

(defn pdrop-nth [n coll] (mapcat rest (partition-all n coll)))



;; SEM: this is my preferred version.  NB: drops first (like complement of take-nth), unlike
;; some other versions in this file that were built for lucky numbers.


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
     (lazy-drop-nth n coll))))

(defn my-lucky-drop-nth [n coll]
  (my-drop-nth n (lazy-cat '(nil) coll)))



;; Reminder: the eager way to concat two vectors is just to use (into v1 v2)

;; Note: lucky style is to drop at end Nth, unlike Clojure take-nth which always takes first and
;; nth thereafter.

(defn vdrop-nth [n v]
  (loop [res (transient []) remaining v]
    (if (< (count remaining) n)
      (persistent! (reduce conj! res remaining))
      (recur (reduce conj! res (subvec remaining 0 (dec n))) (subvec remaining n)))))

(defn vrange
  ([end] (vrange 0 end 1))
  ([start end] (vrange start end 1))
  ([start end step]
   (loop [tv (transient []) n start]
     (if (< n end)
       (recur (conj! tv n) (+ n step))
       (persistent! tv)))))

;; somewhat faster than lucky5t
(defn lucky-eager
  ([max] (sequence (when (pos? max) (lucky-eager 1 (vrange 1 max 2)))))
  ([i surv]
   (let [n (nth surv i Long/MAX_VALUE)]
     (if (<= n (count surv))
       (recur (inc i) (vdrop-nth n surv))
       surv))))


;; slightly slower with vector-of
(defn lucky-eager3
  ([max] (sequence (when (pos? max) (lucky-eager3 1 (into (vector-of :long) (range 1 max 2))))))
  ([i surv]
   (let [n (nth surv i Long/MAX_VALUE)]
     (if (> n (count surv))
       surv
       (recur (inc i) (vdrop-nth n surv))))))






;;; SEM: still thinking about the utilty vs. API cost of OFFSET arg -- nothing else has an
;;; offset so it's not worth the confusion.
(defn offset-drop-nth
  "Returns a lazy seq dropping the element at OFFSET index and every Nth item thereafter in COLL.
  Returns a stateful transducer when no collection is provided.  OFFSET defaults to 0.  If N
  = 0, the original COLL is returned."
  {:static true}
  ;; offset and n should be non-neg
  ([n] (offset-drop-nth 0 n))
  ([offset n]
   (if (zero? n)
     conj
     (fn [rf]
       (let [iv (volatile! (inc offset))]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv dec)]
                (if (zero? i)
                  (do (vreset! iv n)
                      result)
                  (rf result input)))))))))
  ([offset n coll]
   (if (zero? n)
     coll
     (if (zero? offset)
       (lazy-drop-nth n coll)
       (lazy-cat (take offset coll) (lazy-drop-nth n (drop offset coll)))))))



;; but none of these (comp) exprs are as fast as my-drop-nth

(defn eager-drop-nth [n coll]
  (transduce (comp (partition-all n) (mapcat rest)) conj coll))

(defn lazy-drop-nth-trans [n coll]
  (sequence (comp (partition-all n) (mapcat rest)) coll))



;; IDEA: take-drop-stepper [2 -3 4] cycles taking and dropping according to sign



(defn lucky-drop-nth [n coll]
  (concat (take (dec n) coll)
          (eager-drop-nth n (drop (dec n) coll))))

;; still slow
(defn lucky11
  ([max] (sequence (when (pos? max) (lucky11 1 (quot max 2) (range 1 max 2)))))
  ([i cnt acc]
   (let [n (long (nth acc i Long/MAX_VALUE))]
     (if (<= n cnt)
       (recur (inc i) (- cnt (quot cnt n)) (lucky-drop-nth n acc))
       acc))))


;; it's worth keeping track of the count to avoid unnecessary drop-nth calls
(defn lucky5t
  ([max] (sequence (when (pos? max) (lucky5t 1 (quot max 2) (range 1 max 2)))))
  ([i cnt acc]
   (let [n (long (nth acc i Long/MAX_VALUE))]
     (if (<= n cnt)
       (recur (inc i) (- cnt (quot cnt n)) (transduce (offset-drop-nth (dec n) n) conj [] acc))
       acc))))






;; Note: nth is not supported on the standard Clojure sorted-set.  However, the contrib avl
;; lib does support a sorted-set with nth.  Just what we need!

;; I decided range-down wasn't really necessary, as the persistent avl is still there in nth
;; order.  Tested both and there wasn't much difference.  Transients didn't help because nth
;; doesn't work on the transient.
;;
;; My attempt to use transients got...
;; UnsupportedOperationException nth not supported on this type: AVLTransientSet
;;
;; It works if you go in and out of transients and persistents as in lucky-avl1. 
;; It's actually faster, but somewhat ugly code.

;; By far the fastest.  And robust over 1e6.
(defn lucky-avl
  ([max] (lucky-avl 1 (into (avl/sorted-set) (range 1 max 2))))
  ([i avl]
   (let [n (nth avl i Long/MAX_VALUE)]
     (if (<= n (count avl))
       (recur (inc i) (reduce (fn [sss m] (disj sss (nth avl m)))
                              avl
                              (range (dec n) (count avl) n)))
       (sequence avl)))))

;; faster than lucky-avl but somewhat uglier
(defn lucky-avl1
  ([max] (lucky-avl1 1 (persistent! (reduce conj! (transient (avl/sorted-set)) (range 1 max 2)))))
  ([i avl]
   (let [n (nth avl i Long/MAX_VALUE)]
     (if (<= n (count avl))
       (recur (inc i) (persistent! (reduce (fn [sss m] (disj! sss (nth avl m)))
                                           (transient avl)
                                           (range (dec n) (count avl) n))))
       (seq avl)))))



;; gen-lucky-ss should work with any persistent sorted-set implementation that supports nth
;; (unlike the built-in Clojure sorted-set).  Of course, conj, disj, and count would naturally be
;; required as well.  It will stress the persistence by making lots of small "updates" with
;; disj. The init-ss should be an empty sorted-set that will be populated by conj.

(defn gen-lucky-ss
  ([init-ss] (gen-lucky-ss init-ss 1))
  ([init-ss start]
   (fn [max]
     (let [lucky (fn [i ss]
                   (let [n (nth ss i Long/MAX_VALUE)]
                     (if (<= n (count ss))
                       (recur (inc i) (reduce (fn [sss m] (disj sss (nth ss m)))
                                              ss
                                              (range (dec n) (count ss) n)))
                       (sequence ss))))]
       (lucky 1 (into init-ss (range start max 2)))))))


;; SEM: wondering about using avl approach for primes.  Answer: not so good.

;; Criterium should be available in my build settings, but it would be better to explicitly
;; require it in the (ns ...)

(require '[criterium.core :as cc])

(require '[clojure.string :as str])

(require 'clojure.repl)

(defn ben [& luckys]
  (let [cnt1 (when (number? (first luckys)) (long (first luckys)))
        luckys (if cnt1 (rest luckys) luckys)
        cnt (or cnt1 10000)]
  (doseq [luck luckys]
    (printf "\n(%s %d)\n" (clojure.repl/demunge (pr-str (class luck))) cnt)
    (cc/quick-bench (luck cnt)))))


;; variant that just starts with the even numbers
;; (only change is the start of the range, from 1 to 2)
;; note results never include the MAX
(defn even-lucky-avl
  ([max] (lucky-avl 1 (into (avl/sorted-set) (range 2 max 2))))
  ([i avl]
   (let [n (nth avl i Long/MAX_VALUE)]
     (if (<= n (count avl))
       (recur (inc i) (reduce (fn [sss m] (disj sss (nth avl m)))
                              avl
                              (range (dec n) (count avl) n)))
       (sequence avl)))))


;; https://oeis.org/A045954
(def even-lucky-100 [2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68,
                     70, 76, 84, 90, 98])

(def even-lucky-301 [2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68,
                     70, 76, 84, 90, 98, 100, 102, 108, 114, 116, 118, 130, 132, 138, 140,
                     148, 150, 164, 170, 172, 178, 182, 186, 196, 198, 212, 214, 218, 228,
                     230, 234, 244, 246, 260, 262, 268, 278, 282, 290, 298, 300])




