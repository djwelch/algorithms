(ns subsetsum.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test.alpha :as stest]))            

(defn sums-to [t s] (= (reduce + 0 s) t))

(defn contains-all? [ret s]
  (if (empty? ret)
    true
    (loop [[s [xr & r]] (split-with (partial not= (first ret)) s) next (rest ret)]
      (if-not xr
        false
        (if (empty? next)
          true
          (recur (split-with (partial not= (first next)) (concat s r)) (rest next)))))))

(def elem-and-set
  (gen/bind (gen/vector (gen/choose 1 100) 1 5)
            #(gen/tuple
              (gen/return (reduce + 0 %))
              (gen/return %))))

(s/fdef ::subsetsum
        :args (s/with-gen
                (s/cat :t int? :s (s/coll-of int? :min-count 1))
                (fn [] elem-and-set))
        :ret (s/coll-of int?)
        :fn #(let [{{:keys [t s]} :args ret :ret} %] (and (contains-all? ret s) (sums-to t ret))))

(defn subsetsum-brute-force [t s]
  (first (->> s comb/subsets (filter #(and (not (empty? %)) (= (reduce + 0 %) t))))))

(comment
  (get-in (stest/check-fn subsetsum-brute-force ::subsetsum)
          [:clojure.spec.test.check/ret
           :shrunk
           :smallest])

  (stest/check-fn subsetsum-brute-force ::subsetsum))

