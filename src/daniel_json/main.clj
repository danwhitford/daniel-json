(ns daniel-json.main
  (:require [clojure.string :as str]))

;; (defn rest-string [s]
;;   (if (first s) (subs s 1) nil))

;; (defn split-string [s]
;;   (list (first s) (rest-string s)))

(defn get-string [head tail]
  (loop [h head t tail ret "" escaped false]
    (case h
      \" (if escaped (recur (first t) (rest t) (str ret h) false) (list ret t))
      \\ (if escaped (recur (first t) (rest t) (str ret h) false) (recur (first t) (rest t) (str ret) true))
      (\b \f \n \r \t) (if escaped (recur (first t) (rest t) (str ret \\ h) false) (recur (first t) (rest t) (str ret h) false))
      (recur (first t) (rest t) (str ret h) false))))

(defn get-number [head tail]
  (loop [h head t tail ret ""]
    (if (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.} h)
      (recur (first t) (rest t) (str ret h))
      (if (re-matches #"[0-9]+[.][0-9]+" ret)
        (list (Float/parseFloat ret) (cons h t))
        (list (Integer/parseInt ret) (cons h t))))))

(defn get-true [head tail]
  (let [[r u e & remaining] tail]
    (assert (= (str head r u e) "true"))
    (list true remaining)))

(defn get-false [head tail]
  (let [[a l s e & remaining] tail]
    (assert (= (str head a l s e) "false"))
    (list false remaining)))

(defn get-token [fchar rstr]
  (case fchar
    \{ (list :lcurly rstr)
    \} (list :rcurly rstr)
    \[ (list :lbrack rstr)
    \] (list :rbrack rstr)
    \, (list :comma rstr)
    \: (list :colon rstr)
    \" (get-string (first rstr) (rest rstr))
    \t (get-true fchar rstr)
    \f (get-false fchar rstr)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (get-number fchar rstr)
    (list :? rstr)))

(defn get-tokens [s]
  (loop [[fst & rst] s tokens []]
    (cond
      (nil? fst) tokens
      (clojure.string/blank? (str fst)) (recur rst tokens)
      :else (let [[token remaining] (get-token fst rst)]
              (recur remaining (conj tokens token))))))

(declare get-map)
(declare get-vec)

(defn make-clj-val [tokens]
  (case (first tokens)
    :lcurly (get-map (rest tokens))
    :lbrack (get-vec (rest tokens))
    (:rparen :rbrack :comma :colon) :error
    (list (first tokens) (rest tokens))))

(defn get-map [tokens]
  (loop [tokens tokens ret {}]
    (let [[key _ & remaining] tokens
          [val rst] (make-clj-val remaining)]

      (case (first rst)
        :rcurly (list (merge ret {key val}) (rest rst))
        :comma (recur (rest rst) (merge ret {key val}))
        :map-error))))

(defn get-vec [tokens]
  (loop [tokens tokens ret []]
    (let [[val rst] (make-clj-val tokens)]
      (case (first rst)
        :rbrack (list (conj ret val) (rest rst))
        :comma (recur (rest rst) (conj ret val))
        :vec-error))))

(defn read-str [s]
  (let [tokens (get-tokens s)]
    (case (first tokens)
      :lcurly (first (get-map (rest tokens)))
      :lbrack (first (get-vec (rest tokens)))
      (:rparen :rbrack :comma) :error
      (first tokens))))

