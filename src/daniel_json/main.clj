(ns daniel-json.main
  (:require [clojure.string :as str] [clojure.java.io :as jio]))

(defn string-to-pbr [s]
  (java.io.PushbackReader. (java.io.StringReader. s)))

(defn read-char [pbs]
  (char (.read pbs)))

(defn unread-char [pbs char]
  (.unread pbs (int char)))

(defn get-string [pbr]
  (loop [pbr pbr ret "" escaped false]
    (let
     [h (read-char pbr)]
      (case h
        \" (if escaped (recur pbr (str ret h) false) ret)
        \\ (if escaped (recur pbr (str ret h) false) (recur pbr (str ret) true))
        (\b \f \n \r \t) (if escaped (recur pbr (str ret \\ h) false) (recur pbr (str ret h) false))
        (recur pbr (str ret h) false)))))

(defn get-number [pbr]
  (loop [pbr pbr ret ""]
    (let [ch (.read pbr)]
      (if (and (> ch 0) (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.} (char ch)))
        (recur pbr (str ret (char ch)))
        (do
          (when (> ch 0) (.unread pbr ch))
          (if (re-matches #"[0-9]+[.][0-9]+" ret)
            (Float/parseFloat ret)
            (Integer/parseInt ret)))))))

(defn get-true [pbr]
  (let [token (apply str (repeatedly 4 #(read-char pbr)))]
    (assert (= token "true"))
    true))

(defn get-false [pbr]
  (let [token (apply str (repeatedly 5 #(read-char pbr)))]
    (assert (= token "false"))
    false))

(defn get-token [pbr]
  (let [fchar (read-char pbr)]
    (case fchar
      \{ :lcurly
      \} :rcurly
      \[ :lbrack
      \] :rbrack
      \, :comma
      \: :colon
      \" (get-string pbr)
      \t (do (unread-char pbr fchar) (get-true pbr))
      \f (do (unread-char pbr fchar) (get-false pbr))
      (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (do (unread-char pbr fchar) (get-number pbr))
      :?)))

(defn get-tokens [pbr]
  (loop [pbr pbr tokens []]
    (let [fst (.read pbr)]
      (cond
        (< fst 0) tokens
        (clojure.string/blank? (str (char fst))) (recur pbr tokens)
        :else (do
                (unread-char pbr fst)
                (let [token (get-token pbr)]
                  (recur pbr (conj tokens token))))))))

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

(defn read-pbr [pbr]
  (let [tokens (get-tokens pbr)]
    (case (first tokens)
      :lcurly (first (get-map (rest tokens)))
      :lbrack (first (get-vec (rest tokens)))
      (:rparen :rbrack :comma) :error
      (first tokens))))

(defn read-str [s]
  (let [pbr (string-to-pbr s)]
    (read-pbr pbr)))

(defn read-file [fn]
  (with-open [f (java.io.PushbackReader. (jio/reader fn))]
    (read-pbr f)))
