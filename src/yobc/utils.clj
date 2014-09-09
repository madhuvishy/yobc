(ns yobc.utils
  (:require [clojure.string :as string]))

(defn sha-1 
  "Computes SHA-1 hash of a given string"
  [string]
  (apply str
    (map (partial format "%02x")
      (.digest (doto (java.security.MessageDigest/getInstance "SHA-1")
                     .reset
                     (.update (.getBytes string "ISO-8859-1")))))))

(defn get-byte [bytearray i]
  (let [bytearray (bytes bytearray)]
    (aget bytearray i)))

(defn get-bytes
  ([bytearray] (map #(aget bytearray %) (range (count bytearray))))
  ([bytearray start end] (map #(aget bytearray %) (range start end))))

(defn hex-to-bytes
  [hex-string]
  (let [b (.toByteArray (BigInteger. hex-string 16))]
    (if (> (count b) 20)
      (drop 1 (take 21 b))
      (get-bytes b))))

(defn dec-to-bytes
  [number]
  (let [b (.toByteArray (BigInteger. (str number) 10))
        diff (- 8 (count b))]
    (concat (repeat diff 0) (get-bytes b))))

(defn host-port
  [url]
  (let [parts (string/split url #"/|:")]
    [(nth parts 3) (Integer/parseInt (nth parts 4))]))
