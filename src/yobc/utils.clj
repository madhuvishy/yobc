(ns yobc.utils
  (:import [java.net InetAddress])
  (:require [clojure.string :as string]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.bencoder :as bencoder :refer [bencode]]))

(defn throw-err [e]
  (when (instance? Throwable e) (throw e))
  e)
 
(defmacro <? [ch]
  `(throw-err (async/<! ~ch)))

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
        diff (- 4 (count b))]
    (concat (repeat diff 0) (get-bytes b))))

(defn host-port
  [url]
  (let [parts (string/split url #"/|:")]
    [(nth parts 3) (Integer/parseInt (nth parts 4))]))

(defn bytes-to-port
  [[byte1 byte2]]
  (bit-or (bit-and byte2 0xFF)
          (bit-shift-left (bit-and byte1 0xFF) 8)))

(defn port-to-bytes
  [port]
  [(bit-and (bit-shift-right port 8) 0xFF) (bit-and port 0xFF)])

(defn address-port-pair
  [[ip port]]
  [(InetAddress/getByAddress (byte-array (map byte ip)))
   (bytes-to-port port)])

(defn address-port-pairs 
  [peers]
  (map address-port-pair peers))

(defn bytes-to-len
  [[byte1 byte2 byte3 byte4]]
  (bit-or (bit-and byte4 0xFF)
          (bit-shift-left (bit-and byte3 0xFF) 8)
          (bit-shift-left (bit-and byte3 0xFF) 16)
          (bit-shift-left (bit-and byte3 0xFF) 24)))

(defn info-hash
  [torrent]
  (sha-1 (bencode (torrent "info"))))

(defn peer-id []
  "ABCDEFGHIJK123456789")

(defn bitfield-to-bits
  [bitfield]
  (mapv #(if (= \0 %) false true) 
       (vec
          (reduce str 
                  (map (fn [number]
                         (let [binary (Integer/toBinaryString number)]
                           (if (< number 0)
                             (subs binary 24)
                             (string/replace (format "%8s" binary) " " "0"))))
                        bitfield)))))
