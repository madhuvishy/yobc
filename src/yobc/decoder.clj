(ns yobc.decoder
  (:require [clojure.java.io :as io]))

(defn get-next-char
  "Given a stream returns the next character to be read"
  [stream]
  (let [value1 (.read stream)]
    (if (not= (int value1) -1) (char value1) nil)))

(defn decode-string
  "Decodes a bencoded string"
  [stream ch]
  (loop [cnt (decode-integer stream ch \:) 
         marker (get-next-char stream) 
         result "" ]
     (if (or (zero? cnt) (nil? marker))
       result
       (recur (dec cnt) (get-next-char stream) (str result marker)))))

(defn decode-integer
  "Decodes a bencoded integer"
  [stream ch delimiter]
  (loop [marker (or ch (get-next-char stream))
         result ""]
    (if (= marker delimiter)
      (Integer/parseInt (str result))
      (recur (get-next-char stream) (str result marker)))))

(defn decode-list
  "Decodes a bencoded string"
  [stream ch]
  0)

(defn decode-dict
  "Decodes a bencoded string"
  [stream ch]
  0)

(defn decode-stream
  "Decode markers in a bencoded stream"
  [stream ch]
  (let [marker (or ch (get-next-char stream))]
    (cond
      (nil? marker) nil
      (Character/isDigit marker) (decode-string stream marker)
      (= marker \i) (decode-integer stream marker \e)
      (= marker \l) (decode-list stream marker)
      (= marker \d) (decode-dict stream marker))))

(defn decode
  "Read torrent file as a stream and decode it"
  [filename]
  (with-open [rdr (io/reader filename)]
    (decode-stream rdr (get-next-char rdr))))

(decode "string.torrent")
