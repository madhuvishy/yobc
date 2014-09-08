(ns yobc.tracker
  (:require [clojure.core.async :as async :refer  [<! >! chan go close!]]
            [yobc.bencoder :as bencoder :refer [bencode]]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.utils :as utils :refer [sha-1]])
  (:import  (java.net InetSocketAddress DatagramPacket DatagramSocket)))


(defn get-byte [bytearray i]
  (let [bytearray (bytes bytearray)]
    (aget bytearray i)))

(defn get-bytes 
  ([bytearray] (map #(aget bytearray %) (range (count bytearray))))
  ([bytearray start end] (map #(aget bytearray %) (range start end))))

(defn make-tid []
  (repeat 4  (rand-int 127)))

(defn tracker!
  "Defines out - a channel that blocks until there's a packet and does a UDP send,
  and in - a channel that blocks until there's a packet from the server and puts it to be read."
  [host port]
  (let [socket (DatagramSocket.)
        out (chan)
        in (chan)
        address (InetSocketAddress. host port)]
    (go (when-let [payload (<! out)]
          (.send socket (DatagramPacket. payload (count payload) address))))
    (go (while true (let [packet (DatagramPacket. (byte-array 512) 512)]
          (.receive socket packet)
          (>! in packet))))
    {:in in :out out}))

(defn connect-req 
  [tid]
  (byte-array
    (map byte
      (concat
        [0 0 4 23 39 16 25 -128]  ;initial-cid
        (repeat 4 0)              ;initial-action
        tid))))

(defn connect!
  [tracker]
  (let [tid (make-tid)
        payload (connect-req tid)]
    (go
      (>! (:out tracker) payload)
      (when-let [data (<! (:in tracker))]
        (let [b (get-bytes (.getData data))
              tid' (take 4 (drop 4 b))
              cid' (take 8 (drop 8 b))]
          (when (= tid' tid)
             cid'))))))

(defn announce-req 
  [cid tid info-hash]
  (byte-array 
    (map byte 
      (concat
        cid
        [0 0 0 1]     ;action 
        tid
        (seq info-hash)
        (repeat 20 0) ;peer-id
        (repeat 8 0)  ;downloaded
        (repeat 8 0)  ;left
        (repeat 8 0)  ;uploaded
        (repeat 4 0)  ;event
        (repeat 4 0)  ;IPv4 address
        (repeat 4 0)  ;key
        (repeat 4 1)  ;num want
        [43 70]       ;Client ip 
        ))))

(defn announce!
  []
  (go
    (let [tracker (tracker! "tracker.publicbt.com" 80)
          cid (<! (connect! (tracker! "tracker.publicbt.com" 80)))
          tid (make-tid)
          info-hash (sha-1 (bencode ((bdecode "mytorr.torrent") "info"))) ]

      (>! (:out tracker) (announce-req cid tid info-hash))
      (when-let [data (<! (:in tracker))]
        (get-bytes (.getData data) 0 (.getLength data)))))) 

(go (println (<! (announce!))))


