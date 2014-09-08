(ns yobc.tracker
  (:require [clojure.core.async :as async :refer  [<! >! chan go close!]])
  (:import  (java.net InetSocketAddress DatagramPacket DatagramSocket)))


(defn get-byte [bytearray i]
  (let [bytearray (bytes bytearray)]
    (aget bytearray i)))

(defn get-bytes 
  ([bytearray] (map #(aget bytearray %) (range (count bytearray))))
  ([bytearray start end] (map #(aget bytearray %) (range start end))))

(defn tracker!
  "Defines out - a channel that blocks until there's a packet and does a UDP send,
  and in - a channel that blocks until there's a packet from the server and puts it to be read."
  [host port in-buf-len]
  (let [socket (DatagramSocket.)
        out (chan)
        in (chan)
        address (InetSocketAddress. host port)]
    (go (when-let [payload (<! out)]
          (.send socket (DatagramPacket. payload (count payload) address))))
    (go (while true (let [packet (DatagramPacket. (byte-array in-buf-len) in-buf-len)]
          (.receive socket packet)
          (>! in packet))))
    {:in in :out out}))

(defn connect!
  []
  (let [t (tracker! "tracker.publicbt.com" 80 16)
        initial-cid  (byte-array  (map byte  [0 0 4 23 39 16 25 -128]))
        initial-action  (byte-array 4)
        initial-tid  (byte-array  (map byte  (repeat 4  (rand-int 127))))
        payload  (byte-array  (concat initial-cid initial-action initial-tid))]
    (go (>! (:out t) payload))
    (go (when-let [data (<! (:in t))]
          (let [b (get-bytes (.getData data))
                tid (take 4 (drop 4 b))
                cid (take 8 (drop 8 b))]
            (when (= tid (get-bytes initial-tid))
              cid))))))


(defn announce!
  []
  )


