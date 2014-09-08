(ns yobc.udp-client
  (:require [clojure.core.async :as async :refer  [<! >! chan go close!]])
  (:import  (java.net InetSocketAddress DatagramPacket DatagramSocket)))


(defn get-byte [bytearray i]
  (let [bytearray (bytes bytearray)]
    (aget bytearray i)))

(defn get-bytes [bytearray]
  (map #(aget bytearray %) (range (count bytearray))))

(defn udp!
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

(def initial-cid  (byte-array  (map byte  [0 0 4 23 39 16 25 -128])))
(def initial-action  (byte-array  (map byte  (repeat 4 0))))
(def initial-tid  (byte-array  (map byte  (repeat 4  (rand-int 127)))))
(def payload  (byte-array  (concat initial-cid initial-action initial-tid)))

(defn connect!
  []
  (let [t (udp! "tracker.publicbt.com" 80 16)
        in (:in t)
        out (:out t)]
    (go (>! out payload))
    (go (when-let [data (<! in)]
          (println (get-bytes (.getData data)))))))

