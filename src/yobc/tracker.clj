(ns yobc.tracker
  (:require [clojure.core.async :as async :refer  [<! >! chan go close! <!!]]
            [yobc.bencoder :as bencoder :refer [bencode]]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.utils :as utils :refer :all])
  (:import  (java.net InetSocketAddress DatagramPacket DatagramSocket)))

(defn make-tid []
  (repeat 4  (rand-int 127)))

(defn connect-req 
  [tid]
  (byte-array
    (map byte
      (concat
        [0 0 4 23 39 16 25 -128]  ;initial-cid
        (repeat 4 0)              ;initial-action
        tid))))

(defn announce-req 
  [cid tid info-hash]
  (byte-array 
    (map byte 
      (concat
        cid
        [0 0 0 1]     ;action 
        tid
        (hex-to-bytes info-hash)
        (repeat 20 0) ;peer-id
        (repeat 8 0)  ;downloaded
        (repeat 8 0)  ;left
        (repeat 8 0)  ;uploaded
        (repeat 4 0)  ;event
        (repeat 4 0)  ;IPv4 address
        (repeat 4 0)  ;key
        (repeat 4 -128)  ;num want
        [43 70]       ;Client ip 
        ))))

(defn tracker!
  "Defines out - a channel that blocks until there's a packet and does a UDP send,
  and in - a channel that blocks until there's a packet from the server and puts it to be read."
  [[host port]]
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

(defn connect!
  [tracker tid]
  (let [payload (connect-req tid)]
    (go
      (>! (:out tracker) payload)
      (when-let [data (<! (:in tracker))]
        (let [b (get-bytes (.getData data))
              tid' (take 4 (drop 4 b))
              cid' (take 8 (drop 8 b))]
          (when (= tid' tid)
             cid'))))))

(defn announce-resp
  [resp]
  {:action (take 4 resp)
   :tid (take 4 (drop 4 resp))
   :interval (take 4 (drop 8 resp))
   :leechers (take 4 (drop 12 resp))
   :seeders (take 4 (drop 16 resp))
   :ip-port-pairs (map #(partition-all 4 %) (partition 6 (drop 20 resp)))})

(defn announce!
  [tracker cid tid info-hash]
  (go
    (>! (:out tracker) (announce-req cid tid info-hash))
    (when-let [data (<! (:in tracker))]
      (let [resp (announce-resp (get-bytes (.getData data) 0 (.getLength data)))]
        (when (= tid (:tid resp))
          resp))))) 

(defn get-peers!
  [torrent-file]
  (go
    (let [torrent (bdecode torrent-file)
          info-hash (sha-1 (bencode (torrent "info")))
          host-port (host-port (torrent "announce"))          
          tid (make-tid)
          cid (<! (connect! (tracker! host-port) tid))
          announce-resp (<! (announce! (tracker! host-port) cid tid info-hash))]

      (:ip-port-pairs announce-resp))))

(println (<!! (get-peers! "mytorr.torrent")))
