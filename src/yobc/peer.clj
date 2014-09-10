(ns yobc.peer
  (:import [java.net Socket InetAddress])
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [<!! go <! >! chan close!]]
            [yobc.tracker :as tracker]
            [yobc.utils :as utils :refer :all]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]))

(def torrent (bdecode "mytorr.torrent"))

(def peers (address-port-pairs (<!! (tracker/get-peers! torrent))))

(defn handshake-msg
  [info-hash]
  (byte-array
    (map byte
         (concat
           [19]
           (seq "BitTorrent protocol")
           (repeat 8 0)
           (hex-to-bytes info-hash)
           (seq (peer-id))))))

(defn connect! [[host port]]
  (let [sock (Socket. host port)
        in (chan) 
        out (chan)]
    (go (when-let [message (<! out)]
      (print "HEY" message)
      (.. sock getOutputStream write message)))
    (go (when-let [data (.. sock getIputStream read)]
          (>! in data)))
    {:in in :out out}))

(defn handshake! [peer]
  (go
    (let [conn (connect! peer)]
      (println conn)
      (>! (:out conn) (handshake-msg (info-hash torrent)))
      (when-let [data (<! (:in conn))]
        (println (data))))))

(handshake! (first peers))
