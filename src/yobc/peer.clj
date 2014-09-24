(ns yobc.peer
  (:import [java.net Socket InetAddress])
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [<!! go go-loop <! >! chan close! sliding-buffer]]
            [yobc.tracker :as tracker]
            [yobc.utils :as utils :refer :all]
            [yobc.pwp :as pwp]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]))

(def torrent (bdecode "mytorr.torrent"))
(def pieces (pwp/pieces (torrent "info")))
(def blocks (pwp/blocks (torrent "info")))
(def peers (address-port-pairs (<!! (tracker/get-peers! torrent))))

(defn connect! [[host port]]
  (let [sock (Socket. host port)
        in (chan (sliding-buffer 100)) 
        out (chan)]
    (go-loop [] 
      (when-let [message (<! out)]
        (.. sock getOutputStream (write message))
        (recur)))
    (let [b (byte-array 1)]  
      (go-loop [] 
        (when-let [len (.. sock getInputStream (read b))]
          (when (>! in (first (get-bytes b)))
            (recur)))))
    {:in in :out out}))

(defn handshake! 
  "#FIXME: Test for valid info hash returned"
  [conn]
  (go
    (>! (:out conn) (pwp/handshake-msg (info-hash torrent)))
    (when-let [data (<! (pwp/handshake (:in conn)))]
      data)))

(defn get-messages! [peer]
  (go
    (let [conn (connect! peer)
          outbox (chan 10)]
      (when-let [handshake (<! (handshake! conn))]
        (println handshake)
        (loop []
          (when-let [data (<! (pwp/parse-message (:in conn)))]
            (println data)
            (>! outbox data)
            (recur)))))))


;(handshake! (nth peers 2))
