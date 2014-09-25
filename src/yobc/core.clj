(ns yobc.core
  (:require [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.bencoder :as bencoder :refer [bencode]]
            [clojure.core.async :as async :refer [alts! <!! <! >! go go-loop close! chan]]
            [yobc.peer :as p]
            [yobc.pwp :as pwp]
            [yobc.utils :as utils :refer :all]
            [yobc.tracker :as tracker]))


(def torrent (bdecode "mytorr.torrent"))
(def pieces (pwp/pieces (torrent "info")))
(def blocks (pwp/blocks (torrent "info")))
(def peers (address-port-pairs (<!! (tracker/get-peers! torrent))))

(defn connect [peer]
  (go (when-let [conn (<! (p/connected-peer! peer (info-hash torrent) (peer-id)))]
        (let [data-chan (p/get-messages! (:inbox conn))]
          (loop []
            (when-let [data (<! data-chan)]
              (println "Heyyy " data)
              (recur)))))))




;Not using this now! 
(defn find-one-peer [peers torrent]
  (go
    (when-let 
      [[handshake ch] (alts! 
                        (map #(p/connected-peer! % (info-hash torrent) (peer-id)) peers))]
      (println handshake))))
