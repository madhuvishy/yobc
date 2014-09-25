(ns yobc.core
  (:require [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.bencoder :as bencoder :refer [bencode]]
            [clojure.core.async :as async :refer [<!! <! >! go go-loop close! chan]]
            [yobc.peer :as p]
            [yobc.pwp :as pwp]
            [yobc.utils :as utils :refer :all]
            [yobc.tracker :as tracker]))


(def torrent (bdecode "mytorr.torrent"))
(def pieces (pwp/pieces (torrent "info")))
(def blocks (pwp/blocks (torrent "info")))
(def peers (address-port-pairs (<!! (tracker/get-peers! torrent))))

(defn connect [peer]
  (go (when-let [shake (<! (p/connected-peer! peer (info-hash torrent) (peer-id)))]
        (println shake))))


