(ns yobc.peer
  (:import [java.net Socket InetAddress])
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [<!! go go-loop <! >! chan close! sliding-buffer]]
            [yobc.tracker :as tracker]
            [yobc.utils :as utils :refer :all]
            [yobc.pwp :as pwp]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]))

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
  [conn info-hash]
  (go
    (>! (:out conn) (pwp/handshake-msg info-hash))
    (when-let [handshake (<! (pwp/handshake (:in conn)))]
      (when (= (:info-hash handshake) (hex-to-bytes info-hash))
        handshake))))

(defn get-messages! [in]
  (let [outbox (chan)]
    (go-loop []
      (when-let [data (<! (pwp/parse-message in))]
        (>! outbox data)
        (recur)))
    outbox))

(defn connected-peer!
  [peer info-hash local-peer-id]
    (go
      (let [conn (connect! peer)]
        (when-let [handshake (<! (handshake! conn info-hash))]
          {:inbox (:in conn) :outbox (:out conn) :peer-id (:peer-id handshake)}))))
