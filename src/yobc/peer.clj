(ns yobc.peer
  (:import [java.net Socket InetAddress])
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [<!! go go-loop <! >! chan close! sliding-buffer]]
            [yobc.tracker :as tracker]
            [yobc.utils :as utils :refer :all]
            [yobc.pwp :as pwp]
            [yobc.bdecoder :as bdecoder :refer [bdecode]]))

(defn connect! [[host port]]
  (when-let [sock (try (Socket. host port) 
                       (catch Exception e (println "Connect failed") nil))]
    (let [in (chan (sliding-buffer 100)) 
          out (chan (sliding-buffer 100))]
      (go-loop [] 
        (when-let [message (<! out)]
          (try 
            (.. sock getOutputStream (write message)) 
            (catch Exception e 
              (println "caught exception")))
          (recur)))
      (let [b (byte-array 1)]  
        (go-loop [] 
          (when-let [len (try (.. sock getInputStream (read b)) 
                              (catch Exception e nil))]
            (when (and (> len 0) (>! in (first (get-bytes b))))
              (recur)))))
      {:in in :out out})))

(defn handshake! 
  [conn info-hash]
  (go
    (>! (:out conn) (pwp/handshake-msg info-hash))
    (when-let [handshake (<? (pwp/handshake (:in conn)))]
      (when (= (:info-hash handshake) (hex-to-bytes info-hash))
        handshake))))

(defn get-messages! [in]
  (let [outbox (chan (sliding-buffer 100))]
    (go-loop []
      (when-let [data (<! (pwp/parse-message in))]
        (println "got some data ")
        (>! outbox data)
        (recur)))
    outbox))

(defn connected-peer!
  [peer info-hash local-peer-id]
    (go
      (let [conn (connect! peer)]
        (when-let [handshake (<! (handshake! conn info-hash))]
          {:inbox (:in conn) :outbox (:out conn) :peer-id (:peer-id handshake)}))))
