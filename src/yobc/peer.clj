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

(defn take-n! [in-chan n]
  (go-loop [arr [] iter 0]
    (when (and (= iter 4) (= arr [0 0 0 0])) (close! in-chan))
    (if (= n iter) 
      (seq arr)
      (recur (conj arr (<! in-chan)) (inc iter)))))

(defn connect! [[host port]]
  (let [sock (Socket. host port)
        in (chan (sliding-buffer 1000)) 
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
    (when-let [data (<! (take-n! (:in conn) 68))]
      data)))

(defn exchange! [peer]
  (go
    (let [conn (connect! peer)]
      (when-let [handshake (<! (handshake! conn))]
        (go-loop [] 
          (when-let [data (<! (take-n! (:in conn) 4))]
            (let [len (bytes-to-len data)]
              (when (> len 0)
                (let [msg-type (<! (:in conn))
                      msg (<! (take-n! (:in conn) (dec len)))]
                  (when (>! (:out conn) (pwp/process msg-type msg)) (recur)))))))))))

;(handshake! (nth peers 2))
