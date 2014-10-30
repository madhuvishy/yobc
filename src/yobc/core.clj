(ns yobc.core
  (:require [yobc.bdecoder :as bdecoder :refer [bdecode]]
            [yobc.bencoder :as bencoder :refer [bencode]]
            [clojure.core.async :as async :refer [alts! <!! <! >! go go-loop put! timeout chan]]
            [yobc.peer :as p]
            [yobc.pwp :as pwp]
            [yobc.utils :as utils :refer :all]
            [yobc.tracker :as tracker]))


(def torrent (bdecode "mytorr.torrent"))
(def peers (address-port-pairs (<!! (tracker/get-peers! torrent))))
(def state (atom {}))
(def block-size (int (Math/pow 2 14)))
(def piece-length ((torrent "info") "piece length"))

(defn find-rand-piece [bf peer-bf]
  (rand-nth
    (filter identity (map (fn [n] (if (and (not (bf n)) (peer-bf n)) n nil)) 
                          (range (count bf))))))

(defn request [data-chan out pid blocks]
  (let [piece-requested (find-rand-piece (:our-bitfield @state) 
                                         (get-in @state [pid :bitfield]))]
    (println blocks (pwp/piece->requests piece-requested blocks block-size))
    (go
      (doseq [block-request
              (pwp/piece->requests piece-requested blocks block-size)]
        (println block-request)
        (>! out block-request)
        (println "do we get here?")
        (let [msg (<! data-chan)]
          (println "Hey" msg))
        (println "how about here?")
        )))) 

(defn start [data-chan out pid pieces blocks]
  (go
    (>! out (pwp/interested))
    (loop []
      (println "about to go through the start loop")
      (when-let [msg (<! data-chan)]
        ;(println  msg)
        (condp = (pwp/msg-to-type msg)
          :bitfield 
          (swap! state assoc-in [pid :bitfield] 
                 (vec (take pieces (bitfield-to-bits (second msg)))))
          :have 
          (swap! state update-in [pid :bitfield (second msg)] #(if (not %) true))
          :unchoke
          (<! (request data-chan out pid blocks))
          ;:piece 
          ;  (println "piece")
          (println msg "Not sure what to do"))
        (recur)))))

(defn download [torrent peer-num]
  (let [pieces (pwp/pieces (torrent "info"))
        blocks (pwp/blocks (torrent "info"))]
    (reset! state {:our-bitfield (vec (repeat pieces false))})
    (let [peer (nth peers peer-num) 
          peer-state {:bitfield (vec (repeat pieces false))}
          peer-chan (p/connected-peer! peer (info-hash torrent) (peer-id))
          t-chan (timeout 2000) ]
      (go (when-let [[conn ch] (alts! [peer-chan t-chan])]
            (if (= ch peer-chan)
              (let [data-chan (p/get-messages! (:inbox conn))
                    id (:peer-id conn)
                    out (:outbox conn)]
                (swap! state assoc id peer-state)
                (<! (start data-chan out id pieces blocks)))
              (println "Timed out") ))))))
