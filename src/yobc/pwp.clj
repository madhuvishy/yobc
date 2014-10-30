(ns yobc.pwp
  (:require [yobc.utils :as utils :refer :all]
            [clojure.core.async :as async :refer [<! >! go go-loop]]))

(defn pieces 
  [torrent-info]
  (let [length (torrent-info "length")
        piece-length (torrent-info "piece length")
        pieces-estimate (quot length piece-length)]
    (if (> (mod length piece-length) 0)
      (inc pieces-estimate)
      pieces-estimate)))

(defn blocks
  [torrent-info]
  (let [piece-length (torrent-info "piece length")
        block-size (int (Math/pow 2 14))
        blocks-estimate (quot piece-length block-size)]
    (if (> (mod piece-length block-size) 0)
      (inc blocks-estimate)
      blocks-estimate)))

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

(defn interested [] (byte-array (map byte [0 0 0 1 2])))

(defn request [piece-index block-offset block-length]
  (byte-array
    (map byte 
         (concat
           [0 0 0 13 6]
           (dec-to-bytes piece-index)
           (dec-to-bytes block-offset)
           (dec-to-bytes block-length)))))

(defn piece->requests [piece-index blocks block-size]
  (mapv #(request piece-index % block-size) (range blocks)))


(defn msg-to-type [msg] 
  (if (keyword? msg)
    msg
    (first msg)))

(defn take-n [in-chan n]
  (go-loop [arr [] iter 0]
    (if (= n iter) 
      (seq arr)
      (recur (conj arr (<! in-chan)) (inc iter)))))

(defn int-val [in-chan]
  (go (let [msg-bytes (<! (take-n in-chan 4))]
        (BigInteger. (byte-array (map byte msg-bytes))))))

(defn handshake [in-chan]
  (go (when-let [data (<! (take-n in-chan 68))]
        {:info-hash (take 20 (drop 28 data)) :peer-id (drop 48 data)})))

(def message-types {0 :choke
                    1 :unchoke
                    2 :interested
                    3 :uninterested})

(defn parse-message [in-chan]
  (println "In parse message")
  (go (when-let [length (<! (int-val in-chan))]
        (print "Read length " length)
        (if 
          (= length 0) :keepalive
          (when-let [msg-type (<! in-chan)]
            (println "Read message type " msg-type)
            (cond
              (and (< msg-type 4) (= length 1)) 
              (do (println "Got something like unchoke " msg-type)
                  (message-types msg-type))

              (and (= msg-type 4) (= length 5))
              (do (println "got a have " msg-type)
                  [:have (<! (int-val in-chan))])

              (and (= msg-type 5) (> length 1))
              (do (println "got a bitfield" msg-type)
                  [:bitfield (<! (take-n in-chan (dec length)))])

              (and (= msg-type 6) (= length 13))
              (when-let [index (<! (int-val in-chan))]
                (when-let [begin (<! (int-val in-chan))]
                  (when-let [length (<! (int-val in-chan))]
                    [:request index begin length])))

              (and (= msg-type 7) (> length 9))
              (when-let [index (<! (int-val in-chan))]
                (println "got a piece index!!!")
                (when-let [begin (<! (int-val in-chan))]
                  (println "got a piece begin!!!")
                  (println length)
                  (when-let [block (<! (take-n in-chan (- length 9)))]
                    (println "about to emit a piece!!!")
                    [:piece index begin block])))

              (and (= msg-type 8) (= length 13))
              (when-let [index (<! (int-val in-chan))]
                (when-let [begin (<! (int-val in-chan))]
                  (when-let [length (<! (int-val in-chan))]
                    [:cancel index begin length])))

              (and (= msg-type 9) (= length 3))
              (when-let [port-bytes (<! (take-n in-chan 2))]
                [:port (bytes-to-port port-bytes)])

              :else
              (do (println "elseee")
                  "None")))))))
