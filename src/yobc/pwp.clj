(ns yobc.pwp
  (:require [yobc.utils :as utils :refer :all]) )

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
    (if (> (mod piece-length block-size))
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

(defn pwp-message
  ([length id] (byte-array [length id])) 
  ([length id payload] (byte-array (map byte (concat [length id] payload)))))

(defn choke [] (pwp-message 1 0))

(defn unchoke [] (pwp-message 1 1))

(defn interested [] (pwp-message 1 2))

(defn uninterested [] (pwp-message 1 3))

(defn request [piece-index block-offset block-length]
  (pwp-message 13 6 (concat piece-index block-offset block-length)))

(def message-type { 0 :choke
                    1 :unchoke
                    2 :interested
                    3 :uninterested
                    4 :have
                    5 :bitfield
                    6 :request
                    7 :block
                    8 :cancel })

(defmulti respond :type)

(defmethod respond :choke [msg]
  (print (:payload msg)))

(defn process
  [msg-type msg]
    (do
      (println (message-type msg-type) msg)
      (byte-array 4)))
