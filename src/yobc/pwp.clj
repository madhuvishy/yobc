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

(defn parse-message [in-chan]
  (go (when-let [length (<! (int-val in-chan))]
        (if 
          (= length 0) :keepalive
          (when-let [msg-type (<! in-chan)]
            (cond
              (and (< msg-type 4) (= length 1)) 
                (message-type msg-type)

              (and (= msg-type 4) (= length 5))
                [:have (<! (int-val in-chan))]

              (and (= msg-type 5) (> length 1))
                [:bitfield (<! (take-n in-chan (dec length)))]

              (and (= msg-type 6) (= length 13))
                (when-let [index (<! (int-val in-chan))]
                  (when-let [begin (<! (int-val in-chan))]
                    (when-let [length (<! (int-val in-chan))]
                      [:request index begin length])))

              (and (= msg-type 7) (> length 9))
                (when-let [index (<! (int-val in-chan))]
                  (when-let [begin (<! (int-val in-chan))]
                    (when-let [block (<! (take-n in-chan (- length 9)))])))

              (and (= msg-type 8) (= length 13))
                (when-let [index (<! (int-val in-chan))]
                  (when-let [begin (<! (int-val in-chan))]
                    (when-let [length (<! (int-val in-chan))]
                      [:cancel index begin length])))

              (and (= msg-type 9) (= length 3))
                (when-let [port-bytes (<! (take-n in-chan 2))]
                  [:port (bytes-to-port port-bytes)])
              
              :else
                nil))))))
