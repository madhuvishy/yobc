(ns yobc.bencoder)

(declare encode-values)

(defn encode-string
  [string]
  (str (count string) ":" string))

(defn encode-number
  [number]
  (str "i" number "e"))

(defn encode-dict
  [dict]
  (str "d" (encode-values (interleave (keys dict) (vals dict))) "e"))

(defn encode-list
  [list]
  (str "d" (encode-values list) "e"))

(defn encode-values
  [values]
  (clojure.string/join 
    (map (fn [value] 
           (cond
             (number? value) (encode-number value)
             (string? value) (encode-string value)
             (seq? value) (encode-list value)
             (map? value) (encode-dict value))) values)))

(defn bencode
  [dict]
  (encode-dict dict))
