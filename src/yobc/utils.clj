(ns yobc.utils)

(defn sha-1 
  "Computes SHA-1 hash of a given string"
  [string]
  (apply str
    (map (partial format "%02x")
      (.digest (doto (java.security.MessageDigest/getInstance "SHA-1")
                     .reset
                     (.update (.getBytes string)))))))
