(ns rasen.methods.cid
  "rasen 螺旋 — kotoba IPFS content-address (CIDv1, raw, sha2-256, base32).

  Delegates to the shared **com-junkawasaki/multiformats-clj** library (`.cljc`,
  byte-identical to `ipfs add --cid-version=1 --raw-leaves` for a single raw block
  < 256 KiB; verified vs `ipfs` 0.41.0). Single-block by design — a bounded public
  slice (G5/G7) fits one raw block; > 256 KiB chunks into a UnixFS dag-pb tree.
  The public surface (base32 / cidv1-raw / single-block-limit) is unchanged."
  (:require [multiformats.core :as mf]))

#?(:clj
   (defn- ->ba ^bytes [data]
     (cond (string? data) (.getBytes ^String data "UTF-8")
           (bytes? data)  data
           :else (byte-array (map #(unchecked-byte (bit-and (int %) 0xff)) data)))))

(def base32 mf/base32)   ;; RFC4648 base32-lower, no padding (seq of bytes/ints → 'b' body)

(defn cidv1-raw
  "CIDv1 / raw (0x55) / sha2-256. Accepts a byte-array, a String (UTF-8), or a
  byte/int seq. Returns the multibase-'b' base32 CID."
  [data]
  #?(:clj (mf/cidv1-raw (->ba data))
     :cljs (throw (js/Error. "cidv1-raw not available in cljs runtime"))))

;; ipfs default chunk size; above this the raw CID no longer applies
(def single-block-limit (* 256 1024))
