#!/usr/bin/env bb
;; rasen 螺旋 — CIDv1 content-address parity test (against `ipfs add`).
;; Run: bb test
(ns rasen.methods.test-cid
  "Pins rasen.methods.cid byte-for-byte against the gold-standard `ipfs add --cid-version=1
  --raw-leaves` (CIDv1 / raw codec 0x55 / sha2-256 / multibase base32-'b'). The module's whole
  value is that a kotoba artifact's content-address is verifiable WITH OR WITHOUT a kubo daemon —
  so it MUST equal what ipfs would produce, byte-for-byte. The pinned literals below are the
  ACTUAL output of:
    printf '%s' INPUT | ipfs add -Q --cid-version=1 --raw-leaves --only-hash
  (verified against /opt/homebrew/bin/ipfs at authoring time). A drift in the multihash, the
  CIDv1 prefix bytes, the RFC4648 base32 alphabet, or the UTF-8 encoding fails this instantly.
  The module had no dedicated test (it was only exercised incidentally by test_ingest)."
  (:require [rasen.methods.cid :as cid]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(deftest cidv1-raw-matches-ipfs-add
  (is (= "bafkreibm6jg3ux5qumhcn2b3flc3tyu6dmlb4xa7u5bf44yegnrjhc4yeq" (cid/cidv1-raw "hello")))
  (is (= "bafkreihipbmlvwyvj6x4af6yt6lxz5zo7s6dpcx43rdfmba2qihkpsgpta" (cid/cidv1-raw "etzhayyim")))
  (is (= "bafkreidjlndn57rjr4ayvztlfim5lmcq7xsiompa57sz5ihpggjvlo3eva" (cid/cidv1-raw "rasen 螺旋 genome"))
      "UTF-8 multibyte content hashes the encoded bytes")
  (is (= "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku" (cid/cidv1-raw ""))
      "empty raw block")
  (is (= "bafkreidm3wdkuycanhkkgxq6hrv5ekmwafsahsyrhjliu7vw5tktpvuova"
         (cid/cidv1-raw "[:db/add \"e1\" \":x/v\" 42]"))
      "an EDN datom artifact"))

(deftest accepts-bytes-and-string-identically
  ;; cidv1-raw takes a String (UTF-8 encoded) or a byte-array — same content, same CID
  (is (= (cid/cidv1-raw "etzhayyim")
         (cid/cidv1-raw (.getBytes "etzhayyim" "UTF-8")))))

(deftest cid-has-the-cidv1-raw-sha256-shape
  (let [c (cid/cidv1-raw "hello")]
    (is (str/starts-with? c "bafkrei")
        "multibase 'b' + CIDv1(0x01) raw(0x55) sha2-256(0x12 0x20) always base32-encodes to 'bafkrei…'")
    (is (= 59 (count c)) "a 32-byte raw-leaf CIDv1 is exactly 59 base32 chars")))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (let [{:keys [fail error]} (run-tests 'rasen.methods.test-cid)]
       (System/exit (if (zero? (+ fail error)) 0 1)))))
