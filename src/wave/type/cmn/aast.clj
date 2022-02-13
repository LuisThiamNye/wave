(ns wave.type.cmn.aast
  (:require
   [crypticbutter.snoop :refer [>defn]]))

(defprotocol AastNode
  :extend-via-metadata true
  )

(>defn node->lexical
  "Gets the lexical binding data represented by the node"
  [node]
  [[:map [:lexical-id [:tuple [:enum :locals :frees] some?]] [:ctx map?]] '=> map?]
  (get-in (:ctx node) (:lexical-id node)))
