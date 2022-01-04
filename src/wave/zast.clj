(ns wave.zast)

(comment
  ;; expr
  [:invoke {} '_ 'args*]
  [:access {} '_]
  [:symbol {} 'sym]
  [:raw 'text]
  [:string 'raw]
  [:anon-array {} 'items*]
  [:block {:label ""} 'statements+ '?retexpr]

  ;; statment
  [:assign {} 'sym 'val]

  #!
  )
