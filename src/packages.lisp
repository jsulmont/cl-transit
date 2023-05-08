(defpackage :cl-transit
  (:use :cl)
  (:import-from :bit-smasher :int->octets)
  (:import-from :parse-float :parse-float)
  (:import-from :serapeum :dict)
  (:nicknames #:clt)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:alex #:alexandria))
  (:export
   -INF INF NAN MSGPACK JSON
   decode-json
   decode-mp
   encode-json
   encode-mp
   *encode-target*
   encode-target
   *encode-alist-as-map*
   *encode-plist-as-map*
   tagged-value
   tr-timestamp
   tr-set
   tr-link))
