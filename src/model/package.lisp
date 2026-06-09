;;;; package.lisp -- the pure game model (no SDL, no I/O).

(defpackage #:civ-model
  (:use #:cl)
  (:nicknames #:civm)
  (:export
   ;; definitions (the data-driven rulebook)
   #:*terrain* #:*units* #:*buildings* #:*techs* #:*special-bonus*
   #:terrain-def #:unit-def #:building-def #:tech-def
   #:def-get
   ;; map / tiles
   #:tile #:make-tile #:tile-terrain #:tile-feature #:tile-resource
   #:tile-river #:tile-special
   #:tile-road #:tile-irrigation #:tile-mine #:tile-owner #:tile-city #:tile-units
   #:game-map #:map-width #:map-height #:map-tiles
   #:tile-at #:in-bounds-p #:neighbors #:do-tiles
   ;; entities
   #:player #:make-player #:player-id #:player-name #:player-kind
   #:player-gold #:player-government #:player-techs #:player-researching #:player-beakers
   #:player-color #:player-tax-rate #:player-science-rate #:player-luxury-rate
   #:unit #:unit-id #:unit-type #:unit-owner #:unit-x #:unit-y
   #:unit-hp #:unit-moves-left #:unit-orders #:unit-veteran
   #:city #:city-id #:city-name #:city-owner #:city-x #:city-y #:city-size
   #:city-food-box #:city-shield-box #:city-buildings #:city-production #:city-worked
   ;; state
   #:game-state #:gs-turn #:gs-year #:gs-map #:gs-players #:gs-units #:gs-cities
   #:make-new-game #:player-by-id #:unit-by-id #:city-by-id
   #:gs-random #:gs-rand #:gs-phase #:player-has-tech-p
   ;; rules
   #:tile-yield #:city-yields #:end-turn
   ;; ai
   #:run-ai-players #:ai-take-turn
   ;; commands
   #:apply-command #:command-error))
