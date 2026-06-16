;;;; package.lisp -- the pure game model (no SDL, no I/O).

(defpackage #:civ-model
  (:use #:cl)
  (:nicknames #:civm)
  (:export
   ;; definitions (the data-driven rulebook)
   #:*terrain* #:*units* #:*buildings* #:*techs* #:*special-bonus* #:*wonders*
   #:*terraform* #:*governments*
   #:terrain-def #:unit-def #:building-def #:tech-def #:wonder-def #:terraform-def
   #:government-def #:def-get
   ;; map / tiles
   #:tile #:make-tile #:tile-terrain #:tile-feature #:tile-resource
   #:tile-river #:tile-special
   #:tile-road #:tile-railroad #:tile-irrigation #:tile-mine #:tile-fort
   #:tile-pollution #:tile-hut #:tile-airbase #:tile-owner #:tile-city #:tile-units
   #:game-map #:map-width #:map-height #:map-tiles
   #:tile-at #:in-bounds-p #:neighbors #:do-tiles #:wrap-x #:map-dx #:signed-dx
   ;; entities
   #:player #:make-player #:player-id #:player-name #:player-kind
   #:player-gold #:player-government #:player-techs #:player-researching #:player-beakers
   #:player-color #:player-tax-rate #:player-science-rate #:player-luxury-rate
   #:player-gov-target #:player-anarchy-left #:player-spaceship #:player-landing
   #:player-score #:player-peace-turns #:compute-score #:score-breakdown
   #:player-personality #:player-city-names #:next-city-name
   #:gs-difficulty #:difficulty-level #:*difficulties* #:*nation-city-names*
   #:*nation-techs* #:nation-starting-techs
   #:unit #:unit-id #:unit-type #:unit-owner #:unit-x #:unit-y
   #:unit-hp #:unit-moves-left #:unit-orders #:unit-veteran #:unit-fuel
   #:unit-goto-x #:unit-goto-y #:unit-work #:unit-work-left
   #:city #:city-id #:city-name #:city-owner #:city-x #:city-y #:city-size
   #:city-food-box #:city-shield-box #:city-buildings #:city-production #:city-worked
   #:city-specialists #:city-worker-count #:city-manual-tiles #:city-tile-locks #:city-gov
   ;; state
   #:game-state #:gs-turn #:gs-year #:gs-map #:gs-players #:gs-units #:gs-cities
   #:make-new-game #:player-by-id #:unit-by-id #:city-by-id
   #:gs-random #:gs-rand #:gs-phase #:gs-warming #:player-has-tech-p
   #:gs-winner #:gs-victory #:gs-message #:gs-log #:gs-note #:process-victory #:player-alive-p
   #:enter-hut #:barbarian-player
   #:*spaceship-parts* #:*spaceship-flight* #:*spaceship-part-cost*
   ;; diplomacy
   #:gs-relations #:relation #:at-war-p #:allied-p #:truce-active-p #:senate-p
   #:barbarian-id-p
   #:gs-offers #:human-id #:gs-history #:gs-foundings
   #:best-trade-with #:*tech-trade-value*
   ;; espionage
   #:gs-embassies #:has-embassy-p #:adjacent-enemy-city #:adjacent-enemy-unit
   #:city-report #:incite-cost
   ;; trade routes / caravans
   #:gs-routes #:route-exists-p #:city-route-count #:adjacent-city-if
   ;; fog of war
   #:player-seen #:update-visibility #:visible-set #:seen-p
   ;; rules
   #:unit-obsolete-p #:upgrade-cost
   #:tile-yield #:city-yields #:end-turn #:resolve-combat #:heal-units
   #:enemy-adjacent-p #:city-defended-p #:wonder-built-p #:city-upkeep #:can-found-here-p
   #:city-happiness #:city-disorder-p #:city-celebrating-p #:count-city-military
   #:research-cost #:researchable-techs #:city-population #:civ-population
   #:civ-research-rate #:civ-gold-rate #:research-eta
   ;; persistence
   #:save-game #:load-game #:dump-game #:load-game-form
   ;; pathfinding / goto
   #:find-path #:process-goto
   ;; ai
   #:run-ai-players #:ai-take-turn
   ;; commands
   #:apply-command #:command-error))
