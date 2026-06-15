;;;; defs.lisp -- the rulebook as DATA, not code.
;;;;
;;;; Terrain, units, buildings and techs are plain tables keyed by keyword.
;;;; Balancing or modding the game = editing these literals.  Each entry is a
;;;; plist; look values up with DEF-GET.  This deliberately mirrors the
;;;; data-driven *sprite-sheets* table in the sibling civ-extract project.

(in-package #:civ-model)

(defun %table (entries)
  "Build a keyword->plist hash-table from ((:key . plist) ...)."
  (let ((h (make-hash-table :test 'eq)))
    (dolist (e entries h)
      (setf (gethash (car e) h) (cdr e)))))

(defparameter *terrain*
  (%table
   '((:ocean     :food 1 :shields 0 :trade 2 :move 1 :defense 0)
     (:grassland :food 2 :shields 0 :trade 0 :move 1 :defense 0)
     (:plains    :food 1 :shields 1 :trade 0 :move 1 :defense 0)
     (:forest    :food 1 :shields 2 :trade 0 :move 2 :defense 25)
     (:hills     :food 1 :shields 0 :trade 0 :move 2 :defense 50)
     (:mountains :food 0 :shields 1 :trade 0 :move 3 :defense 100)
     (:desert    :food 0 :shields 1 :trade 0 :move 1 :defense 0)
     (:tundra    :food 1 :shields 0 :trade 0 :move 1 :defense 0)
     (:arctic    :food 0 :shields 0 :trade 0 :move 2 :defense 0)
     (:swamp     :food 1 :shields 0 :trade 0 :move 2 :defense 0)
     (:jungle    :food 1 :shields 0 :trade 0 :move 2 :defense 0)))
  "Terrain types -> base yields, move cost, % defense bonus.")

(defparameter *units*
  (%table
   '((:warriors    :attack 1 :defense 1 :move 1 :cost 10 :requires nil :domain :land :obsolete-by :gunpowder :upgrade-to :musketeers)
     (:cavalry     :attack 2 :defense 1 :move 2 :cost 20 :requires :horseback-riding :domain :land :obsolete-by :automobile :upgrade-to :armor)
     (:legion      :attack 3 :defense 1 :move 1 :cost 20 :requires :iron-working :domain :land :obsolete-by :conscription :upgrade-to :riflemen)
     (:phalanx     :attack 1 :defense 2 :move 1 :cost 20 :requires :bronze-working :domain :land :obsolete-by :gunpowder :upgrade-to :musketeers)
     (:diplomat    :attack 0 :defense 0 :move 2 :cost 30 :requires :writing :domain :land :abilities (:espionage))
     (:musketeers  :attack 2 :defense 3 :move 1 :cost 30 :requires :gunpowder :domain :land :obsolete-by :conscription :upgrade-to :riflemen)
     (:riflemen    :attack 3 :defense 5 :move 1 :cost 30 :requires :conscription :domain :land)
     (:cannon      :attack 8 :defense 1 :move 1 :cost 40 :requires :metallurgy :domain :land :obsolete-by :robotics :upgrade-to :artillery)
     (:catapult    :attack 6 :defense 1 :move 1 :cost 40 :requires :mathematics :domain :land :obsolete-by :metallurgy :upgrade-to :cannon)
     (:chariot     :attack 4 :defense 1 :move 2 :cost 40 :requires :the-wheel :domain :land :obsolete-by :chivalry :upgrade-to :knights)
     (:frigate     :attack 2 :defense 2 :move 3 :cost 40 :requires :magnetism :domain :sea :capacity 4 :carries :land :obsolete-by :combustion :upgrade-to :cruiser)
     (:knights     :attack 4 :defense 2 :move 2 :cost 40 :requires :chivalry :domain :land :obsolete-by :automobile :upgrade-to :armor)
     (:sail        :attack 1 :defense 1 :move 3 :cost 40 :requires :navigation :domain :sea :capacity 3 :carries :land :obsolete-by :magnetism :upgrade-to :frigate)
     (:settlers    :attack 0 :defense 1 :move 1 :cost 40 :requires nil :domain :land :abilities (:found-city :terraform))
     (:trireme     :attack 1 :defense 0 :move 3 :cost 40 :requires :map-making :domain :sea :capacity 2 :carries :land :obsolete-by :magnetism :upgrade-to :frigate)
     (:caravan     :attack 0 :defense 1 :move 1 :cost 50 :requires :trade :domain :land :abilities (:caravan))
     (:mech-inf    :attack 6 :defense 6 :move 3 :cost 50 :requires :labor-union :domain :land)
     (:submarine   :attack 8 :defense 2 :move 3 :cost 50 :requires :mass-production :domain :sea)
     (:transport   :attack 0 :defense 3 :move 4 :cost 50 :requires :industrialization :domain :sea :capacity 8 :carries :land)
     (:artillery   :attack 12 :defense 2 :move 2 :cost 60 :requires :robotics :domain :land)
     (:fighter     :attack 4 :defense 2 :move 10 :cost 60 :requires :flight :domain :air :range 2)
     (:ironclad    :attack 4 :defense 4 :move 4 :cost 60 :requires :steam-engine :domain :sea :obsolete-by :combustion :upgrade-to :cruiser)
     (:armor       :attack 10 :defense 5 :move 3 :cost 80 :requires :automobile :domain :land)
     (:cruiser     :attack 6 :defense 6 :move 6 :cost 80 :requires :combustion :domain :sea)
     (:bomber      :attack 12 :defense 1 :move 8 :cost 120 :requires :advanced-flight :domain :air :range 5)
     (:battleship  :attack 18 :defense 12 :move 4 :cost 160 :requires :steel :domain :sea)
     (:carrier     :attack 1 :defense 12 :move 5 :cost 160 :requires :advanced-flight :domain :sea :capacity 8 :carries :air)
     (:nuclear     :attack 99 :defense 0 :move 16 :cost 160 :requires :rocketry :domain :air :range 8 :abilities (:nuke))))
  "Unit types -> combat stats, build cost, tech requirement, domain.
:domain is :land (off ocean), :sea (only ocean) or :air (unrestricted).")

(defparameter *buildings*
  (%table
   '((:barracks         :cost 40 :upkeep 0 :requires nil :effect "builds veterans")
     (:temple           :cost 40 :upkeep 1 :requires :ceremonial-burial)
     (:granary          :cost 60 :upkeep 1 :requires :pottery :effect "keeps food on growth")
     (:courthouse       :cost 80 :upkeep 1 :requires :code-of-laws)
     (:library          :cost 80 :upkeep 1 :requires :writing :effect "boosts science")
     (:marketplace      :cost 80 :upkeep 1 :requires :currency)
     (:colosseum        :cost 100 :upkeep 4 :requires :construction)
     (:aqueduct         :cost 120 :upkeep 2 :requires :construction)
     (:bank             :cost 120 :upkeep 3 :requires :banking)
     (:walls            :cost 120 :upkeep 2 :requires :masonry :effect "stronger defense")
     (:cathedral        :cost 160 :upkeep 3 :requires :religion)
     (:mass-transit     :cost 160 :upkeep 4 :requires :mass-production)
     (:nuclear-plant    :cost 160 :upkeep 2 :requires :nuclear-power)
     (:power-plant      :cost 160 :upkeep 4 :requires :refining)
     (:university       :cost 160 :upkeep 3 :requires nil)
     (:factory          :cost 200 :upkeep 4 :requires :industrialization)
     (:palace           :cost 200 :upkeep 5 :requires :masonry)
     (:recycling-center :cost 200 :upkeep 2 :requires :recycling)
     (:sdi-defense      :cost 200 :upkeep 4 :requires :super-conductor)
     (:hydro-plant      :cost 240 :upkeep 4 :requires :electronics)
     (:mfg-plant        :cost 320 :upkeep 6 :requires :robotics)))
  "City improvements -> cost, gold upkeep, tech requirement, effect.")

(defparameter *wonders*
  (%table
   '((:colossus               :cost 200 :requires :bronze-working :effect "boosts trade")
     (:lighthouse             :cost 200 :requires :map-making)
     (:copernicus-observatory :cost 300 :requires :astronomy)
     (:darwins-voyage         :cost 300 :requires :rail-road)
     (:great-library          :cost 300 :requires :literacy :effect "boosts science")
     (:great-wall             :cost 300 :requires :masonry :effect "stronger city defense")
     (:hanging-gardens        :cost 300 :requires :pottery :effect "extra food")
     (:michelangelos-chapel   :cost 300 :requires :religion)
     (:oracle                 :cost 300 :requires :mysticism)
     (:pyramids               :cost 300 :requires :masonry :effect "boosts production")
     (:isaac-newtons-college  :cost 400 :requires :theory-of-gravity)
     (:j-s-bachs-cathedral    :cost 400 :requires :religion)
     (:magellans-expedition   :cost 400 :requires :navigation)
     (:shakespeares-theatre   :cost 400 :requires :medicine)
     (:apollo-program         :cost 600 :requires :space-flight)
     (:cure-for-cancer        :cost 600 :requires :genetic-engineering)
     (:hoover-dam             :cost 600 :requires :electronics)
     (:manhattan-project      :cost 600 :requires :nuclear-fission)
     (:s-e-t-i-program        :cost 600 :requires :computers)
     (:united-nations         :cost 600 :requires :communism)
     (:womens-suffrage        :cost 600 :requires :industrialization)))
  "World wonders (one per game) -> cost, tech requirement, effect.")

(defparameter *techs*
  (%table
   '((:advanced-flight    :prereqs (:flight :electricity)            :name "Advanced Flight")
     (:alphabet           :prereqs ()                                :name "Alphabet")
     (:astronomy          :prereqs (:mysticism :mathematics)         :name "Astronomy")
     (:atomic-theory      :prereqs (:theory-of-gravity :physics)     :name "Atomic Theory")
     (:automobile         :prereqs (:combustion :steel)              :name "Automobile")
     (:banking            :prereqs (:trade :the-republic)            :name "Banking")
     (:bridge-building    :prereqs (:iron-working :construction)     :name "Bridge Building")
     (:bronze-working     :prereqs ()                                :name "Bronze Working")
     (:ceremonial-burial  :prereqs ()                                :name "Ceremonial Burial")
     (:chemistry          :prereqs (:university :medicine)           :name "Chemistry")
     (:chivalry           :prereqs (:feudalism :horseback-riding)    :name "Chivalry")
     (:code-of-laws       :prereqs (:alphabet)                       :name "Code of Laws")
     (:combustion         :prereqs (:refining :explosives)           :name "Combustion")
     (:communism          :prereqs (:philosophy :industrialization)  :name "Communism")
     (:computers          :prereqs (:mathematics :electronics)       :name "Computers")
     (:conscription       :prereqs (:the-republic :explosives)       :name "Conscription")
     (:construction       :prereqs (:masonry :currency)              :name "Construction")
     (:currency           :prereqs (:bronze-working)                 :name "Currency")
     (:democracy          :prereqs (:philosophy :literacy)           :name "Democracy")
     (:electricity        :prereqs (:magnetism :metallurgy)          :name "Electricity")
     (:electronics        :prereqs (:electricity)                    :name "Electronics")
     (:engineering        :prereqs (:the-wheel :construction)        :name "Engineering")
     (:explosives         :prereqs (:gunpowder :chemistry)           :name "Explosives")
     (:feudalism          :prereqs (:masonry :monarchy)              :name "Feudalism")
     (:flight             :prereqs (:combustion :physics)            :name "Flight")
     (:fusion-power       :prereqs (:nuclear-power :super-conductor) :name "Fusion Power")
     (:genetic-engineering :prereqs (:medicine :the-corporation)     :name "Genetic Engineering")
     (:gunpowder          :prereqs (:invention :iron-working)        :name "Gunpowder")
     (:horseback-riding   :prereqs ()                                :name "Horseback Riding")
     (:industrialization  :prereqs (:rail-road :banking)            :name "Industrialization")
     (:invention          :prereqs (:engineering :literacy)          :name "Invention")
     (:iron-working       :prereqs (:bronze-working)                 :name "Iron Working")
     (:labor-union        :prereqs (:mass-production :communism)     :name "Labor Union")
     (:literacy           :prereqs (:writing :code-of-laws)          :name "Literacy")
     (:magnetism          :prereqs (:navigation :physics)            :name "Magnetism")
     (:map-making         :prereqs (:alphabet)                       :name "Map Making")
     (:masonry            :prereqs ()                                :name "Masonry")
     (:mass-production    :prereqs (:automobile :the-corporation)    :name "Mass Production")
     (:mathematics        :prereqs (:alphabet :masonry)              :name "Mathematics")
     (:medicine           :prereqs (:philosophy :trade)              :name "Medicine")
     (:metallurgy         :prereqs (:gunpowder :university)          :name "Metallurgy")
     (:monarchy           :prereqs (:ceremonial-burial :code-of-laws) :name "Monarchy")
     (:mysticism          :prereqs (:ceremonial-burial)              :name "Mysticism")
     (:navigation         :prereqs (:map-making :astronomy)          :name "Navigation")
     (:nuclear-fission    :prereqs (:mass-production :atomic-theory) :name "Nuclear Fission")
     (:nuclear-power      :prereqs (:nuclear-fission :electronics)   :name "Nuclear Power")
     (:philosophy         :prereqs (:mysticism :literacy)            :name "Philosophy")
     (:physics            :prereqs (:mathematics :navigation)        :name "Physics")
     (:plastics           :prereqs (:refining :space-flight)         :name "Plastics")
     (:pottery            :prereqs ()                                :name "Pottery")
     (:rail-road          :prereqs (:steam-engine :bridge-building)  :name "Railroad")
     (:recycling          :prereqs (:mass-production :democracy)     :name "Recycling")
     (:refining           :prereqs (:chemistry :the-corporation)     :name "Refining")
     (:religion           :prereqs (:philosophy :writing)            :name "Religion")
     (:robotics           :prereqs (:plastics :computers)            :name "Robotics")
     (:rocketry           :prereqs (:advanced-flight :electronics)   :name "Rocketry")
     (:space-flight       :prereqs (:computers :rocketry)            :name "Space Flight")
     (:steam-engine       :prereqs (:physics :invention)            :name "Steam Engine")
     (:steel              :prereqs (:metallurgy :industrialization)  :name "Steel")
     (:super-conductor    :prereqs (:plastics :mass-production)      :name "Superconductor")
     (:the-corporation    :prereqs (:banking :industrialization)     :name "The Corporation")
     (:the-republic       :prereqs (:code-of-laws :literacy)         :name "The Republic")
     (:the-wheel          :prereqs ()                                :name "The Wheel")
     (:theory-of-gravity  :prereqs (:astronomy :university)          :name "Theory of Gravity")
     (:trade              :prereqs (:currency :code-of-laws)         :name "Trade")
     (:university         :prereqs (:mathematics :philosophy)        :name "University")
     (:writing            :prereqs (:alphabet)                       :name "Writing")))
  "The Civilization (1991) advance tree: each advance with its prerequisites
and display name.  Sourced from the CC0 CivOne advance data.")

(defparameter *special-bonus*
  ;; terrain -> (food shields trade) added when the tile has its special resource
  '((:grassland 0 1 0)   ; shield
    (:plains    0 2 0)   ; horses
    (:forest    2 0 0)   ; game
    (:hills     0 2 0)   ; coal
    (:mountains 0 0 6)   ; gold
    (:desert    3 0 0)   ; oasis
    (:ocean     2 0 0)   ; fish
    (:tundra    2 0 0)   ; game
    (:arctic    2 1 0)   ; seals
    (:swamp     0 4 0)   ; oil
    (:jungle    0 0 4))  ; gems
  "Yield bonus per terrain's special resource.")

(defparameter *governments*
  (%table
   '((:anarchy   :name "Anarchy"   :requires nil          :max-rate 60 :martial-law 0
                 :tile-penalty t   :trade-bonus nil :corruption 60 :science nil)
     (:despotism :name "Despotism" :requires nil          :max-rate 60 :martial-law 3
                 :tile-penalty t   :trade-bonus nil :corruption 40 :science t)
     (:monarchy  :name "Monarchy"  :requires :monarchy    :max-rate 70 :martial-law 3
                 :tile-penalty nil :trade-bonus nil :corruption 25 :science t)
     (:communism :name "Communism" :requires :communism   :max-rate 80 :martial-law 3
                 :tile-penalty nil :trade-bonus nil :corruption 15 :science t)
     (:republic  :name "Republic"  :requires :the-republic :max-rate 80 :martial-law 0
                 :tile-penalty nil :trade-bonus t   :corruption 25 :science t)
     (:democracy :name "Democracy" :requires :democracy   :max-rate 90 :martial-law 0
                 :tile-penalty nil :trade-bonus t   :corruption 0  :science t)))
  "Governments -> the advance that unlocks them and their effects:
MAX-RATE   cap on any single tax/luxury/science rate;
MARTIAL-LAW how many military units in a city quiet an unhappy citizen each;
TILE-PENALTY  the despotic -1 to any tile yielding 3+ of a category;
TRADE-BONUS   +1 trade on every tile already producing trade (republic/democracy);
CORRUPTION    percent of a city's trade lost; SCIENCE  whether research happens.")

(defparameter *terraform*
  (%table
   '((:build-road :flag :road       :verb "road"       :turns 2
                  :terrains (:grassland :plains :forest :hills :mountains :desert
                             :tundra :arctic :swamp :jungle))
     (:build-railroad :flag :railroad :verb "railroad" :turns 3
                  :requires :rail-road :needs :road
                  :terrains (:grassland :plains :forest :hills :mountains :desert
                             :tundra :arctic :swamp :jungle))
     (:irrigate   :flag :irrigation :verb "irrigation" :turns 4
                  :terrains (:grassland :plains :desert :hills))
     (:mine       :flag :mine       :verb "mine"        :turns 4
                  :terrains (:hills :mountains :desert))
     (:build-fort :flag :fort       :verb "fort"        :turns 3 :requires :construction
                  :terrains (:grassland :plains :forest :hills :mountains :desert
                             :tundra :arctic :swamp :jungle))
     (:build-airbase :flag :airbase :verb "airbase"     :turns 4 :requires :flight
                  :terrains (:grassland :plains :forest :hills :mountains :desert
                             :tundra :arctic :swamp :jungle))
     ;; clearing reveals the land under vegetation/wetland (terrain-dependent)
     (:clear-forest :flag nil       :verb "clearing"    :turns 3
                  :becomes ((:forest . :plains) (:jungle . :grassland) (:swamp . :grassland))
                  :terrains (:forest :jungle :swamp))))
  "Settler terraform jobs -> the tile flag they set, how many turns they take,
the terrains that allow them, and any tech (:requires) or prerequisite
improvement (:needs) they depend on.")

(declaim (inline def-get))
(defun def-get (table key prop &optional default)
  "Look up PROP for KEY in a definition TABLE."
  (getf (gethash key table) prop default))

;; convenience wrappers
(defun terrain-def   (key prop &optional d) (def-get *terrain* key prop d))
(defun unit-def      (key prop &optional d) (def-get *units* key prop d))
(defun building-def  (key prop &optional d) (def-get *buildings* key prop d))
(defun wonder-def    (key prop &optional d) (def-get *wonders* key prop d))
(defun tech-def      (key prop &optional d) (def-get *techs* key prop d))
(defun terraform-def (key prop &optional d) (def-get *terraform* key prop d))
(defun government-def (key prop &optional d) (def-get *governments* key prop d))
