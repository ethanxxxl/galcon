(in-package :galcon)

(defstruct (ship
            (:print-object (lambda (object stream)
                             (with-accessors ((name ship-name)
                                              (power ship-power)
                                              (health ship-health)
                                              (speed ship-speed)
                                              (orders ship-orders)
                                              (progress ship-progress)
                                              (owner ship-owner)) object
                               (format stream "[~A] [p/h/s: ~D/~D/~D]"
                                       name power health speed)

                               (when orders
                                 (format stream "[~A] [~A%]"
                                         (if (listp orders)
                                             (first orders)
                                             orders)
                                         progress))))))
  "Describes a space ship that is used by the player.

* Ship Attributes

  There are four main attributes of concern to a player:

  - `power' :: This is the amount of damage that the ship can do every turn. the
    ship can focus its power on a single combatant, or its power may be
    dispbursed across multiple combatants. The power applied to a combatant is
    subtracted from that combatants health.

  - `health' :: When the ship is dealt damage (ex, from the power of another
    ship) that damage is subtracted from the ships health. The more health a
    ship has, the longer it may remain in the game. When the health of a ship
    reaches zero, the ship is removed from the game.

  - `speed' :: The rate at which the ship moves across the map in pixels/turn. A
    ship that has zero speed cannot move between planets, and is appropriate for
    a purely defensive unit.

  - `orders' :: In order for a ship to interact with other ships/planets, then
    it must have orders. Every turn, a ship will attempt to execute its orders.
    A ship that has no orders is completely inert. This parameter may be either
    a single orders structure or a list of orders structures. The ship will
    execute the orders sequentially one after another. If orders are created
    which the ship cannot follow (ie the ship is unable to get to the planet by
    the report turn) then the ship will attempt to minimize the error.

  Additional parameters which programmers may find helpful:

  - `name' :: this is the randomly assigned name of the ship.

  - `progress' :: This is only meaningful while the ship has orders.

  - `owner' :: a string denoting which player owns the ship in question. All
    deployed ships are kept in the `*ships*' global variable (ie all ships with
    orders). It is therefore necessary to distinguish which player each ship
    belongs to.

* Using a Ship

  A ship is built on a planet. Upon creation, the ship is garrisoned on the
  planet. A garrisoned ship is not visible to other players. Players may deploy
  ships and give them orders. Once a ship is deployed, it will move towards the
  planet listed on its orders. While in transit, a ship cannot interact with
  other ships. Neither can a ship recieve new orders once it has begun it's its
  trip."

  (name)
  (power 0 :type integer)
  (health 0 :type integer)
  (speed 0.0 :type float)
  (orders)
  (progress 0.0 :type float)
  (owner "" :type string))

(defun copy-ship (ship)
  "overwrites the default copy-ship function, so that orders is also created as a
new object."

  ;; create new ship object
  (let ((new-ship (copy-structure ship)))

    ;; create new orders object
    (setf (ship-orders new-ship)
          (copy-tree (ship-orders ship)))

    new-ship))

(defun make-garrisoned-ship (power health speed owner)
  (make-ship
   :name (format nil "~A" (gensym "SHIP-"))
   :power power
   :health health
   :speed speed
   :orders nil
   :progress 0.0
   :owner owner))

(defun ship-cost (ship)
  "returns the cost of the ship"
  (truncate
   (+ (* 10 (+ (ship-health ship)
               (ship-power ship)))
      (* 0.5 (ship-speed ship)))))

(defun ships-at-planet (planet-name &optional (ships *ships*))
  "returns a list of all the ships at the specified planet in ships list."
  (remove-if (lambda (s)
               (not
                (equalp
                 planet-name
                 (cond ((= 0 (ship-progress s))
                        ;; the ship is at its departure planet
                        (planet-name (orders-leave-planet (ship-active-orders s))))

                       ((>= (ship-progress s) 1.0)
                        ;; the ship is at its destination planet
                        (planet-name (orders-report-planet (ship-active-orders s))))

                       (t
                        ;; the ship is in transit, so it is not at a planet
                        nil)))))
             ships))

(defun ship-necessary-speed (ship)
  "returns the speed at which the ship should go to follow its orders."
  ;; speed is the units/turn that the ship will go.

  (cond ((and
          (> 1.0 (ship-progress ship))
          (>= *current-turn* (orders-report-turn (ship-active-orders ship))))
         ;; full speed if report turn is in the past and ship has not arrived
         ;; yet
         (ship-speed ship))

        ((>= *current-turn* (orders-report-turn (ship-active-orders ship)))
         ;; otherwise if report turn is in the past, speed should be zero
         0.0)

        ((< *current-turn* (orders-leave-turn (ship-active-orders ship)))
         ;; no movement if the ship doesn't leave until later
         0.0)

        (t
         ;; ship should be in transit at this point.
         (min (ship-speed ship)
              ;; \/ is the remaining distance divided by the remaining time.
              (/ (* (- 1.0 (ship-progress ship)) (orders-distance (ship-active-orders ship)))
                 (- (orders-report-turn (ship-active-orders ship)) *current-turn*))))))

(defun ship-update (ship)
  "returns an updated copy of ship. the ship paremeter is not modified."
  (let* ((new-ship (copy-ship ship))
         (d (orders-distance (ship-active-orders new-ship))) )
    (setf (ship-progress new-ship)
          (/ (+ (ship-necessary-speed new-ship)
                (* (ship-progress new-ship) d))
             d))
    new-ship))

(defun ship-active-orders (ship)
  (first (ship-orders ship)))
