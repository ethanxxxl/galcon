;;;; Programmer: Ethan Smith

(declaim (optimize (debug 3)))

(ql:quickload :mcclim)
(defpackage :galcon
  (:use :clim :clim-lisp))

(in-package :galcon)

(define-application-frame app-frame nil
  ()
  (:panes
   (app :application :height 400
                     :scroll-bars t
                     :display-function #'display)
   (details :application :height 20
                         :width 1000
                         :display-time nil)
   (next-turn :push-button :height 20
                           :width 10
                           :label "next-turn")
   (int :interactor :height 75))
  (:layouts
   (default (vertically ()
              app
              (horizontally () details next-turn)
              int))))

(defgeneric display (frame pane))

(defstruct (planet
            (:print-object (lambda (object stream)
                             (format stream "#PLANET{~A}"
                                     (planet-name object)))))
  (x 0 :type integer)
  (y 0 :type integer)
  (name "" :type string)
  (level 1 :type integer)
  (credits 0 :type integer)
  (garrison)
  (orbit)
  (owner "" :type string))

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
  (speed 0 :type integer)
  (orders)
  (progress)
  (owner "" :type string))

(defun make-garrisoned-ship (power health speed owner)
  (make-ship
   :name (format nil "~A" (gensym "SHIP-"))
   :power power
   :health health
   :speed speed
   :orders nil
   :progress 0.0
   :owner owner))

(defstruct (orders
            (:print-object (lambda (object stream)
                             (with-accessors ((p1 orders-leave-planet)
                                              (t1 orders-leave-turn)
                                              (p2 orders-report-planet)
                                              (t2 orders-report-turn)
                                              (task orders-task)) object
                               (format stream "~A -> ~A, [~D,~D] // ~A"
                                       p1 p2 t1 t2 task)))))
  "These are orders that instruct a ship where to be, when to be there, and what
to do at that location.

if turn is in the past, or nil, the ship will move to the position at top speed.
otherwise, the ship will go at a speed which will place it at the location
precisely at the turn specified.

valid tasks are:
:standby   -- ship remains completely passive, not attacking even if provoked
:sentry    -- ship only attacks if it is attacked first
:attack    -- ship will attack any hostile ships found within range
:garrison  -- attempts to garrison on destination. the the planet is hostile,
              the planet is attacked."
  (leave-planet)
  (leave-turn)

  (report-planet)
  (report-turn)
  (task))

(defstruct player
  (name 'string)
  (color))

(defparameter *players* (list
                        (make-player :name "ethanxxxl" :color +dark-green+)
                        (make-player :name "bot 1" :color +dark-blue+)
                        (make-player :name "bot 2" :color +dark-cyan+)))

(defparameter *ships* (list))

(defparameter *planets*
  (list (make-planet :x 23 :y 23 :name "MERCURY" :owner "ethanxxxl")
        (make-planet :x 130 :y 159 :name "VENUS" :owner "ethanxxxl")
        (make-planet :x 384 :y 289 :name "EARTH" :owner "ethanxxxl")
        (make-planet :x 495 :y 84 :name "MARS" :owner "bot 1")
        (make-planet :x 53 :y 341 :name "JUPITER" :owner "bot 2")))

(defparameter *current-turn* 0)

(defparameter *connections* (list))

(defgeneric get-color (object))
(defmethod get-color ((object planet))
  (player-color
   (find (planet-owner object) *players* :test #'equal :key #'player-name)))
(defmethod get-color ((object ship))
  (player-color
   (find (ship-owner object) *players* :test #'equal :key #'player-name)))

(defun next-turn-update ()
  ;; planet stuff
  (dolist (p *planets*)
    (setf (planet-credits p) (+ (planet-credits p)
                                (* 10 (planet-level p)))))

  (incf *current-turn*))
(defun upgrade-planet (planet)
  "returns an upgraded copy of planet")

(defun ship-cost (ship)
  "returns the cost of the ship"
  (+ (* 10 (+ (ship-health ship)
              (ship-power ship)))
     (* 10 (ship-speed ship))))

(defun planet-produce-ship (planet ship)
  "returns a copy of planet with a new ship in garison, or "
  (when (>= (planet-credits planet) (ship-cost ship))
    (with-accessors ((x planet-x)
                     (y planet-y)
                     (name planet-name)
                     (owner planet-owner)
                     (credits planet-credits)
                     (garrison planet-garrison)) planet
      (make-planet
       :x x :y y :name name :owner owner
       :credits (- credits (ship-cost ship))
       :garrison (append garrison (list ship))))))

(defmethod display ((frame app-frame) pane)
  ;; draw planets
  (loop for p in *planets* do
    (with-output-as-presentation (pane p 'planet)
      (with-accessors ((x planet-x)
                       (y planet-y))
          p
        (draw-circle* pane x y 25 :ink (get-color p))
        (draw-text* pane (planet-name p) x y :align-x :center
                                             :align-y :center))))

  ;; draw ships
  (loop for s in *ships* do
    (with-accessors ((x ship-x)
                     (y ship-y)) s
      (draw-circle* pane x y 2 :ink (get-color s))))

  ;; draw connectors
  (loop for c in *connections*
        do
           (let* ((p1 (car c))
                  (p2 (cdr c)))

             (apply #'draw-arrow*
                    pane
                    (append (planet-arrow 25 p1 p2)
                            (list
                             :head-filled t
                             :line-thickness 2
                             :head-width 7))))))

(defun planet-pos (p1 p2)
  (with-accessors ((x1 planet-x)
                   (y1 planet-y)) p1
    (with-accessors ((x2 planet-x)
                     (y2 planet-y)) p2
      (list x1 y1 x2 y2))))

(defun normalize (x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (l (sqrt (+ (expt dx 2) (expt dy 2)))))
    (list (/ dx l)
          (/ dy l))))

(defun planet-arrow (r p1 p2)
  "returns a list of points which will go between the radius's of two planets"
  (let ((norm1 (apply #'normalize (planet-pos p1 p2)))
        (norm2 (apply #'normalize (planet-pos p2 p1))))
    (mapcar (lambda (n p) (truncate (+ p (* r n))))
            (append norm1 norm2)
            (planet-pos p1 p2))))

(define-presentation-type planet ())
(define-presentation-type ship ())
(define-presentation-type garrisoned-ship ())
(define-presentation-type orders ())
(define-presentation-to-command-translator describe-planet
    (planet planet-stats app-frame)
    (obj) (list obj))

(define-presentation-to-command-translator deploy-this-ship
    (ship assign-orders app-frame)
    (obj) (list obj))

(define-presentation-to-command-translator deploy-this-garrisoned-ship
    (garrisoned-ship deploy-ship app-frame)
    (obj) (list obj))

;; this command is for garrissoned ships
(define-app-frame-command (deploy-ship :name "deploy-ship")
    ((ship 'garrisoned-ship)
     (report-planet 'planet)
     (turn 'integer)
     (task 'string))
  (setf (ship-orders ship)
        (list (make-orders
               :leave-planet (find-if (lambda (p) (find ship (planet-garrison p)))
                                      *planets*)
               :leave-turn *current-turn*
               :report-planet report-planet
               :report-turn turn
               :task task))))

;; this command is only for ships that are currently deployed
(define-app-frame-command (assign-orders :name "assign-orders")
    ((ship 'ship) (planet 'planet) (turn '(integer *current-turn*)))
  ;; if ship orders are nil, then the ship is currently garrisoned.

  ;; update the orders on the ship
  (setf (ship-orders ship)
        (make-orders :leave-planet ))

  ;; if the ship already had orders, then the ship was garrisoned, remove it
  ;; from the planet garrison, and push it into the ship tracker.
  (unless (ship-orders ship)
    (let ((planet (find-if (lambda (p)
                             (find ship (planet-garrison p)) :test #'equal)
                           *planets*)))
      (setf (planet-garrison planet) (remove ship (planet-garrison planet)))
      (push ship *ships*))))

(define-app-frame-command (add-connection :name "add-connection" :menu t)
    ((p1 'planet) (p2 'planet))
  (setf *connections* (cons (cons p1 p2)
                            *connections*)))

(define-app-frame-command (foo :name "planet" :menu t)
    ((owner 'string) (n 'string) (x 'integer) (y 'integer))
  (setf *planets* (cons (make-planet :x x :y y :name n :owner owner)
                        *planets*)))

(define-app-frame-command (redisp :name "redisp" :menu t) ()
  (update-display-functions))

(defmethod activate-callback ((button push-button) (client app-frame) id)
  (next-turn-update))

(define-app-frame-command (planet-stats :name "planet-stats" :menu t)
    ((p 'planet))

  (let ((pane (app-pane 'details)))
    (with-new-output-record (pane)
      (window-clear pane)

      (format (app-pane 'details)
              "~&name: ~A ~&owner: ~A ~&level: ~A ~&credits: ~A ~&garrison: ~%"
              (planet-name p)
              (planet-owner p)
              (planet-level p)
              (planet-credits p))
      (dolist (ship (planet-garrison p))
        (with-output-as-presentation (pane ship 'garrisoned-ship)
          ;; todo, maybe format this as a table
          (format pane "~&  ~A" ship))))))

(define-app-frame-command (produce-ship :name "produce-ship" :menu t)
    ((p 'planet) (power 'integer) (health 'integer) (speed 'integer))
  (let ((new-planet (planet-produce-ship p (make-garrisoned-ship power
                                                                 health
                                                                 speed
                                                                 (planet-owner p)))))
    (if new-planet
        (setf *planets* (append (list new-planet)
                                (remove p *planets*)))
        ;; else make a notification saying that this operation cannot be completed
        )))

(defun app-pane (&optional (pane 'app) (frame *application-frame*))
  (find-pane-named frame pane))

(defun update-display-functions ()
  (setf (slot-value (app-pane 'app) 'clim-internals::display-function) #'display))

(defun run ()
  (bt:make-thread
   (lambda ()
     (let ((frame (make-application-frame 'app-frame)))
       (setf *application-frame* frame)
       (run-frame-top-level frame)))))
