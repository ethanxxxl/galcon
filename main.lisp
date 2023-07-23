;;;; Programmer: Ethan Smith

(declaim (optimize (debug 3)))

(ql:quickload :mcclim)
(defpackage :galcon
  (:use :clim :clim-lisp))

(load "./orders.lisp")
(load "./ship.lisp")

(in-package :galcon)

(define-application-frame app-frame nil
  ()
  (:panes
   (app :application :height 10000
                     :scroll-bars t
                     :display-function #'graphics-display)
   (details :application :height 10000
                         :width 30
                         :display-time nil)
   (status-bar :application :height 5
               :scroll-bars nil
               :display-time t
               :display-function #'status-display)
   (int :interactor :height 75))
  (:layouts
   (default (vertically ()
              (horizontally () app details)
              status-bar
              int))))

(defgeneric display (frame pane))

;;; Generic position accessors
(defgeneric x (obj))
(defgeneric y (obj))

;;;
;;; vec2 structure, methods, and functions
;;;

(defstruct (vec2
            (:constructor make-vec2-default)
            (:constructor make-vec2 (x y)))
  (x 0.0 :type float)
  (y 0.0 :type float))

(defmethod x ((obj vec2))
  (vec2-x obj))

(defmethod y ((obj vec2))
  (vec2-y obj))

(defun vec2-norm (vec)
  (make-vec2 (/ (x vec) (vec2-len vec))
             (/ (y vec) (vec2-len vec))))

(defun vec2-sub (&rest vecs)
  (make-vec2 (reduce #'- (mapcar #'x vecs))
             (reduce #'- (mapcar #'y vecs))))

(defun vec2-add (&rest vecs)
  (make-vec2 (reduce #'+ (mapcar #'x vecs))
             (reduce #'+ (mapcar #'y vecs))))

(defun vec2-mul (scalar vec)
  (make-vec2 (* (x vec) scalar)
             (* (y vec) scalar)))

(defun vec2-len (vec)
  (sqrt (+ (expt (x vec) 2)
           (expt (y ved) 2))))

;;;
;;; planet structure, methods, and functions
;;;

;; TODO change positions to floats, make display relative.
(defstruct (planet
            (:print-object (lambda (object stream)
                             (format stream "#PLANET{~A}"
                                     (planet-name object)))))
  (pos (make-vec2-default) :type vec2)
  (name "" :type string)
  (level 1 :type integer)
  (credits 0 :type integer)
  (garrison)
  (orbit)
  (owner "" :type string))

(defmethod x ((obj planet))
  (vec2-x (planet-pos obj)))

(defmethod y ((obj planet))
  (vec2-y (planet-pos obj)))


;;;
;;; list x and y accessors
;;;

(defmethod x ((obj list))
  (first obj))

(defmethod y ((obj list))
  (second obj))

;;;
;;; Player structure and accessors
;;;

(defstruct player
  (name 'string)
  (color))

;;;
;;; Game Parameters
;;;

(defparameter *players* (list
                        (make-player :name "ethanxxxl" :color +dark-green+)
                        (make-player :name "bot 1" :color +dark-red+)
                        (make-player :name "bot 2" :color +dark-cyan+)))

(defparameter *ships* (list))

(defparameter *planets*
  (list (make-planet :pos (make-vec2 23.0 23.0) :name "MERCURY" :owner "ethanxxxl")
        (make-planet :pos (make-vec2 130.0 159.0) :name "VENUS" :owner "ethanxxxl")
        (make-planet :pos (make-vec2 384.0 289.0) :name "EARTH" :owner "ethanxxxl")
        (make-planet :pos (make-vec2 495.0 84.0) :name "MARS" :owner "bot 1")
        (make-planet :pos (make-vec2 53.0 341.0) :name "JUPITER" :owner "bot 2")))

(defparameter *current-turn* 0)

(defparameter *connections* (list))

(defparameter *zoom* 1.0)
(defparameter *pan* (make-vec2 0.0 0.0))


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


  (setf *ships*
        (mapcar
         (lambda (s)
           (ship-update s))
         *ships*))

  (incf *current-turn*))

(defun graphics-space-translation (vec)
  "takes a vec2, and returns the zoomed/panned version of it."
  (vec2-add *pan*
            (vec2-mul *zoom* vec)))

(defun vec2-to-point (vec)
  "makes a CLIM point out of a vector"
  (make-point (truncate (x vec))
              (truncate (y vec))))

;; TODO draw deployed ships on route
;; TODO draw garrissoned ships as little circles around the planet
;; TODO when you change planets to floats, add the ability to pan and zoom
(defun graphics-display (frame pane)
  (declare (ignore frame))
  ;; draw planets
  (loop for p in *planets* do
    (let ((pos (vec2-to-point (graphics-space-translation (planet-pos p)))))
      (draw-circle pane pos 35 :filled nil :ink +gray+)

      (with-output-as-presentation (pane p 'planet)
        (draw-circle pane pos 25 :ink (get-color p)))


      (draw-text pane (planet-name p) pos :align-x :center
                                          :align-y :center)))

  ;; draw ships at planet
  (dolist (p *planets*)
    (let ((pos (graphics-space-translation (planet-pos p))))
      (loop for s in (ships-at-planet (planet-name p))
            for angle upto (* 2 pi) by (/ (* 2 pi) 20) do
              (with-output-as-presentation (pane s 'ship)
                (draw-circle pane
                             (vec2-to-point
                              (vec2-add pos
                                        (vec2-mul 35 (make-vec2 (cos angle)
                                                                (sin angle)))))
                             5
                             :ink (get-color s))))))

  ;; draw enroute ships
  (dolist (s *ships*)
    (let ((pos1 (graphics-space-translation (planet-pos (orders-leave-planet (ship-active-orders s)))))
          (pos2 (graphics-space-translation (planet-pos (orders-report-planet (ship-active-orders s))))))
      (when (< 0.0 (ship-progress s) 1.0)
        (draw-line pane
                   (vec2-to-point (first (offset-line pos1 pos2 35 35)))
                   (vec2-to-point (second (offset-line pos1 pos2 35 35)))
                   :ink +grey+
                   :line-dashes t)

        (let* ((d (vec2-len (vec2-sub pos1 pos2)))
               (vecs (offset-line pos1 pos2
                                  (- (* (ship-progress s) (- d 70)) 15 -35)
                                  (+ (* (- 1.0 (ship-progress s)) (- d 70)) 35)
                                  )))
          (draw-arrow pane
                      (vec2-to-point (first vecs))
                      (vec2-to-point (second vecs))
                      :ink +black+
                      :head-filled t
                      :head-width 10
                      :head-length 15
                      :line-thickness 2))))))

(defun details-display (frame pane)
  )

(defun status-display (frame pane)
  (formatting-table (pane)
    (formatting-row (pane)
      (formatting-cell (pane :align-y :center)
        (format pane "turn: ~A" *current-turn*))
      (formatting-cell (pane)
        (with-output-as-gadget (pane)
          (make-pane 'push-button
                     :activate-callback (lambda (b) (declare (ignore b))
                                          (next-turn-update)
                                          (redisplay-frame-panes frame :force-p t))
                     :client pane
                     :label "next-turn"))))))


(defun offset-line (v1 v2 r1 r2)
  "this draws a line between two imaginary circles at v1 and v2, with radius's
r1 and r2 respecively. the line connects to the radius of each circle.

the line is drawn from v1 to v2"

  (list (vec2-add v1 (vec2-mul (- r1) (vec2-norm (vec2-sub v1 v2))))
        (vec2-add v2 (vec2-mul (- r2) (vec2-norm (vec2-sub v2 v1))))))

(define-presentation-type planet ())
(define-presentation-type ship ())
(define-presentation-type orders ())
(define-presentation-to-command-translator describe-planet
    (planet planet-stats app-frame)
    (obj) (list obj))

(define-presentation-to-command-translator deploy-this-ship
    (ship assign-orders app-frame)
    (obj) (list obj))

(defun garrisoned-ship-planet (ship)
  "returns the planet that the ship is garrisoned on. If the ship is not garrisoned
on any planet, the function returns nil"
  (find-if (lambda (p) (find ship (planet-garrison p)))
           *planets*))

(define-app-frame-command (assign-orders :name "assign-orders")
    ((ship 'ship)
     (report-planet 'planet)
     (turn 'integer)
     (task 'string))

  (setf (ship-orders ship)
        (append (ship-orders ship)
                (list (make-orders
                       ;; BUG: if a ship is on route to planet, adding orders
                       ;; will still work.
                       :leave-planet (or (garrisoned-ship-planet ship)
                                         (orders-report-planet
                                          (first (ship-orders ship))))
                       :leave-turn *current-turn*
                       :report-planet report-planet
                       :report-turn turn
                       :task task))))

  ;; if the ship is garrisoned, move it out of garrison since it has recieved
  ;; orders.
  (let ((home-planet (garrisoned-ship-planet ship)))
    (when home-planet
      (setf (planet-garrison home-planet)
            (remove ship (planet-garrison home-planet)))
      (push ship *ships*))))

(define-app-frame-command (add-connection :name "add-connection" :menu t)
    ((p1 'planet) (p2 'planet))
  (setf *connections* (cons (cons p1 p2)
                            *connections*)))

(define-app-frame-command (foo :name "planet" :menu t)
    ((owner 'string) (n 'string) (x 'integer) (y 'integer))
  (setf *planets* (cons (make-planet :pos (make-vec2 x y) :name n :owner owner)
                        *planets*)))

(define-app-frame-command (redisp :name "redisp" :menu t) ()
  (update-display-functions))

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
        (with-output-as-presentation (pane ship 'ship)
          ;; todo, maybe format this as a table
          (format pane "~&  ~A" ship))))))

(define-app-frame-command (produce-ship :name "produce-ship" :menu t)
    ((p 'planet) (power 'integer) (health 'integer) (speed 'float))

  (let* ((new-ship (make-garrisoned-ship power
                                         health
                                         speed
                                         (planet-owner p)))
         (remaining-funds (- (planet-credits p) (ship-cost new-ship))))
    (when (>= remaining-funds 0)
      (setf (planet-credits p) remaining-funds)
      (push new-ship (planet-garrison p)))

    ;; else make a notification saying that this operation cannot be completed
    ))

(defun app-pane (&optional (pane 'app) (frame *application-frame*))
  (find-pane-named frame pane))

(defun update-display-functions ()
  (setf (slot-value (app-pane 'app) 'clim-internals::display-function)
        #'graphics-display)
  (setf (slot-value (app-pane 'status-bar) 'clim-internals::display-function)
        #'status-display))

(defun run ()
  (bt:make-thread
   (lambda ()
     (let ((frame (make-application-frame 'app-frame)))
       (setf *application-frame* frame)
       (run-frame-top-level frame)))))

(defun get-planet (name)
  (find-if (lambda (p) (equalp name (planet-name p))) *planets*))

(defun print-garrisons ()
  (dolist (p *planets*)
    (format t "~&~A: ~S" (planet-name p) (planet-garrison p))))

(defun reset-ships ()
  (dolist (s *ships*)
    (setf (ship-progress s) 0.0)))
