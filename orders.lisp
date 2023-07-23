(in-package :galcon)

;; BUG by referring directly to planet objects, if a planet is updated, and that
;; object is obselete, then this reference to the object will also be obselete.
;; you should make these strings which are names.
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

(defun orders-distance (orders)
  (let ((p1 (orders-leave-planet orders))
        (p2 (orders-report-planet orders)))

    ;; a^2 + b^2 = c^2
    (sqrt (+ (expt (- (x p1) (x p2)) 2)
             (expt (- (y p1) (y p2)) 2)))))
