;;;; Movement procedures

(define (check-position return)
  (define (check-item-position items)
    (when (pair? items)
          (let ((item (car items))) ; You bumped into an item
            (when (and (= (item-row item) (robot-row robot))
                       (= (item-col item) (robot-col robot)))
                  ;; Clip the string if it's too long.
                  (if (>= (string-length (item-message item)) (- (COLS) 3))
                      (begin
                        (mvprintw 1 2 (string-take (item-message item) (- (COLS) 5)))
                        (addch (ACS_RARROW))
                        (addch (ACS_RARROW)))
                      (mvprintw 1 2 (item-message item))) ; Print item's message
                  (robot-row-set! robot (robot-row-prev robot))
                  (robot-col-set! robot (robot-col-prev robot))
                  (return #f)))
          (check-item-position (cdr items)))) ; Or was it another item?

  (if (and (= (chicken-col) (robot-col robot)) ; You found chicken!
           (= (chicken-row) (robot-row    robot)))
      (rfc-win))

  (check-item-position items)
  (return #t)) ; Nope, you're clear.


(define (move-robot #!key (v 'nil) (h 'nil))
  ;; Erase the message line
  (mvprintw 1 2 (make-string (- (COLS) 3) #\ ))

  ;; Save old position
  (robot-row-prev-set! robot (robot-row robot))
  (robot-col-prev-set! robot (robot-col robot))

  ;; Vertical movement
  (if (eq? v 'up)
      (robot-row-set! robot (sub1 (robot-row robot)))
      (if (eq? v 'down)
          (robot-row-set! robot (add1 (robot-row robot)))))

  ;; Horizontal movement
  (if (eq? h 'left)
      (robot-col-set! robot (sub1 (robot-col robot)))
      (if (eq? h 'right)
          (robot-col-set! robot (add1 (robot-col robot)))))

  ;; Vertical boundary check
  (if (= (robot-row robot) (- (LINES) 1))
      (robot-row-set! robot (- (LINES) 2))
      (if (= (robot-row robot) 2)
          (robot-row-set! robot 3)))

  ;; Horizontal boundary check
  (if (= (robot-col robot) (- (COLS) 1))
      (robot-col-set! robot (- (COLS) 2))
      (if (= (robot-col robot) 0)
          (robot-col-set! robot 1)))

  ;; Did we move or bump into something?
  (robot-moved?-set! robot
                     (call-with-current-continuation
                      (lambda (return) (check-position return)))))

;;;;
