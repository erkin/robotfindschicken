;;;; Movement procedures

(define (check-position return)
  (define (check-item-position items)
    (when (pair? items)
          (let ((item (car items))) ; You bumped into an item
            (when (and (= (row-get item) (row-get robot))
                       (= (col-get item) (col-get robot)))
                  ;; Clip the string if it's too long.
                  (if (>= (string-length (message-get item)) (- (COLS) 3))
                      (begin
                        (mvprintw 1 2 (string-take (message-get item) (- (COLS) 5)))
                        (addch (ACS_RARROW)) (addch (ACS_RARROW)))
                      (mvprintw 1 2 (message-get item))) ; Print item's message
                  (row-set! robot (row-prev-get robot))
                  (col-set! robot (col-prev-get robot))
                  (return #f)))
          (check-item-position (cdr items)))) ; Or was it another item?

  (if (and (= (col-get chicken) (col-get robot)) ; You found chicken!
           (= (row-get chicken) (row-get robot)))
      (rfc-win))

  (check-item-position items)
  (return #t)) ; Nope, you're clear.


(define (move-robot #!key (v 'nil) (h 'nil))
  ;; Erase the message line
  (mvprintw 1 2 (make-string (- (COLS) 3) #\ ))

  ;; Save old position
  (row-prev-set! robot (row-get robot))
  (col-prev-set! robot (col-get robot))

  ;; Vertical movement
  (if (eq? v 'up)
      (row-set! robot (sub1 (row-get robot)))
      (if (eq? v 'down)
          (row-set! robot (add1 (row-get robot)))))

  ;; Horizontal movement
  (if (eq? h 'left)
      (col-set! robot (sub1 (col-get robot)))
      (if (eq? h 'right)
          (col-set! robot (add1 (col-get robot)))))

  ;; Vertical boundary check
  (if (= (row-get robot) (- (LINES) 1))
      (row-set! robot (- (LINES) 2))
      (if (= (row-get robot) 2)
          (row-set! robot 3)))

  ;; Horizontal boundary check
  (if (= (col-get robot) (- (COLS) 1))
      (col-set! robot (- (COLS) 2))
      (if (= (col-get robot) 0)
          (col-set! robot 1)))

  ;; Did we move or bump into something?
  (moved! robot
          (call-with-current-continuation
           (lambda (return) (check-position return)))))

;;;;
