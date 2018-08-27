;;;; Drawing procedures

;;; Write messages in the middle of the window
(define (centre-message . messages)
  (define (centre-message-iter messages n) ; with doublespacing
    (when (pair? messages)
          (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                    (quotient (- (COLS) (string-length (car messages))) 2)
                    (car messages))
          (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

;;; Draw non-robot non-chicken items
(define (draw-items item-list)
  (unless (null? item-list)
          (let ((item (car item-list)))
            (attron (COLOR_PAIR (item-colour item)))
            (mvaddch (item-row item) (item-col item)
                     (item-char item))
            (attroff (COLOR_PAIR (item-colour item))))
          (draw-items (cdr item-list))))

(define (draw-chicken)
  (attron (COLOR_PAIR *chicken-colour*))
  (mvaddch (chicken-row) (chicken-col) chicken-char)
  (attroff (COLOR_PAIR *chicken-colour*)))

(define (draw-robot)
  ;; Remove the previous robot
  (mvaddch (robot-row-prev robot) (robot-col-prev robot) #\ )

  (attron (COLOR_PAIR (robot-colour robot)))
  (mvaddch (robot-row robot) (robot-col robot) (robot-char robot))
  (attroff (COLOR_PAIR (robot-colour robot)))

  ;; Reset status
  (robot-moved?-set! robot #f))

(define (draw-frame)
  ;; Frame around the box
  (attron (COLOR_PAIR *frame-colour*))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (mvhline 2 1 (ACS_HLINE) (- (COLS) 2))
  (mvaddch 2 0 (ACS_LTEE))
  (mvaddch 2 (sub1 (COLS)) (ACS_RTEE))
  (attroff (COLOR_PAIR *frame-colour*))

  ;; Fancy branding
  (attron (COLOR_PAIR *decor-colour*))
  (mvprintw 2 2 "robotfinds")
  (attroff (COLOR_PAIR *decor-colour*))

  (attron (COLOR_PAIR *chicken-colour*))
  (printw "chicken")
  (attroff (COLOR_PAIR *chicken-colour*)))

;;;;
