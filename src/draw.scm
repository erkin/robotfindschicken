;;;; Herein lie ncurses drawing procedures

(module rfc-draw *
  (import scheme
          (chicken base))
  (import ncurses)

  (import rfc-const
          rfc-internal)

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

;;; Draw keyboard layout on the help screen
  (define (draw-layout line keys spacing)
    (mvprintw
     (- (LINES) line) 6
     (apply string-append (intersperse (map (compose string layout-ref) keys) spacing))))
  
;;; Draw non-robot non-chicken items
  (define (draw-items item-list)
    (unless (null? item-list)
      (let ((item (car item-list)))
        (attron (COLOR_PAIR (colour item)))
        (mvaddch (row item) (col item)
                 (char item))
        (attroff (COLOR_PAIR (colour item))))
      (draw-items (cdr item-list))))

  (define (draw-chicken chicken)
    (attron (COLOR_PAIR (colour chicken)))
    (mvaddch (row chicken) (col chicken) (char chicken))
    (attroff (COLOR_PAIR (colour chicken))))

  (define (draw-robot robot)
    ;; Remove the previous robot
    (mvaddch (row-prev robot) (col-prev robot) #\ )

    (attron (COLOR_PAIR (colour robot)))
    (mvaddch (row robot) (col robot) (char robot))
    (attroff (COLOR_PAIR (colour robot)))

    ;; Reset status
    (moved! robot #f))

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
    (attroff (COLOR_PAIR *chicken-colour*))))
