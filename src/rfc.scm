#!/usr/bin/csi -ss
(use ncurses)
(require-extension srfi-13)
(require-extension srfi-18)

(define robot-row (make-parameter 1))
(define robot-col (make-parameter 1))

(define robot-row-prev (make-parameter 1))
(define robot-col-prev (make-parameter 1))

(define moved? (make-parameter #f))

(define chicken-row (make-parameter 1))
(define chicken-col (make-parameter 1))

(define (quit message code)
  (flushinp)
  (clear)
  (endwin)
  (unless (zero? code)
    (fprintf (current-error-port) message)
    (exit code))
  (print message)
  (exit))

(define (centre-message . messages)
  (define (centre-message-iter messages n)
    (when (pair? messages)
      (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                (quotient (- (COLS) (string-length (car messages))) 2)
                (car messages))
      (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

(define (repeat f n)
  (if (= n 1)
      (f)
      ((lambda ()
         (f)
         (repeat f (sub1 n))))))

(define (random-row)
  (+ 3 (random (- (LINES) 5))))
(define (random-col)
  (+ 1 (random (- (COLS) 2))))

(define (draw-chicken)
  (attron (COLOR_PAIR 3))
  (mvaddch (chicken-row) (chicken-col) (ACS_CKBOARD))
  (attroff (COLOR_PAIR 3)))

(define (draw-robot)
  (mvaddch (robot-row-prev) (robot-col-prev) #\ )
  (attron (COLOR_PAIR 7))
  (mvaddch (robot-row) (robot-col) (ACS_DIAMOND))
  (attroff (COLOR_PAIR 7))
  (moved? #f))

(define (move #!key (v 'nil) (h 'nil))
  (robot-row-prev (robot-row))
  (robot-col-prev (robot-col))

  (if (eq? v 'up)
      (robot-row (sub1 (robot-row)))
      (if (eq? v 'down)
          (robot-row (add1 (robot-row)))))
  (if (eq? h 'left)
      (robot-col (sub1 (robot-col)))
      (if (eq? h 'right)
          (robot-col (add1 (robot-col)))))

  (if (= (robot-row) (- (LINES) 1))
      (robot-row (- (LINES) 2))
      (if (= (robot-row) 2)
          (robot-row 3)))

  (if (= (robot-col) (- (COLS) 1))
      (robot-col (- (COLS) 2))
      (if (= (robot-col) 0)
          (robot-col 1)))

  (if (and (= (chicken-col) (robot-col))
           (= (chicken-row) (robot-row)))
      (rfc-win))

  (moved? #t))

(define (rfc-frame)
  (attron (COLOR_PAIR 1))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (mvhline 2 1 (ACS_HLINE) (- (COLS) 2))
  (mvaddch 2 0 (ACS_LTEE))
  (mvaddch 2 (sub1 (COLS)) (ACS_RTEE))
  (attroff (COLOR_PAIR 1))

  (attron (COLOR_PAIR 2))
  (mvprintw 2 2 "robotfinds")
  (attroff (COLOR_PAIR 2))

  (attron (COLOR_PAIR 3))
  (printw "chicken")
  (attroff (COLOR_PAIR 3)))

(define (rfc-splash)
  (rfc-frame)

  (attron (COLOR_PAIR 6))
  (mvprintw (- (LINES) 2) (- (COLS) 18) "Press Q to exit.")
  (mvprintw (- (LINES) 3) 2 "Use       to move.")
  (mvprintw 3 (- (COLS) 44) "Press H any time to view this help screen.")
  (mvprintw 3 3 "Press any key to begin/resume.")
  (attroff (COLOR_PAIR 6))
  
  (attron (COLOR_PAIR 5))
  (mvprintw (- (LINES) 4) 6 "7 8 9")
  (mvprintw (- (LINES) 3) 6 "4   6")
  (mvprintw (- (LINES) 2) 6 "1 2 3")
  (attroff (COLOR_PAIR 5))

  (attron (COLOR_PAIR 4))
  (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
  (mvaddch (- (LINES) 3) 8 (ACS_PLUS))
  (attroff (COLOR_PAIR 4))
    
  (if (member (getch) '(#\q #\Q KEY_F0))
      (quit "That was quick." 0))
  
  (clear)

  (rfc-frame)
  (draw-robot)
  (draw-chicken))

(define (rfc-init)
  (initscr)

  (if (not (has_colors))
      (quit "Your terminal does not support colours." 1))
  
  (start_color)
  (init_pair 1 COLOR_BLUE COLOR_BLACK)
  (init_pair 2 COLOR_CYAN COLOR_BLACK)
  (init_pair 3 COLOR_RED COLOR_BLACK)
  (init_pair 4 COLOR_YELLOW COLOR_BLACK)
  (init_pair 5 COLOR_MAGENTA COLOR_BLACK)
  (init_pair 6 COLOR_GREEN COLOR_BLACK)
  (init_pair 7 COLOR_GREEN COLOR_WHITE)

  (raw) (noecho)
  (keypad (stdscr) #t)
  (curs_set 0)

  (attron A_BOLD)

  (robot-row (random-row))
  (robot-col (random-col))
  (robot-row-prev (robot-row))
  (robot-col-prev (robot-col))
  (chicken-row (random-row))
  (chicken-col (random-col))
  
  (rfc-splash)

  (rfc-loop))

(define (rfc-loop)
  (if (moved?) (draw-robot))

  (case (getch)
    ((#\q #\Q KEY_F0)
     (quit "You couldn't find the chicken." 0))
    ((#\8 KEY_UP)
     (move v: 'up))
    ((#\9)
     (move v: 'up
           h: 'right))
    ((#\6 KEY_RIGHT)
     (move h: 'right))
    ((#\3)
     (move v: 'down
           h: 'right))
    ((#\2 KEY_DOWN)
     (move v: 'down))
    ((#\1)
     (move v: 'down h: 'left))
    ((#\4 KEY_LEFT)
     (move h: 'left))
    ((#\7)
     (move v: 'up
           h: 'left))
    ((#\h #\H KEY_F1 KEY_HELP)
     (call-with-current-continuation (lambda _ (rfc-splash)))))

  (rfc-loop))

(define (rfc-win)
  (clear)
  (flushinp)
  (rfc-frame)

  (robot-row 1)
  (robot-col (- (quotient (COLS) 2) 3))
  
  (chicken-row 1)
  (chicken-col (+ (quotient (COLS) 2) 3))

  (draw-robot)
  (draw-chicken)

  (repeat
   (lambda ()
     (thread-sleep! 0.8)
     (move h: 'right)
     (draw-robot)
     (refresh))
   5)
  
  (attron (COLOR_PAIR 4))
  (centre-message "You found the chicken!" "Bravo!")
  (attroff (COLOR_PAIR 4))

  (refresh)
  (thread-sleep! 1)
  (getch)
  (quit "Thanks for playing!" 0))

(define (main args)  
  (if (null? args)
      (rfc-init)
      (print "This program does not take options.")))
