#!/usr/bin/csi -ss
(use ncurses)
(require-extension srfi-13)

(define robot-row (make-parameter 1))
(define robot-col (make-parameter 1))

(define (centre-message . messages)
  (define (centre-message-iter messages n)
    (when (pair? messages)
      (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                (quotient (- (COLS) (string-length (car messages))) 2)
                (car messages))
      (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

(define (random-row)
  (random (sub1 (LINES))))
(define (random-col)
  (random (sub1 (COLS))))

(define (draw-robot)
  (attron (COLOR_PAIR 2))
  (mvaddch (robot-row) (robot-col) (ACS_DIAMOND))
  (attroff (COLOR_PAIR 2))
  (refresh))

(define (quit message code)
  (clear)
  (endwin)
  (unless (zero? code)
    (fprintf (current-error-port) message)
    (exit code))
  (print message)
  (exit))

(define (move-left)
  (if (> (robot-col) 2)
      (robot-col (sub1 (robot-col)))))

(define (move-right)
  (if (< (robot-col) (- (COLS) 2))
      (robot-col (add1 (robot-col)))))

(define (move-up)
  (if (> (robot-row) 2)
      (robot-row (sub1 (robot-row)))))

(define (move-up)
  (if (< (robot-row) (- (LINES) 2))
      (robot-row (add1 (robot-row)))))

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

  (raw) (noecho)
  (keypad (stdscr) #t)
  (curs_set 0)

  (attron A_BOLD)

  (attron (COLOR_PAIR 1))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (attroff (COLOR_PAIR 1))

  (attron (COLOR_PAIR 4))
  (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
  (attroff (COLOR_PAIR 4))

  (attron (COLOR_PAIR 6))
  (mvprintw (- (LINES) 2) 2 "Press Q to exit.")
  (attroff (COLOR_PAIR 6))

  (refresh)

  (if (member (getch) '(#\q #\Q KEY_EXIT KEY_F0 KEY_CANCEL KEY_CLOSE))
      (quit "That was quick." 0))

  (robot-row (random-row))
  (robot-col (random-col))
  
  (rfc-loop))

(define (rfc-loop)
  (clear)
  (attron (COLOR_PAIR 1))
  (box (stdscr) (ACS_VLINE) (ACS_HLINE))
  (attroff (COLOR_PAIR 1))

  (draw-robot)

  (case (getch)
    ((#\q #\Q KEY_EXIT KEY_F0 KEY_CANCEL KEY_CLOSE)
     (quit "You couldn't find the chicken." 0))
    ((KEY_LEFT KEY_A1 KEY_C1) (move-left))
    ((KEY_RIGHT KEY_A3 KEY_C3) (move-right))
    ((KEY_UP) (move-up))
    ((KEY_DOWN) (move-down)))

  (mvprintw 5 5 "Position: (~S, ~S)~%" (robot-row) (robot-col))
  
  (rfc-loop))

(define (main args)  
  (if (null? args)
      (rfc-init)
      (print "This program does not take options.")))
