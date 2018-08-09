#!/usr/bin/csi -ss
(use ncurses)
(require-extension srfi-13)

(define (centre-message . messages)
  (define (centre-message-iter messages n)
    (when (pair? messages)
      (mvprintw (+ (quotient (LINES) 2) (- n (length messages)))
                (quotient (- (COLS) (string-length (car messages))) 2)
                (car messages))
      (centre-message-iter (cdr messages) (add1 n))))
  (centre-message-iter messages 0))

(define (run-row row col char)
  (when (< col (COLS))
    (mvaddch row col char)
    (run-row row (add1 col) char)))

(define (run-col row col char)
  (when (< row (LINES))
    (mvaddch row col char)
    (run-col (add1 row) col char)))

(define (frame char colour)
  (attron A_BOLD)
  (attron (COLOR_PAIR colour))

  (run-row 0       0      char)
  (run-row (LINES) 0      char)
  (run-col 0       0      char)
  (run-col 0       (COLS) char)

  (attroff (COLOR_PAIR colour))
  (attroff A_BOLD))

(define (main args)  
  (initscr)

  (unless (has_colors)
    (fprintf (current-error-port) "Your terminal does not support colours.~%")
    (endwin)
    (exit 1))

  (start_color)
  (init_pair 1 COLOR_BLUE COLOR_BLACK)
  (init_pair 2 COLOR_GREEN COLOR_BLACK)

  (cbreak) (noecho)
  (keypad (stdscr) #t)
  (curs_set 0)
  
  (frame #\# 1)
  (centre-message "You are the Robot!" "Find the Chicken!" "Godspeed!")
  (refresh) (getch) (clear)

  (frame #\@ 2)
  (centre-message "game goes here")
  (refresh) (getch) (clear)
  
  (endwin)
  (exit))

