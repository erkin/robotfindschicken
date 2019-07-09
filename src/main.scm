(include-relative "const")
(include-relative "internal")
(include-relative "draw")
(include-relative "game")

(import (chicken process-context))
(import rfc-const rfc-internal
        rfc-draw rfc-game)

(define layout-arg 'numpad)

(define (rfc-usage)
  (print "Usage: rfc [-n ITEMS]")
  (print "  --help, -?, -h              Display this help message")
  (print "  --items ITEMS, -n ITEMS     Play the game with ITEMS number of items")
  (print "  --layout LAYOUT, -l LAYOUT  Switch keyboard layout to LAYOUT (default: numpad)")
  (print "  --version, -v, -V           Display version and licence information")
  (newline)
  (print* "Available layouts are: ")
  (print* "qwerty/qwertz/azerty, [numpad], dvorak/svorak, ")
  (print "f, colemak, workman")
  (exit))

(define (rfc-version)
  (print   "robotfindschicken v1.-1.420")
  (print   "https://github.com/erkin/robotfindschicken")
  (print   "Copyright (C) 2019 Erkin Batu Altunbaş")
  (newline)
  (print* "Each file of this project's source code is subject ")
  (print  "to the terms of the Mozilla Public Licence v2.0")
  (print* "If a copy of the MPL was not distributed with this file, ")
  (print  "you can obtain one at https://mozilla.org/MPL/2.0/")
  (exit))

(define (main args)
  (define (take-items num)
    (let ((n (string->number num)))
      (cond
       ((not n)
        (print "Not a valid number: " (car rest))
        (newline)
        (exit 1))
       ((and
         (exact? n)
         (> n 0))
        (set! item-count n))
       (else
        (print "Not a suitable number: " n)
        (print "Ignoring...")
        (newline)))))

  (when (null? args)
    (rfc-init layout-arg))

  ;; A (case) would've normally sufficed but since it uses (eq?)
  ;; strings don't match.
  (let ((arg (car args)))
    (cond
     ((member arg '("-h" "-?" "--help" "-help"))
      (rfc-usage))
     ((member arg '("-v" "-V" "--version" "-version"))
      (rfc-version))
     ((member arg '("-n" "--items" "-items"))
      (when (null? (cdr args))
        (print "This option needs an argument.")
        (exit 1))
      (take-items (cadr args))
      (main (cddr args)))
     ((member arg '("-l" "--layout" "-layout"))
      (when (null? (cdr args))
        (print "This option needs an argument.")
        (exit 1))
      (set! layout-arg (string->symbol (cadr args)))
      (main (cddr args)))
     (else
      (rfc-usage)))))

(main (command-line-arguments))
