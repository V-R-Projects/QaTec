#lang racket

#|
*****************************************
Genetic Algorithm Overview
START
    Creating a Initial Population
    Selection & Fitness Evaluation
REPEAT
    Selection & Fitness Evaluation
    Crossover
    Mutation
UNTIL
    Defined number of generations
STOP
*****************************************
|#

#|
*****************************************
Creating Initial Population

generatePopulation is a function that recieves
a number of teams to create the initial population,
it uses the functions generateTeam, generatePlayer
and generateAttribute to create recursively each
component of the population randomly.
*****************************************
|#

(define (generateAttribute bits)
  (cond
    ((zero? bits) '())
    (else (cons (random 2) (generateAttribute (- bits 1))))
    )
  )

(define (generatePlayer attributes)
  (cond
    ((zero? attributes) '())
    (else (cons (generateAttribute 4) ; 4 is the number of bits for each attribute
                (generatePlayer (- attributes 1))))
    )
  )

(define (generateTeam players)
  (cond
    ((zero? players) '())
    (else (cons (generatePlayer 3) ; 3 is the number of attributes for each player
                (generateTeam (- players 1))))
    )
  )

(define (generatePopulation teams) ;; Main generatePopulation function
  (cond
    ((zero? teams) '())
    (else (cons (generateTeam 11) ; 11 is the number of players for each team
                (generatePopulation (- teams 1))))
    )
  )

#|
*****************************************
Fitness Evaluation

fitnessPlayer is a function that takes
a player and generates a calification based
on the average of its 3 attributes.
fitnessTeam is a function that quicksorts an array
of a team from greater to least based on the
fitness evaluation of each player.
*****************************************
|#

(define (binToDec list)
  (+ (* 4 (car list)) (+ (* 2 (cadr list)) (caddr list)))
  )

(define (fitnessPlayer list)
  (quotient (+ (binToDec(car list)) (+ (binToDec(cadr list)) (binToDec(caddr list)))) 3)
  )

(define (filterList condition list)
  (cond ((null? list) null)
        ((condition (fitnessPlayer(car list)))
         (cons (car list) (filterList condition (cdr list))))
        (else (filterList condition (cdr list)))))

(define (fitnessTeam list)
  (cond
    ((< 1 (length list))
     (append
      (fitnessTeam (filterList (lambda (x) (> x (fitnessPlayer(car list)))) list))
      (filterList (lambda (x) (equal? x (fitnessPlayer(car list)))) list)
      (fitnessTeam (filterList (lambda (x) ((lambda (left right) (not (or (> left right) (equal? left right))))
                                            x (fitnessPlayer(car list)))) list))))
    (else list)))

#|
*****************************************
Natural Selection

selectPopulation is a function that evaluates
two teams and selects the best 6 of each.
*****************************************
|#

(define (lengthList list)
  (cond
    ((null? list) 0)
    (else (+ 1 (lengthList (cdr list)))))
  )

(define (selectionTeam list) ;; selects the first 6 players  of a fitness evaluated team
  (cond
    ((equal? (lengthList list) 5) '())
    (else (cons (car list) (selectionTeam (cdr list))))
    )
  )

(define (selectPopulation population)
  (cond
    ((null? population) '())
    (else (cons (selectionTeam (fitnessTeam (car population))) (selectPopulation (cdr population))))
    )
  )

#|
*****************************************
Crossover

*****************************************
|#

(define (truncateBits attribute1 attribute2)
  (cond
    ((null? attribute1) '())
    ((> (lengthList attribute1) 2) 
     (cons (car attribute1) (truncateBits (cdr attribute1) (cdr attribute2))))
    (else (cons (car attribute2) (truncateBits (cdr attribute1)(cdr attribute2))))
  )
)

(define (crossoverPlayers player1 player2)
  (cond
    ((null? player1) '())
    (else (cons (truncateBits (car player1) (car player2)) 
                (crossoverPlayers (cdr player1) (cdr player2))))
    )
  )

(define (crossoverSelected bests) ;; 6 bests players 
  (cond
    ((equal? (lengthList bests) 2)
     (list (crossoverPlayers (car bests) (cadr bests))))
    (else (append (list (crossoverPlayers (car bests) (cadr bests)) (crossoverPlayers (cadr bests) (car bests))) 
                  (crossoverSelected (cddr bests)) ))
    )
  )

(define (crossoverTeam team)
  (append team (crossoverSelected team))
  )

(define (crossoverPopulation population)
  (cons (crossoverTeam (car population)) (list(crossoverTeam (cadr population))))
  )

;(selectPopulation (generatePopulation 2))
;(crossoverPopulation (selectPopulation (generatePopulation 2)))
;(crossoverPlayers (generatePlayer 3) (generatePlayer 3))
;(crossoverTeam (selectionTeam (generateTeam 11)))
;(list '(0) '(1))
;(cons '(0) '(1))
;(append '(0) '(1))