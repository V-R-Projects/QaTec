#lang racket

#|
*****************************************
Genetic Algorithm Overview
START
    Creating a Initial Population
    Fitness Evaluation & Selection
REPEAT
    Crossover
    Mutation
    Fitness Evaluation & Selection
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

(define (binToDec list lenList)
  (cond
    ((null? list) 0)
    (else (+ (* (expt 2 (- lenList 1)) (car list)) (binToDec (cdr list) (- lenList 1))))
    )
  )

(define (fitnessPlayer list)
  (quotient (+ (binToDec (car list) 4) (+ (binToDec (cadr list) 4) (binToDec (caddr list) 4))) 3)
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

(define (selectTeam list) ;; selects the first 6 players  of a fitness evaluated team
  (cond
    ((equal? (lengthList list) 5) '())
    (else (cons (car list) (selectTeam (cdr list))))
    )
  )

(define (selectPopulation population) ;; main
  (cond
    ((null? population) '())
    (else (cons (selectTeam (fitnessTeam (car population))) (selectPopulation (cdr population))))
    )
  )

#|
*****************************************
Crossover

The crossover stage takes a population that
already passed the selection process, when each
team has only the best 6 players of the generation.
The crossoverPopulation crossover the players and
adds 5 new players to each team (product of the crossover)
producing a new generation.
*****************************************
|#

(define (truncateBits attribute1 attribute2)
  (cond
    ((null? attribute1) '())
    ((> (lengthList attribute1) 2) ; 2 is the half ot the lenght of the attributes
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

(define (crossoverPopulation population) ;; main
  (cons (crossoverTeam (car population)) (list(crossoverTeam (cadr population))))
  )

#|*****************************************
Mutation

This algorithm takes a random bit from each
attribute of each player and mutates it, this
happens with the entire population.
*****************************************
|#

(define (alterBit bit)
  (cond
    ((zero? bit) 1)
    (else 0)
    )
  )

(define (mutateAttribute attribute randomBit) ;; randombit must be an int from 0 to 3
  (cond
    ((null? attribute) '())
    ((zero? randomBit) (cons (alterBit (car attribute))
                             (mutateAttribute (cdr attribute) (- randomBit 1))))
    (else (cons (car attribute)
                (mutateAttribute (cdr attribute) (- randomBit 1))))
    )
  )

(define (mutatePlayer player)
  (cond
    ((null? player) '())
    (else (cons (mutateAttribute (car player) (random 4)) (mutatePlayer (cdr player))))
    )
  )

(define (mutateTeam team)
  (cond
    ((null? team) '())
    (else (cons (mutatePlayer (car team)) (mutateTeam (cdr team))))
    )
  )

(define (mutatePopulation population) ;; main
  (cons (mutateTeam (car population)) (list (mutateTeam (cadr population))))
  )



#|*****************************************
Main Function

This function recieves a population and applies
the three steps of the genetic algorithm. It
returns the new population.
*****************************************
|#

(define (convert-player-dec player)
  (cond
    ((null? player) '())
    (else (cons (binToDec (car player) 4) (convert-player-dec (cdr player))))
    )
  )

(define (convert-team-dec team)
  (cond
    ((null? team) '())
    (else (cons (convert-player-dec (car team)) (convert-team-dec (cdr team))))
    )
  )

(define (convert-population population)
  (list (convert-team-dec (car population)) (convert-team-dec (cadr population)))
)

(define (geneticAlgorithm population)
  (convert-population (mutatePopulation (crossoverPopulation (selectPopulation population))))
  )

(geneticAlgorithm (generatePopulation 2))