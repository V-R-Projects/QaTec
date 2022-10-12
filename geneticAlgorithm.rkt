#lang racket

#|
*****************************************
Genetic Algorithm Overview
START
    Creating a Initial Population
    Fitness Evaluation
REPEAT
    Selection
    Crossover
    Mutation
    Fitness Evaluation
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
    (else (cons (generateAttribute 3) ; 3 is the number of bits for each attribute
                (generatePlayer (- attributes 1))))
    )
  )

(define (generateTeam players)
  (cond
    ((zero? players) '())
    (else (cons (generatePlayer 4) ; 4 is the number of attributes for each player
                (generateTeam (- players 1)))) 
    )
  )

(define (generatePopulation teams)
  (cond
    ((zero? teams) '())
    (else (cons (generateTeam 2) ; 11 is the number of players for each team
                (generatePopulation (- teams 1)))) 
    )
  )