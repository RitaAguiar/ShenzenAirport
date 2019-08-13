#lang racket

(provide (all-defined-out))
(require "ParametersAirport.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ROBOT

(require rosetta/robot/backend)
(require (prefix-in % rosetta/robot/robot-com))
(require (prefix-in base- racket/base))
(require (prefix-in ac: rosetta/autocad))
(ac:delete-all-shapes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

(define comp-barras 0) ;comprimento inicial barras
(define n-nos 0) ;número inicial nós
(define robot #f) ;robot ou cálculo

;GUARDA CONTAGEM DE NÓS E BARRAS

(define (set-n-nos valor)  ;guardar valor dos nós contados
  (set! n-nos valor))

(define (set-comp-barras valor)  ;guardar valor de comprimento contados
  (set! comp-barras valor))

(define (set-robot valor)  ;guardar valor de comprimento contados
  (set! robot valor))

;;FAMÍLIAS

;em breve vão ser comuns a todos os backends

(define fixed-truss-node-family
  (truss-node-family-element
   (default-truss-node-family)
   #:support (create-node-support "SupportA" #:ux #t #:uy #t #:uz #t)))

(default-truss-bar-family
  (truss-bar-family-element
   (default-truss-bar-family)
   #:material (list "S460"  ;nome
                    I_MT_STEEL          ;tipo
                    "Steel"             ;nome do tipo
                    "I'm really steel"  ;nuance
                    210000000000.0      ;E
                    0.3                 ;nu - 
                    81000000000.0       ;G
                    76500.0             ;sw
                    1.2E-05             ;cte
                    0.04                ;dumpcoef
                    460000000.0         ;fy
                    540000000.0)        ;Limit Tension resistance
   #:section (list "PIPE_S460"
                   "S460"
                   #f ;madeira?
                   (list (list #f ;sólido?
                               (* raio-barra-trelica 2)
                               esp-tubo-aco)))))

;;MODELO BIM PARA O ROBOT

(define (no-trelica espec-trelica p)
  ; (printf "robot = ~a\n" robot)

  (if robot
      (truss-node p)
      (begin
        (set! n-nos (+ n-nos 1))
        ; (printf "n-nos = ~a\n" n-nos)
)))

(define (fixed-no-trelica espec-trelica p)
  (if robot
      (truss-node p fixed-truss-node-family)
      (begin
        (set! n-nos (+ n-nos 1))
        ; (printf "n-nos = ~a\n" n-nos)
)))

(define (barra-trelica espec-trelica p0 p1)
  (if robot
      (truss-bar p0 p1)
      (begin
        (set! comp-barras (+ comp-barras (distance p0 p1)))
        ; (printf "(distance p0 p1) = ~a comp-barra = ~a\n"
;                 (distance p0 p1) comp-barras)
)))
