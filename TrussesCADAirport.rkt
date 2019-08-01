#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RHINO/AUTOCAD

(require rosetta/rhino)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;LAYERS

(define nodes-layer (create-layer "Nodes"))
(define fixed-nodes-layer (create-layer "Fixed Nodes"))
(define bars-layer (create-layer "Bars"))

;;GEOMETRIA

;;;TRELIÇAS

(define (no-trelica espec-trelica no)
  (define raio-no-trelica (list-ref espec-trelica 0))
  (define opcao-nos (list-ref espec-trelica 2))
  (with-current-layer nodes-layer 
    (case opcao-nos
      ((1) (point no))
      ((2) (sphere no raio-no-trelica)))))

(define (fixed-no-trelica espec-trelica no)
  (define raio-no-trelica (list-ref espec-trelica 0))
  (define aresta-fixed-no-trelica (list-ref espec-trelica 1))
  (define opcao-nos (list-ref espec-trelica 2))
  (define d (/ aresta-fixed-no-trelica 2))
  (with-current-layer fixed-nodes-layer 
    (case opcao-nos
      ((1) (point no))
      ((2) (sphere no raio-no-trelica)
           #;(box (+xyz no (- d)(- d)(- d)) (+xyz no d d d))))))

(define (barra-trelica espec-trelica p q)
  (define raio-barra-trelica (list-ref espec-trelica 3))
  (define opcao-barras (list-ref espec-trelica 4))
  (with-current-layer bars-layer 
    (case opcao-barras
      ((1) (line p q))
      ((2) (cylinder p raio-barra-trelica q)))))
