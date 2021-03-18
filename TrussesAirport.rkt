#lang racket

(provide (all-defined-out))

(require "TrussesCADAirport.rkt")
;OU
;(require "TrussesRobotAirport.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rosetta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;;TRELIÇAS

;Cria uma treliça espacial do tipo Warren With Verticals dada a respetiva rede de nós

;raio-no-trelica - valor do raio da superfície esférica que representa um nó da treliça
                   ;que não é nó de apoio
;aresta-fixed-no-trelica - valor do raio da superfície esférica que representa um nó
                           ;de apoio da treliça (nó fixo)
;opcao-nos - código que indica o tratamento a dar aos nós, nomeadamente visualização
;raio-barra-trelica - valor do raio da superfície cilindrica que representa uma barra da treliça
;opcao-barras - código que indica o tratamento a dar às barras, nomeadamente visualização
;nos-i - matriz (lista de listas) dos nós inferiores da treliça
;nos-s - matriz (lista de listas) dos nós superiores da treliça

;;;;CRIAÇÃO DA MALHA DOS NÓS

(define (cria-nos-p-warren n-trelicas l-trelica f-sup-nos-p x-inic-p x-fin-p
                     n-interv-p n-nos-ret f-dist-nos)
  (define comp (* n-trelicas l-trelica))
    (define desf-s-warren-x (/ (/ (- x-inic-p x-fin-p) n-interv-p) 2))
    (define desf-s-warren-y (/ (/ comp n-trelicas) 2))
  (define (ponto-fila y)
    (λ(x)(f-sup-nos-p x y)))
  (define (fila-nos y)
    (define fila-completa
      (map-division (ponto-fila y)
                    x-inic-p #;(+ x-inic-p desf-s-warren-x)
                    x-fin-p #;(- x-fin-p desf-s-warren-x) (- n-interv-p 3) #t f-dist-nos))
    (list-tail (drop-right fila-completa n-nos-ret) n-nos-ret))
  (map-division fila-nos desf-s-warren-y
                (- comp desf-s-warren-y) (- n-trelicas 1) #t))

(define (cria-nos-p n-trelicas l-trelica f-sup-nos-p x-inic-p x-fin-p
                     n-interv-p n-nos-ret f-dist-nos)
  (define comp (* n-trelicas l-trelica))
  (define (ponto-fila y)
    (λ(x)(f-sup-nos-p x y)))
  (define (fila-nos y)
    (define fila-completa
      (map-division (ponto-fila y) x-inic-p
                    x-fin-p n-interv-p #t f-dist-nos))
    (list-tail (drop-right fila-completa n-nos-ret) n-nos-ret))
  (map-division fila-nos 0
                comp n-trelicas #t))

(define (cria-nos-s  n-trelicas l-trelica f-sup-nos-s x-inic-p x-fin-p
                     n-interv-p n-nos-ret f-dist-nos f-ajust)
  (define comp (* n-trelicas l-trelica))
  (define n-interv-s (- n-interv-p 2))
  (define desf (* f-ajust (/ (- x-fin-p x-inic-p) n-interv-p)))
  #;(printf "(+ x-inic-p desf) = ~a\n" (+ x-inic-p desf))
  #;(printf "(+ x-fin-p desf) = ~a\n" (+ x-fin-p desf))
  (define (ponto-fila y)
    (λ(x)(f-sup-nos-s x y)))
  (define (fila-nos y)
    (define fila-completa
      (map-division (ponto-fila y)
                    (+ x-inic-p desf)
                    (- x-fin-p desf)
                    n-interv-s #t f-dist-nos))
    (list-tail (drop-right fila-completa n-nos-ret) n-nos-ret))
  (map-division fila-nos 0
                comp n-trelicas #t))

;;;;TIPOS DE TRELIÇAS

(define (trelica tipo espec-trelica pontos-i pontos-s)
  (if (equal? tipo 'warren)
      (nos-trelica-warren espec-trelica pontos-i pontos-s)
      (nos-trelica espec-trelica pontos-i pontos-s))
  #;(printf "tipo  = ~a \n" tipo )
  (case tipo
    ((wout-bracing)
     (barras-trelica-wout-bracing espec-trelica pontos-i pontos-s))
    ((pratt-planar)
     (barras-trelica-pratt-planar espec-trelica pontos-i pontos-s))
    ((pratt-transversal)
     (barras-trelica-pratt-transversal espec-trelica pontos-i pontos-s))
    ((pratt-diagonal)
     (barras-trelica-pratt-diagonal espec-trelica pontos-i pontos-s))
    ((pratt)
     (barras-trelica-pratt espec-trelica pontos-i pontos-s))
    ((howe-planar)
     (barras-trelica-howe-planar espec-trelica pontos-i pontos-s))
    ((howe-transversal)
     (barras-trelica-howe-transversal espec-trelica pontos-i pontos-s))
    ((howe-diagonal)
     (barras-trelica-howe-diagonal espec-trelica pontos-i pontos-s))
    ((howe)
     (barras-trelica-howe espec-trelica pontos-i pontos-s))
    ((warren-verticals-planar)
     (barras-trelica-warren-verticals-planar espec-trelica pontos-i pontos-s))
    ((warren-verticals-transversal)
     (barras-trelica-warren-verticals-transversal espec-trelica pontos-i pontos-s))
    ((warren-verticals-diagonal)
     (barras-trelica-warren-verticals-diagonal espec-trelica pontos-i pontos-s))
    ((warren-verticals)
     (barras-trelica-warren-verticals espec-trelica pontos-i pontos-s))
    ((warren)
     (barras-trelica-warren espec-trelica pontos-i pontos-s))))

;;;;;NOS-TRELIÇAS

(define (nos-trelica espec-trelica pontos-i pontos-s)
  (define (nos-fila-trelica fila)
    (for/list ((no fila))
      (no-trelica espec-trelica no)))
  (if (null? pontos-i)
        '()
        (begin
          (let ((a-i (car pontos-i))
                (a-s (car pontos-s)))
            (fixed-no-trelica espec-trelica (car a-i))
            (nos-fila-trelica (cdr (drop-right a-i 1)))
            (fixed-no-trelica espec-trelica (last a-i))
            (nos-fila-trelica a-s)
            (nos-trelica espec-trelica (cdr pontos-i) (cdr pontos-s))))))

(define (nos-trelica-warren espec-trelica pontos-i pontos-s)
  (define (nos-fila-trelica fila)
    (for/list ((no fila))
      (no-trelica espec-trelica no)))
  (let ((as (car pontos-s))
        (bs (car pontos-i)))
    (nos-fila-trelica as)
    (fixed-no-trelica espec-trelica (car bs))
    (nos-fila-trelica (cdr (drop-right bs 1)))
    (fixed-no-trelica espec-trelica (last bs))
    (if (null? (cdr pontos-i))
        (let ((as (cadr pontos-s)))
          (nos-fila-trelica as))
        (nos-trelica-warren espec-trelica (cdr pontos-i) (cdr pontos-s)))))

;;;;;BARRAS TRELIÇAS

;1st - Without verticals

(define (barras-trelica-wout-bracing espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          
          ;diagonal facial inicial
          (barra-trelica espec-trelica (car c-i) (car c-s))
          (barra-trelica espec-trelica (last c-s) (last c-i)))
        
        ;recursão
        (barras-trelica-wout-bracing espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;2nd - Planar Pratt bracing

(define (barras-trelica-pratt-planar espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
;    
    (barras-trelica (metade-1 a-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 b-i)))
;    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-2 a-i) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 b-s)))
;    
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (car c-i) (car c-s))
          ;diagonais faciais intermédias
          (barras-trelica (metade-1 c-s) (cddr (metade-1 c-i)))
          (barras-trelica (metade-2 c-i) (cdr (metade-2 c-s)))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (last c-s) (last c-i)))
        ;recursão
        (barras-trelica-pratt-planar espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;3rd - Planar Pratt + transversal bracing

(define (barras-trelica-pratt-transversal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
;    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
;    
;    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-1 a-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 b-i)))
;    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-2 a-i) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 b-s)))
;    
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (car c-i) (car c-s))
          ;diagonais faciais intermédias
          (barras-trelica (metade-1 c-s) (cddr (metade-1 c-i)))
          (barras-trelica (metade-2 c-i) (cdr (metade-2 c-s)))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (last c-s) (last c-i)))
        ;recursão
        (barras-trelica-pratt-transversal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;4th - Planar Pratt + transversal bracing + diagonal bracing

(define (barras-trelica-pratt-diagonal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    ;diagonais iniciais
    (barra-trelica espec-trelica (car a-i) (car a-s))
    (barra-trelica espec-trelica (car b-i) (car b-s))
    (barra-trelica espec-trelica (car a-i) (car b-s))
    (barra-trelica espec-trelica (car c-i) (car b-s))
    (barra-trelica espec-trelica (car a-i) (cadr b-i))
    (barra-trelica espec-trelica (car c-i) (cadr b-i))
    ;diagonais finais
    (barra-trelica espec-trelica (last a-s) (last a-i))
    (barra-trelica espec-trelica (last b-s) (last b-i))
    (barra-trelica espec-trelica (last b-s) (last a-i))
    (barra-trelica espec-trelica (last b-s) (last c-i))
    (barra-trelica espec-trelica (list-ref b-i (- (length b-i)2)) (last a-i))
    (barra-trelica espec-trelica (list-ref b-i (- (length b-i)2)) (last c-i))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-1 a-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 b-i)))
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-2 a-i) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 b-s)))

;    ;diagonais espaciais 1a metade
    (barras-trelica (metade-1 b-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 c-i)))
    ;diagonais espaciais 2a metade
    (barras-trelica (drop-right (metade-2 a-i) 1) (cdr (metade-2 b-s)))
    (barras-trelica (drop-right (metade-2 c-i) 1) (cdr (metade-2 b-s)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (car c-i) (car c-s))
          ;diagonais faciais intermédias
          (barras-trelica (metade-1 c-s) (cddr (metade-1 c-i)))
          (barras-trelica (metade-2 c-i) (cdr (metade-2 c-s)))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (last c-s) (last c-i)))
        ;recursão
        (barras-trelica-pratt-diagonal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;5th - Pratt

(define (barras-trelica-pratt espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    ;diagonais iniciais
    (barra-trelica espec-trelica (car a-i) (car a-s))
    (barra-trelica espec-trelica (car b-i) (car b-s))
    (barra-trelica espec-trelica (car a-i) (car b-s))
    (barra-trelica espec-trelica (car c-i) (car b-s))
    (barra-trelica espec-trelica (car a-i) (cadr b-i))
    (barra-trelica espec-trelica (car c-i) (cadr b-i))
    ;diagonais finais
    (barra-trelica espec-trelica (last a-s) (last a-i))
    (barra-trelica espec-trelica (last b-s) (last b-i))
    (barra-trelica espec-trelica (last b-s) (last a-i))
    (barra-trelica espec-trelica (last b-s) (last c-i))
    (barra-trelica espec-trelica (list-ref b-i (- (length b-i)2)) (last a-i))
    (barra-trelica espec-trelica (list-ref b-i (- (length b-i)2)) (last c-i))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-1 a-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 b-i)))
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-2 a-i) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 b-s)))
    ;diagonais faciais inferiores longitudinais 1a metade
    (barras-trelica (cdr (metade-1 b-i)) (cddr (metade-1 a-i)))
    (barras-trelica (cdr (metade-1 b-i)) (cddr (metade-1 c-i)))
    ;diagonais faciais inferiores longitudinais 2a metade
    (barras-trelica (drop-right (metade-2 a-i) 1) (cdr (drop-right (metade-2 b-i) 1)))
    (barras-trelica (drop-right (metade-2 c-i) 1) (cdr (drop-right (metade-2 b-i) 1)))
    ;diagonais faciais superiores longitudinais 1a metade
    (barras-trelica (metade-1 b-s) (cdr (metade-1 a-s)))
    (barras-trelica (metade-1 b-s) (cdr (metade-1 c-s)))
    ;diagonais faciais superiores longitudinais 2a metade
    (barras-trelica (metade-2 a-s) (cdr (metade-2 b-s)))
    (barras-trelica (metade-2 c-s) (cdr (metade-2 b-s)))
    ;diagonais espaciais 1a metade
    (barras-trelica (metade-1 b-s) (cddr (metade-1 a-i)))
    (barras-trelica (metade-1 b-s) (cddr (metade-1 c-i)))
    ;diagonais espaciais 2a metade
    (barras-trelica (drop-right (metade-2 a-i) 1) (cdr (metade-2 b-s)))
    (barras-trelica (drop-right (metade-2 c-i) 1) (cdr (metade-2 b-s)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (car c-i) (car c-s))
          ;diagonais faciais intermédias
          (barras-trelica (metade-1 c-s) (cddr (metade-1 c-i)))
          (barras-trelica (metade-2 c-i) (cdr (metade-2 c-s)))
          ;diagonal facial inicial
          (barra-trelica espec-trelica (last c-s) (last c-i)))
        ;recursão
        (barras-trelica-pratt espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;6th - Planar Howe

(define (barras-trelica-howe-planar espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais transversais
    ;(barras-trelica (cdr a-i) b-s)
    ;(barras-trelica b-s (cdr c-i))
    
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 a-s))
    (barras-trelica (metade-1 b-i) (metade-1 b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-2 a-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 b-i)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (metade-1 c-i) (metade-1 c-s))
          (barras-trelica (metade-2 c-s) (cdr (metade-2 c-i))))
        ;recursão
        (barras-trelica-howe-planar espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;7th - Planar Howe + transversal bracing

(define (barras-trelica-howe-transversal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 a-s))
    (barras-trelica (metade-1 b-i) (metade-1 b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-2 a-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 b-i)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (metade-1 c-i) (metade-1 c-s))
          (barras-trelica (metade-2 c-s) (cdr (metade-2 c-i))))
        ;recursão
        (barras-trelica-howe-transversal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;8th - Planar Howe + transversal bracing + diagonal bracing

(define (barras-trelica-howe-diagonal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 a-s))
    (barras-trelica (metade-1 b-i) (metade-1 b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-2 a-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 b-i)))

    ;diagonais espaciais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 b-s))
    (barras-trelica (metade-1 c-i) (metade-1 b-s))
    ;diagonais espaciais decrescentes
    (barras-trelica (metade-2 b-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 c-i)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (metade-1 c-i) (metade-1 c-s))
          (barras-trelica (metade-2 c-s) (cdr (metade-2 c-i))))
        ;recursão
        (barras-trelica-howe-diagonal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;9th - Howe

(define (barras-trelica-howe espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 a-s))
    (barras-trelica (metade-1 b-i) (metade-1 b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (metade-2 a-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 b-i)))
    ;diagonais faciais inferiores longitudinais metade 1
    (barras-trelica (metade-1 a-i) (cdr (metade-1 b-i)))
    (barras-trelica (metade-1 c-i) (cdr (metade-1 b-i)))
    ;diagonais faciais inferiores longitudinais metade 2
    (barras-trelica (metade-2 b-i) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-i) (cdr (metade-2 c-i)))
     ;diagonais faciais superiores longitudinais metade 1
    (barras-trelica (metade-1 a-s) (cdr (metade-1 b-s)))
    (barras-trelica (metade-1 c-s) (cdr (metade-1 b-s)))
    ;diagonais faciais superiores longitudinais metade 2
    (barras-trelica (metade-2 b-s) (cdr (metade-2 a-s)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 c-s)))
    ;diagonais espaciais crescentes
    (barras-trelica (metade-1 a-i) (metade-1 b-s))
    (barras-trelica (metade-1 c-i) (metade-1 b-s))
    ;diagonais espaciais decrescentes
    (barras-trelica (metade-2 b-s) (cdr (metade-2 a-i)))
    (barras-trelica (metade-2 b-s) (cdr (metade-2 c-i)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (metade-1 c-i) (metade-1 c-s))
          (barras-trelica (metade-2 c-s) (cdr (metade-2 c-i))))
        ;recursão
        (barras-trelica-howe espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;10th - Planar Warren with verticals

(define (barras-trelica-warren-verticals-planar espec-trelica pontos-i pontos-s)
  ;(printf "(length pontos-i) = ~a (length pontos-s) = ~a\n" (length pontos-i) (length pontos-s))
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (impares a-i) (impares a-s))
    (barras-trelica (impares b-i) (impares b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (impares a-s) (pares (cdr a-i)))
    (barras-trelica (impares b-s) (pares (cdr b-i)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (impares c-i) (impares c-s))
          (barras-trelica (impares c-s) (pares (cdr c-i))))
        ;recursão
        (barras-trelica-warren-verticals-planar espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;11th - Planar Warren with verticals + transversal bracing

(define (barras-trelica-warren-verticals-transversal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))

    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (impares a-i) (impares a-s))
    (barras-trelica (impares b-i) (impares b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (impares a-s) (pares (cdr a-i)))
    (barras-trelica (impares b-s) (pares (cdr b-i)))

    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (impares c-i) (impares c-s))
          (barras-trelica (impares c-s) (pares (cdr c-i))))
        ;recursão
        (barras-trelica-warren-verticals-transversal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;12th - Planar Warren with verticals + transversal + diagonal bracing

(define (barras-trelica-warren-verticals-diagonal espec-trelica pontos-i pontos-s)
  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)

    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))

    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (impares a-i) (impares a-s))
    (barras-trelica (impares b-i) (impares b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (impares a-s) (pares (cdr a-i)))
    (barras-trelica (impares b-s) (pares (cdr b-i)))

    ;diagonais espaciais crescentes
    (barras-trelica (impares a-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr c-i)))
    ;diagonais espaciais decrescentes
    (barras-trelica (impares c-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr a-i)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (impares c-i) (impares c-s))
          (barras-trelica (impares c-s) (pares (cdr c-i))))
        ;recursão
        (barras-trelica-warren-verticals-diagonal espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;13th - Warren with verticals

(define (barras-trelica-warren-verticals espec-trelica pontos-i pontos-s)
  ; (printf "barras-trelica-warren-verticals")

  (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((a-i (car pontos-i))
        (b-i (cadr pontos-i))
        (c-i (caddr pontos-i))
        (a-s (car pontos-s))
        (b-s (cadr pontos-s))
        (c-s (caddr pontos-s)))
    ;arestas verticais
    (barras-trelica (cdr a-i) a-s)
    (barras-trelica (cdr b-i) b-s)
    ;arestas longitudinais
    (barras-trelica a-i (cdr a-i))
    (barras-trelica b-i (cdr b-i))
    (barras-trelica a-s (cdr a-s))
    (barras-trelica b-s (cdr b-s))
    ;arestas transversais
    (barras-trelica a-i b-i)
    (barras-trelica b-i c-i)
    (barras-trelica a-s b-s)
    (barras-trelica b-s c-s)
    ;diagonais faciais laterais transversais
    (barras-trelica (cdr a-i) b-s)
    (barras-trelica b-s (cdr c-i))
    ;diagonais faciais laterais longitudinais crescentes
    (barras-trelica (impares a-i) (impares a-s))
    (barras-trelica (impares b-i) (impares b-s))
    ;diagonais faciais laterais longitudinais decrescentes
    (barras-trelica (impares a-s) (pares (cdr a-i)))
    (barras-trelica (impares b-s) (pares (cdr b-i)))
    ;diagonais faciais inferiores longitudinais crescentes
    (barras-trelica (impares a-i) (pares b-i))
    (barras-trelica (pares b-i) (pares (cdr c-i)))
    ;diagonais faciais inferiores longitudinais decrescentes
    (barras-trelica (pares b-i) (pares (cdr a-i)))
    (barras-trelica (impares c-i) (pares b-i))
    ;diagonais faciais superiores longitudinais crescentes
    (barras-trelica (impares a-s) (pares b-s))
    (barras-trelica (pares b-s) (pares (cdr c-s)))
    ;diagonais faciais superiores longitudinais decrescentes
    (barras-trelica (impares c-s) (pares b-s))
    (barras-trelica (pares b-s) (pares (cdr a-s)))
    ;diagonais espaciais crescentes
    (barras-trelica (impares a-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr c-i)))
    ;diagonais espaciais decrescentes
    (barras-trelica (impares c-i) (impares b-s))
    (barras-trelica (impares b-s) (pares (cdr a-i)))
    (if (null? (cdddr pontos-i))
        ;fachada posterior
        (begin
          ;arestas verticais
          (barras-trelica (cdr c-i) c-s)
          ;arestas longitudinais
          (barras-trelica c-i (cdr c-i))
          (barras-trelica c-s (cdr c-s))
          ;diagonais faciais
          (barras-trelica (impares c-i) (impares c-s))
          (barras-trelica (impares c-s) (pares (cdr c-i))))
        ;recursão
        (barras-trelica-warren-verticals espec-trelica (cddr pontos-i) (cddr pontos-s)))
    ))

;14th - Warren

(define (barras-trelica-warren espec-trelica pontos-i pontos-s)
    (define (barras-trelica ps qs)
    (for/list ((p ps) (q qs))
      (barra-trelica espec-trelica p q)))
  (let ((as (car pontos-s))
        (bs (car pontos-i))
        (cs (cadr pontos-s)))
    (barras-trelica as cs)
    (barras-trelica bs as)
    (barras-trelica bs cs)
    (barras-trelica bs (cdr as))
    (barras-trelica bs (cdr cs))
    (barras-trelica (cdr as) as)
    (barras-trelica (cdr bs) bs)
    (if (null? (cdr pontos-i))
        (barras-trelica (cdr cs) cs)
        (begin
          (barras-trelica bs (cadr pontos-i))
          (barras-trelica-warren espec-trelica (cdr pontos-i) (cdr pontos-s))))))

; (define (barras-trelica-warren espec-trelica pontos)
;     (define (barras-trelica ps qs)
;     (for/list ((p ps) (q qs))
;       (barra-trelica espec-trelica p q)))
;   (let ((as (car pontos))
;         (bs (cadr pontos))
;         (cs (caddr pontos)))
;     (barras-trelica as cs)
;     (barras-trelica bs as)
;     (barras-trelica bs cs)
;     (barras-trelica bs (cdr as))
;     (barras-trelica bs (cdr cs))
;     (barras-trelica (cdr as) as)
;     (barras-trelica (cdr bs) bs)
;     (if (null? (cdddr pontos))
;         (barras-trelica (cdr cs) cs)
;         (begin
;           (barras-trelica bs (cadddr pontos))
;           (barras-trelica-warren (cddr pontos))))))
; 

;;AUXILIARES

;;6th, 5th, 4th, 3rd, 2nd, 1st:

(define (metade-1 lista)
  (drop-right lista (/ (- (length lista) 1) 2)))

(define (metade-2 lista)
  (drop lista (/ (- (length lista) 1) 2)))

;Dada uma lista devolve a lista dos elementos de ordem ímpar sendo o 
;primeiro elemento o número 1

(define (impares lista)
  (if (null? lista)
      (list)
      (if (null? (cdr lista))
          (list (car lista))
          (cons (car lista) (impares (cddr lista))))))

;Dada uma lista devolve a lista dos elementos de ordem ímpar sendo o 
;primeiro elemento o número 1

(define (pares lista)
  (if (null? lista)
      (list)
      (impares (cdr lista))))
