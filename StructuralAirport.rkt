#lang racket

(require "FunctionsAirport.rkt")
(require "TrussesAirport.rkt")
(require "TrussesRobotAirport.rkt")
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

;;GLOBAIS

;Constantes
(define factor 100) ;factor
(define caso 1) ;número da combinação
(define testes 5) ;Número de profundidades aleatórias a gerar
(define n-grids 1) ;Número de grids/samples para o Latin Hypercube Sampling, (- p-max p-min) deve ser divisivel por n-grids
(define p-min 11.00) ;Profundidade Mínima
(define p-max 11.14) ;Profundidade Máxima
(define casas-decimais 2) ;Para arredondar no random
(define red (rgb 255 0 0)) ;cor vermelha (stress)
(define ficheiro "StructuralAirport.txt") ;ficheiro texto onde os resultados são guardados

; Geração de um numero aleatorio entre min e max
(define (constrained-random min max)
  (+ min (* (- max min)(base-random))))

;Geração de profundidades aleatórias entre 0.10 e 1.00.
;Tenta gerar 'testes' valores, mas remove valores duplicados.
;Devolve os valores ordenados em ordem crescente
(define (full-random)
  (sort
   (remove-duplicates
    (for/list ((i testes))
      (string->number
       (real->decimal-string
        (constrained-random p-min p-max) casas-decimais)))) <))

;Geração de profundidades entre 'p-min' e 'p-max' utilizando Latin Hypercube Sampling.
;Gera um valor para cada grid, sendo o número de grids definido por 'n-grids'
(define (latin-hypercube-sampling)
  (let* ((delta (/ (- p-max p-min) n-grids))
         (curr-min (- p-min delta)))
    (sort (for/list ((i n-grids))
            (set! curr-min (+ curr-min delta))
            (string->number
             (real->decimal-string
              (constrained-random curr-min (+ curr-min delta)) casas-decimais))) <)))

;guarda os resultados no ficheiro .txt
(define (store-single-result tipo p l r)
  (let ((s (list (format "~a - ~a,~a,~a\n" tipo p l r))))
    (display-lines-to-file s ficheiro #:exists 'append)
    r))

;;ANÁLISE

;Função que gera a trelica e devolve o UZ Maximum Displacement
(define (analise tipo n-trelicas larg-trelicas n-interv fa fb)
  (define heights #;(latin-hypercube-sampling) '(15.00)) ;treliça(s) de altura(s) a analisar
  #;'(9.00 11.00 13.00 15.00 17.00 19.00 21.00 23.00 25.00 27.00 29.00 31.00)
  (for/list ((height heights))
    (printf "heights = ~a\n" heights)
    (printf "height = ~a\n" height)
    (define b-ts height)
    (printf "b-ts = ~a\n" b-ts)
    
    (define b-ti (- b-ts 2))         ;medidas cobertura
    (define bs-s (+ b-ts 0.3))       ;medidas cobertura
    (define bs-i (- b-ti 0.3))       ;medidas cobertura
    (printf "b-ti = ~a\n" b-ti)
    
    (define comp (* n-trelicas larg-trelicas))
    (define n-trelicas-duplas (/ n-trelicas 2))
    (define larg-trelica-duplas (/ comp n-trelicas-duplas))
    (define n-secoes (/ n-interv 2))
    #;(define larg-trelica (/ comp n-trelicas))
    
    ;volumes materialidade
    (define v-cob-total (* comp ;volume total painéis de metal/cobertura sem buracos
                           (+
                            (area-ext-sup-param
                             (λ (t) (* as-s (expt (expt (cos t) 2) (/ 1.0 n)) (sgn (cos t))))
                             (λ (t) (* bs-s (expt (expt (sin t) 2) (/ 1.0 n)) (sgn (sin t))))
                             (- pi/6) (* 7 pi/6) (- esp-cob) 100 0.00001)
                            (area-ext-sup-param
                             (λ (t) (* as-i (expt (expt (cos t) 2) (/ 1.0 n)) (sgn (cos t))))
                             (λ (t) (* bs-i (expt (expt (sin t) 2) (/ 1.0 n)) (sgn (sin t))))
                             (- pi/6) (* 7 pi/6) esp-cob 100 0.00001))))

    (printf "v-cob-total = ~a m3\n" v-cob-total)

    (define v-hexag (* v-cob-total 0.2747)) ;volume total painéis de vidro hexagonais
    (printf "v-hexag = ~a m3\n" v-hexag)

    (define v-cob (- v-cob-total v-hexag))   ;volume total painéis de metal/cobertura
    (printf "v-cob = ~a m3\n\n" v-cob)
    
    ;criação de nós inferiores para treliça
    (define f-nos-p (superel-var-base-plana p-ti psi0-ti (fa a-ti comp) #;fa-ti
                                            (fb b-ti comp) n))
    ; (define nos-i (mat-sup f-nos-i psi0-ti psi1-ti
;                            (* 2 n-secoes) f-dist-t-ti
;                            0 comp (* 2 n-trelicas) (λ(t) t)))

    (define nos-p
      (if (equal? tipo 'warren)
          (cria-nos-p-warren n-trelicas larg-trelicas f-nos-p psi0-ti psi1-ti
                             n-interv n-nos-ret f-dist-t-ts)
          (cria-nos-p n-trelicas larg-trelicas f-nos-p psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts)))
    ;criação de nós superiores para treliça
    (define f-nos-s (superel-var-base-plana p-ts psi0-ts (fa a-ts comp) #;fa-ts
                                            (fb b-ts comp) n))
    ; (define nos-s (mat-sup f-nos-s psi0-ts psi1-ts
;                            (- (* 2 n-secoes) 2) f-dist-t-ts
;                            0 comp (* 2 n-trelicas) (λ(t) t)))

    (define nos-s
      (if (equal? tipo 'warren)
          (cria-nos-s n-trelicas larg-trelicas f-nos-s psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts f-ajust)
          (cria-nos-s n-trelicas larg-trelicas f-nos-s psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts f-ajust)))
    (printf "n-trelicas-duplas = ~a larg-trelica-duplas = ~a\n" n-trelicas-duplas larg-trelica-duplas)
    (printf "(length nos-p) = ~a (length nos-s) = ~a\n" (length nos-p) (length nos-s))
    (define (calc-load n-nos comp-barras)
      (define carga-barras
        (* area-tubo-aco
           peso-esp-aco comp-barras))
      (define carga-cob
        (+ (* peso-esp-vidro v-hexag)
           (* peso-esp-cob v-cob)))
      #;(printf "carga-barras = ~a N\n" carga-barras)
      #;(printf "carga-cob = ~a N\n" carga-cob)
      (/ (+ (- carga-barras) (- carga-cob)) n-nos))
    (set-n-nos 0)
    #;(printf "n-nos-inicial = ~a\n" n-nos)
    (set-comp-barras 0)
    #;(printf "comp-barras-inicial = ~a\n" comp-barras)
    (set-robot #f)
    (trelica tipo #f nos-p nos-s)
    #;(printf "n-nos-final = ~a\n" n-nos)
    #;(printf "comp-barras-final = ~a\n" comp-barras)
    (define load (calc-load n-nos comp-barras))
    (printf "load = ~a N\n\n" load)
    (set-robot #t)
    (define expressao '(trelica tipo #f nos-p nos-s))
    (with-robot-analysis (results)
      (begin
        (delete-all-shapes)
        #;expressao (trelica tipo #f nos-p nos-s))
      (vz load) ;5000N = 5KN
      (list
       tipo
       height
       load
       (store-single-result
        tipo height load
        (apply min
               (for/list ((node (in-hash-values (%added-nodes))))
                 (cz (v*r (%node-displacement-vector
                           results
                           (%truss-node-data-id node)
                           caso)
                          factor)))))))))

;função que otimiza a altura da treliça
(define (optimal-truss tipo n-trelicas larg-trelicas n-interv fa fb)
  (let ((results
         (analise tipo n-trelicas larg-trelicas n-interv fa fb)))
       (list results
             (argmax fourth results))))

;função que cria os modelos de flecha ou stress da treliça de deformada otimizada
(define (analise-optimal tipo n-trelicas larg-trelicas n-interv fa fb opcao)
  (let ((results
         (analise tipo n-trelicas larg-trelicas n-interv fa fb)))

    (display (list results
               (argmax fourth results)))

    (define b-ts (cadr (argmax fourth results)))
    (printf "b-ts = ~a\n" b-ts)
    (define load (caddr (argmax fourth results)))
    (printf "load = ~a N\n" load)
    
    (define b-ti (- b-ts 2))         ;medidas cobertura
    (define bs-s (+ b-ts 0.3))       ;medidas cobertura
    (define bs-i (- b-ti 0.3))       ;medidas cobertura
    (printf "b-ti = ~a\n" b-ti)

    (define comp (* n-trelicas larg-trelicas))
    (define n-trelicas-duplas (/ n-trelicas 2))
    (define larg-trelica-duplas (/ comp n-trelicas-duplas))
    (define n-secoes (/ n-interv 2))
    #;(define larg-trelica (/ comp n-trelicas))
    
    ;criação de nós inferiores para treliça
    (define f-nos-p (superel-var-base-plana p-ti psi0-ti fa-ti
                                            (fb b-ti comp) n))
    ; (define nos-i (mat-sup f-nos-i psi0-ti psi1-ti
;                            (* 2 n-secoes) f-dist-t-ti
;                            0 comp (* 2 n-trelicas) (λ(t) t)))

    (define nos-p
      (if (equal? tipo 'warren)
          (cria-nos-p-warren n-trelicas larg-trelica f-nos-p psi0-ti psi1-ti
                             n-interv n-nos-ret f-dist-t-ts)
          (cria-nos-p n-trelicas larg-trelica f-nos-p psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts)))
    
    ;criação de nós superiores para treliça
    (define f-nos-s (superel-var-base-plana p-ts psi0-ts fa-ts
                                            (fb b-ts comp) n))
    ; (define nos-s (mat-sup f-nos-s psi0-ts psi1-ts
;                            (- (* 2 n-secoes) 2) f-dist-t-ts
;                            0 comp (* 2 n-trelicas) (λ(t) t)))

    (define nos-s
      (if (equal? tipo 'warren)
          (cria-nos-s n-trelicas larg-trelica f-nos-s psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts f-ajust)
          (cria-nos-s n-trelicas larg-trelica f-nos-s psi0-ti psi1-ti
                      n-interv n-nos-ret f-dist-t-ts f-ajust)))
     
    (with-robot-analysis (results)
      (begin
        (delete-all-shapes)
        (trelica tipo #f nos-p nos-s))
      (vz load)
    
    (case opcao
      ((deflection-model)
       (let* ((node-radius raio-no-trelica #;0.04)
              (bar-radius raio-barra-trelica #;0.02)
              (factor 100)
              (node-displacement (lambda (node)
                                   (let ((node-id (%truss-node-data-id node)))
                                     (let ((d (%node-displacement
                                               (%Displacements (%nodes results)) node-id 1)))
                                       (v*r (vxyz (%UX d) (%UY d) (%UZ d)) factor))))))

         (printf "node-radius = ~a\n" node-radius)
         (printf "bar-radius = ~a\n" bar-radius)
         
         (for ((node (in-hash-values (%added-nodes))))
           (let ((node-id (%truss-node-data-id node)))
             (let ((d (node-displacement node)))
               (let ((p (%truss-node-data-loc node)))
                 (ac:sphere p node-radius)
                 (ac:shape-color (ac:sphere (p+v p d) node-radius) red)))))
         (for ((bar (in-hash-values (%added-bars))))
           (let ((node0 (%truss-bar-data-node0 bar))
                 (node1 (%truss-bar-data-node1 bar)))
             (let ((p0 (%truss-node-data-loc node0))
                   (p1 (%truss-node-data-loc node1)))
               (ac:cylinder p0 bar-radius p1)
               (let ((d0 (node-displacement node0))
                     (d1 (node-displacement node1)))
                 (ac:shape-color (ac:cylinder (p+v p0 d0) bar-radius (p+v p1 d1)) red)))))))
      ((stress-model)
       (let* ((node-radius raio-no-trelica #;0.08)
              (bar-radius raio-barra-trelica #;0.04)
              (factor 100))
         (for ((node (in-hash-values (%added-nodes))))
           (let ((node-id (%truss-node-data-id node)))
             (let ((p (%truss-node-data-loc node)))
               (ac:sphere p node-radius))))
         (let* ((bars (hash-values (%added-bars)))
                (bars-id (map %truss-bar-data-id bars))
                (bars-stress (map (lambda (bar-id)
                                    (abs (%bar-max-stress results bar-id 1)))
                                  bars-id)))
           (let ((max-stress (argmax identity bars-stress))
                 (min-stress (argmin identity bars-stress)))
             (for ((bar (in-list bars))
                   (bar-in (in-list bars-id))
                   (bar-stress (in-list bars-stress)))
               (let ((node0 (%truss-bar-data-node0 bar))
                     (node1 (%truss-bar-data-node1 bar)))
                 (let ((p0 (%truss-node-data-loc node0))
                       (p1 (%truss-node-data-loc node1)))
                   (ac:shape-color (ac:cylinder p0 bar-radius p1)
                                   (color-in-range bar-stress min-stress max-stress)))))))))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUÇÕES

; 1ST   'wout-bracing
; 2ND   'pratt-planar
; 3RD   'pratt-transversal
; 4TH   'pratt-diagonal
; 5TH   'pratt
; 6TH   'howe-planar
; 7TH   'howe-transversal
; 8TH   'howe-diagonal
; 9TH   'howe
; 10TH  'warren-verticals-planar
; 11TH  'warren-verticals-transversal
; 12TH  'warren-verticals-diagonal
; 13TH  'warren-verticals
; 14TH  'warren


; 1ST   10 6 36 2
; 2ND   16 3.75 54 0.5
; 3RD   20 3 72 0.5
; 
; 4TH   2 2 4
; 5TH   4 2 10


; 1ST   (λ(a comp)(λ(y) a)) (λ(b comp)(λ(y) b))
; 2ND   (λ(a comp)(λ(y) a)) (λ(b comp)(fsin -3 (* 3/2 comp) 0 b))
; 3RD   (λ(a comp)(λ(y) a)) (λ(b comp)(fsin 3 (* 2/3 comp) 0 b))
; 
; 4TH   (λ(a comp)(λ(y) a))(λ(a comp)(fsin 3 (* 1 comp) 0 a))
; 
; 5TH   (λ(a comp)(fsin 3 (* 2/3 comp) 0 a)) (λ(b comp)(fsin 3 (* 1 comp) 0 b))
; 6TH   (λ(a comp)(fsin 3 (* 2/3 comp) pi a)) (λ(b comp)(fsin 3 (* 2/3 comp) 0 b))


;;ANÁLISE ROBOT

; (analise #;'warren-verticals #;'warren 'warren-verticals-planar
;          ;20 3 72
;          ;16 3.75 54
;          10 6 36
;           #;(λ(a comp)(fsin 3 (* 1 comp) 0 a))
;           ;(λ(a comp)(fsin 3 (* 2/3 comp) 0 a)) (λ(b comp)(fsin 3 (* 1 comp) 0 b))
;           ;(λ(a comp)(fsin 3 (* 2/3 comp) pi a)) (λ(b comp)(fsin 3 (* 2/3 comp) 0 b))
;           (λ(a comp)(λ(y) a)) (λ(b comp)(λ(y) b))
;           #;(λ(b comp)(fsin 3 (* 2/3 comp) 0 b)))


                                        ;2 2 4
                                        ;4 2 10
                                        ;20 3 72
                                        ;16 3.75 54
                                        ;10 6 36
; (optimal-truss #;'pratt-planar #;'warren 'warren-verticals-planar
;                10 6 36
;                (λ(a comp)(λ(y) a))
;                (λ(b comp)(λ(y) b)) #;(λ(b comp)(fsin 3 (* 2/3 comp) 0 b)))


(analise-optimal #;'warren 'warren-verticals-planar
                 10 6 36
                 (λ(a comp)(λ(y) a))
                 (λ(b comp)(λ(y) b))
                 #;'deflection-model 'stress-model)
