#lang racket

(provide (all-defined-out))
(require "FunctionsAirport.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RHINO/AUTOCAD

(require rosetta/rhino)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;LAYERS

(define black-background-layer (create-layer "Black Background"))
(define surface-layer (create-layer "Surface"))
(define sensor-nodes-layer (create-layer "Sensor nodes"))
(define colors-layer (create-layer "Colors"))
(define openings-layer (create-layer "Openings"))
(define janela-layer (create-layer "Window"))
(define window-frame-layer (create-layer "Window frame"))
(define ground-floor-layer (create-layer "Ground floor"))
(define chao-layer (create-layer "Ground"))
(define asphalt-layer (create-layer "Asphalt"))
(define guard-layer (create-layer "Guard"))
(define guard-support-layer (create-layer "Guard support"))

;;GEOMETRIA

;;;BLACK BACKGROUND

(define (black-background)
    (with-current-layer black-background-layer
      (surface (closed-line (xyz -1000 200 -110) (xyz 1000 200 -110)
                          (xyz 1000 200 500) (xyz -1000 200 500)))
      (surface (closed-line (xyz -1000 210 -100) (xyz 1000 210 -100)
                          (xyz 1000 -500 -100) (xyz -1000 -500 -100)))))

;;;COBERTURA

;;;;RAIO LUMINOSO

(define (desenha-raio-luminoso posicao-raio-luminoso raio-luminoso)
  (define extremo (+c posicao-raio-luminoso raio-luminoso))
  (cylinder posicao-raio-luminoso 0.05 extremo)
  (cone extremo 0.1 (+c extremo (*c raio-luminoso 1/4))))

;;;;ATRATOR

(define (desenha-atrator posicao-atrator raio-atrator)
  (sphere posicao-atrator raio-atrator))

;;;FORMA DA ABERTURA

;Cria uma forma hexagonal para a abertura dado o ponto central o vetor normal nesse ponto, o valor
; radiação e o método de cálculo desse valor em função da mesma

;valor-abert - função que dá o valor da abertura em função da radiação

(define (abert-hexag valor-abert larg-par-filas fl fs)
  (λ(p n rad)
    (define ux (versor (xz (cz n) (- (cx n))))) ;vetor unitário perpendicular ao vetor normal contido num plano
    ; paralelo ao xOz
    (define vx (*c ux (* 1/2 (valor-abert rad)))); vetor com a direção do eixo variável do hexágono e de
    ; comprimento igual a metade desse eixo
    (define uy (versor (yz (cz n) (- (cy n))))) ;vetor unitário perpendicular ao vetor normal contido num plano
    ; paralelo ao yOz
    (define vl (*c uy (* 1/2 fl larg-par-filas))) ;vetor  perpendicular ao vetor normal com a direção de Oy que é a
    ;do eixo fixo do hexágono e de comprimento igual a metade desse eixo
    (define vs (*c vl fs)) ;vetor  perpendicular ao vetor normal com a direção de Oy e que determina
    ;o comprimento dos lados do hexágono segundo Oy
    (define q (-c p vs)) ;ponto auxiliar para obtenção dos vértices b e d do hexágono
    (define r (+c p vs)) ;ponto auxiliar para obtenção dos vértices c e e do hexágono
    (define a (-c p vl))
    (define b (+c q vx))
    (define c (+c r vx))
    (define d (+c p vl))
    (define e (-c r vx))
    (define f (-c q vx))
    (closed-line a b c d e f)))

;Cria uma forma elipsoidal para a abertura baseada num hexágonodado o ponto central o vetor normal nesse ponto,
; o valor da radiação e o método de cálculo desse valor em função da mesma

(define (abert-elips valor-abert larg-par-filas fl fs)
  (λ(p n rad)
    (define ux (versor (xz (cz n) (- (cx n))))) ;vetor unitário perpendicular ao vetor normal contido num plano
    ; paralelo ao xOz
    (define vx (*c ux (* 1/2 (valor-abert rad)))); vetor com a direção do eixo variável do hexágono e de
    ; comprimento igual a metade desse eixo
    (define uy (versor (yz (cz n) (- (cy n))))) ;vetor unitário perpendicular ao vetor normal contido num plano
    ; paralelo ao yOz
    (define vl (*c uy (* 1/2 fl larg-par-filas))) ;vetor  perpendicular ao vetor normal com a direção de Oy que é a
    ;do eixo fixo do hexágono e de comprimento igual a metade desse eixo
    (define vs (*c vl fs)) ;vetor  perpendicular ao vetor normal com a direção de Oy e que determina
    ;o comprimento dos lados do hexágono segundo Oy
    (define q (-c p vs)) ;ponto auxiliar para obtenção dos vértices b e d do hexágono
    (define r (+c p vs)) ;ponto auxiliar para obtenção dos vértices c e e do hexágono
    (define a (-c p vl))
    (define b (+c q vx))
    (define c (+c r vx))
    (define d (+c p vl))
    (define e (-c r vx))
    (define f (-c q vx))
    (spline a b c d e f a)))
  
;;;CRIAÇÃO DA ABERTURA

;Dadas a forma pretendida para a abertura e o método de cálculo do respetivo valor e a matriz
; (lista de listas) de pontos cria uma abertura em cada ponto

(define (cria-aberturas forma-abert mat-abert)
  (define (cria-abert p+n+r)
    (define p (car p+n+r))
    (define n (cadr p+n+r))
    (define rad (caddr p+n+r))
    (define vn (vxyz (- (cx n)) (- (cy n)) (- (cz n))))
    (define p1 (+c p (*c n 0.5)))
    (extrusion (surface (forma-abert p1 n rad)) vn))
  (define (cria-abert-fila lista-pontos)
    (map cria-abert lista-pontos))
  (map cria-abert-fila mat-abert))

;Dadas a forma pretendida para a abertura e o método de cálculo do respetivo valor sem utilizar a radiação
;incidente e a matriz (lista de listas) de pontos, cria uma abertura em cada ponto

(define (cria-aberturas-SAL forma-abert mat-abert)
  (define (cria-abert p+n)
    (define p (car p+n))
    (define n (cadr p+n))
    (define rad 0)
    (define vn (vxyz (- (cx n)) (- (cy n)) (- (cz n))))
    (define p1 (+c p (*c n 0.5)))
    (extrusion (surface (forma-abert p1 n rad)) vn))
  (define (cria-abert-fila lista-pontos)
    (map cria-abert lista-pontos))
  (map cria-abert-fila mat-abert))

;;VISUALIZAÇÕES

;Cria uma imagem da superfície e das respetivas aberturas

(define (ve-sup+abert mat-sup mat-abert forma-abert)
  (define (ve-abert p+n+l)
    (define p (car p+n+l))
    (define n (cadr p+n+l))
    (define rad (caddr p+n+l))
    (forma-abert p n rad ))
  (define (ve-fila-abert lista-pontos)
    (map ve-abert lista-pontos))
  (surface-grid mat-sup)
  (map ve-fila-abert mat-abert))

;Dadas a forma pretendida para a abertura e o método de cálculo do respetivo valor sem utilizar a radiação
;incidente e a matriz (lista de listas) de pontos, visualiza uma abertura em cada ponto

(define (ve-sup+abert-SAL mat-sup mat-abert forma-abert)
  (define (ve-abert p+n)
    (define p (car p+n))
    (define n (cadr p+n))
    (define rad 0)
    (forma-abert p n rad))
  (define (ve-fila-abert lista-pontos)
    (map ve-abert lista-pontos))
  (with-current-layer surface-layer (surface-grid mat-sup))
  (map ve-fila-abert mat-abert))

;Cria uma imagem dos pontos da matriz que define a superfície

(define (ve-mat-sup mat)
  (define (ve-ponto p)
    (sphere p .2))
  (define (ve-fila lista-pontos)
    (map ve-ponto lista-pontos))
  (map ve-fila mat))

;Cria uma imagem dos pontos sensores

(define (ve-pontos-sensores mat)
  (define (ve-ponto p+n)
    ;(sphere (car p+n) .1)
    (point (car p+n))
    )
  (define (ve-fila lista-pontos)
    (map ve-ponto lista-pontos))
  (map ve-fila mat))

;Cria uma imagem dos pontos sensores e respetivas normais

(define (ve-sensores mat)
  (define (ve-ponto-normal p+n)
    (sphere (car p+n) .01)
    (line (car p+n) (+c (car p+n) (cadr p+n))))
  (define (ve-fila lista-pontos)
    (map ve-ponto-normal lista-pontos))
  (map ve-fila mat))

;;;ELEMENTOS GEOMÉTRICOS

;;;;JANELAS

;Define uma função que para cada ordenada y relativa ao ponto base cria uma lista de pontos
; que definem uma linha fechada que representa um contorno de uma superfície fechada ao nível
; dessa ordenada relativa y.
;Parte desse contorno vai ter a forma de uma parte da superelipse tal como é usada para as superfícies
; da construção do edifício

;se - função superelipse utilizada
;psi0 - ângulo inicial para geração da linha em forma de superelipse
;psi1 - ângulo final para geração da linha em forma de superelipse
;n-pontos - número de pontos considerados para a porção do contorno definido pela superelipse

(define (pontos-contorno se psi0 psi1 n-pontos)
  (define n-interv (- n-pontos 1))
  (define (ponto-fila y)
    (λ(psi)(se psi y)))
  (λ(y)
    (define lista-pontos (map-division (ponto-fila y) psi0 psi1 n-interv))
    lista-pontos))

;Define uma função que para cada ordenada y relativa ao ponto base cria uma janela ao nível
; dessa ordenada relativa y.

;pontos-contorno-i - pontos que definem o contorno interior da moldura da janela e que
;                    constitui de facto o contorno da parte envidarçada
;pontos-contorno-e - pontos que definem o contorno exterior da moldura da janela
;p-i - ponto base da superfície inferior e que serve de ponto base para desenhar o vidro
;se-i - superelipse que se aplica à superfície inferior
;psi0-sup-i - parâmetro inicial para os pontos da superelipse inferior
;espess-moldura - espessura da moldura que envolve a janela
;espess-vidro - espessura do vidro da janela
;raio-caix-horiz - raio dos cilindros que constituem os caisilhos horizontais
;raio-caix-vert - raio dos cilindros que constituem os caisilhos verticais
;altura-painel - altura de cada painel de vidro (retângulo de vidro entre caixilhos consecutivos
;largura-painel - largura de cada painel de vidro (retângulo de vidro entre caixilhos consecutivos

(define (cria-janela
         pontos-contorno-i
         pontos-contorno-e
         p-i
         se-i
         psi0-sup-i
         espess-moldura
         espess-vidro
         raio-caix-horiz
         raio-caix-vert
         altura-painel
         largura-painel)
  (λ(y sentido)
    (define contorno-i (spline (pontos-contorno-i y)))
    (define contorno-e (spline (pontos-contorno-e y)))
    (with-current-layer janela-layer
      (extrusion
       (surface
        contorno-i
        (line (car (pontos-contorno-i y))
              (car (pontos-contorno-e y)))
        contorno-e
        (line (last (pontos-contorno-i y))
              (last (pontos-contorno-e y))))
        (vy (* sentido espess-vidro)))
      (extrusion
       (surface
        (line
         (car (pontos-contorno-i y))
         (last (pontos-contorno-i y)))
        (spline (pontos-contorno-i y)))
        (vy (* sentido espess-vidro))))
    (define x-max (cx (se-i 0 y)))
    (define x-min (cx (se-i pi y)))
    (define x-med (/ (+ x-min x-max) 2))
    (define z-min (cz (se-i psi0-sup-i y)))
    (define z-max (cz (se-i pi/2 y)))
    (define y-abs (+ (cy p-i) y (* sentido espess-vidro 1/2)))
    (define (cria-lista-caix-horiz z)
      (cond ((> z z-max)
             (list))
            (else
             (with-current-layer window-frame-layer
               (cons (cylinder (xyz x-min y-abs z) raio-caix-horiz (xyz x-max y-abs z))
                   (cria-lista-caix-horiz (+ z altura-painel)))))))
    (define (cria-lista-caix-vert x)
      (cond ((> x x-max)
             (list))
            (else
             (with-current-layer window-frame-layer
               (cons (cylinder (xyz x y-abs z-min) raio-caix-vert (xyz x y-abs z-max))
                     (cons (cylinder (xyz (- (* x-med 2) x) y-abs z-min) raio-caix-vert (xyz (- (* x-med 2) x) y-abs z-max))
                           (cria-lista-caix-vert (+ x largura-painel))))))))
    (define lista-caix-horiz (cria-lista-caix-horiz (+ z-min altura-painel)))
    (define lista-caix-vert (cria-lista-caix-vert (+ x-med (/ largura-painel 2))))
    (define lista-caix (append lista-caix-horiz lista-caix-vert))
    (define v-transl (vy (* 2 sentido (max raio-caix-horiz raio-caix-vert))))
    (define vol-caix
      (extrusion
       (move (surface (closed-line
                       (pontos-contorno-i y)))
             v-transl) (v*r v-transl -2)))
    (with-current-layer window-frame-layer
      (intersection vol-caix (union lista-caix)))))

;;;;;CHÃO

(define (laje x0 x1 r h-laje comp)
  (define x0-laje (+xz x0 4 3.9))
  (define x1-laje (+xz x1 -4 3.9))
  (with-current-layer ground-floor-layer
    (box x0-laje (+yz x1-laje comp (- h-laje)))))

(define (chao x0 x1 r h-chao comp)
  (define x0-chao (+xz x0 (- r) (- r)))
  (define x1-chao (+xz x1 r (- r)))
  (with-current-layer ground-floor-layer
    (box x0-chao (+yz x1-chao comp (- h-chao)))))

;;;;;COLUNAS E PAREDES

(define (coluna x0 x1 r d-coluna h-piso)
  (define x0-laje (+xz x0 4 3.9))
  (define x1-laje (+xz x1 -4 3.9))
  (with-current-layer ground-floor-layer
    (box (+z x0-laje -0.5)
         (+xyz x0-laje d-coluna d-coluna (- 23.9)))))

(define (colunas x0 x1 r h-laje d-coluna comp h-piso n-pisos
                 n-colunas-x n-colunas-y)
  (define x0-laje (+xz x0 4 3.9))
  (define x1-laje (+xz x1 -4 3.9))
  (define x1-x0-laje (distance x0-laje x1-laje))
  (map-division (λ(x y) (coluna (+xy x0 x y) (+xy x1 x y)
                                r d-coluna h-piso))
                0 (- x1-x0-laje d-coluna) n-colunas-x
                0 (- comp d-coluna) n-colunas-y))

(define (paredes x0 x1 r h-laje d-coluna comp h-piso n-pisos)
  (define x0-laje (+xz x0 4 3.5))
  (define x1-laje (+xz x1 -4 3.5))
  (with-current-layer ground-floor-layer
    (box (+z x0-laje (+ -4.6 r))
         (+xyz x0-laje (* d-coluna 2) comp (- 23.5)))
    (box (+xz x1-laje (- (* d-coluna 2)) (+ -4.6 r))
         (+yz x1-laje comp (- 23.5)))))

(define (apoios-trelica x0 x1 h-laje d-coluna r comp)
  (define x0-chao (+xz x0 (- r) (- r)))
  (define x0-laje (+xz x0 (+ (- r) 1.6) (- r)))
  (with-current-layer ground-floor-layer
    (extrusion
     (surface (line (+z x0-chao (/ (- h-laje) 2))
                    (+xz x0-chao 8 (/ (- h-laje) 2)))
              (spline (+xz x0-chao 8 (/ (- h-laje) 2))
                      (+xz x0-chao 6.5 (- (/ (- h-laje) 2) 1.7))
                      (+xz x0-chao (+ 4.3 (* d-coluna 2)) (- (/ (- h-laje) 2) 3.5)))
              (line (+xz x0-chao (+ 4.3 (* d-coluna 2)) (- (/ (- h-laje) 2) 3.5))
                    (+xz x0-chao 4.3 (- (/ (- h-laje) 2) 3.5)))
              (spline (+xz x0-chao 4.3 (- (/ (- h-laje) 2) 3.5))
                      (+xz x0-chao 2.7 (- (/ (- h-laje) 2) 1.7))
                      (+xz x0-chao 1 (- (/ (- h-laje) 2) 0.6))
                      (+z x0-chao (/ (- h-laje) 2))))
     comp)))

(define (guardas ponto-base x0 x1 r comp n-suportes)
  (define x0-x1 (distance x1 x0))
    (define (guarda x0 x1 r comp)
      (with-current-layer guard-layer
        (mirror
         (extrusion
         (surface
          (closed-line
           (+xz x0 (- 4.3 r) 3.9)
           (+xz x0 (- 4.35 r) 3.9)
           (+xz x0 (- 4.35 r) 4.9)
           (+xz x0 (- 4.3 r) 4.9)))
         (- comp))
         ponto-base (vx 1))))
  (define (suporte x0)
    (with-current-layer guard-support-layer
      (box (+xz x0 (- 4.3 r) 3.9)
           (+xyz x0 (- 4.35 r) 0.05 4.9))))
  (define x0-dist (+x x0 (- x0-x1 8.05)))
  (define (suportes x0)
    (map-division (λ(y)
                    (suporte (+y x0 y)))
                  0 comp n-suportes)
    (map-division (λ(y)
                    (suporte
                     (+y x0-dist y)))
                    0 comp n-suportes))
  (suportes x0)
  (guarda x0 x1 r comp))

;;;;ANDARES INFERIORES

(define (andares-inferiores ponto-base x0 x1 r h-laje d-coluna comp
                            h-piso n-pisos n-colunas-x n-colunas-y n-suportes)
  ;(printf "h-piso = ~a  n-pisos = ~a\n" h-piso n-pisos)
  (define x1-x0/2 (/ (distance x0 x1) 2))
  (with-current-layer ground-floor-layer
    (laje x0 x1 r (/ h-laje 2) comp)
    (chao x0 x1 r (/ h-laje 2) comp)
    (map-division (λ(z) (laje (+z x0 (- (- z) 8.2))
                              (+z x1 (- (- z) 8.2))
                              r h-laje comp))
                  0 (* h-piso n-pisos) n-pisos)
    (colunas x0 x1 r h-laje d-coluna comp h-piso n-pisos
             n-colunas-x n-colunas-y)
    (paredes x0 x1 r h-laje d-coluna comp h-piso n-pisos)
    (mirror (apoios-trelica x0 x1 h-laje d-coluna r comp) ponto-base (vx 1))
    (guardas ponto-base x0 x1 r comp n-suportes)))

;;;;TERRA

(define (terra x0 x1 r h-laje comp)
  (define x0-terra (+xz x0 4 (- (- (- r) h-laje) 3.6)))
  (define x1-terra (+xz x1 -4 (- (- (- r) h-laje) 3.6)))
  (with-current-layer chao-layer
    (extrusion
     (surface
      (closed-line (xyz -1000 (cy x0-terra) -100)
                   (xyz 1000 (cy x0-terra) -100)
                   (xyz 1000 (cy x1-terra) (cz x1-terra))
                   x1-terra
                   (+z x1-terra -15.6)
                   (+xz x1-terra -1.4 -15.6)
                   (+xz x1-terra -1.4 -14.9)
                   (+xz x1-terra -8.4 -14.9)
                   (+xz x1-terra -8.4 -15.6)
                   (+xz x1-terra -9.1 -15.6)
                   (+xz x1-terra -9.1 -14.9)
                   (+xz x1-terra -16.8 -14.9)
                   (+xz x1-terra -16.8 -15.6)
                   (+xz x0-terra 16.8 -15.6)
                   (+xz x0-terra 16.8 -14.9)
                   (+xz x0-terra 9.1 -14.9)
                   (+xz x0-terra 9.1 -15.6)
                   (+xz x0-terra 8.4 -15.6)
                   (+xz x0-terra 8.4 -14.9)
                   (+xz x0-terra 1.4 -14.9)
                   (+xz x0-terra 1.4 -15.6)
                   (+z x0-terra -15.6)
                   x0-terra
                   (xyz -1000 (cy x0-terra) (cz x0-terra))))
     (- comp))))

;;;;ASFALTO

(define (asfalto x0 x1 r h-laje comp)
  (define x0-asfalto (+xz x0 4 (- (- (- r) h-laje) 3.6)))
  (define x1-asfalto (+xz x1 -4 (- (- (- r) h-laje) 3.6)))
  (with-current-layer asphalt-layer
    (extrusion
     (surface
      (closed-line (xyz (cx x0-asfalto) (cy x1-asfalto) (cz x1-asfalto))
                   (xyz -1000 (cy x0-asfalto) (cz x0-asfalto))
                   (xyz -1000 (+ (cy x1-asfalto) comp) (cz x1-asfalto))
                   (xyz (cx x0-asfalto) (+ (cy x1-asfalto) comp) (cz x1-asfalto))))
     -0.05)
    (extrusion
     (surface
      (closed-line (xyz 1000 (cy x1-asfalto) (cz x1-asfalto))
                   (xyz (cx x1-asfalto) (cy x0-asfalto) (cz x0-asfalto))
                   (xyz (cx x1-asfalto) (+ (cy x1-asfalto) comp) (cz x1-asfalto))
                   (xyz 1000 (+ (cy x1-asfalto) comp) (cz x1-asfalto))))
     -0.05)))

