#lang racket

(provide (all-defined-out))
(require rosetta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES                                                                           

;;GLOBAIS

;Dados os parâmetros da superelipse define uma função que a cada valor do parâmetro angular t
; e a cada cordenada y relativa a um ponto p de referência faz corresponder um ponto da superfície.
;Os valores dos parâmetros a e b dependem do valor da ordenada relativa y e essa dependência é
;conseguida fornecendo as funções fa e fb como parâmetros.

;p-base - ponto base central de assentamento de cada superelipse da família dado y
;psi0 - valor do parâmetro da superelipse correspondente a um ponto que assenta na base
;fa - função que dá o valor do semi-eixo horizontal em função de y
;fb - função que dá o valor do semi-eixo vertical em função de y
;n - inverso do expoente utilizado

;psi - parâmetro angular expresso em radianos que permite obter as abcissas dos pontos da superelipse
;y - ordenada relativa ao ponto origem p da superelipse da família
;f-cota - fator do semieixo b que determina o desnivelamento do ponto central em relação à base de
; assentamento

(define (planar-wall p-base h-wall)
  (λ(psi y)
    (+xyz p-base
          x
          y
          h-wall)))
  
(define (superel-var-base-plana p-base psi0 fa fb n)
  (define f-cota (expt (expt (sin psi0) 2) (/ 1.0 n)))
  (λ(psi y)
    (+xyz p-base
          (* (fa y) (expt (expt (cos psi) 2) (/ 1.0 n)) (sgn (cos psi)))
          y
          (* (fb y) (+ (* (expt (expt (sin psi) 2) (/ 1.0 n)) (sgn (sin psi))) f-cota)))))

;;ÁREA SUPERELIPSE

(define (area-ext-sup-param fx fy ti tf e n dt)
  (define (sist-2eq a1 b1 c1 a2 b2 c2)
    (define det (- (* a1 b2) (* a2 b1)))
    (if (= det 0)
        #f
        (list (/ (- (* c1 b2) (* c2 b1)) det)
              (/ (- (* a1 c2) (* a2 c1)) det))))
  (define (c-ponto+vn t)
    (define x (fx t))
    (define y (fy t))
    (define x+ (fx (+ t dt)))
    (define y+ (fy (+ t dt)))
    (define x- (fx (- t dt)))
    (define y- (fy (- t dt)))
    (define vx (- x+ x-))
    (define vy (- y+ y-))
    (define nx (- vy))
    (define ny vx)
    (define norma (sqrt (+ (sqr nx) (sqr ny))))
    (define nux (/ nx norma))
    (define nuy (/ ny norma))
    (list x y nux nuy))
  (define (area-elem p1 p2)
    (define x1 (car p1))
    (define y1 (cadr p1))
    (define ux (caddr p1))
    (define uy (cadddr p1))
    (define x2 (car p2))
    (define y2 (cadr p2))
    (define vx (caddr p2))
    (define vy (cadddr p2))
    (define uv (+ (* ux vx) (* uy vy)))
    (if (< uv 0)
        (error "vetores unitários em sentido inverso")
        #t)
    (define kl (sist-2eq ux (- vx) (- x2 x1) uy (- vy) (- y2 y1)))
    (cond (kl
           (define rb (/ (+ (car kl) (cadr kl)) 2))
           (define re (- rb e))
           (define alfa (acos uv))
           ; (printf "rb=~a re=~a alfa=~a\n" rb re alfa)

           (* (/ alfa 2) (abs (- (sqr re) (sqr rb))))
           )
          (else
           (* e (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))))
  (define lista-pontos+vn
    (map-division c-ponto+vn ti tf n))
  (define l-a-elem
    (for/list
        ((p1 lista-pontos+vn)
         (p2 (cdr lista-pontos+vn)))
         (area-elem p1 p2)))
  ; (display l-a-elem)

  (foldl + 0 l-a-elem))

;Transformações da função sin através de a*sin(2pi/T*y+fi)+d

;a - amplitude
;T - período
;fi - fase
;d - deslocamento na vertical

(define (fsin a T fi d)
  (λ(y)
    (+ (* a (sin (+ (* (/ 2pi T) y) fi))) d)))

;Devolve a norma do vetor u.
 
(define (norma vu)
  (sqrt (+ (sqr (cx vu)) (sqr (cy vu)) (sqr (cz vu)))))

;Devolve o produto escalar dos vetores u e v.

(define (produto-escalar u v)
  (+ (* (cx u) (cx v)) (* (cy u) (cy v)) (* (cz u) (cz v))))

;Devolve o versor do vetor u que é um vetor unitário com a direção e sentido de u.

(define (versor vu)
  (define n (norma vu))
  (if (not (= n 0))
      (/c vu (norma vu))
      (error "versor: vetor nulo como argumento")))

;Devolve o produto vetorial dos vetores u e v.

(define (produto-vetorial u v)
  (let ((ux (cx u))(uy (cy u))(uz (cz u))
                   (vx (cx v))(vy (cy v))(vz (cz v)))
    (xyz (- (* uy vz) (* uz vy))
         (- (* uz vx) (* ux vz))
         (- (* ux vy) (* uy vx)))))

;;GEOMETRIA

;;;SUPERFÍCIES

;Cria uma matriz (lista de listas) que representa uma malha de pontos de uma superfície

;func - função que cria o ponto da superfície correspondente às coordenadas (x,y)
;inic-x - valor inicial para a coordenada x
;fin-x - valor final para a coordenada x
;n-interv-x - número de intervalos de uma fila de pontos segundo a direção de x
;f-distr-x - função que permite uma redistribuição dos pontos segundo x de modo que seja
;variável a distância entre eles
;inic-y - valor inicial para a coordenada y
;fin-y - valor final para a coordenada y
;n-interv-y - número de intervalos de uma fila de pontos segundo a direção de x
;f-distr-y - função que permite uma redistribuição dos pontos segundo y de modo que seja
;variável a distância entre eles

(define (cria-mat-sup func
                      inic-psi fin-psi n-pontos-psi 
                      inic-y fin-y n-pontos-y)
  (define (ponto-fila y)
    (λ(psi)(func psi y)))
  (define (fila-sup y)
    (map-division (ponto-fila y) inic-psi
                  fin-psi (- n-pontos-psi 1) #t))
  (map-division fila-sup inic-y
                fin-y (- n-pontos-y 1) #t))

(define (mat-sup func inic-x fin-x n-interv-x
                 f-distr-x inic-y fin-y n-interv-y f-distr-y)
  (define (ponto-fila y)
    (λ(x)(func x y)))
  (define (fila-sup y)
    (map-division (ponto-fila y) inic-x fin-x n-interv-x #t f-distr-x))
  (map-division fila-sup inic-y fin-y n-interv-y #t f-distr-y))

;;;CRIAÇÃO DE UMA MATRIZ DE SENSORES PARA ABERTURAS

;Cria a matriz (lista de listas) de sensores  e respetivas normais unitárias dirigidas para o
; exterior da superfície, alternando filas com n-interv + 1 sensores e filas com n-interv sensores
; com desfasamento posicional que cria uma alternância

;se - função que cria o ponto da superelipse de ordenda y correspondente ao ângulo t
;psi0 - ângulo inicial correspondente ao primeiro ponto das superelipses
;psi1 - ângulo final correspondente ao último ponto das superelipses
;n-interv - número de intervalos entre sensores consecutivos (número de sensores - 1)
;larg-pares-filas - largura de uma faixa que contém um par de filas de sensores
;n-t-pares-filas-l - número total de faixas que contêm um par de filas de sensores
;dpsi - acréscimo considerado para os parâmetros angular t e ordenada relativa y para cálculo
; do vetor normal

(define (mat-sensores se psi0 psi1 n-interv larg-par-filas n-t-pares-filas-l dpsi)
  (define dt (/ (- psi1 psi0) n-interv 2))
  (define (ponto+normal-fila y)
    (λ(t)
      (define p (se t y))
      (define p1 (se (- t dpsi) y))
      (define p2 (se (+ t dpsi) y))
      (define p3 (se t (- y dpsi)))
      (define p4 (se t (+ y dpsi)))
      (define n (versor (produto-vetorial (-c p4 p3) (-c p2 p1))))
      (list p n)))
  (define (fila-sensores psi0 psi1 n-interv)
    (λ(y) (map-division (ponto+normal-fila y) psi0 psi1 n-interv)))
  (define (lista-filas i)
    (if (< i 0.001)
        (list)
        (cons
         ((fila-sensores psi0 psi1 n-interv) (* larg-par-filas (- i 1/4 )))
         (cons
          ((fila-sensores (+ psi0 dt) (- psi1 dt) (- n-interv 1))
           (* larg-par-filas (- i 3/4 )))
          (lista-filas (- i 1))))))
  (lista-filas n-t-pares-filas-l))

;;ANÁLISE LUMÍNICA

;;ANÁLISE DA MATRIZ DE SENSORES CRIANDO UMA MATRIZ DE PONTOS COM INFORMAÇÃO LUMÍNICA

;Dada a matriz de sensores cria uma matriz com os resultados da análise lumínica em que para cada
; ponto analisado, à lista que contém o ponto e a normal acrescenta-se um terceiro elemento, a radiação,
; resultante da análise lumínica.
;Aqui esse elemento consiste no resultado do produto escalar entre um vetor raio luminosodefinido
; pelo utilizador e o vetor normal

;raio-luminoso - vetor que representa a disreção sentido e intensidade do raio luminoso incidente
; sobre a superfície
;mat-p+n - matriz (lista de listas) de sensores  e respetivas normais unitárias dirigidas para o
; exterior da superfície. Cada elemento desta matriz consiste numa lista em que o primeiro elemento
; é um ponto sensor e o segundo elemento, o vetor normal à superf+icie nesse ponto, unitário e
; dirigido para o exterior

(define (an-luminica raio-luminoso mat-p+n)
  (define (proc-sensor p+n)
    (define p (car p+n))
    (define n (cadr p+n))
    (define rad (- (produto-escalar raio-luminoso n)))
    (list p n rad))
  (define (proc-fila lista-pontos)
    (map proc-sensor lista-pontos))
  (map proc-fila mat-p+n))

;;ABERTURA SEGUNDO ATRATOR

;;;MATRIZ DE SENSORES PARA ABNERTURAS SEGUNDO DISTÂNCIA AO ATRATOR

;Dada a matriz de sensores cria uma matriz com os resultados da distância ao atrator em que para cada
; ponto analisado, à lista que contém o ponto e a normal acrescenta-se um terceiro elemento, a radiação,
; resultante da distância ao atrator.
;Aqui esse elemento consiste no resultado da distância entre o ponto atrator e o sensor.

;atrator - ponto atrator
;mat-p+n - matriz (lista de listas) de sensores  e respetivas normais unitárias dirigidas para o
; exterior da superfície. Cada elemento desta matriz consiste numa lista em que o primeiro elemento
; é um ponto sensor e o segundo elemento, o vetor normal à superf+icie nesse ponto, unitário e
; dirigido para o exterior

(define (dist-atrator atrator mat-p+n)
  (define (proc-sensor p+n)
    (define p (car p+n))
    (define n (cadr p+n))
    ; (define fator (expt 1.011 (distance p atrator)))

    ; (define fator (+ (- (exp (/ (distance p atrator) 60))) 2.6))

    ; (define fator (- (exp (/ 60 (+ (distance p atrator) 60))) 1))

    ; (define fator (* 1 (expt 0.85 (distance p atrator))))

    ; (define fator (/ 1 (sqrt (distance p atrator))))

    (define fator (+ (- (/ (distance p atrator) 20)) 2))
    (printf "dist-atrator = ~a\n" (distance p atrator)) 
    (printf "fator = ~a\n" fator)
    (list p n fator))
  (define (proc-fila lista-pontos)
    (map proc-sensor lista-pontos))
  (map proc-fila mat-p+n))

;;ABERTURAS

;;;VALOR DA ABERTURA

;Dado o valor radiação obtido pela análise lumínica para esse ponto, define o valor para a
; dimensão variável da abertura que é colocada nesse ponto

;abx - dimensão base da abertura que se pode fazer variar em função da exposição à luz

(define (abert-var abx)
  (λ(rad)
    (cond ((<= rad 0) (* abx 1.3))
          (else (min (/ abx rad)(* abx 1.3))))))

(define (abert-atrator abx)
  (λ(rad)
    (cond ((>= rad abx) abx)
          (else (max rad 0.5)))))

;Dado o valor radiação obtido pela análise lumínica para esse ponto, define o valor fixo para
; a dimensão da abertura que é colocada nesse ponto

;abx - dimensão base da abertura que se pode fazer variar em função da exposição à luz

(define (abert-fixa abx)
  (λ(rad)
    abx))

;Dado o valor radiação obtido pela análise lumínica para esse ponto, define o valor máximo para
; a dimensão da abertura que é colocada nesse ponto

;abx - dimensão máxima da abertura que se pode fazer variar em função da exposição à luz

(define (gama-abert abx)
  (λ(rad)
    (cond ((< rad 1) abx)
          ((< rad 2) (* 0.8 abx))
          ((< rad 3) (* 0.6 abx))
          ((< rad 4) (* 0.4 abx))
          ((< rad 5) (* 0.2 abx))
          (else (* 0.1 abx)))))
