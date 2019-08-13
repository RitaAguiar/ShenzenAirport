#lang racket

(provide (all-defined-out))
(require "FunctionsAirport.rkt")
(require "FunctionsGeometryAirport.rkt")
(require rosetta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;GLOBAIS

(define ponto-base (u0)) ;ponto central de toda a construção situado na base de assentamento
(define n 2) ;inverso dos expoentes das expressões geradoras da superelipse

;;SUPERFÍCIES

;;;SUPERFÍCIE SUPERIOR

;;;;PONTO BASE

(define e 1) ;deslocamento dos pontos origem da treliça inferior da superfície superior
                ;em relação à inferior
(define p-s (+z ponto-base e)) ;ponto de referência da superelipse superior inicial

;;;;PARÂMETROS ANGULARES INICIAL E FINAL

(define psi0-sup-s #;(- (/ pi 5)) #;(- (/ 2pi 11)) (- pi/6)) ;ângulo inicial para geração da superfície em forma de superelipse
                             ;cilíndrica
(define psi1-sup-s (- pi psi0-sup-s)) ;ângulo final para geração da superfície em forma de superelipse
                                      ;cilíndrica

;;;;SEMIEIXO HORIZONTAL

(define as-s 27.3) ;parâmetro do eixo maior da superelipse
(define fa-s (λ(y) as-s)) ;função semieixo horizontal

;;;;SEMIEIXO VERTICAL

(define bs-s 15.3) ;parâmetro do eixo menor da superelipse
(define ampl-b-s 2) ;amplitude da onda senoidal
(define f-T-b-s 2/3) ;período da onda senoidal / comprimento do edifício
(define fase-b-s 0) ;fase da onda senoidal
(define fb-s (λ(y) bs-s)) ;função semieixo vertical
; (define fb-s (fsin ampl-b-s (* f-T-b-s comp) fase-b-s bs-s))
; (define fb-s (λ(y) bs-s))


;;;;NÚMERO DE PONTOS

(define n-p-sup-t-s 37) ;número de pontos para o parâmetro t

(define n-p-sup-y-s (/ 20 f-T-b-s)) ;número de pontos para o parâmetro y

;;;SUPERFÍCIE INFERIOR

;;;;PONTO BASE

(define p-i ponto-base) ;ponto de referência da superelipse inferior inicial

;;;;PARÂMETROS ANGULARES INICIAL E FINAL

(define psi0-sup-i (- pi/6)) ;ângulo inicial para geração da superfície em forma de superelipse
                       ;cilíndrica
(define psi1-sup-i (- pi psi0-sup-i)) ;ângulo final para geração da superfície em forma de superelipse
                       ;cilíndrica

;;;;SEMIEIXO HORIZONTAL

(define as-i 23.7) ;parâmetro do eixo maior da superelipse
(define fa-i (λ(y) as-i)) ;função semieixo horizontal

;;;;SEMIEIXO VERTICAL

(define bs-i 12.7) ;parâmetro do eixo menor da superelipse
(define fb-i (λ(y) bs-i)) ;função semieixo vertical
; (define fb-i (λ(y) bs-i))
; (define fb-i (fsin ampl-b-s (* f-T-b-s comp) fase-b-s bs-i))


;;;;NÚMERO DE PONTOS

(define n-p-sup-t-i 37) ;número de pontos para o parâmetro t

(define n-p-sup-y-i (/ 20 f-T-b-s)) ;número de pontos para o parâmetro y

;;;TRELIÇAS

(define tipo 'warren-verticals)
(define larg-trelica 6) ;lagura treliça (não está a ser usado)
(define n-trelicas 10) ;número de treliças (tem que ser par)
#;(define n-trelicas (/ m 2)) ;número de pares treliças que constituem a treliça espacial
(define n-interv 36)
(define n-secoes (/ n-interv 2)) ;número de secções que compões cada treliça da treliça espacial. A largura da cada treliça
                    ;corresponde a duas barras
(define comp (* n-trelicas larg-trelica)) ;comprimento (profundidade) do edifício

(define raio-no-trelica 0.3)
(define aresta-fixed-no-trelica 0.2)
(define raio-barra-trelica 0.15) ;0.15 0.10 0.07
(define esp-tubo-aco 0.02)
(define area-tubo-aco (* pi  ;área do tubo de aço
                         (- (sqr raio-barra-trelica)
                            (sqr (- raio-barra-trelica
                                    esp-tubo-aco)))))
(define peso-esp-aco (* 76.5 1000)) ;peso específico do aço em N/m3
(define esp-cob 0.1) ;espessura da cobertura

(define peso-esp-cob (* 27 1000)) ;peso específico da cobertura de alumínio (metal) em N/m3
(define peso-esp-vidro (* 25 1000)) ;peso específico painéis de vidro hexagonais em N/m3
#;(define larg-trelica (/ comp n-trelicas)) ;medida da largura de cada faixa da superfície
                                          ; para cada uma das treliças
(define n-nos-ret 0) ;número de nós retirados à treliça
(define f-ajust 0) ;ajustamento dos nós da treliça

;;;SUPERFÍCIE SUPERIOR QUE CONTÉM NÓS DAS TRELIÇAS

(define p-ts p-s) ;ponto de referência da superelipse superior inicial onde se situam os nós superiores
                  ;da treliça

(define a-ts 27) ;parâmetro do eixo maior da superelipse
(define fa-ts (λ(y) a-ts)) ;função semieixo horizontal

(define b-ts 15) ;parâmetro do eixo menor da superelipse
(define fb-ts (λ(y) b-ts)) ;função semieixo vertical
; (define fb-ts (λ(y) b-ts))
; 
; (define ampl-b-ts 2) ;amplitude da onda senoidal
; (define f-T-b-ts 2/3) ;período da onda senoidal / comprimento do edifício
; (define fase-b-ts 0) ;fase da onda senoidal
; (define fb-ts (fsin ampl-b-ts (* f-T-b-ts comp) fase-b-ts b-ts))


(define psi0-ts psi0-sup-s) ;valor inicial da variável t para a função fa-ts
(define psi1-ts psi1-sup-s) ;valor final da variável t para a função fa-ts

(define alfa 0.6)
(define f-dist-t-ts (lambda (t) (/ (- 1 (cos (* t pi))) 2))) ;função que define a distribuição dos pontos entre os valores inicial e final no intervalo
                             ;[inic-x-i,fin-x-i]

; (define f-dist-t-ts (lambda (t) (/ (- 1 (cos (* t pi))) 2)))
; (define f-dist-t-ts (λ(t) t))
; (define f-dist-t-ts (lambda (t) (/ (+ (* (expt (abs (- (* 2 t) 1)) alfa )(sgn (- (* 2 t) 1))) 1) 2)))


;;;SUPERFÍCIE INFERIOR QUE CONTÉM NÓS DAS TRELIÇAS

(define p-ti p-i) ;ponto de referência da superelipse superior inicial onde se situam os nós inferiores
                  ;da treliça

(define a-ti 24) ;parâmetro do eixo maior da superelipse
(define fa-ti (λ(y) a-ti)) ;função semieixo horizontal

(define b-ti 13) ;parâmetro do eixo menor da superelipse
(define fb-ti (λ(y) b-ti))
; (define fb-ti (fsin ampl-b-ti (* f-T-b-ti comp) fase-b-ts b-ti))
; (define fb-ti (λ(y) b-ti))
; 
; (define ampl-b-ti 2) ;amplitude da onda senoidal
; (define f-T-b-ti 2/3) ;período da onda senoidal / comprimento do edifício
; (define fase-b-ti 0) ;fase da onda senoidal
; (define fb-ti (fsin ampl-b-ti (* f-T-b-ti comp) fase-b-ts b-ti)) ;função semieixo vertical


(define psi0-ti psi0-sup-i) ;valor inicial da variável t para a função fa-ts
(define psi1-ti psi1-sup-i) ;valor final da variável t para a função fa-ts

(define f-dist-t-ti f-dist-t-ts) ;função que define a distribuição dos pontos entre os valores inicial e final no intervalo
                             ;[inic-x-i,fin-x-i]
; (define f-dist-t-ti (lambda (t) (/ (- 1 (cos (* t pi))) 2)))
; (define f-dist-t-ti (λ(t) t))


;;ANÁLISE LUMÍNICA

;;;RAIO LUMINOSO

(define raio-luminoso (xyz 0 2 -2)) ;vetor que indica a direção e sentido do raio luminoso
                                     ;que determina a variação nas aberturas em função da
                                     ;exposição

(define posicao-raio-luminoso (xyz 0 0 30))

;;;SENSORES E ABERTURAS

(define fl 17/9) ;3/8   ;fração da largura da treliça ocupada pela dimensão correspondente de
                       ;uma abertura para iluminação
(define fs 7/12) ;7/12 ;fração da dimensão da abertura para iluminação ocupada pelo lado dos
                       ;hexágonos que lhe são paralelos
                       ;uma abertura para iluminação
(define abx 2) ;0.5    ;dimensão base da abertura que se pode fazer variar em função da
                       ;exposição à luz

;;;;SUPERIORES

(define psi0-sens-s (+ psi0-sup-s (/ pi 48))) ;ângulo inicial para geração da superfície em forma de
                                              ;superelipse cilíndrica
(define psi1-sens-s (- psi1-sup-s (/ pi 48))) ;ângulo final para geração da superfície em forma de
                                              ;superelipse cilíndrica
(define n-sens-s 46) ;número de sensores nas filas que possuem mais um
(define n-pares-filas-trelica-s 4) ;número de pares de filas que se pretendem por cada faixa da superfície correspondente a uma
                                   ;treliça

(define larg-par-filas-s (/ larg-trelica n-pares-filas-trelica-s)) ;largura de uma faixa que contém um par de filas de sensores
(define n-t-pares-filas-s (* n-trelicas n-pares-filas-trelica-s)) ;número total de faixas que contêm um par de filas de sensores
(define forma-abert-s (abert-hexag (abert-atrator abx) larg-par-filas-s fl fs)) ;forma abertura com abertura variável
; (define forma-abert-s (abert-hexag (gama-abert abx) larg-par-filas-s fl fs))
 ;forma abertura com gama aberturas

;;;;INFERIORES

(define psi0-sens-i (+ psi0-sup-i (/ pi 48))) ;ângulo inicial para geração da superfície em forma de superelipse
                       ;cilíndrica
(define psi1-sens-i (- psi1-sup-i (/ pi 48))) ;ângulo final para geração da superfície em forma de superelipse
                       ;cilíndrica
(define n-sens-i 46) ;número de sensores nas filas que possuem mais um
(define n-pares-filas-trelica-i 4) ;número de pares de filas que se pretendem por cada faixa da superfície

(define larg-par-filas-i (/ larg-trelica n-pares-filas-trelica-i)) ;largura de uma faixa que contém um par de filas de sensores
(define n-t-pares-filas-i (* n-trelicas n-pares-filas-trelica-i)) ;número total de faixas que contêm um par de filas de sensores
(define forma-abert-i (abert-hexag (abert-atrator abx) larg-par-filas-s fl fs))
; (define forma-abert-i (abert-hexag (abert-fixa abx) larg-par-filas-i fl fs))
 ;função forma da abertura

;;ABERTURAS SEGUNDO ATRATOR

(define atrator (xyz -3 27 25)) ;ponto atrator
(define raio-atrator 0.5) ;raio de representação do atrator

;;ELEMENTOS GEOMÉTRICOS

;;;ANDARES INFERIORES

(define h-laje 0.5)
(define h-piso 7)
(define d-coluna 0.7)
(define n-pisos 2)
(define n-colunas-x 4)
(define n-colunas-y 9)
(define h-acessos 3.5)
(define h-comboio 6)
(define esp 0.1)
(define n-suportes 60)

;;;JANELAS

(define espess-moldura 0.5) ;espessura da moldura que envolve a janela
(define espess-vidro 0.2) ;espessura do vidro da janela
(define raio-caix-horiz 0.01) ;raio dos cilindros que constituem os caisilhos horizontais
(define raio-caix-vert 0.06) ;raio dos cilindros que constituem os caisilhos verticais
(define altura-painel 1.5) ;altura de cada painel de vidro (retângulo de vidro entre caixilhos consecutivos
(define largura-painel 3.8) ;altura de cada painel de vidro (retângulo de vidro entre caixilhos consecutivos

;;TÉCNICAS

(define dpsi 0.0001) ;deslocamento na direção de x para obtenção de p0ontos vizinhos dos
                     ;pontos da superfície com o objetivo de obter o vetor normal nesse
                     ;ponto
