#lang racket

(provide (all-defined-out))
(require "FunctionsAirport.rkt")
(require "FunctionsGeometryAirport.rkt")
(require "TrussesAirport.rkt")
(require "ParametersAirport.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RHINO/AUTOCAD

(require rosetta/autocad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;VISUALIZAÇÕES

(define apagar-desenhos 0)

;;ELEMENTOS GEOMÉTRICOS

(define background 0)
(define cria-andares-inferiores 0)
(define cria-terra 0)
(define cria-asfalto 0)
(define cria-guarda 0)
(define janelas 0) ;0 - ñ desenha 1-desenha a geometria das janelas

;;;SUPERFÍCIE SUPERIOR

(define pontos-sup-s 0) ;desenha os pontos que consituem a matriz que gera a superfície
(define superficie-s 0) ;desenha a superfície
(define sensores-s 0) ;1-desenha os pontos sensores  2-desenha os pontos + vetores normais
(define aberturas-s 0) ;1-desenha as aberturas em esboço  2-desenho completo 3-faz a superfície com espessura

;;;SUPERFÍCIE INFERIOR

(define pontos-sup-i 0) ;desenha os pontos que consituem a matriz que gera a superfície
(define superficie-i 0) ;desenha a superfície
(define sensores-i 0) ;1-desenha os pontos sensores  2-desenha os pontos + vetores normais
(define aberturas-i 0) ;1-desenha as aberturas em esboço  2-desenho completo 3-faz a superfície com espessura

;;;RAIO LUMINOSO

(define ve-raio-luminoso 0) ;1-ve o volume da direção do raio luminoso

;;;ATRATOR

(define ve-atrator 0) ;1-vê o volume do ponto atrator

;;;TRELIÇAS

(define opcao-nos 0) ;0 - ñ desenha 1-desenha pontos 2-desenha sólidos
(define opcao-barras 0) ;0 - ñ desenha 1-desenha linhas 2-desenha cilindros

(define espec-trelica (list raio-no-trelica aresta-fixed-no-trelica opcao-nos
                     raio-barra-trelica opcao-barras))

;;RENDER

(define render 0) ;1-faz o render

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUÇÕES                                                                         

(define (executa tipo ponto-base n-trelicas larg-trelica n-interv-p
                 n-pares-filas-trelica fb n-nos-ret)
  (define p-s (+z ponto-base e))
  (define p-i ponto-base)
  (define p-ti p-i)
  (define p-ts p-s)
  (define heights #;(latin-hypercube-sampling) '(15.00) ;treliça(s) de altura(s) a analisar
    #;'(11.10 11.30 11.50 11.70 11.90
            12.10 12.30 12.50 12.70 12.90
            13.10 13.30 13.50 13.70 13.90
            14.10 14.30 14.50 14.70 14.90
            15.10 15.30 15.50 15.70 15.90)
    #;'(11.20 11.40 11.60 11.80 
              12.20 12.40 12.60 12.80 
              13.20 13.40 13.60 13.80 
              14.20 14.40 14.60 14.80 
              15.20 15.40 15.60 15.80)
    #;'(11.00 12.00 13.00 14.00 15.00 16.00))
  (for/list ((height heights))
    (printf "heights = ~a\n" heights)
    (printf "height = ~a\n" height)
    (define b-ts height)
    (printf "b-ts = ~a\n" b-ts)
    
    (define b-ti (- b-ts 2))         ;medidas cobertura
    (define bs-s (+ b-ts 0.3))       ;medidas cobertura
    (define bs-i (- b-ti 0.3))       ;medidas cobertura
    (printf "b-ti = ~a\n" b-ti)
    (define comp (* n-trelicas larg-trelica))
    (define n-trelicas-duplas (/ n-trelicas 2))
    (define n-secoes (/ n-interv-p 2))
  (define larg-trelica-duplas (* larg-trelica 2))

  (define larg-par-filas (/ larg-trelica-duplas n-pares-filas-trelica))
  (define n-t-pares-filas (* n-trelicas-duplas n-pares-filas-trelica))

  (define larg-par-filas-i larg-par-filas)
  (define larg-par-filas-s larg-par-filas)
  (define n-t-pares-filas-i n-t-pares-filas)
  (define n-t-pares-filas-s n-t-pares-filas)

  (define f-ajust 0)
  
  ;criação de nós inferiores para treliça e para os elementos geométricos
  (define f-nos-p (superel-var-base-plana p-ti psi0-ti fa-ti (fb b-ti comp) n))
  (define nos-p
    (if (equal? tipo 'warren)
        (cria-nos-p-warren n-trelicas larg-trelica f-nos-p psi0-ti psi1-ti
                           n-interv-p n-nos-ret f-dist-t-ts)
        (cria-nos-p n-trelicas larg-trelica f-nos-p psi0-ti psi1-ti
                    n-interv-p n-nos-ret f-dist-t-ts)))
  ;elementos globais de desenho
  (cond ((= apagar-desenhos 1)
         (delete-all-shapes)))
  (cond ((= background 1)
         (black-background)))
  (cond ((= ve-raio-luminoso 1)
         (desenha-raio-luminoso posicao-raio-luminoso raio-luminoso)))
  (cond ((= ve-atrator 1)
         (desenha-atrator atrator raio-atrator)))

  ;execuções relativas à superfície inferior
  (define se-i (superel-var-base-plana p-i psi0-sup-i fa-i (fb bs-i comp) n))
  (define sup-i (mat-sup se-i
                         psi0-sup-i psi1-sup-i (- n-p-sup-t-i 1) (λ(t) t)
                         0 comp (- n-p-sup-y-i 1) (λ(t) t)))
  (define sens-i
    (mat-sensores se-i
                  psi0-sens-i
                  psi1-sens-i
                  (- n-sens-i 1)
                  larg-par-filas-i
                  n-t-pares-filas-i
                  dpsi))
  ; (define sens-i-rad (an-luminica raio-luminoso sens-i))

  (define sens-i-rad (dist-atrator atrator sens-i))
  (cond ((= pontos-sup-i 1)
         ;(printf "pontos-sup-s  = ~a \n" pontos-sup-s )
         (ve-mat-sup sup-i)))
  (cond ((= superficie-i 1)
         (with-current-layer surface-layer
           (surface-grid sup-i))))
  (cond ((= sensores-i 1)
         (with-current-layer sensor-nodes-layer
           (ve-pontos-sensores sens-i))))
  (cond ((= sensores-i 2)
         (with-current-layer sensor-nodes-layer
           (ve-sensores sens-i))))
  (cond ((= aberturas-i 1)
         (with-current-layer surface-layer
         (ve-sup+abert sup-i sens-i-rad forma-abert-i))))
  (cond ((= aberturas-i 2)
         (with-current-layer surface-layer
           (subtraction (thicken (surface-grid sup-i) (- esp))
                       (union
                        (cria-aberturas
                        forma-abert-i
                        sens-i-rad))))))
  (cond ((= aberturas-s 3)
         (with-current-layer surface-layer
           (thicken (surface-grid sup-i) (- esp)))))
  ;execuções relativas à superfície superior
  (define se-s (superel-var-base-plana p-s psi0-sup-s fa-s (fb bs-s comp) n))
  (define sup-s (mat-sup se-s psi0-sup-s psi1-sup-s (- n-p-sup-t-s 1) (λ(t) t)
                         0 comp (- n-p-sup-y-s 1) (λ(t) t)))
  (define sens-s
    (mat-sensores se-s
                  psi0-sens-s
                  psi1-sens-s
                  (- n-sens-s 1)
                  larg-par-filas-s
                  n-t-pares-filas-s
                  dpsi))
  ; (define sens-s-rad (an-luminica raio-luminoso sens-s))

  (define sens-s-rad (dist-atrator atrator sens-s))
  (cond ((= pontos-sup-s 1)
         ;(printf "pontos-sup-s  = ~a \n" pontos-sup-s )
         (ve-mat-sup sup-s)))
  (cond ((= superficie-s 1)
         (with-current-layer surface-layer
         (surface-grid sup-s))))
  (cond ((= sensores-s 1)
         (with-current-layer sensor-nodes-layer
           (ve-pontos-sensores sens-s))))
  (cond ((= sensores-s 2)
         (with-current-layer sensor-nodes-layer
           (ve-sensores sens-s))))
  (cond ((= aberturas-s 1)
         (with-current-layer surface-layer
           (ve-sup+abert sup-s sens-s-rad forma-abert-s))))
  (cond ((= aberturas-s 2)
         (with-current-layer surface-layer
           (subtraction (thicken (surface-grid sup-s) esp)
                       (union
                        (cria-aberturas
                        forma-abert-s
                        sens-s-rad))))))
  (cond ((= aberturas-s 3)
         (with-current-layer surface-layer
           (thicken (surface-grid sup-s) esp)))) 
  ;execuções relativas às treliças
  (cond ((not (= (+ opcao-nos opcao-barras) 0))
         (define f-nos-s (superel-var-base-plana p-ts psi0-ts fa-ts (fb b-ts comp) n))
         (define nos-s
           (cria-nos-s n-trelicas larg-trelica f-nos-s psi0-ti psi1-ti
                       n-interv-p n-nos-ret f-dist-t-ts f-ajust))
         (printf "psi0-ti = ~a\n" psi0-ti)
         (printf "psi0-ts = ~a\n" psi0-ts)
         (printf "n-interv-p = ~a\n" n-interv-p)
         (printf "n-secoes = ~a\n" n-secoes)
         (trelica tipo espec-trelica nos-p nos-s)
         ))
  
  ;execuções relativas às janelas
  (cond ((= janelas 1)
         (define pontos-contorno-i (pontos-contorno se-i psi0-sup-i psi1-sup-i n-p-sup-t-i))
         (define pontos-contorno-e (pontos-contorno se-s psi0-sup-s psi1-sup-s n-p-sup-t-s))
         (define janela (cria-janela
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
                         largura-painel))
         (janela 0 -1)
         (janela comp +1)))
  ;execuções relativas aos elementos geométricos
  (define nos-p-ret (list-tail (drop-right nos-p n-nos-ret) n-nos-ret))
  (cond ((= cria-andares-inferiores 1)
         (andares-inferiores ponto-base
                             (last (car nos-p)) (car (car nos-p))
                             raio-no-trelica (* h-laje 2) d-coluna comp
                             h-piso n-pisos n-colunas-x n-colunas-y n-suportes)))
  (cond ((= cria-guarda 1)
         (guardas ponto-base (last (car nos-p)) (car (car nos-p))
                  raio-no-trelica comp n-suportes)))
  (cond ((= cria-terra 1)
         (terra (last (car nos-p)) (car (car nos-p))
                raio-no-trelica h-laje comp)))
  (cond ((= cria-asfalto 1)
         (asfalto (last (car nos-p)) (car (car nos-p))
                  raio-no-trelica h-laje comp)))
  ;execução relativa a render
  (cond ((= render 1)
         (render-dir "D:\\Rita\\Dissertation")
         (render-size 1920 1080)
         (view (xyz 45.9396 -73.7058 28.881) (xyz -0.66744 21.3679 8.93447) 65.0)
         (render-view (format "gif-heights-~a" height))))
  ))
 
;JANELA DE EXECUÇÕES

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


; 1ST   (λ(a comp)(λ(y) a)) (λ(b comp)(λ(y) b))
; 2ND   (λ(a comp)(λ(y) a)) (λ(b comp)(fsin -3 (* 3/2 comp) 0 b))
; 3RD   (λ(a comp)(λ(y) a)) (λ(b comp)(fsin 3 (* 2/3 comp) 0 b))


(executa 'warren-verticals-planar
         (u0)
        10 6 36 2
        (λ(b comp)(λ(y) b))
         2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RENDER

;(render-dir "D:\\Rita\\Escola\\Dissertation")
(render-dir "D:\\Rita\\Dissertation")

(define (vistas nome)
  (render-size 1920 1080)
  ;(render-size 3840 2160)

; (view (xyz 45.9396 -73.7058 28.881) (xyz -0.66744 21.3679 8.93447) 65.0)
;   (render-view (string-append nome "-presentation-11"))
;   (render-view (format "gif-heights-presentation-~a" height))

  
  ; (view (xyz 0 6.93677 9.5) (xyz 1.43442 170.977 15.7074) 35.0)
; (render-view (string-append nome "-0.07m"))


  ; (view (xyz 47.4447 -75.5002 18.8852) (xyz -57.2404 135.468 -24.3642) 45.0)
;   ;(view (xyz 41.0993 -62.3272 19.6354) (xyz -0.288747 19.1309 -0.299412) 65.0)
;   (render-view (string-append nome "-geral-8"))


  ; (view (xyz 36.05 -22.0016 11.4643) (xyz -59.6873 140.741 -21.3611) 100.0)
;   ;(view (xyz 24.9553 -9.49717 20.9464) (xyz -59.6873 140.741 -21.3611) 45.0)
;   (render-view (string-append nome "-aprox-hd2"))

  
  ; (render-size 1080 1080)
; (view (xyz 36.05 -22.0016 11.4643) (xyz -59.6873 140.741 -21.3611) 100.0)
; (render-view (string-append nome "-aprox"))

  )

#;(vistas "planar-warren-verticals-10-6-36")
#;(vistas "spatial-warren-verticals10-6-36")
#;(vistas "planar-warren-verticals-16-3.75-54")
#;(vistas "spatial-warren-verticals-16-3.75-54")
#;(vistas "planar-warren-verticals-20-3-72")
#;(vistas "spatial-warren-verticals-20-3-72")
#;(vistas "corte-perspetivado")
#;(vistas "8th-warren-with-verticals-black")
#;(vistas "5th-Pratt-white")

; ;;MODELO ANALÍTICO
; 
; ;geral
; (render-size 1920 1080)
; (view (xyz 41.0993 -62.3272 19.6354) (xyz -0.288747 19.1309 -0.299412) 65.0) ;original
; (render-view (string-append nome "3"))
; ;aprox
; (render-size 1080 1080)
; (view (xyz 36.05 -22.0016 11.4643) (xyz -59.6873 140.741 -21.3611) 100.0)
; (render-view (string-append nome "-aprox3"))
; ;aprox-hd
; (render-size 1920 1080)
; (view (xyz 0 -131.785 0.296739) (xyz 1.9277 169.899 5.28933) 60.0)
; (render-view (string-append nome "-aprox-hd"))
; 
; ;;MODELO GEOMÉTRICO
; 
; ;interior - renders comparação lumínica
; (view (xyz 0 10.0541 7.5344) (xyz 3.04419 168.899 -6.64518) 35.0)
; 
; ;vista interior renders finais tese
; (view (xyz 0 6.93677 9.5) (xyz 1.43442 170.977 15.7074) 35.0)
; 
; ;interior
; (view (xyz 0 -365.641 -3.57337) (xyz 1.9277 169.899 5.28933) 200.0)
; ;geral!!!
; (view (xyz 47.4447 -75.5002 18.8852) (xyz -57.2404 135.468 -24.3642) 45.0)
; ;corte normal
; (view (xyz 0 -131.785 0.296739) (xyz 1.9277 169.899 5.28933) 60.0)
; ;corte recto
; (view (xyz 0 -365.641 -3.57337) (xyz 1.9277 169.899 5.28933) 200.0)
; ;corte perspectivado acentuado1
; (view (xyz 0 -43.5025 0.331107) (xyz 1.04803 169.933 3.86327) 25.0)
; (view (xyz 0 -43.5025 1) (xyz 1.05056 169.933 3.86519) 25.0)
; ;corte perspectivado acentuado2
; (view (xyz 0 -62.8658 0.0106614) (xyz 1.04803 169.933 3.86327) 35.0)
; (view (xyz 0 -66.2069 -0.0446308) (xyz 1.04803 169.933 3.86327) 25.0)
; ;corte perspectivado acentuado3
; (view (xyz 0 -79.7351 -0.268511) (xyz 1.04803 169.933 3.86327) 45.0)
