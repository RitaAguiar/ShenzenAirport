#lang racket
(provide (all-defined-out))
(require "FunctionsAirport.rkt")
(require "FunctionsGeometryAirport.rkt")
(require "ParametersAirport.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ANÁLISE DE RADIAÇÃO

(require rosetta/lighting-simulation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

;;;ANÁLISE DA MATRIZ DE SENSORES CRIANDO UMA MATRIZ DE PONTOS COM INFORMAÇÃO LUMÍNICA

;Dada a matriz de sensores cria uma matriz com os resultados da análise lumínica em que para cada
; ponto analisado, à lista que contém o ponto e a normal acrescenta-se um terceiro elemento, a radiação,
; resultante da análise lumínica.
;Aqui esse elemento consiste no resultado do produto escalar entre um vetor raio luminoso definido
; pelo utilizador e o vetor normal

(define (an-luminica mat-sens mat-sup) ;eliminar raio-luminoso
      (define (rgb-for-loc p sensors)
        (let ((nearest (argmin (lambda (s) (distance p (car s))) sensors)))
          (cdr nearest)))
      (define sensors-colors
        (parameterize
            ((analysis-nodes-height 0.4)
             (analysis-nodes-separation 10)
             #;(current-location "https://energyplus.net/weather-download/asia_wmo_region_2/CHN/CHN_Hong.Kong.SAR.450070_CityUHK//CHN_Hong.Kong.SAR.450070_CityUHK.epw"))
          (with-simulation
              (add-radiance-grid! mat-sup)
            (add-radiance-shape! (shape-material (surface-grid mat-sup) (material/roof-ceiling)))
            (let ((colors
                   (with-current-layer colors-layer
                     (radiation-map (string->path "D:\\Rita\\Dissertation\\Airport\\Model\\Model\\Radiance")
                                    #;(make-date 0 0 14 21 6 2017 0 0 #f 0))))
                  (sensors (sensors)))
              (map cons sensors colors)))))
      (for/list ((l0 (in-list mat-sens)))
        (for/list ((l1 (in-list l0)))
          (match l1
            ((list p n)
             (list p n (radiance<-color (rgb-for-loc p sensors-colors))))))))

;SUPERFÍCIE

(define fa (λ(a comp)(λ(y) a)))
(define fb (λ(b comp)(fsin 3 (* 2/3 comp) 0 b)))

;ELIPSE

(define se-s (superel-var-base-plana p-s psi0-sup-s (fa as-s comp) (fb bs-s comp) n))
(display "se-s\n")

;;SUPERFÍCIE SUPERIOR

(define sup-s (mat-sup se-s psi0-sup-s psi1-sup-s (- n-p-sup-t-s 1) (λ(t) t)
                           0 comp (- n-p-sup-y-s 1) (λ(t) t)))

;;SENSORES SUPERIORES

(define sens-s
      (mat-sensores se-s
                    psi0-sens-s
                    psi1-sens-s
                    (- n-sens-s 1)
                    larg-par-filas-s
                    n-t-pares-filas-s
                    dpsi))
(display "sens-s\n")

;;SENSORES SUPERFÍCIE SUPERIOR PARA ANÁLISE

(define sens-s-rad (an-luminica sens-s sup-s))
(display "sens-s-rad\n")

;;SUPERFÍCIE SUPERIOR PARA ANÁLISE

; FALTA A SUPERFÍCIE SEGUNDO FICHEIRO!!!


;;FICHEIRO PARA OUTPUT DOS SENSORES

(define fich "Radiation-Model-3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUÇÕES

; EXECUÇÃO DA SUPERFÍCE!!!


;Cria uma porta para escrita no ficheiro de dados
(define out (open-output-file fich #:exists 'truncate))
;Escreve no ficheiro de dados a matriz de pontos que definem a superfície mas guardados como
; ternos de coordenadas
(write (conv-pontos-coord sup-s) out)
;Escreve no ficheiro de dados a matriz de pontos sensores e respetivas normais mas guardados como
; ternos de coordenadas
(write (conv-p+n+l-coord sens-s-rad) out)
;Fecha a porta de esccrita de forma a que o apontador no ficheiro aponte para o início
(close-output-port out)

;vista
(view (xyz 45.9396 -73.7058 28.881) (xyz -0.66744 21.3679 8.93447) 65.0)
