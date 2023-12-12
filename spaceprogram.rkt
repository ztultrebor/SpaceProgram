;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceprogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

(define-struct constellation [craft moon earth]) 
;; A Constellation is a make-constellation [Satellite Satellite Satellite]
;;     a collection of space-borne bodies; craft, moon and earth
#;
(define (fn-for-constellation const)
  (some-fn
   (... (constellation-craft const) ...)
   (... (constellation-moon const) ...)
   (... (constellation-craft earth) ...)))


(define-struct satellite [pos vel acc])
;; A Satellite is a make-satellite [Vector Vector Vector]
;;     a collection of vectors that represent the satellite's position,
;;          velocity and acceleration in 2D cartesian coordinates
#;
(define (fn-for-satellite sat)
  (some-fn
   (... (satellite-pos sat) ...)
   (... (satellite-vel sat) ...)
   (... (satellite-acc sat) ...)))


(define-struct vector [x y])
;; A Vector is a make-vector [Number Number]
;;     that represents a 2D mathematical object in cartesian plane
#;
(define (fn-for-vector v)
  (some-fn
   (... (vector-x v) ...)
   (... (vector-y v) ...)))


;; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define EARTHRADIUS 30)
(define MOONRADIUS 15)
(define ORIGIN (make-vector (quotient WIDTH 2) (quotient HEIGHT 2)))
(define EARTHGRAVITY 100)  ;; grav field strength due to earth mass
(define MOONGRAVITY 10) ;; grav field strength due to moon mass
(define THEVOID (empty-scene WIDTH HEIGHT "black"))
(define MOONDIST (- (/ HEIGHT 2) (* 2 MOONRADIUS)))
(define MOONSPEED (sqrt (/ EARTHGRAVITY MOONDIST))) ;; stable circ orbit, (sqrt G/r)
(define EARTH (circle EARTHRADIUS "solid" "light blue"))
(define MOON (circle MOONRADIUS "solid" "light grey"))
(define SPACECRAFT (triangle 10 "solid" "silver"))
(define BOOM! (radial-star 8 8 16 "solid" "red")) ;; whoops!



;; functions


(define (main sats)
  ;; Constellation -> Constellation
  ;; run the pocket universe
  (big-bang sats
    [on-tick update-constellation] ;; any faster and render glitches
    [to-draw render]
    [on-key impulse]
    [stop-when crashed? render])) ;; be sure to show explosion


(define (update-constellation sats)
  ;; Constellation -> Constellation
  ;; a function that updates the information for a suite of satellites
  (make-constellation (update-satellite (constellation-craft sats)
                                        (constellation-moon sats)
                                        (constellation-earth sats))
                      (update-satellite (constellation-moon sats)
                                        (constellation-moon sats)
                                        (constellation-earth sats))
                      (update-satellite (constellation-earth sats)
                                        (constellation-moon sats)
                                        (constellation-earth sats))))


(define (render sats)
  ;; Constellation -> Img
  ;; render an image of the earth, moon and rocket in glorious danse
  (image-insert
   (constellation-craft sats)
   (if (crashed? sats) BOOM! SPACECRAFT)
   (image-insert
    (constellation-moon sats)
    MOON
    (image-insert
     (constellation-earth sats)
     EARTH
     THEVOID))))


(define (impulse sats ke)
  ; Satellite KeyEvent -> Satellite
  ; allow the user to give an impulse thrust to the rocket
  (make-constellation
   (cond [(key=? " "  ke)
          (make-satellite
           (satellite-pos (constellation-craft sats))
           (satellite-vel (constellation-craft sats))
           (+vec (satellite-acc (constellation-craft sats))
                 (make-vector 0 -0.84)))]
         [else (constellation-craft sats)])
   (constellation-moon sats)
   (constellation-earth sats)))


(define (crashed? sats)
  ;; Constellation -> Boolean
  ;; end program if rocket crashes into the earth
  (< (normalize (-vec (satellite-pos (constellation-craft sats))
                      (satellite-pos (constellation-earth sats))))
     EARTHRADIUS))


(define (update-satellite sat moon earth)
  ;; Satellite, Constellation -> Satellite
  ;; a function that updates the rocket information
  (make-satellite (+vec (satellite-pos sat) (satellite-vel sat))
                  (+vec (satellite-vel sat) (satellite-acc sat))
                  (cond
                    [(equal? (make-vector 0 0) (satellite-vel sat))
                     (satellite-acc sat)]
                    [else (+vec
                           (update-acceleration EARTHGRAVITY
                            (satellite-pos sat) (satellite-pos earth))
                           (update-acceleration MOONGRAVITY
                            (satellite-pos sat) (satellite-pos moon)))])))
;; checks
(check-expect (update-satellite
               (make-satellite (make-vector 380 300) (make-vector 0 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 700 300) (make-vector 10 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 350 300) (make-vector 1 1)
                               (make-vector 0 0)))
              (make-satellite (make-vector 380 300) (make-vector 0 0)
                              (make-vector 0 0)))
(check-within (update-satellite
               (make-satellite (make-vector 380 300) (make-vector 40 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 700 300) (make-vector 10 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 350 300) (make-vector 1 1)
                               (make-vector 0 0)))
              (make-satellite (make-vector 420 300) (make-vector 40 0)
                              (+vec
                               (update-acceleration EARTHGRAVITY
                                (make-vector 380 300) (make-vector 350 300))
                               (update-acceleration MOONGRAVITY
                                (make-vector 380 300) (make-vector 700 300))))
              1e-8)
(check-within (update-satellite
               (make-satellite (make-vector 700 300) (make-vector 10 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 700 300) (make-vector 10 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 350 300) (make-vector 1 1)
                               (make-vector 0 0)))
              (make-satellite (make-vector 710 300) (make-vector 10 0)
                              (update-acceleration EARTHGRAVITY
                               (make-vector 700 300) (make-vector 350 300)))
              1e-8)
(check-within (update-satellite
               (make-satellite (make-vector 350 300) (make-vector 1 1)
                               (make-vector 0 0))
               (make-satellite (make-vector 700 300) (make-vector 10 0)
                               (make-vector 0 0))
               (make-satellite (make-vector 350 300) (make-vector 1 1)
                               (make-vector 0 0)))
              (make-satellite (make-vector 351 301) (make-vector 1 1)
                              (update-acceleration  MOONGRAVITY
                               (make-vector 350 300) (make-vector 700 300)))
              1e-8)


(define (update-acceleration gravity p p0)
  ;; Vector -> Vector
  ;; a function that updates acceleration vector based on current position
  (c*vec gravity (inverse-sqr (-vec p0 p))))
;; checks
(check-within (update-acceleration EARTHGRAVITY (make-vector 10 0) (make-vector 0 0))
              (c*vec EARTHGRAVITY (make-vector -1/100 0)) 1e-8)


;; Satellite, Img, Img -> Img
;; place a satellite onto a background
(define (image-insert sat satimg backdrop)
  (place-image satimg
               (vector-x (satellite-pos sat))
               (vector-y (satellite-pos sat))  
               backdrop))
;; checks
(check-expect (image-insert (make-satellite ORIGIN (make-vector 0 0)
                                            (make-vector 0 0)) EARTH THEVOID)
              (place-image EARTH (vector-x ORIGIN)
                           (vector-y ORIGIN) THEVOID))
(check-expect (image-insert (make-satellite (+vec ORIGIN (make-vector 100 100))
                                            (make-vector 0 0)
                                            (make-vector 0 0)) MOON THEVOID)
              (place-image MOON (+ (vector-x ORIGIN) 100)
                           (+ (vector-y ORIGIN) 100) THEVOID))


(define (normalize vec)
  ;; Vector -> Number
  ;; normalizes the length of a vector in cartesian space
  (sqrt (+ (expt (vector-x vec) 2)
           (expt (vector-y vec) 2)
           1e-120))) ;; some bug in finding the roots of  numbers?
;; checks
(check-within (normalize (make-vector 0 0)) 0 1e-10)
(check-within (normalize (make-vector 12 5)) 13 1e-10)
(check-within (normalize (make-vector -12 5)) 13 1e-10)


(define (inverse-sqr vec)
  ;; Vector -> Vector
  ;; calculates the inverse square of a vector, useful for physics
  (c*vec (/ 1 (sqrt (+ (* 1 (expt (vector-x vec) 6) (expt (vector-y vec) 0))
                       (* 3 (expt (vector-x vec) 4) (expt (vector-y vec) 2))
                       (* 3 (expt (vector-x vec) 2) (expt (vector-y vec) 4))
                       (* 1 (expt (vector-x vec) 0) (expt (vector-y vec) 6))
                       1e-120))) ;; some bug in finding the roots of numbers?
         vec)) 
;; checks
(check-within (inverse-sqr (make-vector 10 0)) (make-vector 1/100 0) 1e-10)


(define (+vec v1 v2)
  ;; Vector, Vector -> Vector
  ;; add one vector to another
  (make-vector (+ (vector-x v1) (vector-x v2))
               (+ (vector-y v1) (vector-y v2))))
;; checks
(check-expect (+vec (make-vector 12 5) (make-vector 12 5)) (make-vector 24 10))


(define (-vec v1 v2)
  ;; Vector, Vector -> Vector
  ;; subtract one vector from another
  (make-vector (- (vector-x v1) (vector-x v2))
               (- (vector-y v1) (vector-y v2))))
;; checks
(check-expect (-vec (make-vector 12 5) (make-vector 12 5)) (make-vector 0 0))


(define (c*vec c vec)
  ;; Number, Vector -> Vector
  ;; multiply a vector by a scalar
  (make-vector (* c (vector-x vec)) (* c (vector-y vec))))
;; checks
(check-expect (c*vec 10 (make-vector 10 10)) (make-vector 100 100))


(define (gravity rkt moon)
  ; Vector, Vector -> Vector
  ; account for gravitational effects of the moon
  ; !!!
  (satellite-acc rkt))



;; action!

(main
 (make-constellation
  (make-satellite
   (make-vector (+ (/ WIDTH 2) EARTHRADIUS) (/ HEIGHT 2) )
   (make-vector 0 0)
   (make-vector 0 0))
  (make-satellite
   (make-vector (/ WIDTH 2) (- (/ HEIGHT 2) MOONDIST))
   (make-vector (- MOONSPEED) 0)
   (make-vector 0 0))
  (make-satellite
   ORIGIN
   (make-vector 0 -1e-10)
   (make-vector 0 0))))