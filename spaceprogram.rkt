;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceprogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

; A Vector is a structure
;   make-vector [Number Number]
; that represents a 2D mathematical object in cartesian plane
(define-struct vector (x y)) 

; A Satellite is a make-satellite [Vector Velocity Vector Img]
;     a collection of vectors that represent the rocket's position,
;          velocity and acceleration in 2D cartesian coordinates
(define-struct satellite (pos vel acc image)) 

; A Constellation is a make-constellation [Satellite Satellite]
;     a collection of satellites orbiting earth
(define-struct constellation (craft moon)) 


; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define EARTHRADIUS 50)
(define MOONRADIUS 15)
(define ORIGIN (make-vector (quotient WIDTH 2) (quotient HEIGHT 2)))
(define GRAVITY 200) ; the force of gravity
(define THEVOID (empty-scene WIDTH HEIGHT "black"))
(define EARTH (circle EARTHRADIUS "solid" "light blue"))
(define MOON (circle MOONRADIUS "solid" "light grey"))
(define SPACECRAFT (triangle 10 "solid" "silver"))


; functions

; Vector -> Vector
; a function that updates the acceleration vector based on the current position
(check-within (update-acceleration (make-vector (/ WIDTH 2) (+ (/ HEIGHT 2) 12))
                                   ORIGIN)
              (make-vector 0 (* (/ GRAVITY (expt (normalize (make-vector (/ WIDTH 2) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) -12)) 1/1000000)
(check-within (update-acceleration (make-vector (- (/ WIDTH 2) 5) (/ HEIGHT 2))
                                   ORIGIN)
              (make-vector (* (/ GRAVITY (expt (normalize (make-vector (- (/ WIDTH 2) 5) (/ HEIGHT 2)) ORIGIN) 3)) 5) 0) 1/1000000)
(check-within (update-acceleration (make-vector (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12))
                                   ORIGIN)
              (make-vector (* (/ GRAVITY (expt (normalize (make-vector (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) 5) (* (/ GRAVITY (expt (normalize (make-vector (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) -12)) 1/1000000)
(define (update-acceleration p p0)
  (c*vec (/ GRAVITY (expt (normalize p p0) 3)) (-vec p0 p)))

; Vector, Vector -> Number
; normalizes the distance between two vectors in cartesian space
(check-within (normalize (make-vector 12 5) (make-vector 12 5)) 0 1/1000000)
(check-within (normalize (make-vector 12 5) (make-vector 0 0)) 13 1/1000000)
(check-within (normalize (make-vector 0 0) (make-vector 12 5)) 13 1/1000000)
(define (normalize p1 p2)
  (sqrt (+ (sqr (- (vector-x p1) (vector-x p2)))
           (sqr (- (vector-y p1) (vector-y p2))) 1.3e-134))) ; there's some kind of crazy rounding error going on here that sometimes causes the program to virtually halt

; Vector, Vector -> Vector
; add one vector to another
(check-expect (+vec (make-vector 12 5) (make-vector 12 5)) (make-vector 24 10))
(check-expect (+vec (make-vector 12 5) (make-vector 0 0)) (make-vector 12 5))
(check-expect (+vec (make-vector 0 0) (make-vector 12 5)) (make-vector 12 5))
(define (+vec v1 v2)
  (make-vector (+ (vector-x v1) (vector-x v2))
               (+ (vector-y v1) (vector-y v2))))

; Vector, Vector -> Vector
; subtract one vector from another
(check-expect (-vec (make-vector 12 5) (make-vector 12 5)) (make-vector 0 0))
(check-expect (-vec (make-vector 12 5) (make-vector 0 0)) (make-vector 12 5))
(check-expect (-vec (make-vector 0 0) (make-vector 12 5)) (make-vector -12 -5))
(define (-vec v1 v2)
  (make-vector (- (vector-x v1) (vector-x v2))
               (- (vector-y v1) (vector-y v2))))

; Number, Vector -> Vector
(check-expect (c*vec 10 (make-vector 10 10)) (make-vector 100 100))
; multiply a vector by a scalar
(define (c*vec c vec)
  (make-vector (* c (vector-x vec)) (* c (vector-y vec))))

; Constellation -> Constellation
; a function that updates the information for a suite of satellites
(define (update-constellation sats)
  (make-constellation (update-satellite (constellation-craft sats))
                      (update-satellite (constellation-moon sats))))

; Satellite -> Satellite
; a function that updates the rocket information
(check-within (update-satellite (make-satellite
                                 (make-vector 350 300)
                                 (make-vector 0 20)
                                 (make-vector 0 0)
                                 SPACECRAFT))
              (make-satellite (make-vector 350 320)
                              (make-vector 0 20)
                              (update-acceleration (make-vector 350 300) ORIGIN)
                              SPACECRAFT)
              1/100000)
(define (update-satellite rkt)
  (make-satellite (+vec (satellite-pos rkt) (satellite-vel rkt))
                  (+vec (satellite-vel rkt) (satellite-acc rkt))
                  (update-acceleration (satellite-pos rkt) ORIGIN)
                  (satellite-image rkt)))

; Constellation -> Img
; render an image of the rocket flying around, and the orbiting moon
(check-expect (render (make-constellation
                       (make-satellite ORIGIN (make-vector 0 0)
                                       (make-vector 0 0) SPACECRAFT)
                       (make-satellite ORIGIN (make-vector 0 0)
                                       (make-vector 0 0) MOON)))
              (place-image SPACECRAFT (vector-x ORIGIN) (vector-y ORIGIN)
                           (place-image MOON (vector-x ORIGIN) (vector-y ORIGIN)
                                        (place-image EARTH (vector-x ORIGIN)
                                                     (vector-y ORIGIN) THEVOID)))) ; checks
(define (render sats)
  (place-image (satellite-image (constellation-craft sats))
               (vector-x (satellite-pos (constellation-craft sats)))
               (vector-y (satellite-pos (constellation-craft sats)))
               (place-image (satellite-image (constellation-moon sats))
                            (vector-x (satellite-pos (constellation-moon sats)))
                            (vector-y (satellite-pos (constellation-moon sats)))
                            (place-image EARTH (vector-x ORIGIN) (vector-y ORIGIN) THEVOID))))

; Rocket KeyEvent -> Rocket
; allow the user to give an impulse thrust to the rocket
; !!!
(define (impulse rkt ke)
  (cond [(... ke) (... rkt)]
        [(... ke) (... rkt)]))

; Rocket -> Rocket
; what happens when the rocket crashes back to earth
; !!!
(define (crash rkt)
  rkt)

; Vector, Vector -> Vector
; account for gravitational effects of the moon
(define (gravity rkt moon)
  (satellite-acc rkt))


; action!

(define Apollo (make-satellite (make-vector (+ (/ WIDTH 2) EARTHRADIUS) (/ HEIGHT 2) )
                               (make-vector 0 -1.89)
                               (make-vector 0 0)
                               SPACECRAFT))

(define Luna (make-satellite (make-vector (/ WIDTH 2) (- (/ HEIGHT 2) 350))
                             (make-vector -0.7505 0)
                             (make-vector 0 0)
                             MOON))

(big-bang (make-constellation Apollo Luna)
  [to-draw render]
  [on-tick update-constellation])