;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceprogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

(define-struct vector (x y)) 
;; A Vector is a structure
;;   make-vector [Number Number]
;; that represents a 2D mathematical object in cartesian plane

(define-struct satellite (pos vel acc))
;; A Satellite is a make-satellite [Vector Vector Vector]
;;     a collection of vectors that represent the satellite's position,
;;          velocity and acceleration in 2D cartesian coordinates
 
(define-struct constellation (craft moon earth)) 
;; A Constellation is a make-constellation [Satellite Satellite Satellite]
;;     a collection of space-borne bodies


;; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define EARTHRADIUS 50)
(define MOONRADIUS 15)
(define ORIGIN (make-vector (quotient WIDTH 2) (quotient HEIGHT 2)))
(define GRAVITY 100) ;; the force of gravity
(define THEVOID (empty-scene WIDTH HEIGHT "black"))
(define MOONDIST (- (/ HEIGHT 2) (* 2 MOONRADIUS)))
(define MOONSPEED (sqrt (/ GRAVITY MOONDIST))) ;; stable circ orbit, (sqrt G/r)
(define CRAFTSPEED (sqrt (/ GRAVITY EARTHRADIUS)))
(define EARTH (circle EARTHRADIUS "solid" "light blue"))
(define MOON (circle MOONRADIUS "solid" "light grey"))
(define SPACECRAFT (triangle 10 "solid" "silver"))
(define BOOM! (radial-star 8 8 16 "solid" "red")) ;; whoops!


;; functions


(define (main stelln)
  ;; Constellation -> Constellation
  ;; run the pocket universe
  (big-bang stelln
    [on-tick update-constellation 1/140] ;; any faster and render glitches
    [to-draw render]
    ; [on-key impulse)) !!!
    [stop-when crashed?]))


(define (update-constellation sats)
  ;; Constellation -> Constellation
  ;; a function that updates the information for a suite of satellites
  (make-constellation (update-satellite (constellation-craft sats))
                      (update-satellite (constellation-moon sats))
                      (constellation-earth sats)))


(define (render sats)
  ;; Constellation -> Img
  ;; render an image of the earth, moon and rocket in glorious danse
  (image-insert
   (constellation-craft sats)
   (cond
     [(crashed? sats) BOOM!]
     [else SPACECRAFT])
   (image-insert
    (constellation-moon sats)
    MOON
    (image-insert
     (constellation-earth sats)
     EARTH
     THEVOID))))


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


(define (crashed? sats)
  ;; Constellation -> Boolean
  ;; end program if rocket crashes into the earth
  (< (normalize (-vec (satellite-pos (constellation-craft sats)) ORIGIN))
     EARTHRADIUS))


(define (update-satellite rkt)
  ;; Satellite -> Satellite
  ;; a function that updates the rocket information
  (make-satellite (+vec (satellite-pos rkt) (satellite-vel rkt))
                  (+vec (satellite-vel rkt) (satellite-acc rkt))
                  (update-acceleration (satellite-pos rkt) ORIGIN)))
;; checks
(check-within (update-satellite (make-satellite
                                 (make-vector 350 300) (make-vector 0 20)
                                 (make-vector 0 0)))
              (make-satellite (make-vector 350 320) (make-vector 0 20)
                              (update-acceleration (make-vector 350 300) ORIGIN)) 1e-10)
(check-within (update-satellite (make-satellite
                                 (+vec ORIGIN (make-vector 10 10))
                                 (make-vector 0 20) (make-vector 0 0)))
              (make-satellite (+vec ORIGIN (make-vector 10 30))
                              (make-vector 0 20) (update-acceleration
                                                  (+vec ORIGIN (make-vector 10 10))
                                                  ORIGIN)) 1e-10)


(define (update-acceleration p p0)
  ;; Vector -> Vector
  ;; a function that updates acceleration vector based on current position
  (c*vec GRAVITY (inverse-sqr (-vec p0 p))))
;; checks
(check-within (update-acceleration (make-vector 10 0) (make-vector 0 0))
              (c*vec GRAVITY (make-vector -1/100 0)) 1e-10)
(check-within (update-acceleration (make-vector 0 5) (make-vector 0 -5))
              (c*vec GRAVITY (make-vector 0 -1/100)) 1e-10)
(check-within (update-acceleration (make-vector -3 12) (make-vector 2 0))
              (c*vec GRAVITY (make-vector 5/2197 -12/2197)) 1e-10)


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
(check-expect (+vec (make-vector 12 5) (make-vector 0 0)) (make-vector 12 5))
(check-expect (+vec (make-vector 0 0) (make-vector 12 5)) (make-vector 12 5))


(define (-vec v1 v2)
  ;; Vector, Vector -> Vector
  ;; subtract one vector from another
  (make-vector (- (vector-x v1) (vector-x v2))
               (- (vector-y v1) (vector-y v2))))
;; checks
(check-expect (-vec (make-vector 12 5) (make-vector 12 5)) (make-vector 0 0))
(check-expect (-vec (make-vector 12 5) (make-vector 0 0)) (make-vector 12 5))
(check-expect (-vec (make-vector 0 0) (make-vector 12 5)) (make-vector -12 -5))


(define (c*vec c vec)
  ;; Number, Vector -> Vector
  ;; multiply a vector by a scalar
  (make-vector (* c (vector-x vec)) (* c (vector-y vec))))
;; checks
(check-expect (c*vec 10 (make-vector 10 10)) (make-vector 100 100))


(define (impulse rkt ke)
  ; Rocket KeyEvent -> Rocket
  ; allow the user to give an impulse thrust to the rocket
  ; !!!
  (cond [(... ke) (... rkt)]
        [(... ke) (... rkt)]))


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
   (make-vector 0 (- CRAFTSPEED))
   (make-vector 0 0))
  (make-satellite
   (make-vector (/ WIDTH 2) (- (/ HEIGHT 2) MOONDIST))
   (make-vector (- MOONSPEED) 0)
   (make-vector 0 0))
  (make-satellite
   ORIGIN
   (make-vector 0 0)
   (make-vector 0 0))))