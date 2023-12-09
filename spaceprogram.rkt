;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceprogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

; A Position is a structure
;   make-position [Number Number]
; a vector that represents a position in 2D cartesian coordinates
(define-struct position (x y)) 

; A Velocity is a structure
;   make-velocity [Number Number]
; a vector that represents a velocity in 2D cartesian coordinates
(define-struct velocity (x y)) 

; A Acceleration is a structure
;   make-acceleration [Number Number]
; a vector that represents an acceleration in 2D cartesian coordinates
(define-struct acceleration (x y)) 

; A Satellite is a make-satellite [Position Velocity Acceleration Img]
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
(define ORIGIN (make-position (quotient WIDTH 2) (quotient HEIGHT 2)))
(define GRAVITY 200) ; the force of gravity
(define THEVOID (empty-scene WIDTH HEIGHT "black"))
(define EARTH (circle EARTHRADIUS "solid" "light blue"))
(define MOON (circle MOONRADIUS "solid" "light grey"))
(define SPACECRAFT (triangle 10 "solid" "silver"))


; functions

; Position -> Acceleration
; a function that updates the acceleration vector based on the current position
(check-within (update-acceleration (make-position (/ WIDTH 2) (+ (/ HEIGHT 2) 12))
                                   ORIGIN)
              (make-acceleration 0 (* (/ GRAVITY (expt (normalize (make-position (/ WIDTH 2) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) -12)) 1/1000000)
(check-within (update-acceleration (make-position (- (/ WIDTH 2) 5) (/ HEIGHT 2))
                                   ORIGIN)
              (make-acceleration (* (/ GRAVITY (expt (normalize (make-position (- (/ WIDTH 2) 5) (/ HEIGHT 2)) ORIGIN) 3)) 5) 0) 1/1000000)
(check-within (update-acceleration (make-position (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12))
                                   ORIGIN)
              (make-acceleration (* (/ GRAVITY (expt (normalize (make-position (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) 5) (* (/ GRAVITY (expt (normalize (make-position (- (/ WIDTH 2) 5) (+ (/ HEIGHT 2) 12)) ORIGIN) 3)) -12)) 1/1000000)
(define (update-acceleration p p0)
  (make-acceleration (* (/ GRAVITY (expt (normalize p p0) 3))
                        (- (position-x p0) (position-x p)))
                     (* (/ GRAVITY (expt (normalize p p0) 3))
                        (- (position-y p0) (position-y p)))))

; Velocity, Acceleration -> Velocity
; a function that updates the velocity vector based on the current velocity
;    and acceleration
(check-expect (update-velocity (make-velocity 0 0) (make-acceleration 10 0))
              (make-velocity 10 0))
(check-expect (update-velocity (make-velocity 0 0) (make-acceleration 0 10))
              (make-velocity 0 10))
(check-expect (update-velocity (make-velocity 100 200) (make-acceleration -5 -8))
              (make-velocity 95 192))
(define (update-velocity v a)
  (make-velocity (+ (velocity-x v) (acceleration-x a))
                 (+ (velocity-y v) (acceleration-y a))))

; Position, Velocity -> Position
; a function that updates the position vector based on the current position
;    and velocity
(check-expect (update-position (make-position 0 0) (make-velocity 10 0))
              (make-position 10 0))
(check-expect (update-position (make-position 0 0) (make-velocity 0 10))
              (make-position 0 10))
(check-expect (update-position (make-position 100 200) (make-velocity -5 -8))
              (make-position 95 192))
(define (update-position p v)
  (make-position (+ (position-x p) (velocity-x v))
                 (+ (position-y p) (velocity-y v))))

; Position, Position -> Number
; normalizes the distance between two positions in cartesian space
(check-expect (normalize (make-position 12 5) (make-position 12 5)) 0)
(check-expect (normalize (make-position 12 5) (make-position 0 0)) 13)
(check-expect (normalize (make-position 0 0) (make-position 12 5)) 13)
(define (normalize p1 p2)
  (sqrt (+ (sqr (- (position-x p1) (position-x p2)))
           (sqr (- (position-y p1) (position-y p2))))))

; Constellation -> Constellation
; a function that updates the information for a suite of satellites
(define (update-constellation sats)
  (make-constellation (update-satellite (constellation-craft sats))
                      (update-satellite (constellation-moon sats))))

; Satellite -> Satellite
; a function that updates the rocket information
(check-within (update-satellite (make-satellite
                                 (make-position 350 300)
                                 (make-velocity 0 20)
                                 (make-acceleration 0 0)
                                 SPACECRAFT))
              (make-satellite (make-position 350 320)
                              (make-velocity 0 20)
                              (update-acceleration (make-position 350 300) ORIGIN)
                              SPACECRAFT)
              1/100000)
(define (update-satellite rkt)
  (make-satellite (update-position (satellite-pos rkt) (satellite-vel rkt))
                  (update-velocity (satellite-vel rkt) (satellite-acc rkt))
                  (update-acceleration (satellite-pos rkt) ORIGIN)
                  (satellite-image rkt)))

; Constellation -> Img
; render an image of the rocket flying around, and the orbiting moon
(check-expect (render (make-constellation
                       (make-satellite ORIGIN (make-velocity 0 0)
                                       (make-acceleration 0 0) SPACECRAFT)
                       (make-satellite ORIGIN (make-velocity 0 0)
                                       (make-acceleration 0 0) MOON)))
              (place-image SPACECRAFT (position-x ORIGIN) (position-y ORIGIN)
                           (place-image MOON (position-x ORIGIN) (position-y ORIGIN)
                                        (place-image EARTH (position-x ORIGIN)
                                                     (position-y ORIGIN) THEVOID)))) ; checks
(define (render sats)
  (place-image (satellite-image (constellation-craft sats))
               (position-x (satellite-pos (constellation-craft sats)))
               (position-y (satellite-pos (constellation-craft sats)))
               (place-image (satellite-image (constellation-moon sats))
                            (position-x (satellite-pos (constellation-moon sats)))
                            (position-y (satellite-pos (constellation-moon sats)))
                            (place-image EARTH (position-x ORIGIN) (position-y ORIGIN) THEVOID))))

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


; action!

(define Apollo (make-satellite (make-position (+ (/ WIDTH 2) EARTHRADIUS) (/ HEIGHT 2) )
                               (make-velocity 0 -1.95)
                               (make-acceleration 0 0)
                               SPACECRAFT))

(define Luna (make-satellite (make-position (/ WIDTH 2) (- (/ HEIGHT 2) 350))
                             (make-velocity -0.75 0)
                             (make-acceleration 0 0)
                             MOON))
(big-bang (make-constellation Apollo Luna)
  [to-draw render]
  [on-tick update-constellation])