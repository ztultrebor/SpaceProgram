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
#; (define (fn-with-position p)
  (... p))

; A Velocity is a structure
;   make-velocity [Number Number]
; a vector that represents a velocity in 2D cartesian coordinates
(define-struct velocity (x y)) 
#; (define (fn-with-velocity v)
  (... v))

; A Acceleration is a structure
;   make-acceleration [Number Number]
; a vector that represents an acceleration in 2D cartesian coordinates
(define-struct acceleration (x y)) 
#; (define (fn-with-acceleration a)
  (... a))

; A Rocket is a make-rocket [Position Velocity Acceleration]
;     a collection of vectors that represent the rocket's position,
;          velocity and acceleration in 2D cartesian coordinates
(define-struct rocket (pos vel acc)) 
#; (define (fn-with-rocket rkt)
  (... rkt))


; constants

(define WIDTH 600)
(define HEIGHT 600)
(define EARTHRADIUS 50)
(define ORIGIN (make-position (quotient WIDTH 2) (quotient HEIGHT 2)))
(define THEVOID (empty-scene WIDTH HEIGHT "black"))
(define EARTH (circle EARTHRADIUS "solid" "light blue"))
(define SPACECRAFT (triangle 10 "solid" "silver"))


; functions

; Position -> Acceleration
; a function that updates the acceleration vector based on the current position
; !!!
(define (update-acceleration p)
  (make-position 0 0))

; Velocity, Acceleration -> Velocity
; a function that updates the velocity vector based on the current velocity
;    and acceleration
(check-expect (update-velocity (make-velocity 0 0) (make-acceleration 10 0)) (make-velocity 10 0))
(check-expect (update-velocity (make-velocity 0 0) (make-acceleration 0 10)) (make-velocity 0 10))
(check-expect (update-velocity (make-velocity 100 200) (make-acceleration -5 -8)) (make-velocity 95 192))
(define (update-velocity v a)
  (make-velocity (+ (velocity-x v) (acceleration-x a))
                 (+ (velocity-y v) (acceleration-y a))))

; Position, Velocity -> Position
; a function that updates the position vector based on the current position
;    and velocity
(check-expect (update-position (make-position 0 0) (make-velocity 10 0)) (make-position 10 0))
(check-expect (update-position (make-position 0 0) (make-velocity 0 10)) (make-position 0 10))
(check-expect (update-position (make-position 100 200) (make-velocity -5 -8)) (make-position 95 192))
(define (update-position p v)
  (make-position (+ (position-x p) (velocity-x v))
                 (+ (position-y p) (velocity-y v))))

; Rocket -> Rocket
; a function that updates the rocket information
(check-expect (update-rocket (make-rocket (make-position 0 0) (make-velocity 10 20) (make-acceleration 0 0))) (make-rocket (make-position 10 20) (make-velocity 10 20) (make-acceleration 0 0)))
(define (update-rocket rkt)
  (make-rocket (update-position (rocket-pos rkt) (rocket-vel rkt))
               (update-velocity (rocket-vel rkt) (rocket-acc rkt))
               (rocket-acc rkt)))

; Rocket -> Img
; render an image of the rocket flying around
(check-expect (render (make-rocket ORIGIN (make-velocity 0 0) (make-acceleration 0 0))) (place-image SPACECRAFT (position-x ORIGIN) (position-y ORIGIN) (place-image EARTH (position-x ORIGIN) (position-y ORIGIN) THEVOID))) ; checks
(define (render rkt)
  (place-image SPACECRAFT (position-x (rocket-pos rkt)) (position-y (rocket-pos rkt))
             (place-image EARTH (position-x ORIGIN) (position-y ORIGIN) THEVOID)))


; action!

(define Apollo (make-rocket ORIGIN (make-velocity 3 -4) (make-acceleration -1/10 1/10)))
(big-bang Apollo
  [to-draw render]
  [on-tick update-rocket])