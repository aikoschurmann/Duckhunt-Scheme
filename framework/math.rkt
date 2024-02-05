#lang r5rs

(#%provide math::decimal-round)

(define (math::decimal-round number decimals)
  (let ((decimal (expt 10 decimals)))
    (/ (round (* number decimal)) decimal)))