;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Problem
;Design a function that consumes two images and produces true if the first is larger than the second
;Assumption: images will be rectangles

(require 2htdp/image)

;; 2 Images -> Boolean
;; Produce true if area of first image greater than area of second, otherwise false
(check-expect (area-larger (square 10 "solid" "red") (rectangle 10 20 "solid" "red")) false)
(check-expect (area-larger (square 10 "solid" "red") (rectangle 2 50 "solid" "red")) false)
(check-expect (area-larger (square 10 "solid" "red") (rectangle 8 10 "solid" "red")) true)

;(define (area-larger img1 img2) false) ;stub

;(define (area-larger img1 img2) ;template
;  (> areaofimg1 areaofimg2)

(define (area-larger img1 img2)
  (> (* (image-width img1) (image-height img1)) (* (image-width img2) (image-height img2))))