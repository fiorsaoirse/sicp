#lang sicp

(#%require rackunit)

(car ''abracadabra)
; т.к. кавычка - это сокращенная запись (quote (выражение))
; значение '' - (quote (quote abracadabra))
; т.е. буквально (car (quote (quote abracadabra)))