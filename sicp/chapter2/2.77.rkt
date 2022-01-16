#lang sicp

(#%require rackunit)

; получается следующая структура:

; dispatcher = {
;     real-part:
;         rectangular: () => ...
;         polar: () => ,,,
;     imag-part:
;         rectangular: () => ... 
;         polar: () => ,,,
; }

; в результате, когда мы запрашиваем у диспетчера процедуру по метке "real-part"
; нам возвращается под-диспетчер, который содержит в себе типы "rectangular" и "polar",
; но не "complex"

; решение - добавить по [типу операции] c меткой "complex" эту же процедуру,
; чтобы она снимала метку два раза

; пример - запись формата ([тип операции] 'complex 'polar procedure)
; в первый раз снимаем метку - получается запись формата ([тип операции] 'polar procedure)
; а такая процедура уже есть в диспетчере (т.е. идет двойная диспетчеризация)