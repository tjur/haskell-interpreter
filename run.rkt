#lang racket/base

(require (only-in racket/file
                  file->bytes))
(require "interp.rkt")


(if (not (eqv? (vector-length (current-command-line-arguments)) 1))
    (printf "Usage: racket run.rkt <path-to-file-with-program>\n")
    (let* ([path-to-file (vector-ref (current-command-line-arguments) 0)]
           [program (bytes->string/utf-8 (file->bytes path-to-file))])
      (run program)))      
