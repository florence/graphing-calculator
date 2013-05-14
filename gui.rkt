#lang racket/gui
(require "graph.rkt" (only-in plot plot-new-window?))
(plot-new-window? #t)

(define frame
  (new frame% [label "A Basic Calculator"]))

(define function (new text-field% [label "function"] [parent frame]))
(define max (new text-field% [label "max"] [parent frame]))
(define min (new text-field% [label "min"] [parent frame]))

(define go!
  (new button% [parent frame]
       [label "Graph It!"]
       [callback (lambda (button event)
                   (with-handlers ([exn:fail? show-error-dialog])
                     (graph2d (send function get-value)
                              (string->number (send min get-value))
                              (string->number (send max get-value)))))]))

(define (show-error-dialog e)
  ; Create a dialog
  (define dialog (instantiate dialog% ("Error")))
  
  ; Add a text field to the dialog
  (new message% [parent dialog] [label (exn-message e)])
  
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel (new horizontal-panel% [parent dialog]
                     [alignment '(center center)]))
  
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel] [label "Ok"]
       [callback (λ _ (send dialog show #f))])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  
  ; Show the dialog
  (send dialog show #t))

          
(send frame show #t)

