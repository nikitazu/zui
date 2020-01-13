#lang scribble/manual

@require[@for-label[racket/base
                    zui/notify-changes
                    zui/syntax]]

@title{zui}
@author{nikitazu}

Zuev's User Interface library.

Please refer to @secref{zui/syntax} module documentation
in order to see how to start with creating declarative user interfaces.

After that read the @secref{zui/notify-changes} module to find out
how to create a view model with data binding.

Let us review a simple example of defining a frame with declarative UI.

@racketblock[
 (require racket/gui
          zui/syntax)

 (define frame
   (zui frame% ([label "Test"]
                [width 320]
                [height 240])
        (zui vertical-pane% ([alignment '(center center)])
             (zui message% ([label "Hello, declarative User Interface!"]))
             (zui message% ([label "I love you so much!"]))
             (zui button% ([label "Quit"]
                           [callback (λ (s e) (exit))])))))

 (send frame show #t)
 ]

First expression in example is:

@racketblock[
 (require racket/gui
          zui/syntax)
 ]

Here we require @racket[racket/gui] module which provides standard racket GUI primitives
and @racket[zui/syntax] module which provides @racket[zui] macro.

In the next expression we define an instance of a @racket[frame%] with declarative user interface.

@racketblock[
 (define frame
   (zui frame% ([label "Test"]
                [width 320]
                [height 240])
        (zui vertical-pane% ([alignment '(center center)])
             (zui message% ([label "Hello, declarative User Interface!"]))
             (zui message% ([label "I love you so much!"]))
             (zui button% ([label "Quit"]
                           [callback (λ (s e) (exit))])))))
 ]

Let us review a single macro expression which creates a @racket[message%] object.

@racketblock[
 (zui message% ([label "Hello, declarative User Interface!"]))
 ]

First argument is class identifier: @racket[message%].

Then goes a list of init arguments: @racket[([label "Hello ..."])].

After a list of init arguments we can put any number of sub-expressions within the @racket[zui] macro.

@racketblock[
 (zui vertical-pane% ([alignment '(center center)])
      (zui message% ([label "Hello..."]))
      (zui message% ([label "I love..."]))
      ...)
 ]

As in the example above the @racket[zui] macro would handle the parent/child relationships
between the classes automatically.

The last expression shows the @racket[frame%] on screen.

@racketblock[
 (send frame show #t)
 ]


@table-of-contents[]

@include-section["notify-changes.scrbl"]
@include-section["syntax.scrbl"]
