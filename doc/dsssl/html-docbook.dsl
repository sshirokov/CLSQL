<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY dbstyle PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet/EN" CDATA DSSSL>
]>

 <style-sheet>
 <style-specification use="docbook">
 <style-specification-body>
(element envar ($mono-seq$))
(element symbol ($mono-seq$))
(element type ($mono-seq$))
(element errortype ($mono-seq$))
(element returnvalue ($italic-mono-seq$))
(define (book-titlepage-recto-elements)
  (list (normalize "title")
        (normalize "subtitle")
        (normalize "graphic")
        (normalize "corpauthor")
        (normalize "authorgroup")
        (normalize "author")
        (normalize "editor")
	(normalize "printhistory")
        (normalize "copyright")
        (normalize "abstract")
        (normalize "legalnotice")))
(define %use-id-as-filename% #t)
(define use-output-dir #t)
 --> </style-specification-body>
 --> </style-specification>
<external-specification id="docbook" document="dbstyle">
 --> </style-sheet>
