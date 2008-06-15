;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

#lang scheme/gui

(require "application.ss")

(define app (new application%))
;(send app load-file "revenge.dnd")
(send app show)