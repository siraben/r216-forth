# A Forth for the R216K8B computer

![Screenshot](screenshot.png)

## What?
This project aims to implement the Forth programming language on the
[R216 computer by LBPHacker](https://lbphacker.pw/powdertoy/R216/manual.md).  The
computer is implemented completely in the video game [Powder
Toy](https://powdertoy.co.uk/), so this Forth will be one of the few
systems out there that target a fictional computer.  Since Forth is
extremely easy to port, it only takes a couple of primitive routines
to get started writing the whole thing again.

## Why?
Fictional computers are great.  One can learn a lot from both
implementing and playing around with them.  But when was the last time
you saw a fictional computer with an interactive REPL?  What about a
fictional computer implemented in a powder simulation game?

## Future plans
- [ ] An assembler for the R216 written in Scheme
  - [ ] Allows for creation of advanced macros, easy porting of
        parts of [a similar existing implementation](https://github.com/siraben/zkeme80/blob/master/src/forth.scm)
