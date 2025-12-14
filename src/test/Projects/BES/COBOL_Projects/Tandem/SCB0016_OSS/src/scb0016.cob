 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  s2  NATIVE-2 VALUE 22.
  01  s4  NATIVE-4 VALUE 44.

  01 rec.
     02 r1  pic x occurs 7 times.

* the clause 1 to 10 must be used with depending on.
* the depending on variable can be within the struct or outside of it.
  01 rec2.
     02 r21  pic x  occurs 1 to 30 times depending on s2.

  01 rec3.
     02 r31  pic 99 VALUE 5.
     02 r32  pic x  occurs 1 to 10 times depending on r31.


* Index variables are automatically declared by the compiler
* Need different names for all index variables in a program.
  01 rec4.
     02 r41  pic x occurs 4 times indexed by ix4.

  01 rec5.
     02 r51  pic x occurs 1 to 20 times depending on s2 indexed by ix5.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   ENTER TAL DEBUG.

   Move 17 to s2.
   Move "1234567" to rec.

   Move 20 to s2.
   Set ix5 to 17.
   Move "1234567890abcdABCDef" to rec5.
   ENTER TAL DEBUG.

   Move "X" to r51(ix5).

   ENTER TAL DEBUG.
   Display "End main".

 END PROGRAM main.
