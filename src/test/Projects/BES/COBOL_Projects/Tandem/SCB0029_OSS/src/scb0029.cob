* Calls a single perform in the main routine
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MAIN.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01 em  NATIVE-2 VALUE 22.

 PROCEDURE DIVISION.
 main-start.
     display "main-start".
     display em.
     perform perf-one.
     Display "back in main".
     display em.
     Display "falling thru to perf-one".

 perf-one.
     display "perf-one".
     add 1 to em.
     display "perf-one-end".
*    exit perform.
 perf-one-end.
     Display "fell from perf-one".

     display em.
     display "main-end".

 END PROGRAM main.
