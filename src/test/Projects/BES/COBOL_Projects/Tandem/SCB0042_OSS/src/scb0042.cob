 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  77 foo-var PIC 99.
     88 foo-var-is-10 VALUE 10.

  77 loop-var PIC 99.

 PROCEDURE DIVISION.
 main-start.

   DISPLAY "Start main".

   MOVE 0 TO foo-var.
   PERFORM foo.

   MOVE 0 TO foo-var.
   PERFORM foo 10 TIMES.

   MOVE 0 TO foo-var.
   PERFORM 10 TIMES
     ADD 1 TO foo-var
     DISPLAY foo-var
   END-PERFORM.

   MOVE 0 TO foo-var.
   PERFORM 10 TIMES
     PERFORM foo
   END-PERFORM.

   MOVE 0 TO foo-var.
   PERFORM foo UNTIL foo-var-is-10.

   MOVE 0 TO foo-var.
   PERFORM UNTIL foo-var-is-10
     ADD 1 TO foo-var
     DISPLAY foo-var
   END-PERFORM.

   MOVE 0 TO foo-var.
   MOVE 10 TO loop-var.
   PERFORM foo loop-var TIMES.

   MOVE 0 TO foo-var.
   MOVE 10 TO loop-var.
   PERFORM loop-var TIMES
     ADD 1 TO foo-var
     DISPLAY foo-var
   END-PERFORM.

   DISPLAY "End main".
   STOP RUN.

 foo.
   ADD 1 TO foo-var.
   DISPLAY foo-var.

 END PROGRAM main.

