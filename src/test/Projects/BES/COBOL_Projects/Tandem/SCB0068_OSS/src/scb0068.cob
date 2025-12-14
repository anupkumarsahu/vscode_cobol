 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01 condition-variable-1 PIC X.
     88 condition-1 VALUE "T".

  01 condition-variable-2 PIC X.
     88 condition-2 VALUE "T".

  01 condition-variable-3 PIC X.
     88 condition-3 VALUE "T".

 PROCEDURE DIVISION.
 main-start.

   DISPLAY "Start main".

   MOVE "T" TO condition-variable-1.
   MOVE "T" TO condition-variable-2.
   MOVE "T" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "T" TO condition-variable-1.
   MOVE "T" TO condition-variable-2.
   MOVE "F" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "T" TO condition-variable-1.
   MOVE "F" TO condition-variable-2.
   MOVE "T" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "T" TO condition-variable-1.
   MOVE "F" TO condition-variable-2.
   MOVE "F" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "F" TO condition-variable-1.
   MOVE "T" TO condition-variable-2.
   MOVE "T" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "F" TO condition-variable-1.
   MOVE "T" TO condition-variable-2.
   MOVE "F" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "F" TO condition-variable-1.
   MOVE "F" TO condition-variable-2.
   MOVE "T" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   MOVE "F" TO condition-variable-1.
   MOVE "F" TO condition-variable-2.
   MOVE "F" TO condition-variable-3.
   PERFORM main-evaluate-1.
   PERFORM main-evaluate-2.

   DISPLAY "End main".
   STOP RUN.

 main-evaluate-1.

   IF condition-1 THEN
     IF condition-2 THEN
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
       DISPLAY "Condition-2 TRUE"
     ELSE
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
       DISPLAY "Condition-2 FALSE"
     END-IF
     DISPLAY "Condition-1 TRUE"
   ELSE
     IF condition-2 THEN
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
       DISPLAY "Condition-2 TRUE"
     ELSE
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
       DISPLAY "Condition-2 FALSE"
     END-IF
     DISPLAY "Condition-1 FALSE"
   END-IF.

 main-evaluate-2.

   IF condition-1 THEN
     DISPLAY "Condition-1 TRUE"
     IF condition-2 THEN
       DISPLAY "Condition-2 TRUE"
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
     ELSE
       DISPLAY "Condition-2 FALSE"
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
     END-IF
   ELSE
     DISPLAY "Condition-1 FALSE"
     IF condition-2 THEN
       DISPLAY "Condition-2 TRUE"
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
     ELSE
       DISPLAY "Condition-2 FALSE"
       IF condition-3 THEN
         DISPLAY "Condition-3 TRUE"
       ELSE
         DISPLAY "Condition-3 FALSE"
       END-IF
     END-IF
   END-IF.

 END PROGRAM main.

