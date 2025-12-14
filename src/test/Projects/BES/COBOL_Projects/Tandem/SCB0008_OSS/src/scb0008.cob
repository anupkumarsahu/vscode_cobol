* contains two 2 second level and one third main.dog.cat main.dogdog
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  ms  NATIVE-2 VALUE 22.
  01  mg  NATIVE-2 VALUE 23 GLOBAL.

 PROCEDURE DIVISION.
 start-main.

   Display "Start Main".
   PERFORM Call-Main-Level2.
   Display "Back from Main-Level2".
   STOP RUN.

 Call-Main-Level2.
   Move 122 to ms.
   Move 123 to mg.
   Call "Main-Level2".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  Main-Level2.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ds  NATIVE-2 VALUE 122.
  01  dg  NATIVE-2 VALUE 123 GLOBAL.

 PROCEDURE DIVISION.
 Start-Main-Level2.

   Display "Start Main-Level2".
   Move 33 to mg.
   Display "End Main-Level2".

 END PROGRAM Main-Level2.
 END PROGRAM main.
