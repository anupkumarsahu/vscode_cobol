* one 3rd level routine: main.dog.cat
* without a lot of decs.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

 01  emmain PIC x(4)  VALUE "Emma".
 01  recmain.
     02 rm NATIVE-2.
     02 am NATIVE-2 occurs 4 times.

 PROCEDURE DIVISION.
 start-main.

   Move "Sara" to emmain.

 Call-Dog.
   Call "dog".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  emdog PIC x(4)  VALUE "jeff".
 01  recdog.
     02 rg NATIVE-2.
     02 ag NATIVE-2 occurs 3 times.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move "booo" to emdog.

 Call-Cat.
   Call "cat".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  emcat PIC x(4)  VALUE "lisa".
 01  reccat.
     02 rc NATIVE-2.
     02 ac NATIVE-2 occurs 2 times.

 PROCEDURE DIVISION.
 Start-Cat.

   DISPLAY "In cat".
   Move "lizy" to emcat.

 END PROGRAM cat.
 END PROGRAM dog.
 END PROGRAM main.
