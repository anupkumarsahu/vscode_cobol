* contains two 2 second level and one third main.dog.cat main.dogdog
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  ms  NATIVE-2 VALUE 22.
  01  mg  NATIVE-2 VALUE 23 GLOBAL.

* this global has the same name as a non-global in dog
* when within cat, this variable should be visible.
  01  maindog NATIVE-2 VALUE 24 GLOBAL.

* Test GLOBAL with records.
* GLOBAL keyword allowed on rec definition only.
* It applies to all record items.
 01  rec GLOBAL.
     02 r11 PIC X(4) VALUE "Emma".
     02 r12 PIC X(4) VALUE "Sara".

 PROCEDURE DIVISION.
 start-main.

   Move 17 to ms.
   Move 18 to mg.

 Call-Dog.
   Call "dog".

 Call-Dogdog.
   Call "dogdog".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ds  NATIVE-2 VALUE 122.
  01  dg  NATIVE-2 VALUE 123 GLOBAL.

  01  maindog NATIVE-2 VALUE 124.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move 117 to ds.
   Move 118 to dg.
   Move 119 to mg.

 Call-Cat.
   Call "cat".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  cs  NATIVE-2 VALUE 222.

 PROCEDURE DIVISION.
 Start-Cat.

   DISPLAY "In cat".
   Move 217 to cs.
   Move 218 to dg.
   Move 219 to mg.
   Move 220 to maindog.

 END PROGRAM cat.
 END PROGRAM dog.

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*   The program has the initial and common attributes
 PROGRAM-ID.  dogdog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ds  NATIVE-2 VALUE 122.
  01  dg  NATIVE-2 VALUE 123 GLOBAL.

  01  maindog NATIVE-2 VALUE 124.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dogdog".
   Move 117 to ds.
   Move 118 to dg.
   Move 119 to mg.

 END PROGRAM dogdog.

 END PROGRAM main.
