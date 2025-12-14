?SYMBOLS
* This file is for testing the INITIAL attribute for a program.
* This was never tested for TNS or TNS/R.
* When a program is INITIAL it means data is reinitialized every
* time the routine is called.  This is just like C or TAL.
* The default for COBOL is to treat data like C static.  That is,
* the value on entry is the same as the value at exit except for
* the first time when it might have some intialization value.
* Compiler will allow procedure nested within another with a
* different INITIAL attribute.  There is no keyword that means
* not-INITIAL-- just leave the keyword off.  For example:
* PROGRAM-ID. dog INITIAL.
* PROGRAM-ID. dog.
*
* Also, a nested routine does NOT inherit the attribute of its
* parent.  The parent can be initial and if the child program-id
* declaration does not specify initial, the child will be static.

?MAIN main
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

* signed short
  01  mi  NATIVE-2 VALUE 22.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 main-start.
   Display "Start of main".
   Display mi.

 main-start.
   Call "dog".
   Display "Back from dog 1".
   Call "dog".
   Display "Back from dog 2".
 main-start.
   Call "cat".
   Display "Back from cat 1".
   Call "cat".
   Display "Back from cat 2".

 0123456789.
   Call "emma".
   Display "Back from emma 1".
   Call "emma".
   Display "Back from emma 2".

 Call-sara.
   Call "sara".
   Display "Back from sara 1".
   Call "sara".
   Display "Back from sara 2".

   Display "End of main".
 END PROGRAM main.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog INITIAL.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  di  NATIVE-2 VALUE 32.

 PROCEDURE DIVISION.
 dog-start.

   DISPLAY "In dog".
   DISPLAY di.
   Move 132 to di.
   DISPLAY di.
   DISPLAY "Leaving dog".

 END PROGRAM dog.

 IDENTIFICATION DIVISION.
* data within a program without "initial" is treated as static
* that is, it is treated just like C static data.
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ci  NATIVE-2 VALUE 42.

 PROCEDURE DIVISION.
 cat-start.

   DISPLAY "In cat".
   DISPLAY ci.
   Move 142 to ci.
   DISPLAY ci.
   DISPLAY "Leaving cat".

 END PROGRAM cat.

 IDENTIFICATION DIVISION.
* EMMA
 PROGRAM-ID. EMMA INITIAL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

  01  ei  NATIVE-2 VALUE 52.
  01  eg  NATIVE-2 VALUE 252 GLOBAL.

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "Welcome to Emma!".
   DISPLAY ei.
   DISPLAY eg.
   Move 152 to ei.
   Move 352 to eg.
   DISPLAY ei.
   DISPLAY eg.

start-main.
   DISPLAY "Calling Lisa".
   Call "Lisa".
   DISPLAY "Returned from Lisa".
   Call "Lisa".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
*  The level 1 routine, emma, is initial but this routine
*  does not specify initial and is "static".
 PROGRAM-ID.  Lisa.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  li  NATIVE-2 VALUE 62.

 PROCEDURE DIVISION.
 Start-Lisa.

   DISPLAY "In Lisa".
   DISPLAY li.
*  DISPLAY ei. (ei is not GLOBAL)
   DISPLAY eg.
   Move 162 to li.
   DISPLAY li.
   DISPLAY "Leaving Lisa".

 END PROGRAM lisa.
 END PROGRAM emma.

 IDENTIFICATION DIVISION.
* Sara
 PROGRAM-ID. Sara INITIAL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

  01  si NATIVE-2 VALUE 72.
  01  sg NATIVE-2 VALUE 272 GLOBAL.

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "Welcome to Sara!".
   DISPLAY si.
   DISPLAY sg.
   Move 172 to si.
   Move 372 to sg.
   DISPLAY si.

 Call-jeff.
   DISPLAY "Calling jeff".
   Call "jeff".
   DISPLAY "Returned from jeff".
   Call "jeff".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  jeff INITIAL.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ji  NATIVE-2 VALUE 82.

 PROCEDURE DIVISION.
 Start-jeff.

   DISPLAY "In jeff".
   DISPLAY ji.
*  DISPLAY si. (si is not GLOBAL)
   DISPLAY sg.
   Move 182 to ji.
   DISPLAY ji.
   DISPLAY "Leaving jeff".

 END PROGRAM jeff.
 END PROGRAM sara.
