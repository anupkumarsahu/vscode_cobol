?MAIN main
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  mi  NATIVE-2 VALUE 22.
  01  mj  NATIVE-2 VALUE 23.
  01  mk  NATIVE-2 VALUE 24.
  01  ml  NATIVE-2 VALUE 25.
  01  mm  NATIVE-2 VALUE 26.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 main-start.
   Display "Start of main".
   Display mi.

 Call-Dog.
   Display "calling all dogs".
   Call "dog".
   Display "Back from dog".
   Display "calling all dogs again:  test INITIAL".
   Call "dog".
   Display "Back from dog".
 Call-cat.
   Display "calling all cats".
   Call "cat".
   Display "Back from cat".
   Display "calling all cats again:  test static".
   Call "cat".
   Display "Back from cat".

   Display "End of main".
 END PROGRAM main.

 IDENTIFICATION DIVISION.
* PROGRAM-ID.  dog.
 PROGRAM-ID.  dog INITIAL.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  di  NATIVE-2 VALUE 32.
  01  dj  NATIVE-2 VALUE 33.
  01  dk  NATIVE-2 VALUE 34.
  01  dl  NATIVE-2 VALUE 35.
  01  dm  NATIVE-2 VALUE 36.

 PROCEDURE DIVISION.
 dog-start.

   DISPLAY "In dog".
   DISPLAY di.
   Move 132 to di.
   DISPLAY di.
   DISPLAY "Leaving dog".

 END PROGRAM dog.

 IDENTIFICATION DIVISION.
* PROGRAM-ID.  cat.
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ci  NATIVE-2 VALUE 42.
  01  cj  NATIVE-2 VALUE 43.
  01  ck  NATIVE-2 VALUE 44.
  01  cl  NATIVE-2 VALUE 45.
  01  cm  NATIVE-2 VALUE 46.

 PROCEDURE DIVISION.
 cat-start.

   DISPLAY "In cat".
   DISPLAY ci.
   Move 142 to ci.
   DISPLAY ci.
   DISPLAY "Leaving cat".

 END PROGRAM cat.

