?MAIN MAIN
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MAIN.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  mi  NATIVE-2 VALUE 22.
  01  mj  NATIVE-2 VALUE 23.

* Externals
 01  ext     PIC XXX EXTERNAL.
 01  extglob PIC X(7) EXTERNAL GLOBAL.

* External record
 01  recext EXTERNAL.
     02 rx1 OCCURS 2.
        03  rx11 PIC X(14).

* External and Global record
 01  recextg EXTERNAL GLOBAL.
     02 eye  PIC X(3).
     02 rxg1 OCCURS 2.
        03  rxg11 PIC X(17).

* Simple global.
 01  gg     PIC XXXX VALUE "BOLG".
 01  glob   PIC XXXX GLOBAL VALUE "GLOB".

* Test GLOBAL with records.
 01  rec1 GLOBAL.
     02 r11 OCCURS 2.
        03  r111 PIC X(16).

 01  rec2 GLOBAL.
     02 r21 PIC X VALUE "J" OCCURS 2.

* TNS/E compiler put index variables within the record
* only for external records.
 01 recxarr EXTERNAL.
    02 arr1 pic xx occurs 3 times indexed by ix1.

 01 recarr.
    02 arr2 pic xxx occurs 4 times indexed by ix2.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 startt.

   CALL "Dog".

   Set ix1 to 2.
   Move "11" to arr1(1).
   Move "22" to arr1(2).
   Move "33" to arr1(3).
   Display recxarr.

   Set ix2 to 3.
   Move "444" to arr2(1).
   Move "555" to arr2(2).
   Move "666" to arr2(3).
   Move "777" to arr2(4).
   Display recarr.

   Move "EXT" to ext.
   Move "ExtGlob" to extglob.
   Move "rec1.r11(1).r111" to r11(1).
   Move "rec1.r11(2).r111" to r11(2).
   Move "rx.rx1(1).rx11" to rx1(1).
   Move "rx.rx1(2).rx11" to rx1(2).
   Move "rxg.rxg1(1).rxg11" to rxg1(1).
   Move "rxg.rxg1(2).rxg11" to rxg1(2).

 Call-Dog2.
   Call "dog".
 END PROGRAM MAIN.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  ws  PIC XXXXx VALUE "WsDog".
 01  ws1 PIC XXXXxx VALUE "Ws1Dog".

* Externals
 01  ext     PIC XXX EXTERNAL.

 01  recextg EXTERNAL GLOBAL.
     02 eye  PIC X(3).
     02 rxg1 OCCURS 2.
        03  rxg11 PIC X(17).

* External record
 01  recext EXTERNAL.
     02 rx1 OCCURS 2.
        03  rx11 PIC X(14).

 01 recxarr EXTERNAL.
    02 arr1 pic xx occurs 3 times indexed by ix1.

 PROCEDURE DIVISION.
 Startt.

   DISPLAY "In dog".

   Move "eye" to eye.

   Display ext.
   Display recxarr.

   Move "TXE" to ext.

 D-ws.
   DISPLAY ws.

 END PROGRAM dog.
