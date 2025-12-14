* Test packed decimal data type
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  e  PIC x  VALUE "E".
 01  em PIC xx VALUE "Em".

* signed short, long, long long
  01 s2  NATIVE-2 VALUE 22.
  01 s4  NATIVE-4 VALUE 23.
  01 s8  NATIVE-8 VALUE 24.

* packed signed, a.k.a., packed decimal (Chapter 7 of COBOL manual).
* Each digit is put in a nibble and number is right justified with
* sign in highest addressed nibble. 0xC indicates +, 0xD indicates -.
* Old (or maybe new!) IBM hardware had instructions based on this format.
* INFO ID: PAK SIGN
* memory: %H017C, %h02240C, %H2D, %H023C
* vi/inspect: PS =  +17, PS2 =  +22.40, PS3 = -2, PS4 =  +2.3

  01  ps  PIC S99        COMP-3   VALUE 17.
  01  ps2 PIC S999V99    COMP-3   VALUE 22.4.
  01  ps3 Packed-Decimal Pic S9   VALUE -2.
  01  ps4 Packed-Decimal Pic S9V9 VALUE  2.3.

* The PP indicates base 10 exponent to multiply memory.
* Analogous to tal's negative scale.  Memory here is 7C.
  01  ps5 PIC S9PP       COMP-3   VALUE 700.


* packed unsigned, a.k.a., packed decimal
* Each digit is put in a nibble and number is right justified with
* sign in highest addressed nibble. 0xF indicates unsigned.
* Old (or maybe new!) IBM hardware had instructions based on this format.
* INFO ID: PAK UNSIGN
* memory: %H018F, %h12345F, %H4F, %H01816F
* vi/inspect: PU =   18, PU2 =  123.45, PU3 =  4, PU4 =   18.16

  01  pu  PIC  99        COMP-3 VALUE 18.
  01  pu2 PIC  999V99    COMP-3 VALUE 123.45.
  01  pu3 Packed-Decimal Pic 9  VALUE 4.
  01  pu4 Packed-Decimal Pic 99V99 VALUE 18.16.

* Test cases from case 10-041215-5823
* shown as "S9(4) PACKED-DECIMAL"
  01  tc1 PIC S9 COMP-3 VALUE 1.

* shown as "S9(4)P(254) PACKED-DECIMAL"
  01  tc2 PIC S99V99 COMP-3 VALUE 12.34.

* shown as "S9(4) PACKED-DECIMAL"
  01  tc3 PIC S9(18) COMP-3 VALUE 123456789123456789.

* shown as "S9(4)P(238) PACKED-DECIMAL"
  01  tc4 PIC SV9(18) COMP-3 VALUE .987654321987654321.

* these are here because eCOBOL broke scale for all variable types,
* not just packed decimal. The P stands for "padding to the right".

  01  comp1 PIC S99V9 COMP VALUE 34.5.
  01  comp2 PIC 999PP COMP VALUE 78900.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Display ps.
   Display ps2.
   Display ps3.
   Display ps4.

   Display pu.
   Display pu2.
   Display pu3.
   Display pu4.

   Display tc1.
   Display tc2.
   Display tc3.
   Display tc4.

   Move 17 to s2.
   Move 18 to s4.
   Move 19 to s8.

   Display "End main".

 END PROGRAM main.
