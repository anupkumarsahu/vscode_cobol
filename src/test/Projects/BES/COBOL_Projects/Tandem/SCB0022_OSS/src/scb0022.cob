 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  e  PIC x  VALUE "E".
 01  em PIC xx VALUE "Em".

* signed short, long, long long
  01 s2  NATIVE-2 VALUE 22.
     88 nata       VALUE  11.
     88 natb       VALUE  22.
  01 s4  NATIVE-4 VALUE 23.
  01 s8  NATIVE-8 VALUE 24.

* signed and unsigned short
  01 ss  PIC S9999 COMP.
  01 us  PIC  9999 COMP.
  01 ss2 Computational Pic S9(4).
  01 us2 Computational Pic  9(4).
  01 ss3 PIC  S999V9 COMP VALUE 234.5.
  01 us3 PIC   999V9 COMP VALUE 334.5.

  01 ss4 PIC S999PP COMP VALUE 71700.
  01 us4 PIC   99P  COMP VALUE 220.

  01 ss8 PIC  S9(16)V99 VALUE -1234567890123456.78.
  01 us8 PIC   9(16)V99 VALUE 1234567890123456.78.

* In the HP NonStop implementation, BINARY = COMPUTATIONAL

  01 bu  Binary  Pic 9.
  01 bs  Binary  Pic S9.


* Numeric unsigned or unsigned numeric string
* INFO ID: NUM UNSIGN
* memory: "22" "12345"
* vi/inspect: 22, 123.45.

  01 nu  PIC  99     VALUE 22.
  01 nu2 PIC  999V99 VALUE 123.45.
  01 nu3 Display Pic 9.
  01 nu4 PIC  999PP  VALUE 78900.


* signed trailing embedded
* high order bit of trailing byte (highest address) indicates sign
* positive numbers look like NUM UNSIGN in memory.
* INFO ID: NUM TR EM
* memory:  "17" "221"  "1" but <0> = 1, "16" but <8> = 1.
* vi/inspect: 17, 22.1, -1, -16

  01 te  PIC S99         VALUE 17.
  01 te2 PIC S99V9       VALUE 22.1.
  01 te3 Display Pic S9  VALUE -1.
  01 te4 Display Pic S99 Sign is Trailing VALUE -16.


* signed trailing separate
* sign is an ascii byte following ascii data
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM TR SP
* memory: "2+" "18+" "019-" "0020-"
* vi/inspect: TS =  2, TS2 =  18, TS3 =  -19, TS4 =   -20

  01 ts  Display  Pic S9    Sign is Trailing Separate VALUE 2.
  01 ts2 Display  Pic S99   Sign is Trailing Separate VALUE 18.
  01 ts3 Display  Pic S9(3) Sign is Trailing Separate VALUE -19.
  01 ts4 Display  Pic S9(4) Sign is Trailing Separate VALUE -20.


* signed leading embedded
* high order bit of leading byte (lowest address) indicates sign
* positive numbers look like NUM UNSIGN in memory.
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM LD EM
* memory:  "1" "02"  "003" but <0> = 1, "0004" but <0> = 1.
* vi/inspect: 1, 2, -3, -4

  01 le  Display  Pic S9    Sign is Leading VALUE  1.
  01 le2 Display  Pic S99   Sign is Leading VALUE  2.
  01 le3 Display  Pic S9(3) Sign is Leading VALUE -3.
  01 le4 Display  Pic S9(4) Sign is Leading VALUE -4.


* signed leading separate
* sign is an ascii byte preceding ascii data
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM LD SP
* memory: "+9" "+19" "-129" "-1234"
* vi/inspect: LS =  9, LS2 =  19, LS3 = -129, LS4 = -1234

  01 ls  Display Pic S9    Sign is Leading Separate VALUE 9.
  01 ls2 Display Pic S99   Sign is Leading Separate VALUE 19.
  01 ls3 Display Pic S9(3) Sign is Leading Separate VALUE -129.
  01 ls4 Display Pic S9(4) Sign is Leading Separate VALUE -1234.


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

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 17 to s2.
   Move 18 to s4.
   Move 19 to s8.


   Display "End main".

 END PROGRAM main.
