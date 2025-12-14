 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* 01  e  PIC x  EXTERNAL.
* 01  em PIC xx EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    E   | 1  |unsigned char  | DI |   |   |     |
*    EM  | 2  |unsigned char  | DI |   |   |     |
 01  e  DISPLAY PIC x  EXTERNAL.
 01  em DISPLAY PIC x(2) EXTERNAL.
 01  e4 PIC x(4) EXTERNAL.
 01  e5 PIC xxxxx EXTERNAL.

* signed short, long, long long
  01 s2  NATIVE-2 EXTERNAL.
  01 s4  NATIVE-4 EXTERNAL.
  01 s8  NATIVE-8 EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    S2  | 2  |signed         | NA |   |   |     |
*    S4  | 4  |signed         | NA |   |   |     |
*    S8  | 8  |signed         | NA |   |   |     |

* signed and unsigned short
  01 ss  PIC S9999 COMP EXTERNAL.
  01 us  PIC  9999 COMP EXTERNAL.
  01 ss2 Computational Pic S9(4) EXTERNAL.
  01 us2 Computational Pic  9(4) EXTERNAL.
  01 ss3 PIC  S999V9 COMP EXTERNAL.
  01 us3 PIC   999V9 COMP EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    SS  | 2  |signed_fixed   | CO |   | 4 | 0   |
*    US  | 2  |unsigned_fixed | CO |   | 4 | 0   |
*    SS2 | 2  |signed_fixed   | CO |   | 4 | 0   |
*    US2 | 2  |unsigned_fixed | CO |   | 4 | 0   |
*    SS3 | 2  |signed_fixed   | CO |   | 4 | 255 |
*    US3 | 2  |unsigned_fixed | CO |   | 4 | 255 |

  01 ss4 PIC S999PP COMP EXTERNAL.
  01 us4 PIC   99P  COMP EXTERNAL.
  01 ss5 PIC S999999PP COMP EXTERNAL.
  01 us5 PIC   99999P  COMP EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    SS4 | 2  |signed_fixed   | CO |   | 3 | 2   |
*    US4 | 2  |unsigned_fixed | CO |   | 2 | 1   |
*    SS5 | 4  |signed_fixed   | CO |   | 6 | 2   |
*    US5 | 4  |unsigned_fixed | CO |   | 5 | 1   |

* In the HP NonStop implementation, BINARY = COMPUTATIONAL

  01 bu  Binary  Pic 9 EXTERNAL.
  01 bs  Binary  Pic S9 EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    BU  | 2  |unsigned_fixed | CO |   | 1 | 0   |
*    BS  | 2  |signed_fixed   | CO |   | 1 | 0   |

* Numeric unsigned or unsigned numeric string
* INFO ID: NUM UNSIGN
* memory: "22" "12345"
* vi/inspect: 22, 123.45.

  01 nu  PIC  99     EXTERNAL.
  01 nu2 PIC  999V99 EXTERNAL.
  01 nu3 Display Pic 9 EXTERNAL.
  01 nu4 PIC  999PP  EXTERNAL.
  01 nu5 PIC  PP999  EXTERNAL.
  01 nu6 PIC  P999  EXTERNAL.
  01 nu7 PIC  V999  EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    NU  | 2  |zoned_decimal  | DI | 1 | 2 | 0   |
*    NU2 | 5  |zoned_decimal  | DI | 1 | 5 | 254 |
*    NU3 | 1  |zoned_decimal  | DI | 1 | 1 | 0   |
*    NU4 | 3  |zoned_decimal  | DI | 1 | 3 | 2   |
*    NU5 | 3  |zoned_decimal  | DI | 1 | 3 | 251 |

* signed trailing embedded
* high order bit of trailing byte (highest address) indicates sign
* positive numbers look like NUM UNSIGN in memory.
* INFO ID: NUM TR EM
* memory:  "17" "221"  "1" but <0> = 1, "16" but <8> = 1.
* vi/inspect: 17, 22.1, -1, -16

  01 te  PIC S99         EXTERNAL.
  01 te2 PIC S99V9       EXTERNAL.
  01 te3 Display Pic S9  EXTERNAL.
  01 te4 Display Pic S99 Sign is Trailing EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    TE  | 2  |zoned_decimal  | DI | 3 | 2 | 0   |
*    TE2 | 3  |zoned_decimal  | DI | 3 | 3 | 255 |
*    TE3 | 1  |zoned_decimal  | DI | 3 | 1 | 0   |
*    TE4 | 2  |zoned_decimal  | DI | 3 | 2 | 0   |

* signed trailing separate
* sign is an ascii byte following ascii data
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM TR SP
* memory: "2+" "18+" "019-" "0020-"
* vi/inspect: TS =  2, TS2 =  18, TS3 =  -19, TS4 =   -20

  01 ts  Display  Pic S9    Sign is Trailing Separate EXTERNAL.
  01 ts2 Display  Pic S99   Sign is Trailing Separate EXTERNAL.
  01 ts3 Display  Pic S9(3) Sign is Trailing Separate EXTERNAL.
  01 ts4 Display  Pic S9(4) Sign is Trailing Separate EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    TS  | 2  |zoned_decimal  | DI | 5 | 1 | 0   |
*    TS2 | 3  |zoned_decimal  | DI | 5 | 2 | 0   |
*    TS3 | 4  |zoned_decimal  | DI | 5 | 3 | 0   |
*    TS4 | 5  |zoned_decimal  | DI | 5 | 4 | 0   |

* signed leading embedded
* high order bit of leading byte (lowest address) indicates sign
* positive numbers look like NUM UNSIGN in memory.
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM LD EM
* memory:  "1" "02"  "003" but <0> = 1, "0004" but <0> = 1.
* vi/inspect: 1, 2, -3, -4

  01 le  Display  Pic S9    Sign is Leading EXTERNAL.
  01 le2 Display  Pic S99   Sign is Leading EXTERNAL.
  01 le3 Display  Pic S9(3) Sign is Leading EXTERNAL.
  01 le4 Display  Pic S9(4) Sign is Leading EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    LE  | 1  |zoned_decimal  | DI | 2 | 1 | 0   |
*    LE2 | 2  |zoned_decimal  | DI | 2 | 2 | 0   |
*    LE3 | 3  |zoned_decimal  | DI | 2 | 3 | 0   |
*    LE4 | 4  |zoned_decimal  | DI | 2 | 4 | 0   |

* signed leading separate
* sign is an ascii byte preceding ascii data
* nothing in C010REFA
* from jeff lanam's DWDEC
* INFO ID: NUM LD SP
* memory: "+9" "+19" "-129" "-1234"
* vi/inspect: LS =  9, LS2 =  19, LS3 = -129, LS4 = -1234

  01 ls  Display Pic S9    Sign is Leading Separate EXTERNAL.
  01 ls2 Display Pic S99   Sign is Leading Separate EXTERNAL.
  01 ls3 Display Pic S9(3) Sign is Leading Separate EXTERNAL.
  01 ls4 Display Pic S9(4) Sign is Leading Separate EXTERNAL.
*   NAME bytes basetype      name sign count scale
*    LS  | 2  |zoned_decimal  | DI | 4 | 1 | 0   |
*    LS2 | 3  |zoned_decimal  | DI | 4 | 2 | 0   |
*    LS3 | 4  |zoned_decimal  | DI | 4 | 3 | 0   |
*    LS4 | 5  |zoned_decimal  | DI | 4 | 4 | 0   |

* packed signed, a.k.a., packed decimal (Chapter 7 of COBOL manual).
* Each digit is put in a nibble and number is right justified with
* sign in highest addressed nibble. 0xC indicates +, 0xD indicates -.
* Old (or maybe new!) IBM hardware had instructions based on this format.
* INFO ID: PAK SIGN
* memory: %H017C, %h02240C, %H2D, %H023C
* vi/inspect: PS =  +17, PS2 =  +22.40, PS3 = -2, PS4 =  +2.3

  01  ps  PIC S99        COMP-3   EXTERNAL.
  01  ps2 PIC S999V99    COMP-3   EXTERNAL.
  01  ps3 Packed-Decimal Pic S9   EXTERNAL.
  01  ps4 Packed-Decimal Pic S9V9 EXTERNAL.
*    NAME bytes basetype      name sign count scale
*     PS  | 2  |packed_decimal | PD | 5 | 2 | 0   |
*     PS2 | 3  |packed_decimal | PD | 5 | 5 | 254 |
*     PS3 | 1  |packed_decimal | PD | 5 | 1 | 0   |
*     PS4 | 2  |packed_decimal | PD | 5 | 2 | 255 |

* packed unsigned, a.k.a., packed decimal
* Each digit is put in a nibble and number is right justified with
* sign in highest addressed nibble. 0xF indicates unsigned.
* Old (or maybe new!) IBM hardware had instructions based on this format.
* INFO ID: PAK UNSIGN
* memory: %H018F, %h12345F, %H4F, %H01816F
* vi/inspect: PU =   18, PU2 =  123.45, PU3 =  4, PU4 =   18.16

  01  pu  PIC  99        COMP-3 EXTERNAL.
  01  pu2 PIC  999V99    COMP-3 EXTERNAL.
  01  pu3 Packed-Decimal Pic 9  EXTERNAL.
  01  pu4 Packed-Decimal Pic 99V99 EXTERNAL.
*    NAME bytes basetype      name sign count scale
*     PU  | 2  |packed_decimal | PD | 1 | 2 | 0   |
*     PU2 | 3  |packed_decimal | PD | 1 | 5 | 254 |
*     PU3 | 1  |packed_decimal | PD | 1 | 1 | 0   |
*     PU4 | 3  |packed_decimal | PD | 1 | 4 | 254 |

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 17 to s2.
   Move 18 to s4.
   Move 19 to s8.

   Display s2.
   Display guardian-err.

   Display "End main".

 END PROGRAM main.
