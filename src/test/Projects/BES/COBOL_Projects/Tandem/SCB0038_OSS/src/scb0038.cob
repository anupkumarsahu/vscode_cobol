?SYMBOLS
 IDENTIFICATION DIVISION.
 PROGRAM-ID. J.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
*     SELECT tqq-file ASSIGN TO #DYNAMIC
*         FILE STATUS IS tqq-fs
*         .
 DATA DIVISION.
 FILE SECTION.
* FD  tqq-file.
* 01  tqq-rec  PIC X(40).
 WORKING-STORAGE SECTION.
 01  int1   PIC 9(5) PACKED-DECIMAL VALUE 12345.
 01  int2   PIC 9(5) PACKED-DECIMAL VALUE 67890.
 01  temp   PIC X(12) VALUE "Temp1234ZYXW".

*check out FILLER items
 01 filler-rec.
    02 fr1 PIC X(4) VALUE "fr1x".
    02 FILLER PIC X(4) VALUE "fill".
    02 fr2 PIC X(4) VALUE "fr2x".

*Name collision with rec1 and subordinate fields
 01  rec1.
     02  r11  PIC X(8) VALUE "rec1.r11".
     02  r12  PIC X(8) VALUE "rec1.r12".
         02  r13  PIC X(8) VALUE "rec1.r13".

 01  rec1a.
     02  r11a  PIC X(10) VALUE "rec1a.r11a".
     02  r12a  PIC X(10) VALUE "rec1a.r12a".
         02  r13a  PIC X(10) VALUE "rec1a.r13a".

*Name collision with rec2 and subordinate fields
 01  rec2.
         02 r21 PIC X(4) VALUE "Emma".
         02 r22 PIC X(4) VALUE "Sara".
         02 r23.
             03  r231  PIC X(10) VALUE "WorldHello".
         03  r232  PIC X(20) VALUE "abcdefghij0123456789".
         03  r233  PIC X(20) VALUE "3131313131ABCDEFGHIJ".

 01  rec2a.
         02 r21a PIC X(4) VALUE "Emma".
         02 r22a PIC X(4) VALUE "Sara".
         02 r23a.
             03  r231a  PIC X(10) VALUE "WorldHello".
         03  r232a  PIC X(20) VALUE "abcdefghij0123456789".
         03  r233a  PIC X(20) VALUE "3131313131ABCDEFGHIJ".

*Name collision with r31
 01  rec3.
     02 r31.
        03  r311 PIC X(13) VALUE "rec3.r31.r311".
        03  r312 PIC X(9)  VALUE "r312.r312".

 01  rec3a.
     02 r31a.
        03  r311a PIC X(16) VALUE "rec3a.r31a.r311a".
        03  r312a PIC X(16) VALUE "rec3a.r31a.r312a".

* field before and after occurs
 01  rec4.
     02 r42 PIC X(8) VALUE "rec4.r42".
     02 r41 OCCURS 2.
        03  r411 PIC X(16) VALUE "rec4.r41[1].r411".
        03  r412 PIC X(16) VALUE "rec4.r41[1].r412".
     02 r43 PIC X(8) VALUE "rec4.r43".

* occurs follows immediately after 01 field, nested occurs.
 01  rec5.
     02 r51 OCCURS 2.
        03  r511 PIC X(16) VALUE "rec5.r51[1].r511".
        03  r512 PIC X(16) VALUE "rec5.r51[1].r512".
        03  r513 OCCURS 3.
            04 r5131 PIC X(3) VALUE "111".
            04 r5132 PIC X(3) VALUE "112".

* nested occurs with intervening record without occurs clause
 01  rec6.
     02 r61.
                03  r611 OCCURS 2.
                    04 r6111 PIC X(5) VALUE "r6111".
                    04 r6112 PIC X(5) VALUE "r6112".
                        04 r6113.
                           05 r61131 PIC X(2) VALUE "X2" OCCURS 4.
                           05 R61132 OCCURS     3.
                              06 R611321 PIC X VALUE "A".
                              06 R611322 PIC X VALUE "B".

* cobol only puts 5tqs in first TIR record because then no test for
* depending on variable in case it is part of 6th dim is needed.
* remember, max of 6 tqs per TIR record is allowed and depedening on
* variable uses one of them.

 01 rectq.
    02 rtq1 OCCURS 4.
       03 rtq2 OCCURS 2.
          04 rtq3 OCCURS 3.
             05 rtq4 OCCURS 5.
                06 rtq5 OCCURS 2.
                   07 rtq6 PIC X(2).
 01 rec7.
    02 r71 OCCURS 4.
       03 r72 OCCURS 2.
          04 r73 OCCURS 2.
             05 r74 OCCURS 5.
                06 r75 OCCURS 2.
                   07 r76 OCCURS 3.
                       08 r77 PIC X(2).

 01 rec8.
    02 r81 OCCURS 4.
       03 r82 OCCURS 2.
          04 r83 OCCURS 2.
             05 r84 OCCURS 5.
                06 r85 OCCURS 2.
                   07 r86 OCCURS 2.
                      08 r87 OCCURS 3.
                         09 r88 PIC X(2).

*index variable has wrong parent offset.
 01 rec9.
    02 r91   pic x  occurs 7 times indexed by r91i.
    02 r92   pic 99.
    02 r93   pic x  occurs 1 to 10 times depending on r92
                    indexed by r93i.

 01  recx.
     02 rx1 PIC X  VALUE "j"  OCCURS 10 TIMES.
     02 rx2 PIC XX VALUE "JL" OCCURS  9 TIMES.
     02 rx3 PIC 9  VALUE  9   OCCURS  8 TIMES.
     02 rx4 PIC 99 VALUE  78  OCCURS  7 TIMES.

*Test some name collisions
 01 reclast.
    02 int1  PIC X(10) VALUE "Recl.int1 ".
    02 rec1  PIC X(10) VALUE "Recl.rec1 ".
    02 r21   PIC X(10) VALUE "Recl.r21  ".
    02 r231  PIC X(10) VALUE "Recl.r231 ".
        02 r31.
       03 int1  PIC X(13) VALUE "Recl.r31.int1".
       03 rec2  PIC X(13) VALUE "Recl.r31.rec2".
       03 r22   PIC X(12) VALUE "Recl.r31.r22".
       03 r23   PIC X(12) VALUE "Recl.r31.r23".
       03 r233  PIC X(13) VALUE "Recl.r31.r233".

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 startt.
   Move "rec4.r41[2].r411" to r411 of r41(2).
   Move "rec4.r41[2].r412" to r412 of r41(2).

   Move "rec5.r51[2].r511" to r511 of r51(2).
   Move "rec5.r51[2].r512" to r512 of r51(2).

   Move "121" to r5131 of r513 of r51 (1, 2).
   Move "122" to r5132 of r513 of r51 (1, 2).
   Move "131" to r5131 of r513 of r51 (1, 3).
   Move "132" to r5132 of r513 of r51 (1, 3).

   Move "211" to r5131 of r513 of r51 (2, 1).
   Move "212" to r5132 of r513 of r51 (2, 1).
   Move "221" to r5131 of r513 of r51 (2, 2).
   Move "222" to r5132 of r513 of r51 (2, 2).
   Move "231" to r5131 of r513 of r51 (2, 3).
   Move "232" to r5132 of r513 of r51 (2, 3).

   MOVE "r6111r6112X2X2X2X2ABCDEF" to r611(1).
   MOVE "rJEFFrSARAY2Y2Y2Y2GHIJKL" to r611(2).

   Move "44" to rtq6 (1, 1, 1, 1, 1).
   Move "55" to rtq6 (4, 2, 3, 5, 2).

   Move "66" to r77 (1, 1, 1, 1, 1, 1).
   Move "77" to r77 (4, 2, 2, 5, 2, 3).

   Move "88" to r88 (1, 1, 1, 1, 1, 1, 1).
   Move "99" to r88 (4, 2, 2, 5, 2, 2, 3).

     move "abcdefg051234567890" to rec9.
     Display rec9.
     set r91i to 3.
     display r91(r91i).
     set r93i to 2.
     display r93(r93i).

 start-calls.
   Call K using int2 rec1a rec2a rec3a temp.
   Call L using rec4 rec5 rec6.
   Call M using rec7 rec8 rec9.
 END PROGRAM J.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. K.

 data division.
 linkage section.
 01 p1    PIC 9(5) PACKED-DECIMAL.

 01 p2.
    02  p21  PIC X(10).
    02  p22  PIC X(10).
    02  p23  PIC X(10).

 01 p3.
         02 p31 PIC X(4).
         02 p32 PIC X(4).
         02 p33.
            03  p331  PIC X(10).
        03  p332  PIC X(20).
        03  p333  PIC X(20).

 01 p4.
     02 p41.
        03  p411 PIC X(16).
        03  p412 PIC X(16).

 01 p5  PIC X(12).

*Variables in the linkage section that are not parameters are pointers
*they are allocated VFP (-) relative.
 01 retinue.
    02  p21  PIC X(10).
    02  p22  PIC X(10).
    02  p23  PIC X(10).

 PROCEDURE DIVISION using p1 p2 p3 p4 p5.
 Sunset.
     display "Inside Program K".
     set address of retinue to address of p2.
 Leaving-K.
     display "Leaving Program K".
*     display "p5:" p5.
 end program K.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. L.

 data division.
 linkage section.
 01  p1.
     02 r42 PIC X(8).
     02 r41 OCCURS 2.
        03  r411 PIC X(16).
        03  r412 PIC X(16).
     02 r43 PIC X(8).

* occurs follows immediately after 01 field, nested occurs.
 01  p2.
     02 r51 OCCURS 2.
        03  r511 PIC X(16).
        03  r512 PIC X(16).
        03  r513 OCCURS 3.
            04 r5131 PIC X(3).
            04 r5132 PIC X(3).

* nested occurs with intervening record without occurs clause
 01  p3.
     02 r61.
                03  r611 OCCURS 2.
                    04 r6111 PIC X(5).
                    04 r6112 PIC X(5).
                        04 r6113.
                           05 r61131 PIC X(2) OCCURS 4.
                           05 R61132 OCCURS     3.
                              06 R611321 PIC X.
                              06 R611322 PIC X.

 PROCEDURE DIVISION using p1 p2 p3.
 Sunset.
     display "Inside Program L".
     display "Leaving Program L".
 end program L.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. M.

 data division.
 linkage section.
 01 p1.
    02 r71 OCCURS 4.
       03 r72 OCCURS 2.
          04 r73 OCCURS 2.
             05 r74 OCCURS 5.
                06 r75 OCCURS 2.
                   07 r76 OCCURS 3.
                       08 r77 PIC X(2).

 01 p2.
    02 r81 OCCURS 4.
       03 r82 OCCURS 2.
          04 r83 OCCURS 2.
             05 r84 OCCURS 5.
                06 r85 OCCURS 2.
                   07 r86 OCCURS 2.
                      08 r87 OCCURS 3.
                         09 r88 PIC X(2).

 01 p3.
    02 r91   pic x  occurs 7 times indexed by r91i.
    02 r92   pic 99.
    02 r93   pic x  occurs 1 to 10 times depending on r92
                    indexed by r93i.

 PROCEDURE DIVISION using p1 p2 p3.
 Sunset.
     display "Inside Program M".
     set r91i to 4.
     display r91(r91i).
     Move "08" to r92.
     set r93i to 7.
     display r93(r93i).
 Leaving-M.
     display "Leaving Program M".
 end program M.

