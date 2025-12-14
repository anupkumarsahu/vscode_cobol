 IDENTIFICATION DIVISION.
 PROGRAM-ID.   C00UCNA.
 AUTHOR.       Kenneth Luu.
 INSTALLATION. TANDEM SOFTWARE DEPARTMENT.
 DATE-WRITTEN. 13 OCT 1986.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
* The COBOL85 conformance test C00UCNA tests
*
*      USAGE clause ... NATIVE-n
*
* This COBOL85 Tandem extension feature provides direct access to the
* Tandem NonStop architecture's facilities for storing and
* manipulating signed integer numeric values.  The USAGE clause is
* extended by adding three new options:
*
*      [ USAGE is ] { NATIVE-2 }
*                   { NATIVE-4 }
*                   { NATIVE-8 }
*
* The suffix "-n" indicates the numbers of bytes occupied by a data
* item to which a NATIVE-n usage applies.  Tandem COBOL85 will
* interpret the value stored in one of these data items as a signed,
* twos-complement binary interger.  Thus NATIVE-2 correspons to TAL's
* INT, NATIVE-4 to TAL's INT(32), and NATIVE-8 to TAL's FIXED(0).
*
* This COBOL85 source file refers to the COBOL85 COPY library file
* C00QALIB in the subvolume L57SV2.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     COPY COPYLIB-PRINT-FILE IN "C00QALIB".

 DATA DIVISION.
 FILE SECTION.
     COPY COPYLIB-STANDARD-FILE IN "C00QALIB".

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.
     COPY COPYLIB-STANDARD-DATA IN "C00QALIB"
          REPLACING "MMMA" BY "OCT "
                     "YYYA" BY "1986"
                     "MMMB" BY "OCT "
                     "YYYB" BY "1986".
     COPY COPYLIB-PROG-ID IN "C00QALIB"
          REPLACING "XXXXXXX" BY "C00UCNA".
*
* Initialize the elementary data item i1 to -32768
*
 77 i1 USAGE is NATIVE-2 VALUE is -32768.
*
* Initialize the elementary data item i2 to 9009.
* Please note that 9009 is the ASCII representation of "#1"
*
 77 i2 USAGE NATIVE-2 VALUE 9009.
 77 ri2 REDEFINES i2 PIC XX.
*
* Initialize the elementary data item i3 to 32767
*
 01 i3 NATIVE-2 VALUE 32767.
*
* Try native-n data item in a group data item
*
 01 g1.
    05 i4 NATIVE-2 VALUE ZERO.
    05 g2 REDEFINES i4.
       10 unsigned-16-integer-1.
          15 c-place-1            PIC     X.
          15 unsigned-8-integer-1 PICTURE X.
 01 g3 USAGE IS NATIVE-2.
    03 i5.
    03 i6.
 77 d1 USAGE NATIVE-4 VALUE 99999.
 01 d2 NATIVE-4 VALUE ZEROES.
 77 d3 USAGE IS NATIVE-4 VALUE ZEROS.
 77 d4 VALUE ZERO USAGE IS NATIVE-4.
 01 g4 USAGE NATIVE-4.
    04 d5.
    04 d6.
 01 q1 VALUE 000022222222222222 USAGE NATIVE-8.
 77 q2 USAGE NATIVE-8.
 01 q3 NATIVE-8 VALUE ZEROES.
 77 q4 VALUE ZEROS USAGE IS NATIVE-8.
 01 g6 NATIVE-8.
    05 q5.
    05 q6.

?HEADING "EXTENDED-STORAGE SECTION"
/
 EXTENDED-STORAGE SECTION.
*
* Initialize the elementary data item i1e to -32768
*
 77 i1e USAGE is NATIVE-2 VALUE is -32768.
*
* Initialize the elementary data item i2e to 9009.
* Please note that 9009 is the ASCII representation of "#1"
*
 77 i2e USAGE NATIVE-2 VALUE 9009.
 77 ri2e REDEFINES i2e PIC XX.
*
* Initialize the elementary data item i3e to 32767
*
 01 i3e USAGE NATIVE-2 VALUE 32767.
*
* Try native-n data item in a group data item
*
 01 g1e.
    05 i4e USAGE NATIVE-2 VALUE ZERO.
    05 g2e REDEFINES i4e.
       10 unsigned-16-integer-1e.
          15 c-place-1e            PIC     X.
          15 unsigned-8-integer-1e PICTURE X.
 77 d1e USAGE NATIVE-4 VALUE 99999.
 01 q1e VALUE 000022222222222222 USAGE NATIVE-8.

?HEADING "MAIN SECTION"
/
 PROCEDURE DIVISION.
 main SECTION.

     PERFORM copylib-open-file
     PERFORM test-1
     PERFORM test-2
     PERFORM test-3
     PERFORM test-3-1
     PERFORM test-4
     PERFORM test-4-1
     PERFORM test-5
     PERFORM test-6
     PERFORM test-7
     PERFORM test-7-1
     PERFORM test-8
     PERFORM test-8-1
     PERFORM test-8-3
     PERFORM test-8-4
     PERFORM test-8-5
     PERFORM test-8-6
     PERFORM test-8-7
     PERFORM test-8-8
     PERFORM test-8-9
     PERFORM test-9
     PERFORM test-10
     PERFORM test-11
     PERFORM test-11-1
     PERFORM test-12
     PERFORM test-12-1
     PERFORM test-12-2
     PERFORM test-12-3
     PERFORM test-12-4
     PERFORM test-12-5
     PERFORM test-13
     PERFORM test-14
     PERFORM test-15
     PERFORM test-16
     PERFORM test-17
     PERFORM test-18
     PERFORM test-19
     PERFORM test-20
     PERFORM test-21
     PERFORM test-22
     PERFORM test-23
     PERFORM test-24
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-1 SECTION"
/
 test-1 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-1" TO par-name
     MOVE "Initalize a native-2 item" TO re-mark
     MOVE i1 TO computed-i
     MOVE -32768 TO correct-i
     IF i1 = -32768 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-2 SECTION"
/
 test-2 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-2" TO par-name
*
* i2 contains the value 9009 which is the ASCII
* representation of "#1".  The elementary data item ri2 (PIC XX)
* redefines the data item i2 (USAGE NATIVE-2).
*
     MOVE ri2 TO computed-a
     MOVE "#1" TO correct-a
     MOVE "REDEFINES native-2 to PIC XX" TO re-mark
     IF ri2 = "#1" THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-3 SECTION"
/
 test-3 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-3" TO par-name
     MOVE i3 TO computed-i
     MOVE 32767 TO correct-i
     MOVE "Initialize a native-2 item" TO re-mark
     IF i3 = 32767 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-3-1 SECTION"
/
 test-3-1 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-3-1" TO par-name
     COMPUTE i3 = -134
     MOVE i3 TO computed-i
     MOVE -134 TO correct-i
     MOVE "COMPUTE a native-2 item" TO re-mark
     IF i3 = -134 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-4 SECTION"
/
 test-4 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-4" TO par-name
     MOVE i4 TO computed-i
     MOVE 0 TO correct-i
     MOVE "Initialize a native-2 item" TO re-mark
     IF i4 = 0 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-4-1 SECTION"
/
 test-4-1 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-4-1" TO par-name
     MOVE -1 TO i4
     MOVE i4 TO computed-i
     MOVE -1 TO correct-i
     MOVE "MOVE a native-2 item" TO re-mark
     IF i4 = -1 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-5 SECTION"
/
 test-5 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-5" TO par-name
     MOVE i1e TO computed-i
     MOVE "-32768" TO correct-a
     MOVE "Initalize a native-2 item" TO re-mark
     IF i1e = -32768 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-6 SECTION"
/
 test-6 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-6" TO par-name
*
* i2e contains the value 9009 which is the ASCII
* representation of "#1".  The elementary data item ri2e (PIC XX)
* redefines the data item i2e (USAGE NATIVE-2).
*
     MOVE ri2e TO computed-a
     MOVE "#1" TO correct-a
     MOVE "REDEFINES a native-2 item" TO re-mark
     IF ri2e = "#1" THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-7 SECTION"
/
 test-7 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-7" TO par-name
     MOVE i3e TO computed-n
     MOVE "32767" TO correct-a
     MOVE "Initialize a native-2 item" TO re-mark
     IF i3e = 32767 THEN
       PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-7-1 SECTION"
/
 test-7-1 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-7-1" TO par-name
     COMPUTE i3e = -134
     MOVE i3e TO computed-i
     MOVE -134 TO correct-i
     MOVE "COMPUTE a native-2 item" TO re-mark
     IF i3e = -134 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8 SECTION"
/
 test-8 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8" TO par-name
     MOVE i4e TO computed-i
     MOVE 0 TO correct-i
     MOVE "MOVE a native-2 item" TO re-mark
     IF i4e = 0 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-1 SECTION"
/
 test-8-1 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-1" TO par-name
     MOVE -1 TO i4e
     MOVE i4e TO computed-i
     MOVE -1 TO correct-i
     MOVE "MOVE a native-2 item" TO re-mark
     IF i4e = -1 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-3 SECTION"
/
 test-8-3 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-3" TO par-name
     COMPUTE i4 = 1234
     MOVE i4 TO computed-a
     MOVE "01234" TO correct-a
     MOVE "MOVE native-2 to PIC X(20)" TO re-mark
     IF i4 = 1234 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-4 SECTION"
/
 test-8-4 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-4" TO par-name
     COMPUTE i4 = -1234 * 1
     COMPUTE computed-i = i4
     COMPUTE correct-i = -1234
     MOVE "COMPUTE a native-2 item" TO re-mark
     IF i4 = -1234 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-5 SECTION"
/
 test-8-5 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-5" TO par-name
     COMPUTE i4 = 0
     COMPUTE i1 = 2
     COMPUTE i2 = -4
     COMPUTE i3 = 1
     COMPUTE i4 ROUNDED =
       i2 *
       (i1 - i3) +
       12
     END-COMPUTE
     COMPUTE computed-i = i4
     COMPUTE correct-i = 8
     MOVE "COMPUTE native-2 items" TO re-mark
     IF i4 = 8 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-6 SECTION"
/
 test-8-6 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-6" TO par-name
     COMPUTE i1e = 0
     COMPUTE i2e = -4
     COMPUTE i3e = 1
     ADD i2e TO i3e GIVING i1e ROUNDED
     END-ADD
     MOVE i1e TO computed-i
     MOVE -3 TO correct-i
     MOVE "ADD native-2 items" TO re-mark
     IF i1e = -3 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-7 SECTION"
/
 test-8-7 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-7" TO par-name
     COMPUTE i4e = 0
     COMPUTE i1e = 0
     COMPUTE i2e = 32767
     COMPUTE i3e = 1
     ADD
       i2e TO i3e GIVING i1e
       ON SIZE ERROR
         COMPUTE
           i4e = -1
         END-COMPUTE
     END-ADD
     MOVE i4e TO computed-i
     MOVE -1 TO correct-i
     MOVE "ADD native-2 items" TO re-mark
     IF i4e = -1 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-8 SECTION"
/
 test-8-8 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-8" TO par-name
     COMPUTE i1 = 0
     COMPUTE i4 = -1
     COMPUTE i2 = -4
     COMPUTE i3 = 2
     DIVIDE i3 INTO i2 GIVING i1 REMAINDER i4
     END-DIVIDE
     MOVE i1 TO computed-i
     MOVE -2 TO correct-i
     MOVE "DIVIDE native-2 items" TO re-mark
     IF i1 = -2 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-8-9 SECTION"
/
 test-8-9 SECTION.
     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-2" TO feature
     MOVE "TEST-8-9" TO par-name
     COMPUTE i1 = 0
     COMPUTE i4 = -1
     COMPUTE i2 = -4
     COMPUTE i3 = 2
     DIVIDE i3 INTO i2 GIVING i1 REMAINDER i4
     END-DIVIDE
     MOVE i4 TO computed-i
     MOVE 0 TO correct-i
     MOVE "DIVIDE native-2 items" TO re-mark
     IF i4 = 0 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-9 SECTION"
/
 test-9 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-9" TO par-name
     MOVE d1 TO computed-d
     MOVE 99999 TO correct-d
     MOVE "Initalize a native-4 item" TO re-mark
     IF d1 = 99999 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-10 SECTION"
/
 test-10 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-10" TO par-name
     COMPUTE computed-d = d1e
     COMPUTE correct-d = 99999
     MOVE "Initalize a native-4 item" TO re-mark
     IF d1e = 99999 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-11 SECTION"
/
 test-11 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE IS NATIVE-4" TO feature
     MOVE "TEST-11" TO par-name
     MOVE d1 TO computed-a
     MOVE "0000099999" TO correct-a
     MOVE "MOVE native-4 to PIC X(20)" TO re-mark
     IF d1 = 99999 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-11-1 SECTION"
/
 test-11-1 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE IS NATIVE-4" TO feature
     MOVE "TEST-11-1" TO par-name
     COMPUTE d1 = -12345
     MOVE d1 TO computed-d
     MOVE -12345 TO correct-d
     MOVE "MOVE a native-4 item" TO re-mark
     IF d1 = -12345 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12 SECTION"
/
 test-12 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12" TO par-name
     MOVE d1e TO computed-a
     MOVE "0000099999" TO correct-a
     MOVE "MOVE native-4 to PIC X(20)" TO re-mark
     IF d1e = 99999 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12-1 SECTION"
/
 test-12-1 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12-1" TO par-name
     COMPUTE d2 = 12
     COMPUTE d3 = 23
     ADD d2 TO d3 GIVING d3
     MOVE d3 TO computed-n
     MOVE "35" TO correct-a
     MOVE "ADD native-4 items" TO re-mark
     IF d3 = 35 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12-2 SECTION"
/
 test-12-2 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12-2" TO par-name
     COMPUTE d2 = 12
     COMPUTE d3 = 23
     ADD d2 GIVING d3
     MOVE d3 TO computed-n
     MOVE 12 TO correct-n
     MOVE "COMPUTE a native-4 item" TO re-mark
     IF d3 = 12 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12-3 SECTION"
/
 test-12-3 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12-3" TO par-name
     MOVE ZEROES TO d1
     COMPUTE d2 = 12
     COMPUTE d3 = 23
     SUBTRACT d2 FROM d3 GIVING d1
     MOVE d1 TO computed-n
     MOVE 11 TO correct-n
     MOVE "SUBTRACT native-4 items" TO re-mark
     IF d1 = 11 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12-4 SECTION"
/
 test-12-4 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12-4" TO par-name
     MOVE ZEROES TO d1
     COMPUTE d2 = 10
     COMPUTE d3 = 30
     MULTIPLY d2 BY d3 GIVING d1
     MOVE d1 TO computed-d
     MOVE 300 TO correct-d
     MOVE "MULTIPLY native-4 items" TO re-mark
     IF d1 = 300 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-12-5 SECTION"
/
 test-12-5 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-4" TO feature
     MOVE "TEST-12-5" TO par-name
     MOVE ZEROES TO d1
     COMPUTE d2 = 10
     COMPUTE d3 = 30
     DIVIDE d3 BY d2 GIVING d1
     MOVE d1 TO computed-d
     MOVE 3 TO correct-d
     MOVE "DIVIDE native-4 items" TO re-mark
     IF d1 = 3 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-13 SECTION"
/
 test-13 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-13" TO par-name
     MOVE q1 TO computed-q
     MOVE 22222222222222 TO correct-q
     MOVE "Initialize a native-8 item" TO re-mark
     IF q1 = 22222222222222 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-14 SECTION"
/
 test-14 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-14" TO par-name
     MOVE q1e TO computed-q
     MOVE 22222222222222 TO correct-q
     MOVE "Initialize a native-8 item" TO re-mark
     IF q1e = 22222222222222 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-15 SECTION"
/
 test-15 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-15" TO par-name
     MOVE q1e TO computed-a
     MOVE "0000022222222222222" TO correct-a
     MOVE "MOVE native-8 to PIC X(20)" TO re-mark
     IF q1e = 22222222222222 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-16 SECTION"
/
 test-16 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-16" TO par-name
     COMPUTE
       q1e = -33
     END-COMPUTE
     MOVE q1e TO computed-a
     MOVE "0000000000000000033" TO correct-a
     MOVE "MOVE native-8 to PIC X(20)" TO re-mark
     IF q1e = -33 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-17 SECTION"
/
 test-17 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-17" TO par-name
     COMPUTE
       q1 = 666666666666666666
     END-COMPUTE
     MOVE q1 TO computed-n
     MOVE 666666666666666666 TO correct-n
     MOVE "MOVE a native-8 item" TO re-mark
     IF q1 = 666666666666666666 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-18 SECTION"
/
 test-18 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-18" TO par-name
     COMPUTE q1 = 666666666666666666
     COMPUTE q2 = 15
     COMPUTE
       q1 = q1 + q2
     END-COMPUTE
     MOVE q1 TO computed-q
     MOVE 666666666666666681 TO correct-q
     MOVE "COMPUTE a native-8 item" TO re-mark
     IF q1 = 666666666666666681 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-19 SECTION"
/
 test-19 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-19" TO par-name
     COMPUTE q1 = 13
     ADD 15 TO q1
     MOVE q1 TO computed-q
     MOVE 28 TO correct-q
     MOVE "ADD a native-8 item" TO re-mark
     IF q1 = 28 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-20 SECTION"
/
 test-20 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-19" TO par-name
     COMPUTE q1 = -30
     SUBTRACT 12 FROM q1
     MOVE q1 TO computed-q
     MOVE -42 TO correct-q
     MOVE "SUBTRACT a native-8 item" TO re-mark
     IF q1 = -42 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-21 SECTION"
/
 test-21 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-21" TO par-name
     COMPUTE q1 = -40
     MULTIPLY 10 BY q1
     MOVE q1 TO computed-q
     MOVE -400 TO correct-q
     MOVE "MULTIPLY a native-8 item" TO re-mark
     IF q1 = -400 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-22 SECTION"
/
 test-22 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-22" TO par-name
     COMPUTE q1 = 333
     DIVIDE 100 INTO q1 GIVING q1 REMAINDER q2
     MOVE q1 TO computed-q
     MOVE 3 TO correct-q
     MOVE "DIVIDE a native-8 item" TO re-mark
     IF q1 = 3 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-23 SECTION"
/
 test-23 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-23" TO par-name
     COMPUTE q1 = 333
     COMPUTE q2 = 100
     DIVIDE q2 INTO q1 GIVING q1 REMAINDER q2
     MOVE q2 TO computed-q
     MOVE 33 TO correct-q
     MOVE "DIVIDE a native-8 item" TO re-mark
     IF q2 = 33 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-24 SECTION"
/
 test-24 SECTION.

     PERFORM copylib-test-init
     MOVE "USAGE is NATIVE-8" TO feature
     MOVE "TEST-24" TO par-name
     COMPUTE q1 = 12
     MOVE 2 TO i1
     MOVE -2 TO d1
     COMPUTE q2 = (q1 + d1) * i1
     MOVE q2 TO computed-q
     MOVE 20 TO correct-q
     MOVE "COMPUTE native-n items" TO re-mark
     IF q2 = 20 THEN
        PERFORM pass
     ELSE
        PERFORM fail
     END-IF
     PERFORM print-detail
 .

?HEADING " "
?NOLIST
     COPY COPYLIB-OPEN-FILE IN "C00QALIB" .
     COPY copylib-test-init IN "C00QALIB" .
     COPY COPYLIB-CLOSE-FILE IN "C00QALIB" .
     COPY COPYLIB-GENERAL-UTILITIES IN "C00QALIB" .
?LIST
