* ?SEARCH $system.system.CBL85UTL 
* ?SAVEABEND
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TPR-Test-3.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 SPECIAL-NAMES.
     FILE "$dfnq1" IS disp-proc
     .
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT result-file ASSIGN TO "#OUT".
     SELECT sortf ASSIGN TO tprt3f1.
     SELECT t06-f ASSIGN TO "tprt3f2" FILE STATUS IS t06-fs.
     SELECT OPTIONAL testf2 ASSIGN TO tprt3f3 FILE STATUS IS testf2-fs.
     SELECT t17-f ASSIGN TO tprt3f4 RESERVE 8.
     SELECT t33-f ASSIGN TO "$S.#tprt3".
     SELECT t34-f ASSIGN TO tprt3f5.

 DATA DIVISION.
 FILE SECTION.
 FD  Result-file  LABEL RECORDS OMITTED.
 01  Result-rec  PIC X(70).
 FD  t06-f  LABEL RECORDS OMITTED.
 01  t06-rec  PIC X(19).
 FD  testf2  LABEL RECORDS OMITTED.
 01  testf2-rec  PIC X(70).
 SD  sortf.
 01  sortf-rec.
     02  sort-key  PIC X(5).
     02            PIC X(65).
 FD  t17-f.
 01  t17-f-rec.
     02  PIC X(7).
     02  t17-f-rec-no  PIC 9(4).
     02  PIC X(69).
 FD  t33-f
     LINAGE IS 60
     .
 01  t33-rec PIC X(40).
 FD  t34-f
     BLOCK CONTAINS 4 RECORDS
     .
 01  t34-rec  PIC X(76).

 WORKING-STORAGE SECTION.
 01  acc-area   PIC X(40).
 01  error-record.
     02  FILLER  PIC X(15) VALUE "Total Errors = ".
     02  nbr-errors-ed  PIC ZZ9.
 01  nbr-errors  PIC 999 VALUE 0.
 01  result-rec-hdr.
     02  FILLER  PIC X(4) VALUE "Nbr".
     02  FILLER  PIC X(5) VALUE "Res".
     02  FILLER  PIC X(20) VALUE "Remarks".
     02  FILLER  PIC X(22) VALUE "Result Should Be".
     02  FILLER  PIC X(20) VALUE "Result Is".
 01  result-rec-ws.
     02  test-no-ed  PIC ZZZ.
     02  FILLER  PIC X VALUE SPACE.
     02  pass-or-fail  PIC X(5).
     02  result-comment-area.
         03  result-remarks  PIC X(20).
         03  result-should-be-an  PIC X(20).
         03  result-should-be-num REDEFINES result-should-be-an
             PIC -9(9).9(9).
         03  FILLER  PIC XX VALUE SPACES.
         03  result-is-an  PIC X(20).
         03  result-is-num REDEFINES result-is-an
             PIC -9(9).9(9).
 01  sv-result-rec  PIC X(70).
 01  testf2-fs  PIC XX.
 01  test-no  PIC 999 VALUE 1.
*  data for specific test follows
 01  t01-call-nbr  PIC 9 VALUE 1.
 01  t01-err-flag  PIC 9 VALUE 0.
 01  t02-f1  PIC 9(06)V9(10) COMP.
 01  t02-f2  PIC S9(18) LEADING SEPARATE VALUE 123456789012345678.
 01  t02-f3  PIC S9(18) LEADING SEPARATE VALUE 246801234567890.
 01  t02-f4  PIC S9(9) LEADING SEPARATE VALUE 123456789.
 01  t03-s1  PIC S999 COMP VALUE 123.
 01  t03-s2  PIC S999999 VALUE -123456.
 01  t03-s3  PIC S99 SIGN LEADING VALUE -12.
 01  t03-s4  PIC S99999 VALUE -12345.
 01  t03-r1  PIC X(5) JUSTIFIED VALUE ALL "*".
 01  t04-sub  PIC 999 VALUE 3.
 01  t05-sndr    PIC X(10) VALUE "abcdefghij".
 01  t05-g1.
     02  t05-rcvr OCCURS 2 TIMES PIC X(5) VALUE ALL "-".
 01  t06-rec-no  PIC 99 VALUE 1.
 01  t06-fs      PIC XX.
 01  t06-ws-rec  PIC X(19).
 01  t07-dr      PIC 999 COMP VALUE  55.
 01  t07-dd      PIC 9999 COMP VALUE 3333.
 01  t07-quot    PIC 999  COMP.
 01  t07-rem     PIC 9 VALUE 8.
 01  t08-f1      PIC 9(3)V9 COMP VALUE 1.
 01  t08-f2      PIC 9(3)V9 COMP VALUE 10.
 01  t08-res     PIC 9(5)V9(4).
 01  t09-g1.
     02  t09-c1  PIC XXXX VALUE "abcd".
     02  t09-c2  PIC XXX VALUE "efg".
 01  t09-g2 VALUE ALL "*".
     02  t09-t  OCCURS 3 TIMES INDEXED BY t09-ind.
         03  t09-c1  PIC XXXX.
         03  t09-c2  PIC XXX.
 01  t10-f1  PIC 9(5) VALUE 40000.
     88  t10-cn  VALUE 30000 THRU 40000.
 01  t11-dmy GLOBAL PIC X VALUE "N".
     88  t11-dmy-cn  VALUE "Y".
 01  t11-f1  GLOBAL PIC X VALUE "Y".
     88  t11-cn  VALUES "Q", "Y".
 01  t11-rtn PIC X.
 01  t12-f1    PIC 9999 COMP VALUE 2.
 01  t12-g.
     02  t12-o OCCURS 10 TIMES INDEXED BY t12-i1.
         03          PIC XXX.
         03  t12-f2  PIC 9999 COMP.
     02  t12-o2  OCCURS 10 TIMES INDEXED BY t12-i2.
         03          PIC X(6).
         03  t12-f3  PIC 9999 COMP.
 01  t13-f  PIC X(10) VALUE "aabbaaabbb".
 01  t15-odo-i       PIC 9 VALUE 4.
 01  t15-p    VALUE "aabbccddee".
     02      PIC XX OCCURS 1 TO 5 TIMES DEPENDING ON t15-odo-i.
 01  t16-r           PIC 9(12).
 01  t16-s           PIC X(19) VALUE "aabbcc123456ddeeffg".
 01  t16-len         PIC 9 VALUE 6.
 01  t17-rec-no  PIC 9(4).
 01  t18-rcvr  PIC 999999V99999 COMP.
 01  t18-exp   PIC S9 COMP value -3.
 01  t18-f1    PIC 999 COMP VALUE 10.
 01  t18-f2    PIC 9(18) VALUE 2.
 01  t18-f3    PIC 9(5)V9(7)  VALUE 3.
 01  t19-g  VALUE "aaaaaaaaaabbbbbbbbbbcccccccccc".
     02  t19-r  OCCURS 3 TIMES PIC X(10).
 01  t19-sub   PIC 99 COMP VALUE 2.
 01  t20-ct    PIC 99 COMP VALUE 2.
 01  t20-lc    PIC 99 COMP VALUE 1.
 01  t20-bi    PIC 9(18) VALUE 123.
 01  t20-bi2   PIC 9(18) VALUE 456.
 01  t21-ss    PIC 9 COMP VALUE 1.
 01  t21-sel   PIC 9 COMP VALUE 0.
 01  T22-r  PIC 9999 COMP.
 01  T22-fc PIC 999 COMP VALUE 288.
 01  T22-len PIC 999 COMP VALUE 22.
 01  T23-f  PIC 9 VALUE 2.
 01  t24-g.
     02  t24-oi  PIC 99 OCCURS 5 TIMES INDEXED BY t24-i.
?IFNOT 1
 01  t24-r       NATIVE-4.
?ENDIF 1
?IF 1
 01  t24-r    PIC S9(9) COMP.
?ENDIF 1
 01  t25-f    PIC 9 VALUE 1 COMP.
 01  t26-g1.
     02  t26-occ  OCCURS 2 TIMES.
         03  t26-f1  PIC 9999 VALUE 1111.
         03  t26-f2  PIC 9(5) VALUE 22222.
 01  t27-s  PIC 99 VALUE 2.
 01  t27-ind  PIC 99 VALUE 3.
 01  t28-fc  PIC 99 COMP VALUE 2.
 01  t28-len PIC 99 COMP VALUE 5.
 01  t28-sndr  PIC X(7) JUST VALUE "abcdefg".
 01  t29-f1  PIC S9(3) VALUE -180.
 01  t29-res  PIC ---9.
?IFNOT 1
 01  t30-sndr  NATIVE-4 VALUE 67108863.
 01  t30-rcvr  NATIVE-2.
?ENDIF 1
?IF 1
 01  t30-sndr  PIC 9(8) COMP  VALUE 67108863.
 01  t30-rcvr  PIC 9(4) COMP.
?ENDIF 1
 01  t31-bef-big  PIC XX VALUE "&^".
 01  t31-too-big  PIC X(32767).
 01  t31-g2.
     02  t31-aft-big  PIC X.
     02  t31-aft-that  PIC X VALUE "*".
 01  t31-ctr      PIC 9(5) COMP VALUE 0.
 01  t32-f        PIC 9(4) COMP VALUE 10.
 01  t32-rcv      PIC 9(4) COMP.
 01  t33-rec-ws.
     02              PIC X(4) VALUE "Rec ".
     02  t33-rec-no  PIC 999  VALUE 1.
     02              PIC XX   VALUE SPACES.
     02  t33-des     PIC X(31).
 01  PIC 9 VALUE 0.
     88  t33-decl-exec      VALUE 1.
     88  t33-decl-not-exec  VALUE 2.
 01  t34-nothing     PIC 999999.
 01  t34-rec-ws.
     02              PIC X(4) VALUE "Rec ".
     02  t34-rec-no  PIC 999  VALUE 1.
     02              PIC XX   VALUE SPACES.
     02  t34-des     PIC X(22).
 01  t35-param  PIC X(20) VALUE ALL "*".
 01  t36-f1          PIC 9(8) COMP VALUE 123.
 01  t36-res         PIC 9(8) COMP.
 01  t37-res  PIC 9(4) COMP.
 01  t37-f1   PIC 9V9999 COMP VALUE 1.
 01  t37-f2-g.
     02  t37-f2  OCCURS 12 TIMES PIC 999 COMP VALUE 3.
 01  t37-f3   PIC 99 COMP VALUE 1.
 01  t37-f4   PIC 99 COMP VALUE 2.
 01  t37-f5   PIC 99 COMP VALUE 3.
 01  t37-f6   PIC 99 COMP VALUE 2.
 01  t38-f1   PIC 9 VALUE 4.
 01  t38-f2   PIC 9 VALUE 8.
 01  t38-rcv  PIC 9 VALUE 0.
 01  t38-se   PIC 9 VALUE 0.
 01  t39-f1   PIC S9(4) VALUE -1.
 01  t39-f2   PIC 9(4) COMP VALUE 3.
 01  t40-s1   PIC +++.++ VALUE "  -.25".
 01  t40-s2   PIC +++.++ VALUE " +1.23".
 01  t40-r1   PIC S99V99.
 01  t40-r2   PIC S99V99.
 01  t41-a    PIC 9(2)V9(10).
 01  t41-b    PIC S9(3)V9(10) VALUE 1.5.
 01  t41-c    PIC S9(1)       VALUE 1.
 01  t41-d    PIC 9V9(10)     VALUE .5.
 01  t42-rcv  PIC 9(2)V9(14)  VALUE 12.34567.
 01  t42-b    PIC S9(3)V9(12) VALUE 123.456789012345.
 01  t42-c    PIC S9(1)       VALUE 3.
 01  t42-d    PIC 9V9(12)     VALUE 3.12345.
 01  t42-e    PIC S9(3)V9(12) VALUE 6.54321.
 01  t43-sub  PIC 99 COMP     VALUE 2.
 01  t44-grp1.
     02                PIC X(50).
     02  t44-ind       PIC s9(04) value 2 COMP.
 01  t45-s-r           PIC X(8) VALUE "12345678".
 01  t45-sndr  REDEFINES t45-s-r  NATIVE-8.
 01  t45-grp.
     02  t45-sub       PIC 9 VALUE 2.
     02                PIC X(4) VALUE "abcd".
     02  t45-tab       OCCURS 3 TIMES.
         03  t45-x     PIC X.
         03  t45-r     NATIVE-8.
     02                PIC XX VALUE "zz".
 01  t46-grp-1.
     02  t46-s         PIC S9(4)V9(4) SIGN TRAILING SEPARATE VALUE 1234.5678.
     02  t46-r1        PIC S9(4)V9(6) SIGN TRAILING SEPARATE.
     02  t46-r2        PIC S9(4)V9(2) SIGN TRAILING SEPARATE.
* t47 thru t50 in Ext sto

 EXTENDED-STORAGE SECTION.
* t17-e must be the first item in Ext-Sto
 01  t17-e  PIC X(10) VALUE "abcdefghij".
 01  t04-g VALUE
         "A01B02C03D04E05".
     02  t04-t  OCCURS 5 TIMES.
         03  PIC X.
         03  t04-f1  PIC 99.
             88  t04-cn  VALUE 1, 3.
 01  t05-ptr     PIC 99 VALUE 1.
 01  t05-tally   PIC 99 VALUE 0.
 01  T22-s  PIC X(1000) VALUE ALL "*".
 01  t27-g1.
     02  t27-a1  PIC 99 OCCURS 5 TIMES.
     02  t27-rcvr  PIC X(10) OCCURS 5 TIMES VALUE ALL "*".
 01  t28-rcvr  PIC X(10) JUST VALUE ALL "*".
* all after the next one is above 32K in ext sto
 01  past-32K  PIC X(33000).
 01  t43-s     PIC 9(5) VALUE 12345.
 01  t43-g1.
     02  t43-rcvr  NATIVE-2 OCCURS 5 TIMES VALUE 0.
 01  t44-tab VALUE ALL "*".
     02  t44-tab2                    OCCURS 3 TIMES.
      03  t44-r1                     PIC X(6).
      03  t44-substruc.
          04  t44-r3                 PIC X.
          04  t44-r2                 PIC X.
          04  t44-r4                 PIC X(8).
 01  t46-grp-2.
     02  t46-r3        PIC S9(4)V9(6) SIGN TRAILING SEPARATE.
     02  t46-r4        PIC S9(4)V9(2) SIGN TRAILING SEPARATE.
 01  t47-odo           PIC 99 COMP VALUE 6.
 01  t47-grp.
     02  t47-occ PIC X OCCURS 1 TO 10 TIMES DEPENDING ON t47-odo.
 01  t48-grp.
     02  t48-r  PIC XXX VALUE "abc".
     02         PIC XXX VALUE "def".
 01  t49-odo    PIC 9(4) VALUE 10.
 01  t49-into.
     02  t49-occ  PIC X OCCURS 1 TO 10 TIMES DEPENDING ON t49-odo.
 01  t49-sender PIC X(20) VALUE "abcdefghijklmnopqrst".
 01  t49-delim  PIC XXXX VALUE "fghi".
 01  t49-count  PIC 9(5) VALUE 0.
 01  t50-f1     PIC X(5) VALUE "abcde".
 01  t50-num-f  PIC 99 COMP VALUE 1.
 01  t50-rtn    PIC X(10).

 PROCEDURE DIVISION.
 DECLARATIVES.
 t33-s SECTION.
     USE AFTER STANDARD EXCEPTION PROCEDURE ON T33-f.
 t33-p.
     SET t33-decl-exec TO TRUE.
 END DECLARATIVES.

 STARTT.
*     ENTER "COBOL85^ARMTRAP"
     OPEN OUTPUT result-file.
     WRITE result-rec FROM result-rec-hdr.
     MOVE spaces TO result-rec.
     WRITE result-rec.
*  Test 01 - Extended-Storage not initialized in cancelled prog
*  TPR 880524 1530 4534  -- not in B40
 Test-01.
?IFNOT 1
     CALL "t01-prog" USING t01-call-nbr, t01-err-flag
     IF t01-err-flag NOT = 0
         MOVE "CALL " TO result-remarks
         MOVE "CALL failed" TO result-is-an
         MOVE t01-err-flag TO result-should-be-num
         PERFORM fails
     ELSE
         CANCEL "t01-prog"
         ADD 1 TO t01-call-nbr
         CALL "t01-prog" USING t01-call-nbr, t01-err-flag
         IF t01-err-flag NOT = 0
             MOVE "CALL " TO result-remarks
             MOVE "Init failed" TO result-is-an
             MOVE t01-err-flag TO result-should-be-num
             PERFORM fails
         ELSE
             PERFORM passes
         END-IF
     END-IF
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
?ENDIF 1
     .
*  Test 02 - Compute with large intermediate and constant
*  TPR 880517 1549 9245   (will fail in B40)
 Test-02.
?IFNOT 1
     COMPUTE t02-f1 = ((t02-f2 + t02-f4) * 1.0000000000) / t02-f3
     IF t02-f1 NOT = 500.2275995578
         MOVE "Comp" TO result-remarks
         MOVE t02-f1 TO result-is-num
         MOVE 500.2275995578 TO result-should-be-num
         PERFORM fails
     ELSE
         PERFORM passes
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 03 - Move numeric to justified
*  TPR 880531 1036 7207 -- OK in B40
 Test-03.
     MOVE t03-s1 TO t03-r1
     IF t03-r1 NOT = "  123"
         MOVE "Num to JUST" TO result-remarks
         MOVE t03-r1 TO result-is-an
         MOVE "  123" TO result-should-be-an
         PERFORM fails
     ELSE
         MOVE ALL "*" TO t03-r1
         MOVE t03-s2 TO t03-r1
         IF t03-r1 NOT = "23456"
             MOVE "Num to JUST" TO result-remarks
             MOVE t03-r1 TO result-is-an
             MOVE "23456" TO result-should-be-an
             PERFORM fails
         ELSE
             MOVE ALL "*" TO t03-r1
             MOVE t03-s3 TO t03-r1
             IF t03-r1 NOT = "   12"
                 MOVE "Num to JUST" TO result-remarks
                 MOVE t03-r1 TO result-is-an
                 MOVE "   12" TO result-should-be-an
                 PERFORM fails
             ELSE
                 MOVE ALL "*" TO t03-r1
                 MOVE t03-s4 TO t03-r1
                 IF t03-r1 NOT = "12345"
                     MOVE "Num to JUST" TO result-remarks
                     MOVE t03-r1 TO result-is-an
                     MOVE "12345" TO result-should-be-an
                     PERFORM fails
                 ELSE
                     PERFORM passes
                 END-IF
             END-IF
         END-IF
     END-IF
     .
*  Test 04 - Condition-name in Ext sto & subscripted
*  TPR 880617 1022 8772  -- OK in B40
 Test-04.
     IF t04-cn (t04-sub)
         PERFORM passes
     ELSE
         MOVE "Cond-nm" TO result-remarks
         MOVE t04-f1 (t04-sub) TO result-is-num
         MOVE t04-f1 (3) TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 05 - UNSTRING compiler abort
*  TPR 880629 1532 7163 -- not in B40
 Test-05.
?IFNOT 1
     UNSTRING t05-sndr DELIMITED BY "e" INTO
       t05-rcvr (1), t05-rcvr (2)
       POINTER t05-ptr
       TALLYING t05-tally
     EVALUATE TRUE
       WHEN t05-rcvr (1) NOT = "abcd"
         MOVE "UNSTR" TO result-remarks
         MOVE "abcd" TO result-should-be-an
         MOVE t05-rcvr(1) TO result-is-an
         PERFORM fails
       WHEN t05-rcvr(2) NOT = "fghij"
         MOVE "UNSTR" TO result-remarks
         MOVE "fghij" TO result-should-be-an
         MOVE t05-rcvr(2) TO result-is-an
         PERFORM fails
       WHEN t05-ptr NOT = 11
         MOVE "UNSTR" TO result-remarks
         MOVE 11 TO result-should-be-num
         MOVE t05-ptr TO result-is-an
         PERFORM fails
       WHEN t05-tally NOT = 2
         MOVE "UNSTR" TO result-remarks
         MOVE 2 TO result-should-be-num
         MOVE t05-tally TO result-is-an
         PERFORM fails
       WHEN OTHER
         PERFORM passes
     END-EVALUATE
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 06 - OPEN EXTEND
*  TPR 880623 1200 9245  - OK in B40 (was regression in C00)
 Test-06.
     OPEN OUTPUT t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "OPEN O failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     PERFORM 5 TIMES
         STRING "Record ", t06-rec-no, " 1234567892" DELIMITED BY SIZE
           INTO t06-rec
         WRITE t06-rec
         IF t06-fs NOT = "00"
             MOVE "OPN EXT" TO result-remarks
             MOVE "WRITE1 failed" TO result-is-an
             MOVE t06-fs TO result-should-be-an
             PERFORM fails
             GO TO t06-end
         END-IF
         ADD 1 TO t06-rec-no
     END-PERFORM
     CLOSE t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "CLOSE1 failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     OPEN EXTEND t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "OPEN E failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     PERFORM 5 TIMES
         STRING "Record ", t06-rec-no, " 1234567892" DELIMITED BY SIZE
           INTO t06-rec
         WRITE t06-rec
         IF t06-fs NOT = "00"
             MOVE "OPN EXT" TO result-remarks
             MOVE "WRITE1 failed" TO result-is-an
             MOVE t06-fs TO result-should-be-an
             PERFORM fails
             GO TO t06-end
         END-IF
         ADD 1 TO t06-rec-no
     END-PERFORM
     CLOSE t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "CLOSE2 failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     OPEN INPUT t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "OPEN I failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     MOVE 1 TO t06-rec-no
     PERFORM UNTIL t06-fs = "10"
         READ t06-f NOT AT END
             IF t06-fs NOT = "00"
                 MOVE "OPN EXT" TO result-remarks
                 MOVE "READ failed" TO result-is-an
                 MOVE t06-fs TO result-should-be-an
                 PERFORM fails
                 GO TO t06-end
             END-IF
             STRING "Record ", t06-rec-no, " 1234567892" DELIMITED BY SIZE
               INTO t06-ws-rec
             ADD 1 TO t06-rec-no
             IF t06-ws-rec NOT = t06-rec
                 MOVE "OPN EXT" TO result-remarks
                 MOVE t06-rec TO result-is-an
                 MOVE t06-ws-rec TO result-should-be-an
                 PERFORM fails
                 GO TO t06-end
             END-IF
         END-READ
     END-PERFORM
     CLOSE t06-f
     IF t06-fs NOT = "00"
         MOVE "OPN EXT" TO result-remarks
         MOVE "CLOSE3 failed" TO result-is-an
         MOVE t06-fs TO result-should-be-an
         PERFORM fails
         GO TO t06-end
     END-IF
     IF t06-rec-no = 11
         PERFORM passes
     ELSE
         MOVE "OPN EXT" TO result-remarks
         MOVE t06-rec-no TO result-is-num
         MOVE 11 TO result-should-be-num
         PERFORM fails
     END-IF
     .
 t06-end.
     EXIT
     .
*  Test 07 - OSE not taken on large remainder
*  TPR 880711 1034 6471 -- not in B40
 Test-07.
?IFNOT 1
     DIVIDE t07-dd BY t07-dr GIVING t07-quot REMAINDER t07-rem
       ON SIZE ERROR
         IF t07-quot = 60
             IF t07-rem = 8
                 PERFORM passes
             ELSE
                 MOVE "Rem -rem" TO result-remarks
                 MOVE t07-rem TO result-is-num
                 MOVE 8 TO result-should-be-num
                 PERFORM fails
             END-IF
         ELSE
             MOVE "Rem -quot" TO result-remarks
             MOVE t07-quot TO result-is-num
             MOVE 60 TO result-should-be-num
             PERFORM fails
         END-IF
     NOT ON SIZE ERROR
         MOVE "Rem -nse" TO result-remarks
         MOVE "No SE" TO result-is-an
         PERFORM fails
     END-DIVIDE
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 08 - Exponentiation by a constant integer
*  TPR 880717 1124 2425  -- OK in B40 (regression in C00)
 Test-08.
     COMPUTE t08-res = (t08-f2 / t08-f1) ** 2
     IF t08-res = 100
         PERFORM passes
     ELSE
         MOVE "EXP" TO result-remarks
         MOVE t08-res TO result-is-num
         MOVE 100 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 09 - MOVE CORR in a table
*  TPR 880721 1521 11423  -- not in B40
 Test-09.
?IFNOT 1
     SET t09-ind TO 2
     MOVE CORR t09-g1 TO t09-t (t09-ind)
     IF t09-g2 = "*******abcdefg*******"
         PERFORM passes
     ELSE
         MOVE "MV COR" TO result-remarks
         MOVE t09-g2 TO result-is-an
         MOVE "*******abcdefg*******" TO result-should-be-an
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 10 - Cond-name w/mult values and c variable of 9(5)
*  TPR 880815 0601 5173 -- OK in B40 (regression in C00)
 Test-10.
     IF t10-cn
         PERFORM passes
     ELSE
         MOVE "C-name" TO result-remarks
         MOVE "Tested false" TO result-is-an
         MOVE "Tested true" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 11 global cond-name w/mult values
*  TPR 880822 1722 8641 -- OK on B40
 Test-11.
     CALL t11-prog USING t11-rtn
     IF t11-rtn = "Y"
         PERFORM passes
     ELSE
         MOVE "C-name" TO result-remarks
         MOVE t11-rtn TO result-is-an
         MOVE "Y" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 12 condition with arith-expr on both sides & subs on rt
*  TPR 880726 1414 6002  -- OK in B40
 Test-12.
     SET t12-i1, t12-i2 TO 3
     MOVE 10 TO t12-f2 (3)
     MOVE 10 TO t12-f3 (4)
     IF (t12-f2 (t12-i1) + 2) = (t12-f3 (t12-i2 + 1) + t12-f1)
         PERFORM passes
     ELSE
         MOVE "C-expr" TO result-remarks
         MOVE "not =" TO result-is-an
         MOVE "=" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 13 INSPECT ... "000" BY SPACES
*  TPR 880825 1424 10417  -- OK on B40
 Test-13.
     INSPECT t13-f REPLACING ALL "aaa" BY ZEROS
     IF t13-f = "aabb000bbb"
         PERFORM passes
     ELSE
         MOVE "INSP" TO result-remarks
         MOVE t13-f TO result-is-an
         MOVE "aabb000bbb" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 14 SORT with no RETURN
*  TPR 880728 1122 9057  -- not in B40
 Test-14.
?IFNOT 1
     SORT Sortf
       ASCENDING KEY sort-key
       INPUT PROCEDURE IS test-14-ip
       OUTPUT PROCEDURE IS test-14-op
     GO TO test-14-es
     .
 test-14-ip.
*  don't release any records
     EXIT
     .
 test-14-ip-ft.
     GO TO test-14-err
     .
 test-14-op.
*  don't return any either
     EXIT
     .
 test-14-err.
     MOVE "SORT" TO result-remarks
     MOVE "Fell thru" TO result-is-an
     MOVE SPACES TO result-should-be-an
     PERFORM fails
     GO TO test-14-ex
     .
 test-14-es.
     PERFORM passes
     .
 test-14-ex.
     EXIT
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 15 CALL with ODO param
*  TPR 880831 1357 13351 -- OK in B40 (regression in C00)
 Test-15.
     CALL "t15-prog" USING t15-p
     MOVE 5 TO t15-odo-i
     IF t15-p = "**********"
         PERFORM passes
     ELSE
         MOVE "CALL w/ODO" TO result-remarks
         MOVE t15-p TO result-is-an
         MOVE "**********" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 16 ref mod w/an-num
*  TPR 880905 1054 8588 -- not in B40
 Test-16.
?IFNOT 1
     MOVE t16-s (7: t16-len) TO t16-r
     IF t16-r = 123456
         PERFORM passes
     ELSE
         MOVE "ref mod" TO result-remarks
         MOVE t16-r TO result-is-num
         MOVE 123456 TO result-should-be-num
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 17 fast i-o clobbers ext sto
*  TPR 880902 1602 5922  -- feature not in B40, but should pass
*  Also failure to read past first partition of partitioned file
*  TPR 881122 1418 8492
 Test-17.
     OPEN OUTPUT t17-f
     PERFORM 400 TIMES
        MOVE "Record " TO t17-f-rec
        ADD 1 TO t17-rec-no
        MOVE t17-rec-no TO t17-f-rec-no
        WRITE t17-f-rec
     END-PERFORM
     CLOSE t17-f
     MOVE 1 TO t17-rec-no
     OPEN INPUT t17-f
     IF t17-e = "abcdefghij"
         PERFORM 400 TIMES
             READ t17-f AT END
                 MOVE "Fast I-O-R1" TO result-remarks
                 STRING "At end at ", t17-rec-no DELIMITED BY SIZE
                   INTO result-is-an
                 MOVE "At end at 0401" TO result-should-be-an
                 GO TO t17-fails
             NOT AT END
                 IF t17-f-rec-no = t17-rec-no
                     IF t17-e = "abcdefghij"
                         ADD 1 TO t17-rec-no
                     ELSE
                         MOVE "Fast I-O-R2" TO result-remarks
                         MOVE t17-e TO result-is-an
                         MOVE "abcdefghij" TO result-should-be-an
                         GO TO t17-fails
                     END-IF
                 ELSE
                     MOVE "Fast I-O-R3" TO result-remarks
                     MOVE t17-f-rec TO result-is-an
                     STRING "Record " t17-rec-no DELIMITED BY SIZE
                       INTO result-should-be-an
                     GO TO t17-fails
                 END-IF
             END-READ
         END-PERFORM
         READ t17-f AT END
             PERFORM passes
         NOT AT END
             MOVE "Fast I-O-R3" TO result-remarks
             MOVE "No at end" TO result-is-an
             GO TO t17-fails
         END-READ
     ELSE
         MOVE "fast I-O - O" TO result-remarks
         MOVE t17-e TO result-is-an
         MOVE "abcdefghij" TO result-should-be-an
         GO TO t17-fails
     END-IF
     CLOSE t17-f
     GO TO t17-exit
     .
 t17-fails.
     PERFORM fails
     .
 t17-exit.
     EXIT
     .
*  Test 18 exp w/neg exponent
*  TPR 880915 1703 10512  -- not in B40
 Test-18.
?IFNOT 1
     COMPUTE t18-rcvr = t18-f1 ** t18-exp * t18-f2 * t18-f3
     IF t18-rcvr = .006
         PERFORM passes
     ELSE
         MOVE "exp w/neg" TO result-remarks
         MOVE t18-rcvr TO result-is-num
         MOVE .006 TO result-should-be-num
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 19 ref mod receiver w/even fcp & len
*  TPR 880916 1144 4537  -- in B40
 Test-19.
     MOVE ALL "-" TO t19-r (t19-sub) (2: 4).
     IF t19-g = "aaaaaaaaaab----bbbbbcccccccccc"
         PERFORM passes
     ELSE
         MOVE "ref mod" TO result-remarks
         MOVE t19-g TO result-is-an
         MOVE "aaaaaaaaaab----bbbbbcccccccccc" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 20 - Failure 0 when IF in EVAL in in-line PERF TIMES
*  TPR 880912 1039 5563 (CSG) -- OK in B40 (regression in C00)
 Test-20.
     PERFORM t20-lc TIMES
         EVALUATE t20-ct
             WHEN 1
                 MOVE 1 TO t20-ct
             WHEN 2
                 IF t20-ct = 2
                     MULTIPLY t20-bi BY t20-bi2
                 ELSE
                     MOVE 3 TO t20-ct
                 END-IF
             WHEN OTHER
                 MOVE 4 TO t20-ct
         END-EVALUATE
     END-PERFORM
     IF t20-bi2 = 56088
         IF t20-ct = 2
             PERFORM passes
         ELSE
             MOVE "PERF TIMES 1" TO result-remarks
             MOVE t20-ct TO result-is-num
             MOVE 2 TO result-should-be-num
             PERFORM fails
         END-IF
     ELSE
         MOVE "PERF TIMES 2" TO result-remarks
         MOVE t20-bi2 TO result-is-num
         MOVE 56088 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 21 - multiple WHENs selected in EVAL if delim scope ends one
*  TPR 880920 1816 11349  -- OK in B40
 Test-21.
     EVALUATE t21-ss
         WHEN 1
             IF t21-ss = 1
                 MOVE 1 TO t21-sel
             ELSE
                 GO TO t21-not-here
             END-IF
         WHEN OTHER
             MOVE 2 TO t21-sel
     END-EVALUATE
     GO TO t21-ok
     .
 t21-not-here.
     MOVE 3 TO t21-sel
     .
 t21-ok.
     IF t21-sel = 1
         PERFORM passes
     ELSE
         MOVE "EVAL delim s" TO result-remarks
         MOVE t21-sel TO result-is-num
         MOVE 1 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 22 ref mod sender w/numeric receiver & sndr > 500 chars
*  TPR 881117 1527 10828 -- not in B40
 Test-22.
?IFNOT 1
     MOVE "000000000000004321" TO T22-s (292: 18)
     MOVE T22-s (T22-fc: T22-len) TO T22-r
     IF T22-r = 4321
         PERFORM passes
     ELSE
         MOVE "ref mod" TO result-remarks
         MOVE t22-r TO result-is-num
         MOVE 4321 TO result-should-be-num
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 23 - NEXT SENTENCE in IF in in-line PERFORM
*  TPR 881212 1132 12756 -- not fixed in B40
 Test-23.
     PERFORM 1 TIMES
         IF T23-f = 2
             NEXT SENTENCE
         ELSE
             MOVE "NEXT S" TO result-remarks
             MOVE "ELSE taken"  TO result-is-an
             PERFORM fails
         END-IF
     END-PERFORM
     MOVE "NEXT S" TO result-remarks
     MOVE "Fell thru"  TO result-is-an
     PERFORM fails
     GO TO T23-ex
     .
     PERFORM passes
     .
 T23-ex.
     EXIT
     .
*  Test 24 SET native TO index-name
*  TPR 881219 1726 5581 -- n/a B40 but should pass (NATIVE faked in DD)
 Test-24.
?IFNOT 1
     SET t24-i TO 3
     SET t24-r TO t24-i
     IF t24-r = 3
         PERFORM passes
     ELSE
         MOVE "SET nat" TO result-remarks
         MOVE t24-r TO result-is-num
         MOVE 3 TO result-should-be-num
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 25 ENTER ABEND causes subsequent stuff to abort
*  TPR 881220 1745 7132 -- n/a B40 (should work)
 Test-25.
     IF t25-f = 2
         ENTER ABEND
     END-IF
     IF t25-f NOT = 0
         PERFORM passes
     ELSE
         MOVE "ENT AB" TO result-remarks
         MOVE "ELSE taken" TO result-is-an
         PERFORM fails
     END-IF
     .
*  Test 26 INITIALIZE odd-length item
*  TPR 881216 1136 13090 -- in B40
 Test-26.
     INITIALIZE t26-occ (2)
     IF t26-occ (2) = "000000000"
         IF t26-occ (1) = "111122222"
             PERFORM passes
         ELSE
             MOVE "INIT table" TO result-remarks
             MOVE t26-occ (1) TO result-is-an
             MOVE "111122222" TO result-should-be-an
             PERFORM fails
         END-IF
     ELSE
         MOVE "INIT table" TO result-remarks
         MOVE t26-occ (2) TO result-is-an
         MOVE "000000000" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 27 - ref mod of with fc subscripted
*  TPR 881226 1511 8588 -- not in B40
 Test-27.
?IFNOT 1
     MOVE 1 TO t27-a1 (3)
     MOVE "abcde" TO t27-rcvr (t27-s) (t27-a1 (t27-ind): 5)
     IF t27-rcvr (2) = "abcde*****"
         PERFORM passes
     ELSE
         MOVE "rm w/subs" TO result-remarks
         MOVE t27-rcvr (2) TO result-is-an
         MOVE "abcde*****" TO result-should-be-an
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 28 - just move in extended w/ref mod
*  TPR 881223 1623 13513 -- in B40
 Test-28.
     MOVE t28-sndr (t28-fc: t28-len) TO t28-rcvr
     IF t28-rcvr = "     bcdef"
         PERFORM passes
     ELSE
         MOVE "rm w/just" TO result-remarks
         MOVE t28-rcvr TO result-is-an
         MOVE "     bcdef" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 29 - arith statement w/edited field loses sign
*  TPR 890103 0916 4959 -- in B40
 Test-29.
     COMPUTE t29-res = t29-f1 / 60
     IF t29-res = "  -3"
         PERFORM passes
     ELSE
         MOVE "ed arith res" TO result-remarks
         MOVE t29-res TO result-is-an
         MOVE "  -3" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 30 - truncate native to native move causes arith overflow
*  No TPR -- n/a B40 but should pass (NATIVE faked in DD)
 Test-30.
     MOVE t30-sndr TO t30-rcvr
?IF 1
     IF t30-rcvr = 8863
?ENDIF 1
?IFNOT 1
     IF t30-rcvr = -1
?ENDIF 1
         PERFORM passes
     ELSE
         MOVE "trunc nat mv" TO result-remarks
         MOVE t30-rcvr TO result-is-num
         MOVE -1 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 31 - compiler abort when WS item exceeds 32768
*  TPR 890113 1017 8679 -- n/a B40 -- should pass
 Test-31.
     IF t31-aft-that = "*"
         IF t31-too-big = SPACES
             IF t31-aft-big = SPACES
                 PERFORM passes
             ELSE
                 MOVE "big WS item1" TO result-remarks
                 MOVE t31-aft-big TO result-is-an
                 MOVE SPACES TO result-should-be-an
                 PERFORM fails
             END-IF
         ELSE
             MOVE "big WS item2" TO result-remarks
             PERFORM WITH TEST AFTER VARYING t31-ctr FROM 1 BY 1
               UNTIL t31-ctr = 32767
               OR t31-too-big (t31-ctr: 1) NOT = SPACE
                 CONTINUE
             END-PERFORM
             MOVE t31-ctr TO result-is-num
             MOVE "No sp in" TO result-should-be-an
             PERFORM fails
        END-IF
     ELSE
         MOVE "big WS item3" TO result-remarks
         MOVE t31-aft-that TO result-is-an
         MOVE "*" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 32 - exponentiation with a zero exponent
*  TPR 890131 1334 10725 -- OK in B40 (regression in C10)
 Test-32.
     COMPUTE t32-rcv = t32-f ** 0
     IF t32-rcv = 1
         PERFORM passes
     ELSE
         MOVE "exp w/0" TO result-remarks
         MOVE t32-rcv TO result-is-num
         MOVE 1 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 33 - WRITE with linage & EOP
*  TPR 890215 1332 4534 -- in B40
 Test-33.
     OPEN OUTPUT t33-f
     MOVE "Four recs on this page" TO t33-des
     PERFORM 4 TIMES
         SET t33-decl-not-exec TO TRUE
         WRITE t33-rec FROM t33-rec-ws
         IF t33-decl-exec
             MOVE "EOP & Dec 1" TO result-remarks
             MOVE "Executed" TO result-is-an
             MOVE SPACES TO result-should-be-an
             PERFORM fails
             GO TO t33-exit
         END-IF
         ADD 1 TO t33-rec-no
     END-PERFORM
     MOVE "First line on next page" TO t33-des
     SET t33-decl-not-exec TO TRUE
     WRITE t33-rec FROM t33-rec-ws AFTER ADVANCING PAGE
     IF t33-decl-exec
         MOVE "EOP & Dec 2" TO result-remarks
         MOVE "Executed" TO result-is-an
         MOVE SPACES TO result-should-be-an
         PERFORM fails
     ELSE
         PERFORM passes
     END-IF
     CLOSE t33-f
     .
 t33-exit.
     CONTINUE
     .
*  Test 34 - REWRITE on unstructured file
*  TPR 890216 1010 10465 -- OK in B40 (regression in C10)
 Test-34.
     OPEN OUTPUT t34-f
     MOVE "abcdefghijklmnopqrstuv" TO t34-des
     PERFORM 10 TIMES
         WRITE t34-rec FROM t34-rec-ws
         ADD 1 TO t34-rec-no
     END-PERFORM
     CLOSE t34-f
     OPEN I-O t34-f
     MOVE 0 TO t34-rec-no
     PERFORM 10 TIMES
         MOVE "abcdefghijklmnopqrstuv" TO t34-des
         PERFORM t34-read
*  the following ensures that the stack is clobbered
         ACCEPT t34-nothing FROM DATE
         MOVE "updated rec out to  ->" TO t34-des
         REWRITE t34-rec FROM t34-rec-ws
     END-PERFORM
     CLOSE t34-f
     OPEN INPUT t34-f
     MOVE 0 TO t34-rec-no
     PERFORM t34-read 10 TIMES
     READ t34-f AT END
         PERFORM passes
     NOT AT END
         MOVE "REW unst f" TO result-remarks
         MOVE "NO AT END" TO result-is-an
         MOVE SPACES TO result-should-be-an
         PERFORM fails
     END-READ
     GO TO t34-exit
     .
 t34-read.
     READ t34-f AT END
         MOVE "REW unst f" TO result-remarks
         MOVE "AT END" TO result-is-an
         MOVE SPACES TO result-should-be-an
         PERFORM fails
         GO TO t34-exit
     END-READ
     ADD 1 TO t34-rec-no
     IF t34-rec NOT = t34-rec-ws
         MOVE "REW unst f" TO result-remarks
         MOVE t34-rec TO result-is-an
         MOVE t34-rec-ws TO result-should-be-an
         PERFORM fails
         GO TO t34-exit
     END-IF
     .
 t34-exit.
     CLOSE t34-f
     .
*  Test 35 - Extended-Storage initialization problem
*  TPR 890306 1802 13513 -- OK in B40 (regression in C10 IPM 22AUG88)
 Test-35.
     CALL "t35-prog" USING t35-param
     IF t35-param = SPACES
         PERFORM passes
     ELSE
         MOVE "ES init" TO result-remarks
         MOVE t35-param TO result-is-an
         MOVE "STANDARD ERROR: 4321" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 36 - multiply by a constant of 0
*  TPR 890306 1632 10027 -- OK in B40 (regression in C00)
 Test-36.
     COMPUTE t36-res = 0 * (t36-f1 - 1)
     IF t36-res = 0
         PERFORM passes
     ELSE
         MOVE "* cons 0" TO result-remarks
         MOVE t36-res TO result-is-num
         MOVE 0 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 37 - arith expr with subscripting - failure 0
*  TPR 890317 1443 9856 -- OK in B40 (regression in C00 - maybe)
 Test-37.
?IFNOT 1
     MOVE 4 TO t37-f2 (3)
     COMPUTE t37-res ROUNDED = 40 * t37-f1 * (( (t37-f2 (t37-f3) +
       t37-f4 ) - (t37-f2 (t37-f5) + t37-f6) ) / 7.00000)
     IF t37-res = 6
         PERFORM passes
     ELSE
         MOVE "arith w/subs" TO result-remarks
         MOVE t37-res TO result-is-num
         MOVE 6 TO result-should-be-num
         PERFORM fails
     END-IF
     .
?ENDIF 1
?IF 1
     MOVE "Not in B40" TO result-remarks
     PERFORM passes
     .
?ENDIF 1
*  Test 38 - arith verb with OSE and mult operands
*  TPR 890317 1749 8588 -- now fails in B40
 Test-38.
     PERFORM test-38-add
     IF t38-rcv = 0
         IF t38-se NOT = 1
             MOVE "arith OSE" TO result-remarks
             MOVE t38-se TO result-is-num
             MOVE 1 TO result-should-be-num
             PERFORM fails
         ELSE
             PERFORM passes
         END-IF
     ELSE
         MOVE "arith OSE 2" TO result-remarks
         MOVE t38-rcv TO result-is-num
         MOVE 0 TO result-should-be-num
         PERFORM fails
     END-IF
     GO TO test-38-exit
     .
 test-38-add.
     ADD t38-f1, t38-f2 TO t38-rcv ON SIZE ERROR MOVE 1 TO t38-se
     .
 test-38-fall-thru.
     MOVE "arith OSE 3" TO result-remarks
     MOVE "fell thru perf" TO result-is-an
     PERFORM fails
     .
 test-38-exit.
     EXIT
     .
*  Test 39 - add neg to one-word COMP adds not subs
*  TPR 890323 1246      -- now fails in B40
 Test-39.
     ADD t39-f1 TO t39-f2
     IF t39-f2 = 2
         PERFORM passes
     ELSE
         MOVE "add to comp" TO result-remarks
         MOVE t39-f2 TO result-is-num
         MOVE 3 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 40 - de-edited move with floating + in sender
*  TPR 890331 1319 2887 -- fails in B40
 Test-40.
     MOVE t40-s1 TO t40-r1
     MOVE t40-s2 TO t40-r2
     IF t40-r1 = -.25
         IF t40-r2 = 1.23
             PERFORM passes
         ELSE
             MOVE "de-edit mv" TO result-remarks
             MOVE t40-r1 TO result-is-num
             MOVE 1.23 TO result-should-be-num
             PERFORM fails
         END-IF
     ELSE
         MOVE "de-edit mv" TO result-remarks
         MOVE t40-r1 TO result-is-num
         MOVE -.25 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 41 - exponentiation with exrad exceeding 18 digits
*  TPR 890407 1315 9568 -- fails in B40
 Test-41.
     COMPUTE t41-a ROUNDED = (t41-b * (t41-c + t41-d)) ** (t41-b + t41-c)
     IF t41-a = 7.59375
         PERFORM passes
     ELSE
         MOVE "exp w/large nbr" TO result-remarks
         MOVE t41-a TO result-is-num
         MOVE 7.59375 TO result-should-be-num
         PERFORM fails
     END-IF
     .
*  Test 42 - divide with divisor with big scaling
*  TPR 890406 1600 9568 -- fails in B40
 Test-42.
     COMPUTE t42-rcv ROUNDED = t42-b / ((t42-d * t42-e) + t42-c)
     IF t42-rcv = 5.267514549
         PERFORM passes
     ELSE
         MOVE "div w/large divisor" TO result-remarks
         MOVE t42-rcv TO result-is-num
         MOVE 5.267514549 TO result-should-be-num
         PERFORM fails
     END-IF
           .
*  Test 43 - move to ext sto item above 32K that is comp or native
*  TPR 890593 1617 5581 -- won't fail in B40 (caused by 890317)
 Test-43.
     MOVE t43-s TO t43-rcvr (t43-sub)
     IF t43-rcvr (2) = 12345
         PERFORM passes
     ELSE
         MOVE "mv to ext abv 32K" TO result-remarks
         MOVE t43-rcvr (2) TO result-is-num
         MOVE 12345 TO result-should-be-num
         PERFORM fails
     END-IF
           .
*  Test 44 - move spaces to subscripted mult receivers in ext sto
*  TPR 890607 0958 9982 -- won't fail in B40 (caused by ??)
 Test-44.
     MOVE ALL "*" TO t44-tab
     MOVE SPACES
       TO t44-r1 (t44-ind)
          t44-r2 (t44-ind)
          t44-r3 (t44-ind)
          t44-r4 (t44-ind)
     IF t44-tab2 (2) = SPACES
         IF t44-tab2 (1) = ALL "*"
             IF t44-tab2 (3) = ALL "*"
                 PERFORM passes
             ELSE
                 MOVE "mv to mult rcv abv 32K 3" TO result-remarks
                 MOVE t44-tab2 (3) TO result-is-an
                 MOVE ALL "*" TO result-should-be-an
                 PERFORM fails
             END-IF
         ELSE
             MOVE "mv to mult rcv abv 32K 1" TO result-remarks
             MOVE t44-tab2 (1) TO result-is-an
             MOVE ALL "*" TO result-should-be-an
             PERFORM fails
         END-IF
     ELSE
         MOVE "mv to mult rcv abv 32K" TO result-remarks
         MOVE t44-tab2 (2) TO result-is-an
         MOVE SPACES TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 45 - INITIALIZE subscripted COMP item grp not on wd bound
*  TPR 890613 1703 6977 -- fails in B40 (caused by 881216)
 Test-45.
     MOVE "2abcd1aaaaaaaa12bbbbbbbb23cccccccc3zz" TO t45-grp
     INITIALIZE t45-tab (t45-sub) REPLACING NUMERIC BY t45-sndr
       ALPHANUMERIC BY "q"
     IF t45-grp = "2abcd1aaaaaaaa1q1234567823cccccccc3zz"
         PERFORM passes
     ELSE
         MOVE "INIT subs comp" TO result-remarks
         MOVE t45-grp TO result-is-num
         MOVE "2abcd1aaaaaaaa1q1234567823cccccccc3zz" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 46 - MOVE TRAILING SEPARATE
*  TPR 890828 1553 8768 -- fails in B40
 Test-46.
     MOVE t46-s TO t46-r1
     MOVE t46-s TO t46-r2
     MOVE t46-s TO t46-r3
     MOVE t46-s TO t46-r4
     IF t46-grp-1 = "12345678+1234567800+123456+"
         IF t46-grp-2 = "1234567800+123456+"
             PERFORM passes
         ELSE
             MOVE "MV TRAILING SEP" TO result-remarks
             MOVE t46-grp-2 TO result-is-an
             MOVE "1234567800+123456+" TO result-should-be-an
             PERFORM fails
         END-IF
     ELSE
         MOVE "MV TRAILING SEP" TO result-remarks
         MOVE t46-grp-1 TO result-is-an
         MOVE "12345678+1234567800+123456+" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 47 - comparison of ODO in Ext sto
*  TPR 890824 0835 10383 -- fails in B40
 Test-47.
     MOVE 10 TO t47-odo
     MOVE "abcde" TO t47-grp
     MOVE 8 TO t47-odo
     IF t47-grp = "abcde"
         PERFORM passes
     ELSE
         MOVE "comp of ODO" TO result-remarks
         MOVE t47-grp TO result-is-an
         MOVE "abcde" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 48 - MOVE SPACES to Ext sto above 32K
*  TPR 890823 1909 14014 -- fails in B40
 Test-48.
     MOVE SPACES TO t48-r
     IF t48-grp = "   def"
         PERFORM passes
     ELSE
         MOVE "MV SP to Ext sto" TO result-remarks
         MOVE t48-grp TO result-is-an
         MOVE "   def" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 49 - UNSTRING with var-len INTO in ext sto above 32K
*  TPR 890823 1909 14014 -- fails in B40
 Test-49.
     UNSTRING t49-sender DELIMITED BY t49-delim
       INTO t49-into
       COUNT t49-count
     IF t49-into = "abcde"
         IF t49-count = 1
             MOVE "UNSTR ext sto ab 32K" TO result-remarks
             MOVE t49-count TO result-is-num
             MOVE 1 TO result-should-be-num
             PERFORM fails
         ELSE
             PERFORM passes
         END-IF
     ELSE
         MOVE "UNSTR ext sto ab 32K" TO result-remarks
         MOVE t49-into TO result-is-an
         MOVE "abcde" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  Test 50 - DISPLAY of ext sto item followed by num
*  TPR 890911 1638 4534 -- fails in B40
 Test-50.
     DISPLAY t50-f1, t50-num-f UPON disp-proc
*     ACCEPT t50-rtn FROM disp-proc
     IF t50-rtn = "abcde01"
         PERFORM passes
     ELSE
         MOVE "DISP ext sto" TO result-remarks
         MOVE t50-rtn TO result-is-an
         MOVE "abcde01" TO result-should-be-an
         PERFORM fails
     END-IF
     .
*  End of tests
 End-tests.
     MOVE SPACES TO result-rec.
     WRITE result-rec.
     MOVE nbr-errors TO nbr-errors-ed.
     MOVE Error-record TO Result-rec.
     WRITE Result-rec.
     CLOSE Result-file.
     STOP RUN.
 Passes.
     MOVE "Pass" TO Pass-or-fail.
     PERFORM Write-p-or-f.
 Fails.
     MOVE "Fail" TO Pass-or-fail.
     ADD 1 TO Nbr-errors.
     PERFORM Write-p-or-f.
 Write-p-or-f.
     MOVE Test-no TO Test-no-ed.
     MOVE Result-rec-ws TO Result-rec.
     WRITE Result-rec.
     MOVE SPACES TO Result-comment-area.
     ADD 1 TO Test-no.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  t11-prog.
 DATA DIVISION.
 LINKAGE SECTION.
 01  t11-rtn  PIC X.
 PROCEDURE DIVISION USING T11-rtn.
 Startt.
    IF t11-cn
        MOVE "Y" TO t11-rtn
    ELSE
        MOVE "N" TO t11-rtn
    END-IF
    EXIT PROGRAM
    .
 END PROGRAM t11-prog.

 END PROGRAM TPR-Test-3.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  t01-prog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 EXTENDED-STORAGE SECTION.
 01  item-1  PIC X(20).
 LINKAGE SECTION.
 01  t1-call-nbr  PIC 9.
 01  t1-err-flag  PIC 9.
 PROCEDURE DIVISION USING T1-call-nbr, T1-err-flag.
 Startt.
     EVALUATE T1-call-nbr
       WHEN 1
         MOVE "abcdefghijklmnopqrst" TO Item-1
         MOVE 0 TO T1-err-flag
       WHEN 2
         IF Item-1 NOT = SPACES
             MOVE 1 TO T1-err-flag
         ELSE
             MOVE 0 TO T1-err-flag
         END-IF
       WHEN OTHER
         MOVE 2 TO T1-err-flag
     END-EVALUATE
     EXIT PROGRAM
     .
 END PROGRAM t01-prog.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  t15-prog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 LINKAGE SECTION.
 01  t15-param    PIC X(10).
 PROCEDURE DIVISION USING t15-param.
 Startt.
     MOVE ALL "*" TO t15-param
     EXIT PROGRAM
     .
 END PROGRAM t15-prog.
