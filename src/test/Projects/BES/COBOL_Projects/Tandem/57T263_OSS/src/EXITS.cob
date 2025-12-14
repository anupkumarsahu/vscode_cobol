
 IDENTIFICATION DIVISION.
  PROGRAM-ID. EXITS.

 ENVIRONMENT DIVISION.
   CONFIGURATION SECTION.
     SOURCE-COMPUTER. TANDEM VLX.
     OBJECT-COMPUTER. TANDEM VLX.

   INPUT-OUTPUT SECTION.
     FILE-CONTROL.

       SELECT TELTBL
           ASSIGN       TO TELTBL
           ORGANIZATION IS RELATIVE
           ACCESS MODE  IS RANDOM
           RELATIVE KEY IS WS-TEL-REC-NO
           FILE STATUS  IS WS-FILE-STATUS.

 DATA DIVISION.
  FILE SECTION.
    FD    TELTBL
          RECORD CONTAINS 30 CHARACTERS
          LABEL RECORDS ARE OMITTED.

    01  TEL-RECORD.
        05  TEL-LNAME               PIC X(10).
        05  TEL-FNAME               PIC X(10).
        05  TEL-NO                  PIC X(10).

  WORKING-STORAGE SECTION.
   01  WS-RECORDS.
       05  WS-TEL-RECORDS.
           10  WS-TEL-RECORD.
               15  WS-TEL-LNAME            PIC X(10).
               15  WS-TEL-FNAME            PIC X(10).
               15  WS-TEL-NO               PIC X(10).

           10  WS-PREV-TEL-RECORD.
               15  WS-PREV-TEL-LNAME       PIC X(10).
               15  WS-PREV-TEL-FNAME       PIC X(10).
               15  WS-PREV-TEL-NO          PIC X(10).

           10  WS-CURR-TEL-RECORD.
               15  WS-CURR-TEL-LNAME       PIC X(10).
               15  WS-CURR-TEL-FNAME       PIC X(10).
               15  WS-CURR-TEL-NO          PIC X(10).

           10  WS-NEXT-TEL-RECORD.
               15  WS-NEXT-TEL-LNAME       PIC X(10).
               15  WS-NEXT-TEL-FNAME       PIC X(10).
               15  WS-NEXT-TEL-NO          PIC X(10).

           10  WS-FRST-TEL-RECORD.
               15  WS-FRST-TEL-LNAME       PIC X(10).
               15  WS-FRST-TEL-FNAME       PIC X(10).
               15  WS-FRST-TEL-NO          PIC X(10).

           10  WS-LAST-TEL-RECORD.
               15  WS-LAST-TEL-LNAME       PIC X(10).
               15  WS-LAST-TEL-FNAME       PIC X(10).
               15  WS-LAST-TEL-NO          PIC X(10).

   01  WS-TEST-RESULTS.
       05  WS-TEST-RESULT-LINE       PIC X(80).
       05  WS-TEST-PARA-NAME         PIC X(30).

   01  WS-FILE-STATUS                PIC XX VALUE "00".
       88  WS-REC-NOF                 VALUE "23".
       88  WS-REC-EOF                 VALUE "10".
       88  WS-REC-DUP                 VALUE "22".
       88  WS-REC-PERM-ERR            VALUE "30".
       88  WS-REC-NO-PHY-SP           VALUE "34".
       88  WS-REC-FILE-LO-ERR         VALUE "90".
       88  WS-REC-FILE-FD-ERR         VALUE "91".

   01  WS-RELATIVE-KEY.
       05  WS-TEL-REC-NO            PIC 99.

   01  WS-NUMBER                    PIC 99.
   01  WS-NUMBER-PERF               PIC 99.
   01  WS-NUMBER-PERF-CYC           PIC 99.

   01  EOF-FLG                       PIC X.
       88  EOF-YES                    VALUE "Y".

   01  WS-ANS                        PIC X VALUE SPACES.
       88  WS-YES                     VALUE "Y".
       88  WS-NO                      VALUE "N".

   01  WS-OPT                        PIC X VALUE SPACES.
       88  WS-A                       VALUE "A".
       88  WS-D                       VALUE "D".
       88  WS-C                       VALUE "C".
       88  WS-L                       VALUE "L".
       88  WS-E                       VALUE "E".

  PROCEDURE DIVISION.

   0000-MAIN-PARA.
        MOVE SPACES TO EOF-FLG.
        DISPLAY "*--THIS IS THE BEGINNING OF EXITS--*"
*******************************************************************************
*----------------*PARAGRAPHS FOR TESTING EXIT PARA*---------------------------*
*******************************************************************************
        PERFORM 0001-TEST-EXIT-PARA THRU
                0001-TEST-EXIT-PARA-EXIT.
        DISPLAY "TEST EXITS-1 FOR EXIT PARA ENDS.".
        PERFORM 0002-TEST-EXIT-PARA THRU
                0002-TEST-EXIT-PARA-EXIT.
        DISPLAY "TEST EXITS-2 FOR EXIT PARA ENDS.".
        PERFORM 0003-TEST-EXIT-PARA THRU
                0003-TEST-EXIT-PARA-EXIT.
        DISPLAY "TEST EXITS-3 FOR EXIT PARA ENDS.".
*******************************************************************************
*----------------*PARAGRAPHS FOR TESTING EXIT SECTION*------------------------*
*******************************************************************************
        PERFORM 0001-TEST-EXIT-SECT THRU
                0001-TEST-EXIT-SECT-EXIT.
        DISPLAY "TEST EXITS-1 FOR EXIT SECT ENDS.".
        PERFORM 0002-TEST-EXIT-SECT THRU
                0002-TEST-EXIT-SECT-EXIT.
        DISPLAY "TEST EXITS-2 FOR EXIT SECT ENDS.".
        PERFORM 0003-TEST-EXIT-SECT THRU
                0003-TEST-EXIT-SECT-EXIT.
        DISPLAY "TEST EXITS-3 FOR EXIT SECT ENDS.".
*******************************************************************************
*----------------*PARAGRAPHS FOR TESTING EXIT PERFORM*------------------------*
*******************************************************************************
        PERFORM 0001-TEST-EXIT-PERF THRU
                0001-TEST-EXIT-PERF-EXIT.
        DISPLAY "TEST EXITS-1 FOR EXIT PERFORM ENDS.".
        PERFORM 0002-TEST-EXIT-PERF THRU
                0002-TEST-EXIT-PERF-EXIT.
        DISPLAY "TEST EXITS-2 FOR EXIT PERFORM ENDS.".
        PERFORM 0003-TEST-EXIT-PERF THRU
                0003-TEST-EXIT-PERF-EXIT.
        DISPLAY "TEST EXITS-3 FOR EXIT PERFORM ENDS.".
*******************************************************************************
*---------------*PARAGRAPHS FOR TESTING EXIT PERFORM CYCLE*-------------------*
*******************************************************************************
        PERFORM 0001-TEST-EXIT-PERF-CYC THRU
                0001-TEST-EXIT-PERF-CYC-EXIT.
        DISPLAY "TEST EXITS-1 FOR EXIT PERFORM CYCLE ENDS.".
        PERFORM 0002-TEST-EXIT-PERF-CYC THRU
                0002-TEST-EXIT-PERF-CYC-EXIT.
        DISPLAY "TEST EXITS-2 FOR EXIT PERFORM CYCLE ENDS.".
        PERFORM 0003-TEST-EXIT-PERF-CYC THRU
                0003-TEST-EXIT-PERF-CYC-EXIT.
        DISPLAY "TEST EXITS-3 FOR EXIT PERFORM CYCLE ENDS.".
        DISPLAY "*-----THIS IS THE END OF EXITS-----*"
        STOP RUN.
   0000-MAIN-PARA-EXIT.
       EXIT.
*******************************************************************************
*---------------------*PARTS FOR TESTING EXIT PARA*---------------------------*
*******************************************************************************
   0001-TEST-EXIT-PARA.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-1 FOR EXIT PARA BEGINS".
       MOVE "Y" TO WS-ANS.
       IF  WS-YES
           MOVE "USING EXIT PARAGRAPH" TO WS-TEST-RESULT-LINE
           INITIALIZE WS-ANS
           EXIT PARAGRAPH.
       INITIALIZE WS-TEST-RESULT-LINE.

   0001-TEST-EXIT-PARA-EXIT.

       IF  WS-TEST-RESULT-LINE = "USING EXIT PARAGRAPH"
           DISPLAY "TEST EXITS-1 FOR EXIT PARAGRAPH SUCCESSFUL."
           EXIT PARAGRAPH
       ELSE
           DISPLAY "TEST EXITS-1 FOR EXIT PARAGRAPH FAILED    ."
           EXIT PARAGRAPH.
       DISPLAY "TEST EXITS-1 FOR EXIT PARA ENDED WRONGLY".
******************************************************************************
   0002-TEST-EXIT-PARA.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-OPT.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-2 FOR EXIT PARA BEGINS".
       PERFORM 0002-TEST-EXIT-PARA-1.
       IF  WS-OPT NOT = "F"
           MOVE "C" TO WS-OPT.
       PERFORM 0002-TEST-EXIT-PARA-2.
       IF  WS-D
           DISPLAY "TEST EXITS-2 FOR EXIT PARAGRAPH SUCCESSFUL."
           EXIT PARAGRAPH
       ELSE
           DISPLAY "TEST EXITS-2 FOR EXIT PARAGRAPH FAILED    ."
           EXIT PARAGRAPH.
       DISPLAY "TEST EXITS-2 FOR EXIT PARA ENDED WRONGLY".

   0002-TEST-EXIT-PARA-EXIT.
       EXIT.

   0002-TEST-EXIT-PARA-1.
       MOVE "A"  TO WS-OPT.
       IF  WS-A
           EXIT PARAGRAPH.
       MOVE "F"  TO WS-OPT.

   0002-TEST-EXIT-PARA-2.
       IF  WS-C
           MOVE "D" TO WS-OPT
           EXIT PARAGRAPH.
       MOVE "F" TO WS-OPT.
*******************************************************************************
   0003-TEST-EXIT-PARA.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-OPT.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-3 FOR EXIT PARA BEGINS".
       PERFORM 0003-TEST-EXIT-PARA-1 10 TIMES.
       IF  WS-NUMBER = 99
           DISPLAY "TEST EXITS-3 FOR EXIT PARAGRAPH FAILED    ."
           EXIT PARAGRAPH
       ELSE
           DISPLAY "TEST EXITS-3 FOR EXIT PARAGRAPH SUCCESSFUL."
           EXIT PARAGRAPH.
       DISPLAY "TEST EXITS-3 FOR EXIT PARA ENDED WRONGLY".

   0003-TEST-EXIT-PARA-EXIT.
       EXIT.

   0003-TEST-EXIT-PARA-1.
       IF  WS-NUMBER = 9
           MOVE 98 TO WS-NUMBER
           EXIT PARAGRAPH.
       ADD 1 TO WS-NUMBER.
*******************************************************************************
*---------------------*PARTS FOR TESTING EXIT SECTION*------------------------*
*******************************************************************************
   0001-TEST-EXIT-SECT.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-1 FOR EXIT SECT BEGINS".
       PERFORM SECTION-A THRU
               SECTION-A-EXIT.

   0001-TEST-EXIT-SECT-EXIT.
       EXIT.
******************************************************************************
   0002-TEST-EXIT-SECT.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-2 FOR EXIT SECT BEGINS".
       PERFORM SECTION-B THRU
               SECTION-B-EXIT.

   0002-TEST-EXIT-SECT-EXIT.
       EXIT.
******************************************************************************
   0003-TEST-EXIT-SECT.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-3 FOR EXIT SECT BEGINS".
       PERFORM SECTION-C THRU
               SECTION-C-EXIT.

   0003-TEST-EXIT-SECT-EXIT.
       EXIT.
*******************************************************************************
*******************************************************************************
*----------------*PARAGRAPHS FOR TESTING EXIT PERFORM*------------------------*
*******************************************************************************
   0001-TEST-EXIT-PERF.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-1 FOR EXIT PERFORM BEGINS".
       PERFORM
         UNTIL WS-NUMBER-PERF = 99
               ADD 3 TO WS-NUMBER-PERF
               IF  WS-NUMBER-PERF = 96
                   MOVE 1 TO WS-NUMBER-PERF
                   EXIT PERFORM
               END-IF
           IF WS-NUMBER-PERF = 1
              DISPLAY "DID NOT EXIT PERFORM SUCCESSFULLY"
              MOVE 99 TO WS-NUMBER-PERF
           END-IF
       END-PERFORM.
       IF  WS-NUMBER-PERF = 1
           DISPLAY "TEST EXITS-1 FOR EXIT PERFORM SUCCESSFUL."
       ELSE
           DISPLAY "TEST EXITS-1 FOR EXIT PERFORM FAILED    .".

   0001-TEST-EXIT-PERF-EXIT.
       EXIT.

   0002-TEST-EXIT-PERF.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-2 FOR EXIT PERFORM BEGINS".
       PERFORM
         UNTIL WS-NUMBER-PERF = 99
               PERFORM 0002-TEST-EXIT-PERF-1 THRU
                       0002-TEST-EXIT-PERF-1-EXIT
               IF  WS-NUMBER = 99 AND
                   WS-TEST-PARA-NAME = "0002-TEST-EXIT-PERF-1"
                   MOVE 1 TO WS-NUMBER-PERF
                   EXIT PERFORM
               END-IF
               IF  WS-NUMBER-PERF = 1
                   DISPLAY "DID NOT EXIT PERFORM SUCCESSFULLY"
                   MOVE 99 TO WS-NUMBER-PERF
               END-IF
       END-PERFORM.
       IF  WS-NUMBER-PERF = 1
           DISPLAY "TEST EXITS-2 FOR EXIT PERFORM SUCCESSFUL."
       ELSE
           DISPLAY "TEST EXITS-2 FOR EXIT PERFORM FAILED    .".

   0002-TEST-EXIT-PERF-EXIT.
       EXIT.

   0002-TEST-EXIT-PERF-1.
       ADD 3 TO WS-NUMBER-PERF.
       IF  WS-NUMBER-PERF = 96
           MOVE "0002-TEST-EXIT-PERF-1" TO WS-TEST-PARA-NAME
           MOVE 99 TO WS-NUMBER
           EXIT PARAGRAPH.
   0002-TEST-EXIT-PERF-1-EXIT.
       EXIT.

   0003-TEST-EXIT-PERF.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-3 FOR EXIT PERFORM BEGINS".
       PERFORM
         UNTIL WS-NUMBER-PERF = 99
               ADD 1 TO WS-NUMBER-PERF
               PERFORM
                 UNTIL WS-NUMBER = 90
                       ADD 10 TO WS-NUMBER
                       IF  WS-NUMBER = 80
                           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM IN PROGRESS"
                           DISPLAY "GOING TO EXIT FROM INNER(2) PERFORM"
                           MOVE 99 TO WS-NUMBER
                           EXIT PERFORM
                       END-IF
                       IF  WS-NUMBER = 99
                           DISPLAY "DID NOT EXIT FROM INNERMOST PERFORM"
                       END-IF
               END-PERFORM
               IF  WS-NUMBER = 99
                   DISPLAY "TEST EXITS-3 FOR EXIT PERFORM IN PROGRESS"
                   DISPLAY "GOING TO EXIT FROM INNER(1) PERFORM"
                   MOVE 99 TO WS-NUMBER-PERF
                   EXIT PERFORM
               END-IF
               IF  WS-NUMBER-PERF = 99
                   DISPLAY "DID NOT EXIT PERFORM INNER(1)"
                   MOVE 9 TO WS-NUMBER-PERF
               END-IF
       END-PERFORM.
       IF  WS-NUMBER-PERF = 99
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM SUCCESSFUL."
       ELSE
       IF  WS-NUMBER-PERF = 9
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM FAILED    ."
       ELSE
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM FAILED-UNDEFINED ERROR".

   0003-TEST-EXIT-PERF-EXIT.
       EXIT.

*******************************************************************************
*----------------*PARAGRAPHS FOR TESTING EXIT PERFORM CYCLE*------------------------*
*******************************************************************************
   0001-TEST-EXIT-PERF-CYC.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF-CYC.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-1 FOR EXIT PERFORM CYCLE BEGINS".
       PERFORM
         UNTIL WS-NUMBER-PERF-CYC = 99
               IF WS-NUMBER-PERF-CYC = 1
                  MOVE 30 TO WS-NUMBER-PERF-CYC
                  EXIT PERFORM
               END-IF
               ADD 3 TO WS-NUMBER-PERF-CYC
               IF  WS-NUMBER-PERF-CYC = 96
                   MOVE 1 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM CYCLE
               END-IF
       END-PERFORM.
       IF  WS-NUMBER-PERF-CYC = 30
           DISPLAY "TEST EXITS-1 FOR EXIT PERFORM CYCLE SUCCESSFUL."
       ELSE
           DISPLAY "TEST EXITS-1 FOR EXIT PERFORM CYCLE FAILED    .".

   0001-TEST-EXIT-PERF-CYC-EXIT.
       EXIT.

   0002-TEST-EXIT-PERF-CYC.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF-CYC.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-2 FOR EXIT PERFORM CYCLE BEGINS".
       PERFORM
         UNTIL WS-NUMBER-PERF-CYC = 90
               IF  WS-NUMBER-PERF-CYC = 1
                   MOVE 32 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM
               END-IF
               PERFORM 0002-TEST-EXIT-PERF-CYC-1 THRU
                       0002-TEST-EXIT-PERF-CYC-1-EXIT
               IF  WS-NUMBER = 90 AND
                   WS-TEST-PARA-NAME = "0002-TEST-EXIT-PERF-CYC-1"
                   MOVE 1 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM CYCLE
               END-IF
       END-PERFORM.
       IF  WS-NUMBER-PERF-CYC = 32
           DISPLAY "TEST EXITS-2 FOR EXIT PERFORM CYCLE SUCCESSFUL."
       ELSE
           DISPLAY "TEST EXITS-2 FOR EXIT PERFORM CYCLE FAILED    .".

   0002-TEST-EXIT-PERF-CYC-EXIT.
       EXIT.

   0002-TEST-EXIT-PERF-CYC-1.
       ADD 3 TO WS-NUMBER-PERF-CYC.
       IF  WS-NUMBER-PERF-CYC = 90
           MOVE "0002-TEST-EXIT-PERF-CYC-1" TO WS-TEST-PARA-NAME
           MOVE 90 TO WS-NUMBER
           EXIT PARAGRAPH.
   0002-TEST-EXIT-PERF-CYC-1-EXIT.
       EXIT.

   0003-TEST-EXIT-PERF-CYC.
       INITIALIZE             WS-ANS.
       INITIALIZE             WS-NUMBER.
       INITIALIZE             WS-NUMBER-PERF-CYC.
       INITIALIZE             WS-TEST-RESULTS.
       DISPLAY "TEST EXITS-3 FOR EXIT PERFORM CYCLE BEGINS".
       PERFORM
               9  TIMES
               IF  WS-NUMBER-PERF-CYC = 27
                   DISPLAY "SUCCESSFULLY EXITED INNER(1) PERFORM CYC "
                   MOVE 72 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM
               ELSE
                   IF  WS-NUMBER-PERF-CYC = 89
                       DISPLAY "DID NOT EXIT INNER(1) PERFORM CYC"
                       MOVE 19 TO WS-NUMBER-PERF-CYC
                       EXIT PERFORM
                   END-IF
               END-IF
               ADD 1 TO WS-NUMBER-PERF-CYC
               PERFORM
                 UNTIL WS-NUMBER = 90
                       IF  WS-NUMBER = 99
                           MOVE 27 TO WS-NUMBER
                           EXIT PERFORM
                       ELSE
                           IF  WS-NUMBER = 89
                               MOVE 36 TO WS-NUMBER
                               EXIT PERFORM
                           END-IF
                       END-IF
                       ADD 10 TO WS-NUMBER
                       IF  WS-NUMBER = 10
                           MOVE 99 TO WS-NUMBER
                           EXIT PERFORM CYCLE
                       END-IF
                       MOVE 89 TO WS-NUMBER
                       DISPLAY "THIS SHOULD NOT BE DISPLAYED"
                       DISPLAY "THIS SHOULD NOT BE DISPLAYED"
                       DISPLAY "THIS SHOULD NOT BE DISPLAYED"
               END-PERFORM
               IF  WS-NUMBER = 27
                   DISPLAY "SUCCESSFULLY EXITED INNER(2) PERFORM CYC "
                   MOVE 27 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM CYCLE
               ELSE
                   DISPLAY "DID NOT EXIT INNER(2) PERFORM CYC"
                   MOVE 36 TO WS-NUMBER-PERF-CYC
                   EXIT PERFORM CYCLE
               END-IF
               MOVE 89 TO WS-NUMBER-PERF-CYC
               DISPLAY "THIS SHOULD NOT BE DISPLAYED"
               DISPLAY "THIS SHOULD NOT BE DISPLAYED"
               DISPLAY "THIS SHOULD NOT BE DISPLAYED"
       END-PERFORM.
       IF  WS-NUMBER-PERF-CYC = 72
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM CYCLE SUCCESSFUL."
       ELSE
       IF  WS-NUMBER-PERF-CYC = 19
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM CYCLE FAILED    ."
       ELSE
           DISPLAY "TEST EXITS-3 FOR EXIT PERFORM CYCLE FAILED-UNDEFINED ERROR".

   0003-TEST-EXIT-PERF-CYC-EXIT.
       EXIT.

*******************************************************************************
*              ALL SECTION ARE IN THE END OF THE PROGRAM                      *
*******************************************************************************
   SECTION-A SECTION.

   SECTION-A-PARA-1.
       DISPLAY "ENTERED SECTION-A-PARA-1".
       MOVE 1 TO WS-NUMBER.
       EXIT PARAGRAPH.
       MOVE 91 TO WS-NUMBER.

   SECTION-A-PARA-2.
       DISPLAY "ENTERED SECTION-A-PARA-2".
       IF  WS-NUMBER = 1
           ADD  1 TO WS-NUMBER
       ELSE
       IF  WS-NUMBER = 91
           DISPLAY "TEST EXITS-FOR EXIT PARA WITHIN "
           DISPLAY "-    SECTION-A-PARA-1 FAILED ".

       EXIT SECTION.
       MOVE 92 TO WS-NUMBER.

   SECTION-A-PARA-3.
       DISPLAY "WRONGLY ENTERED SECTION-A-PARA-3".
       IF  WS-NUMBER = 92
           DISPLAY "TEST EXITS-FOR EXIT SECTION WITHIN "
           DISPLAY "-    SECTION-A-PARA-2 FAILED ".

       EXIT SECTION.
       MOVE 93 TO WS-NUMBER.

   SECTION-A-EXIT SECTION.

   SECTION-A-EXIT-PARA-1.
       IF  WS-NUMBER = 2
           ADD  1 TO WS-NUMBER
           DISPLAY "TEST EXITS-1 FOR EXIT SECTION SUCCESSFUL."
           EXIT SECTION
       ELSE
       IF  WS-NUMBER = 93
           MOVE 99 TO WS-NUMBER
           DISPLAY "TEST EXITS-FOR EXIT SECTION WITHIN "
           DISPLAY "-    SECTION-A-PARA-3 FAILED "
           DISPLAY "TEST EXITS-1 FOR EXIT SECTION FAILED."
           EXIT SECTION.

       DISPLAY "TEST EXITS-1 FOR EXIT SECTION ENDED WRONGLY.".

*******************************************************************************

   SECTION-B SECTION.

   SECTION-B-PARA-1.
       DISPLAY "ENTERED SECTION-B-PARA-1".
       INITIALIZE WS-NUMBER.
       PERFORM UNTIL WS-NUMBER > 10
           IF  WS-NUMBER = 9
               EXIT PARAGRAPH
           ELSE
               ADD 1 TO WS-NUMBER
           END-IF
           PERFORM SECTION-B-PARA-1-1
       END-PERFORM
       IF  WS-NUMBER > 10
           MOVE 91 TO WS-NUMBER.

       EXIT SECTION.

       MOVE 92 TO WS-NUMBER.

   SECTION-B-PARA-1-1.
        DISPLAY "ENTERED HERE FOR THE " WS-NUMBER " TIME .".


   SECTION-B-EXIT SECTION.

   SECTION-B-EXIT-PARA-1.

       IF  WS-NUMBER = 9
           DISPLAY "TEST EXITS-2 FOR EXIT SECTION SUCCESSFUL."
           DISPLAY "CAME OUT OF PERFORM LOOP SUCCESSFULLY"
           EXIT SECTION
       ELSE
       IF  WS-NUMBER = 91
           DISPLAY "TEST EXITS-2 FOR EXIT SECTION FAILED."
           DISPLAY "DID NOT COME OUT OF PERFORM LOOP SUCCESSFULLY"
           EXIT SECTION
       ELSE
       IF  WS-NUMBER = 92
           DISPLAY "TEST EXITS-2 FOR EXIT SECTION FAILED."
           DISPLAY "DID NOT COME OUT OF SECTION-B-PARA-1 SUCCESSFULLY"
           EXIT SECTION.

       DISPLAY "TEST EXITS-2 FOR EXIT SECTION ENDED WRONGLY.".
*******************************************************************************

   SECTION-C SECTION.

   SECTION-C-PARA-1.
       DISPLAY "ENTERED SECTION-C-PARA-1".
       INITIALIZE WS-NUMBER.
       PERFORM 10 TIMES
          IF  WS-NUMBER = 9
              EXIT PARAGRAPH
          ELSE
              ADD 1 TO WS-NUMBER
          END-IF
          PERFORM SECTION-C-PARA-1-1
       END-PERFORM
       IF  WS-NUMBER = 10
           MOVE 91 TO WS-NUMBER.

       EXIT SECTION.

       MOVE 92 TO WS-NUMBER.

   SECTION-C-PARA-1-1.
        DISPLAY "ENTERED HERE FOR THE " WS-NUMBER " TIME .".


   SECTION-C-EXIT SECTION.

   SECTION-C-EXIT-PARA-1.

       IF  WS-NUMBER = 9
           DISPLAY "TEST EXITS-3 FOR EXIT SECTION SUCCESSFUL."
           DISPLAY "CAME OUT OF PERFORM LOOP SUCCESSFULLY"
           EXIT SECTION
       ELSE
       IF  WS-NUMBER = 91
           DISPLAY "TEST EXITS-3 FOR EXIT SECTION FAILED."
           DISPLAY "DID NOT COME OUT OF PERFORM LOOP SUCCESSFULLY"
           EXIT SECTION
       ELSE
       IF  WS-NUMBER = 92
           DISPLAY "TEST EXITS-3 FOR EXIT SECTION FAILED."
           DISPLAY "DID NOT COME OUT OF SECTION-C-PARA-1 SUCCESSFULLY"
           EXIT SECTION.

       DISPLAY "TEST EXITS-3 FOR EXIT SECTION ENDED WRONGLY.".
*******************************************************************************
*CMTD*
*CMTD*   0002-TEST-EXIT-SECT.
*CMTD*       INITIALIZE             WS-ANS.
*CMTD*       INITIALIZE             WS-OPT.
*CMTD*       INITIALIZE             WS-TEST-RESULTS.
*CMTD*       DISPLAY "TEST EXITS-2 FOR EXIT SECTION BEGINS".
*CMTD*       PERFORM 0002-TEST-EXIT-SECT-1.
*CMTD*       IF  WS-OPT NOT = "F"
*CMTD*           MOVE "C" TO WS-OPT.
*CMTD*       PERFORM 0002-TEST-EXIT-SECT-2.
*CMTD*       IF  WS-D
*CMTD*           DISPLAY "TEST EXITS-2 FOR EXIT SECTION SUCCESSFUL."
*CMTD*           EXIT SECTION
*CMTD*       ELSE
*CMTD*           DISPLAY "TEST EXITS-2 FOR EXIT SECTION FAILED    ."
*CMTD*           EXIT SECTION.
*CMTD*       DISPLAY "TEST EXITS-2 FOR EXIT SECTION ENDED WRONGLY".
*CMTD*
*CMTD*   0002-TEST-EXIT-SECT-EXIT.
*CMTD*       EXIT.
*CMTD*
*CMTD*   0002-TEST-EXIT-SECT-1.
*CMTD*       MOVE "A"  TO WS-OPT.
*CMTD*       IF  WS-A
*CMTD*           EXIT SECTION.
*CMTD*       MOVE "F"  TO WS-OPT.
*CMTD*
*CMTD*   0002-TEST-EXIT-SECT-2.
*CMTD*       IF  WS-C
*CMTD*           MOVE "D" TO WS-OPT
*CMTD*           EXIT SECTION.
*CMTD*       MOVE "F" TO WS-OPT.
*CMTD********************************************************************************
*CMTD*   0003-TEST-EXIT-SECT.
*CMTD*       INITIALIZE             WS-ANS.
*CMTD*       INITIALIZE             WS-NUMBER.
*CMTD*       INITIALIZE             WS-OPT.
*CMTD*       INITIALIZE             WS-TEST-RESULTS.
*CMTD*       DISPLAY "TEST EXITS-FOR EXIT SECTION 3 BEGINS".
*CMTD*       PERFORM 0003-TEST-EXIT-SECT-1 10 TIMES.
*CMTD*       IF  WS-NUMBER = 99
*CMTD*           DISPLAY "TEST EXITS-FOR EXIT SECTION 3 FAILED    ."
*CMTD*           EXIT SECTION
*CMTD*       ELSE
*CMTD*           DISPLAY "TEST EXITS-FOR EXIT SECTION 3 SUCCESSFUL."
*CMTD*           EXIT SECTION.
*CMTD*       DISPLAY "TEST EXITS-FOR EXIT SECTION 3 ENDED WRONGLY".
*CMTD*
*CMTD*   0003-TEST-EXIT-SECT-EXIT.
*CMTD*       EXIT.
*CMTD*
*CMTD*   0003-TEST-EXIT-SECT-1.
*CMTD*       IF  WS-NUMBER = 9
*CMTD*           MOVE 98 TO WS-NUMBER
*CMTD*           EXIT SECTION.
*CMTD*       ADD 1 TO WS-NUMBER.
*CMTD********************************************************************************
