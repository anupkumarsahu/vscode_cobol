?SYMBOLS
************************************************************************
*                                                                      *
*    Server program to get DISPLAY messages and return them            *
*                                                                      *
************************************************************************

 IDENTIFICATION DIVISION.
 PROGRAM-ID. server.
 AUTHOR.  Don Nelson
 DATE-COMPILED.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. T16.
 OBJECT-COMPUTER. T16.

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.

     SELECT MSG-IN ASSIGN TO $RECEIVE
         FILE STATUS IS FILE-STATUS.

 RECEIVE-CONTROL.
     TABLE OCCURS 2  TIMES
     syncdepth limit is 10
     REPORT SYSTEM
     ERROR IS Err-rep
     MESSAGE SOURCE IS Msg-source
     REPLY CONTAINS 1 CHARACTERS .

 DATA DIVISION.
 FILE SECTION.
 FD  MSG-IN
     LABEL RECORDS ARE OMITTED
     RECORD IS VARYING FROM 1 TO 100 CHARACTERS DEPENDING ON Msg-len.

 01  MSG-REC                       PIC X(100).
 01  Sys-msg.
     02  Msg-flag  PIC S9999 COMP.
     02  Msg       PIC X(30).
 WORKING-STORAGE SECTION.
 01  Err-rep   PIC 99 COMP VALUE 0.
 01  Msg-len   PIC 999 COMP.
 01  Msg-source.
     02  Sys-flag  PIC S9   COMP.
     02  Ent-nbr   PIC S999 COMP.
     02  PIC X(4).
     02  Proc-id  PIC X(8).
     02  PIC X(16).
    01  FILE-STATUS.
        05 STATUS-1                        PIC 9 VALUE 0.
        05 STATUS-2                        PIC 9 VALUE 0.
    01  RE-FILE-STATUS                     REDEFINES FILE-STATUS
                                           PIC 99.
            88  SUCCESSFUL                 VALUE 00.
            88  END-OF-FILE                VALUE 10.
            88  SEQ-ERROR                  VALUE 21.
            88  DUPLICATE-KEY              VALUE 22.
            88  NO-EXISTING-RECORD         VALUE 23.
            88  BOUNDARY-ERROR             VALUE 24.
            88  FILEINFO-ERROR             VALUE 30.
            88  BOUNDARY-ERROR-SEQ         VALUE 34.
            88  FATAL-ERROR                VALUE 90.
            88  FILEDESC-ERROR             VALUE 91.
            88  USER-DEFINED-ERROR         VALUE 99.

    01  Save-last-msg                      PIC X(100).
    01  Save-last-len                      PIC 999 COMP.
    01  TIME-OUT-ERROR                     PIC 99 VALUE 40.

    01  FILE-TIME-OUT-DATA.
        05 FILE-TIME-OUT                   PIC 9.
            88 NO-TIME-OUT                 VALUE 0.
            88 TIME-OUT-OCCURRED           VALUE 1.
 PROCEDURE DIVISION.

 DECLARATIVES.

 EXCEPTION-PROC SECTION.
     USE AFTER ERROR PROCEDURE ON MSG-IN .

 FIND-EXCEPTION-PROC.

  IF FILEINFO-ERROR AND (GUARDIAN-ERR = TIME-OUT-ERROR)
     MOVE 1 TO FILE-TIME-OUT
  ELSE
     MOVE 0 TO FILE-TIME-OUT.

 END DECLARATIVES.

  000-OPEN.
     OPEN I-O MSG-IN.

     PERFORM 300-MAINLINE UNTIL NOT SUCCESSFUL.

     CLOSE MSG-IN.
     STOP RUN.

 300-MAINLINE.
     MOVE SPACES TO MSG-REC.
     READ MSG-IN
        AT END
           GO TO 300-MAINLINE.
     IF NOT SUCCESSFUL
         STOP RUN.

     IF Msg-flag < 0
         IF Msg-flag NOT = -30
           AND Msg-flag NOT = -31
             DISPLAY "Msg " msg-flag " received"
         END-IF
         MOVE 1 TO Msg-len
         WRITE msg-rec
     ELSE
         IF Msg-len = 1
           AND Msg-rec = "?"
             MOVE Save-last-msg TO Msg-rec
             MOVE Save-last-len TO Msg-len
             WRITE Msg-rec
         ELSE
             MOVE Msg-rec TO Save-last-msg
             MOVE Msg-len TO Save-last-len
             MOVE 1 TO Msg-len
             WRITE msg-rec
         END-IF
     END-IF
     .

 CLOSE-AND-REOPEN.
     CLOSE MSG-IN .
     OPEN I-O MSG-IN.
