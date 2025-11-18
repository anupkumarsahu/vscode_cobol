?ANSI
       IDENTIFICATION DIVISION.
* --- *
       PROGRAM-ID.                 PUAUST.
       AUTHOR.                     GROSS.
       DATE-WRITTEN.               19.10.99.
* --- * ------------------------------------------------> REMARKS
* --- *REMARKS.


* --- *
      ?HEADING "~F~ Version:  ~R~.~L~"
* --- * ----------------------------------------------------------------
* --- * CM-CONTROL-HEADER-START
* --- *     ***** Aenderung dieser File  nur in CONTROL erlaubt *****
* --- *     Angaben die von CONTROL ausgewertet werden
* --- *     System Release Name                   :  ~V~
* --- *     Release und Level des Moduls          :  ~R~.~L~
* --- *     Datum                                 :  ~D~.~M~.~Y~
* --- *     Catalog                               :  ~C~
* --- *     Filename des Sourcefiles              :  ~F~
* --- *     Dokument Name                         :
* --- *     Betroffene Sub-Systeme                :
* --- *     Referenzen zu anderen Dokumentationen :
* --- *     Funktions Kurzbeschreibung            :
* --- * CM-CONTROL-HEADER-END
* --- * ----------------------------------------------------------------
* --- *

* --- * ------------------------------------------------> REMARKS
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            TANDEM T16.
       OBJECT-COMPUTER.            TANDEM T16.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "$"
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
* --- * ------------------------------------------------>
       01 T-IDENT.
           05 CLUSTER-ID              PIC X.
           05 CPU                     PIC X.
           05 TRANSACTION-ID          NATIVE-4.
           05 REST                    NATIVE-2.
       01 T-STATUS                    NATIVE-2.
       01 WS-DELAY-TIME               PIC 9(9) COMP VALUE 2.


       EXTENDED-STORAGE SECTION.
* --- * ------------------------------------------------>

       01  MISC-CONSTANTS.
           05  FIELD-ERROR             PIC S9(4) COMP VALUE 999.
           05  INVALID-F-CODE          PIC S9(4) COMP VALUE 998.
           05  OPEN-ERROR              PIC S9(4) COMP VALUE 997.
           05  RECORD-CHANGED          PIC S9(4) COMP VALUE 996.


       01  GUARD-FILE-ERRORS.
           05  FILLER                  PIC S9(4) COMP VALUE 0.
           05  FILLER                  PIC S9(4) COMP VALUE 1.
           05  FILLER                  PIC S9(4) COMP VALUE 59.
           05  FILLER                  PIC S9(4) COMP VALUE 46.
           05  FILLER                  PIC S9(4) COMP VALUE 10.
           05  FILLER                  PIC S9(4) COMP VALUE 11.
           05  FILLER                  PIC S9(4) COMP VALUE 45.
           05  FILLER                  PIC S9(4) COMP VALUE 45.
           05  FILLER                  PIC S9(4) COMP VALUE 300.
           05  FILLER                  PIC S9(4) COMP VALUE 301.
       01  GUARD-FILE-ERROR-TABLE REDEFINES GUARD-FILE-ERRORS.
           05  GUARD-FILE-ERR PIC S9(4) COMP OCCURS 10 TIMES.

       01  FILE-DATA.
           05  ERR-INDEX               PIC S9(4) COMP.
           05  FILE-ERROR-NO           PIC S9(4) COMP VALUE 0.
               88  END-OF-FILE         VALUE 1.
               88  DUPLICATE-KEY       VALUE 10.
               88  NO-EXISTING-RECORD  VALUE 11.
               88  FILE-IS-FULL        VALUE 45.
               88  INVALID-KEY         VALUE 46.
               88  FILE-IS-BAD         VALUE 2 THRU 9,
                                             12 THRU 44,
                                             47 THRU 299.
               88  PROGRAM-LOGIC-ERROR VALUE 300.
               88  FILE-DESCRIPTION-ERROR VALUE 301.

       01  STAT-KEYS.
               10  STAT-KEY1           PIC 9 VALUE 0.
                   88  NO-ERROR        VALUE 0.
                   88  FILE-ERROR      VALUE 1  THRU 9.
               10  STAT-KEY2           PIC 9 VALUE 0.

       01  FILE-STATUS.
               10  STAT1               PIC 9 VALUE 0.
                   88  NO-ERROR        VALUE 0.
                   88  FILE-ERROR      VALUE 1  THRU 9.
               10  STAT2               PIC 9 VALUE 0.
* --- * ------------------------------------------------


       01 RECH01.
          05 RECH01-F1                NATIVE-2.
          05 RECH01-F2                NATIVE-2.
       01 RECH01-R REDEFINES RECH01.
          05 RECHF11                  PIC X.
          05 RECHF12                  PIC X.
          05 RECHF21                  PIC X.
          05 RECHF22                  PIC X.

       LINKAGE SECTION.
* --- * ------------------------------------------------
       01 PARAM-FELD.
          05 DATUM                   PIC 9(8).
          05 ZEIT-SYS                PIC 9(8).
          05 NODE-ID                 PIC 9(4).
          05 CPU-NR                  PIC 99.
          05 TRANS-IDENT             NATIVE-4.
          05 PGMID                   PIC X(8).

       PROCEDURE DIVISION USING PARAM-FELD.
* --- * ------------------------------------------------

       MAIN SECTION.
* --- * ------------------------------------------------
        PERFORM GET-SCHLUESSEL.
        EXIT PROGRAM.
       MAIN-EXIT.
        EXIT.

* --- * ------------------------------------------------
       GET-SCHLUESSEL SECTION.
* --- * ------------------------------------------------
        MOVE FUNCTION CURRENT-DATE (1 : 8) TO DATUM OF PARAM-FELD.

        ENTER TAL GETTRANSID USING T-IDENT GIVING T-STATUS.

        MOVE 0          TO RECH01-F1, RECH01-F2.

        MOVE CLUSTER-ID TO RECHF12.
        MOVE CPU        TO RECHF22.

        IF RECH01-F1 NOT = 0
          COMPUTE NODE-ID = RECH01-F1 - 1
        ELSE
          MOVE RECH01-F1 TO NODE-ID
        END-IF.

        MOVE RECH01-F2 TO CPU-NR.

        MOVE TRANSACTION-ID TO TRANS-IDENT.

        ENTER TAL "DELAY" USING WS-DELAY-TIME
        ACCEPT ZEIT-SYS OF PARAM-FELD FROM TIME.

       GET-SCHLUESSEL-EXIT.
        EXIT.