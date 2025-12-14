?symbols
?inspect
?saveabend

 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      MAIN-PROGRAM-UNIT.
 AUTHOR.
      SOFTWARE QA FOR COBOL74 INSPECT.
      Modified by Kenneth Luu for COBOL85 INPSECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INCORPRATED
      19333 VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA GROUP.
 DATE-WRITTEN.
      STARTED:   AUGUST 30TH, 1982
      COMPLETED: October 23rd, 1985
 DATE-COMPILED.


*     This program defines variables of all flavors and sizes
*     for the INSPECT tests.


 ENVIRONMENT DIVISION.

 CONFIGURATION SECTION.

 SOURCE-COMPUTER.
      TANDEM NONSTOP.

 OBJECT-COMPUTER.
      TANDEM NONSTOP.

 SPECIAL-NAMES.

 INPUT-OUTPUT SECTION.

 FILE-CONTROL.
      SELECT print-file
        ASSIGN TO "$null"
        FILE STATUS IS print-status.

 I-O-CONTROL.
/
 DATA DIVISION.


 FILE SECTION.

 FD  print-file
     LABEL RECORDS ARE OMITTED
     DATA RECORD IS print-rec
     RECORD CONTAINS 132 CHARACTERS.

 01  print-rec                       PIC X(132).

/
 WORKING-STORAGE SECTION.

* Variable(s) pertaining to the previously defined file(s).
* --------------------------------------------------------

 77  print-status                   PIC XX     VALUE  SPACES.

* Flags and indicators.
* ---------------------

 77  program-flag                   PIC 9.
     88  error-occurred                         VALUE 1.
     88  invalid-condition                     VALUE 2.
 77  program-flow                  PIC 9.
     88  branching-and-looping-desired         VALUE 0.
     88  skip-branches-and-loops               VALUE 1.
     88  calculations-desired                  VALUE 0.
     88  skip-calculations-desired             VALUE 2.
 77  branching-and-looping-flag     PIC 9.
     88  we-want-big-numbers                   VALUE 1.
     88  we-want-edited-constants              VALUE 2.
     88  no-more-fun                           VALUE 3.
 77  calculations-flag              PIC 9.
     88  we-want-addition                      VALUE 1.
     88  we-want-division                      VALUE 2.
     88  exhausted                             VALUE 3.
 77  nb-of-ed                       PIC 99     VALUE 0    COMP.
 77  position-in-line               PIC 99     VALUE 1    COMP.
 77  line-number                    PIC 99     VALUE 0.

* Limit cases and confusing mnemonics.
* ------------------------------------

 77  so-how-long-can-cobol-words-be  PIC 99           COMP   VALUE 30.
 77  a-long-literal                  PIC X(120)
  VALUE  "Mignonne, allons voir si la rose,qui ce matin avait declose sa robe de
-        " pourpre au soleil, n'a point perdu cette vespree,".
 77  suite-to-literal                PIC X(60)
     VALUE "les plis de sa robe pourpree et son teint au vostre pareil.".
 77  attrib                          PIC X(6)   VALUE "ATTRIB".
 77  break                           PIC X(5)   VALUE "BREAK".
 77  clear                           PIC X(5)   VALUE "CLEAR".
 77  define                          PIC X(6)   VALUE "DEFINE".
 77  env                             PIC X(3)   VALUE "ENV".
 77  files                           PIC X(5)   VALUE "FILES".
 77  radix                           PIC X(5)   VALUE "RADIX".
 77  save                            PIC X(4)   VALUE "SAVE".
 77  scope                           PIC X(5)   VALUE "SCOPE".
 77  volume                          PIC X(6)   VALUE "VOLUME".

 01 index-type                       USAGE IS INDEX.

 01  test-case.
         02  add-9                           pic 999 VALUE 100.
         02  comp-1                          PIC 999V999 COMP VALUE 123.456.
 01  bn-values.
     05  bn-0                        pic 9(10)
         VALUE  1234567890.
     05  bn-1                        PIC 9(18)
         VALUE  123456789012345678.
     05  bn-2                      PIC S9(18)
         VALUE  -123456789012345678.
     05  bn-3                       PIC 9(9)V9(9)
         VALUE  123456789.987654321.
     05  bn-4                      PIC S9(9)V9(9)
         VALUE  -123456789.987654321.
     05  bn-5                        PIC 9(18)
         VALUE  123456789.
     05  bn-6                      PIC S9(18)
         VALUE  -123456789.
     05  bn-7                       PIC 9(9)V9(9)
         VALUE  12345.9876543.
     05  bn-8                     PIC S9(9)V9(9)
         VALUE  -12345.987654321.
     05  bn-9                      PIC V9(18)
         VALUE  .123456789012345678.
     05  bn-10                       PIC 9V9(17)
         VALUE  5.00000000000000006.
     05  bn-11                     PIC SV9(18)
         VALUE  -.000000000000000005.
     05  bn-12                      PIC 9(17)V9
         VALUE  12345678901234567.1.
     05  bn-13                     PIC S9(17)V9
         VALUE  -12345678901234567.1.
     05  bn-14                      PIC 9(18)
         VALUE  000000000000000006.
     05  bn-15                     PIC S9(18)
         VALUE  -000000000000000006.
     05  bn-16                     PIC 9(5)   VALUE 12345.
     05  bn-17                     PIC 9(10)  VALUE 1234567890.
     05  bn-18                     PIC S9(5)  VALUE -12345.
     05  bn-19                     PIC S9(10) VALUE -1234567890.
     05  bn-20                     PIC 9(5)V9(5) VALUE 12345.12345.
     05  bn-21                     PIC 9(3)V9(3) VALUE 123.456.
     05  bn-22                     PIC S9(3)V9(3) VALUE -123.456.
     05  bn-23                     PIC S9(5)V9(5) VALUE -12345.67890.
     05  bn-24                     PIC 9V9(5)  VALUE  1.12345.
     05  bn-25                     PIC S9V9(5) VALUE -1.12345.
     05  bn-26                     PIC V999    VALUE   .123.
     05  bn-27                     PIC SV999   VALUE  -.123.
     05  bn-28                     PIC S9(18)   COMP VALUE 123456789012345678.
     05  bn-29                     PIC S9(9)V9(9) COMP VALUE -123456789.123456.
     05  bn-30                     PIC S9V9(17) COMP VALUE -9.12345678901234567.
     05  bn-31                     PIC SV9(18)  COMP VALUE .987654321012345678.


 01  bn-destination.
     05  bn-0                        PIC 9(10).
     05  bn-1                        PIC 9(18).
     05  bn-2                      PIC -9(18).
     05  bn-3                       PIC 9(9).9(9).
     05  bn-4                      PIC -9(9).9(9).
     05  bn-5                        PIC 9(18).
     05  bn-6                      PIC -9(18).
     05  bn-7                       PIC 9(9).9(9).
     05  bn-8                     PIC -9(9).9(9).
     05  bn-9                      PIC .9(18).
     05  bn-10                       PIC 9.9(17).
     05  bn-11                     PIC -.9(18).
     05  bn-12                      PIC 9(17).9.
     05  bn-13                     PIC -9(17).9.
     05  bn-14                      PIC 9(18).
     05  bn-15                     PIC -9(18).
     05  bn-16                     PIC 9(5).
     05  bn-17                     PIC 9(10).
     05  bn-18                     PIC S9(5).
     05  bn-19                     PIC S9(10).
     05  bn-20                     PIC 9(5)V9(5).
     05  bn-21                     PIC 9(3)V9(3).
     05  bn-22                     PIC S9(3)V9(3).
     05  bn-23                     PIC S9(5)V9(5).
     05  bn-24                     PIC 9V9(5).
     05  bn-25                     PIC S9V9(5).
     05  bn-26                     PIC V999.
     05  bn-27                     PIC SV999.
     05  bn-28                     PIC 999V99.
     05  bn-29                     PIC S999V99.
     05  bn-30                     PIC V99.
     05  bn-31                     PIC SV99.
     05  bn-32                     PIC 999.
     05  bn-33                     PIC S999.

* Various edition format.
* -----------------------

 01  edit-values.
     05 edit-1                 PIC 9(10) VALUE  1234567.
     05 edit-2                 PIC 9(10) VALUE  00654321.
     05 edit-3                 PIC 9(10) VALUE  123456.
     05 edit-4                 PIC 9(10) VALUE  0213210.
     05 edit-5                 PIC 9(10) VALUE  123456.
     05 edit-6                 PIC X(10) VALUE  "AAABBCCC".
     05 edit-7                 PIC 9(10) VALUE  12345.
     05 edit-8                 PIC 9(10) VALUE  1234567890.
 01 more-edit-values.
     05 given-values.
        10 in-vals.
           15 val-1            PIC 999999V99  VALUE  1234.56.
           15 val-2            PIC 999999V99  VALUE  001234.56.
           15 val-3            PIC 999999V99  VALUE  000012.34.
           15 val-4            PIC 9999999PP  VALUE  123456700.
           15 val-5            PIC 99999      VALUE  54321.
           15 val-6            PIC 9999999    VALUE  1234567.
           15 val-7            PIC S999999V99 VALUE  -12345.67.
           15 val-8            PIC s999999V99 VALUE  -000012.23.
           15 val-9            PIC 999999V99 VALUE  1123.89.
           15 alpha-num.
              16 name-1        PIC A(25) VALUE "AMENHOTEPNEBMAREsarediank".
              16 name-2        PIC A(27) VALUE "RAMESSESIIsetepenamonmeryre".
              16 speaker-parts.
                 17 name       PIC X(14)     VALUE "KLIPSCHHERESY".
                 17 mod-num    PIC 9999      VALUE  2999.
                 17 id         PIC XXXXXXX   VALUE "H220152".
                 17 owner      PIC A(13)     VALUE "MalcolmMosher".
        10 rec-vals.
           15 val-1            PIC ZZZ,Z99.99.
           15 val-2            PIC ZZZ,ZZZ.ZZCR.
           15 val-3            PIC ZZZ,ZZZ.ZZDB.
           15 val-4            PIC 9,999/9,099PP.
           15 val-5            PIC **9/999.
           15 val-6            PIC $$$999.99.
           15 val-7            PIC $99999.90.
           15 val-8            PIC ++++99909.
           15 val-9            PIC ----999V99.
           15 val-10           PIC AB0/9999B09.
           15 val-11           PIC 999/99PP.
           15 val-12           PIC ***,***9.
           15 val-13           PIC Z,ZZZ,ZZZ,ZZZ.Z0DB.
           15 val-14           PIC 9,9/9,09.
           15 val-15           PIC ++++909.
           15 val-16           PIC $$$$$9.99CR.
           15 val-17           PIC 99/9PP.
           15 val-18           PIC ZZZ,ZZZ,ZZZ,ZZZ.00.
           15 val-19           PIC A(5)9(5)X(5)AAAAA.
           15 val-20           PIC ***,***,***,***.**.
           15 val-21           PIC 9/9/9/9/9/9/9/9/99.
           15 alpha-num.
              16 name-1        PIC A(9)BA(7)BA(2)BA(2)BA(2)BAAA.
              16 name-2        PIC A(8)BXXBA(11)BAAAAAAAAAAA.
              16 speaker-parts.
                 17 name       PIC AAAAAAABA(7).
                 17 mod-num    PIC XXXX.
                 17 id         PIC XXXXXX/X09.
                 17 owner      PIC A(7)BA(6).

* Highs and lows.
* ---------------
 01  Figurative-constants.
     05  alpha-low                   PIC A(10)      VALUE LOW-VALUES.
     05  alpha-high                  PIC A(10)      VALUE HIGH-VALUES.
     05  alphanum-low                PIC X(10)      VALUE LOW-VALUES.
     05  alphanum-high               PIC X(10)      VALUE HIGH-VALUES.
     05  some-quotes                 PIC X(10)      VALUE QUOTES.


* Data to generate overflow conditions and similar harassements.
* --------------------------------------------------------------

 01  all-zeros                     PIC 9V9          VALUE 0.
 01  display-num                   PIC 9999999V99999.
 01  calculations-and-concoctions.
     05  addition-data.
         10  op-all-9s             PIC 9(18)  COMP   VALUE 999999999999999999.
         10  op-all-1s             PIC 9(18)  COMP   VALUE 111111111111111111.
         10  op-one-1              PIC 9(18)  COMP   VALUE 000000000000000001.
         10  op-1                  PIC 9(18)  COMP.
         10  op-2                  PIC 9(18)  COMP.
         10  sum-of-ops            PIC 9(18)  COMP.
         10  more-data.
             15 addit-data.
                16 decimal-1          PIC 9999V99  VALUE  1234.65.
                16 decimal-2          PIC 99V999  VALUE  12.3.
                16 decimal-3          PIC S9999V999.
                16 decimal-4          PIC S999V999.
                16 comp-data.
                   17 comp-1          PIC 999V9   COMP  VALUE 123.4.
                   17 comp-2          PIC S99999V999 COMP  VALUE +12345.
                   17 cmp3          PIC S99999V999 COMP VALUE
                   -13243.648.  17 synch-data.
                      18 synch-1         PIC 9(18) SYNC RIGHT VALUE 11111111111.
                      18 synch-2         PIC 999V9 SYNC RIGHT VALUE 123.
                      18 synch-3         PIC S9(9) SYNC LEFT  VALUE -466.
             15 div-data.
                16 decimal-1       PIC 999V99 VALUE 111.2.
                16 decimal-2       PIC 99V99  VALUE 11.3.
                16 decimal-3       PIC 999V9999.
                16 decimal-4       PIC 999999V99.
                16 comp-data.
                   17 comp-1       PIC 999V999 COMP VALUE 111.21.
                   17 comp-2       PIC 9V99  COMP VALUE 1.2.
                   17 cmp3       PIC 999V999 COMP.
                   17 synch-data.
                      18 synch-1   PIC 999  SYNC LEFT VALUE 123.
                      18 synch-2   PIC 99V9 SYNC LEFT VALUE 12.3.
                      18 synch-3   PIC SVP999 SYNC RIGHT.
     05  division-data.
         10  dividend              PIC 9(18)  COMP.
         10  current-divisor       PIC 9(18)  COMP.
         10  op-all-4s             PIC 9(18)  COMP   VALUE 444444444444444444.
         10  op-all-2s             PIC 9(18)  COMP   VALUE 222222222222222222.
         10  op-one-2              PIC 9(18)  COMP   VALUE 000000000000000002.
         10  villain-divisor       PIC 9      COMP   VALUE 0.
         10  quotient              PIC 9(18)  COMP.
         66  somme-des-operandes   RENAMES  sum-of-ops.
         66  resultat              RENAMES quotient.


* Messages and message elements.
* ------------------------------

 77  acknowledge-begin              PIC X(24)
     VALUE "We are now beginning : ".
 77  acknowledge-end                PIC X(24)
     VALUE "We are now done with : ".
 77  invalid-ind                    PIC X(24)
     VALUE "Invalid branch flag in ".
 77  br-routine                     PIC X(30)
     VALUE "branch-and-loop processing.".
 77  bn-routine                     PIC X(24)
     VALUE "big number processing.".
 77  ed-routine                     PIC X(30)
     VALUE "edited constants processing.".
 77  calc-routine                   PIC X(22)
     VALUE "calculation routine.".
 77  add-routine                    PIC X(18)
     VALUE "addition routine.".
 77  divide-routine                 PIC X(18)
     VALUE "division routine.".
 77  add-sign                       PIC X(3)   VALUE " + ".
 77  divide-sign                    PIC X(3)   VALUE " / ".
 77  back-slash                     PIC X(1)   VALUE "\".
 77  all-stars                      PIC X(18)  VALUE "******************".

 01  print-header                   PIC X(37)
     VALUE IS "Start of COBOL INSPECT test C000COB1.".

 01  final-msg                      PIC X(25)
     VALUE  "Test C000COB1 terminated.".

 01  status-msg.
     05  msg-text                   PIC X(25)  VALUE SPACES.
     05  routine                    PIC X(30)  VALUE SPACES.

 01  bn-msg-header                  PIC X(32)
     VALUE "Limit number values manipulated:".

 01  bn-msg-line.
     05  bn-position                OCCURS 4 TIMES.
         10  msg-text               PIC X(20).
         10  msg-sep                PIC X(1).


 01  ed-msg-header                  PIC X(31)
         VALUE "Edited-values of constants : ".


 01  ed-msg.
     05  line-1.
         10  edit-1             PIC $$$,999.99.
         10  edit-2             PIC $$$B99,999.99.
         10  edit-3             PIC ***,999.99.
         10  edit-4             PIC ZZZ,999.99.
      05 line-2.
         10  edit-5             PIC +++,999.99.
         10  edit-6             PIC XXX/XX/XXX.
         10  edit-7             PIC 9090909090.
         10  edit-8             PIC 999999.999.

 01  calc-msg.
     05  op-1                       PIC 9(18).
     05  op-sign                    PIC X(3).
     05  op-2                       PIC 9(18).
     05  FILLER                     PIC X(3)   VALUE " = ".
     05  result                     PIC 9(18).
     05  bad-result REDEFINES result PIC X(18).
/
 PROCEDURE DIVISION.

 Main-processing SECTION.
 Main.
      PERFORM Initialization THRU Initialization-exit.
      IF error-occurred GO TO End-of-main.

      IF skip-branches-and-loops
         NEXT SENTENCE
      ELSE
         MOVE 1 TO branching-and-looping-flag
         PERFORM branches-and-loops UNTIL no-more-fun or invalid-condition.

      IF skip-calculations-desired
         NEXT SENTENCE
      ELSE
         MOVE 0 TO program-flag
         MOVE 1 TO calculations-flag
         PERFORM various-calculations UNTIL exhausted OR invalid-condition.

 End-of-main.

      IF error-occurred
         NEXT SENTENCE
      ELSE
         PERFORM End-of-processing.

      STOP RUN.

 End-of-processing.

      WRITE print-rec FROM final-msg AFTER ADVANCING 2 LINES.
      CLOSE print-file.

 Main-processing-exit.

      EXIT.
/
 Initialization.

      OPEN OUTPUT print-file SHARED.
      IF print-status NOT = ZEROS
         MOVE 1 TO program-flag
         GO TO Initialization-exit.

      MOVE SPACES TO print-rec.
*     SET index-type TO bn-14 of bn-values.

 Initialization-exit.
      EXIT.

/
 Branches-and-loops SECTION.

 Branch-start.
      IF we-want-big-numbers
         PERFORM Big-numbers
      ELSE
         IF we-want-edited-constants
            PERFORM Edited-constants
         ELSE
            MOVE 2 TO program-flag
            MOVE invalid-ind TO msg-text OF status-msg
            MOVE br-routine TO routine IN status-msg
            WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.

 Branches-exit.
      EXIT.
/
 Big-numbers SECTION.
 Big-number-processing.
      MOVE bn-routine TO routine IN status-msg.
      MOVE acknowledge-begin TO msg-text OF status-msg.
      WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.

      PERFORM Move-one-by-one.
      WRITE print-rec FROM bn-msg-header AFTER ADVANCING 2 LINES.
      PERFORM Write-bn-msg.

      GO TO Big-number-processing-end.
/
 Move-one-by-one.
      MOVE bn-0 OF bn-values TO bn-0 OF bn-destination.
      MOVE bn-28 OF bn-values TO bn-1 of bn-values.
      MOVE bn-1 OF bn-values TO bn-1 OF bn-destination.
      MOVE bn-2 OF bn-values TO bn-2 OF bn-destination.
      MOVE bn-1 OF bn-values TO bn-2 OF bn-destination.
      MOVE bn-3 OF bn-values TO bn-3 OF bn-destination.
      MOVE bn-4 OF bn-values TO bn-4 OF bn-destination.
      MOVE bn-5 OF bn-values TO bn-5 OF bn-destination.
      MOVE bn-6 OF bn-values TO bn-6 OF bn-destination.
      MOVE bn-7 OF bn-values TO bn-7 OF bn-destination.
      MOVE bn-8 OF bn-values TO bn-8 OF bn-destination.
      MOVE bn-9 OF bn-values TO bn-9 OF bn-destination.
      MOVE bn-10 OF bn-values TO bn-10 OF bn-destination.
      MOVE bn-11 OF bn-values TO bn-11 OF bn-destination.
      MOVE bn-12 OF bn-values TO bn-12 OF bn-destination.
      MOVE bn-13 OF bn-values TO bn-13 OF bn-destination.
      MOVE bn-14 OF bn-values TO bn-14 OF bn-destination.
      MOVE bn-15 OF bn-values TO bn-15 OF bn-destination.
      MOVE bn-16 OF bn-values TO bn-16 OF bn-destination.
      MOVE bn-17 OF bn-values TO bn-17 OF bn-destination.
      MOVE bn-18 OF bn-values TO bn-18 OF bn-destination.
      MOVE bn-19 OF bn-values TO bn-19 OF bn-destination.
      MOVE bn-20 OF bn-values TO bn-20 OF bn-destination.
      MOVE bn-21 OF bn-values TO bn-21 OF bn-destination.
      MOVE bn-22 OF bn-values TO bn-22 OF bn-destination.
      MOVE bn-23 OF bn-values TO bn-23 OF bn-destination.
      MOVE bn-24 OF bn-values TO bn-24 OF bn-destination.
      MOVE bn-25 OF bn-values TO bn-25 OF bn-destination.
      MOVE bn-26 OF bn-values TO bn-26 OF bn-destination.
      MOVE bn-27 OF bn-values TO bn-27 OF bn-destination.
      MOVE bn-3 OF bn-values TO bn-28 OF bn-destination.
      MOVE bn-4 OF bn-values TO bn-29 OF bn-destination.
      MOVE bn-20 OF bn-values TO bn-30 OF bn-destination.
      MOVE bn-23 OF bn-values TO bn-31 OF bn-destination.
      MOVE bn-12 OF bn-values TO bn-32 OF bn-destination.
      MOVE bn-6 OF bn-values TO bn-33 OF bn-destination.

/
 Write-bn-msg.
      MOVE 1 TO line-number.
      PERFORM Write-line.
 Write-line.
      GO TO
         Write-line-1,
         Write-line-2,
         Write-line-3,
         Write-line-4, DEPENDING ON line-number.

 Write-line-1.
      MOVE bn-1 OF bn-destination TO msg-text OF bn-position(1).
      MOVE bn-2 OF bn-destination TO msg-text OF bn-position(2).
      MOVE bn-4 OF bn-destination TO msg-text OF bn-position(3).
      MOVE bn-4 OF bn-destination TO msg-text OF bn-position(4).
      MOVE back-slash TO msg-sep OF bn-position(1).
      MOVE back-slash TO msg-sep OF bn-position(2).
      MOVE back-slash TO msg-sep OF bn-position(3).
      MOVE back-slash TO msg-sep OF bn-position(4).
      WRITE print-rec FROM bn-msg-line.
      ADD 1 TO line-number.
      GO TO Write-line.

 Write-line-2.
      MOVE bn-4 OF bn-destination TO msg-text OF bn-position(1).
      MOVE bn-6 OF bn-destination TO msg-text OF bn-position(2).
      MOVE bn-7 OF bn-destination TO msg-text OF bn-position(3).
      MOVE bn-8 OF bn-destination TO msg-text OF bn-position(4).
      MOVE back-slash TO msg-sep OF bn-position(1).
      MOVE back-slash TO msg-sep OF bn-position(2).
      MOVE back-slash TO msg-sep OF bn-position(3).
      MOVE back-slash TO msg-sep OF bn-position(4).
      WRITE print-rec FROM bn-msg-line.
      ADD 1 TO line-number.
      GO TO Write-line.

 Write-line-3.
      MOVE bn-9 OF bn-destination TO msg-text OF bn-position(1).
      MOVE bn-10 OF bn-destination TO msg-text OF bn-position(2).
      MOVE bn-11 OF bn-destination TO msg-text OF bn-position(3).
      MOVE bn-12 OF bn-destination TO msg-text OF bn-position(4).
      MOVE back-slash TO msg-sep OF bn-position(1).
      MOVE back-slash TO msg-sep OF bn-position(2).
      MOVE back-slash TO msg-sep OF bn-position(3).
      MOVE back-slash TO msg-sep OF bn-position(4).
      WRITE print-rec FROM bn-msg-line.
      ADD 1 TO line-number.
      GO TO Write-line.

 Write-line-4.
      MOVE bn-13 OF bn-destination TO msg-text OF bn-position(1).
      MOVE bn-14 OF bn-destination TO msg-text OF bn-position(2).
      MOVE bn-15 OF bn-destination TO msg-text OF bn-position(3).
      MOVE SPACES TO msg-text OF bn-position(4).
      MOVE back-slash TO msg-sep OF bn-position(1).
      MOVE back-slash TO msg-sep OF bn-position(2).
      MOVE back-slash TO msg-sep OF bn-position(3).
      MOVE SPACES TO msg-sep OF bn-position(4).
      WRITE print-rec FROM bn-msg-line.
      ADD 1 TO line-number.
      GO TO Write-line.
/
 Big-number-processing-end.
      MOVE bn-routine TO routine IN status-msg.
      MOVE acknowledge-end TO msg-text IN status-msg.
      WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.
      ADD 1 TO branching-and-looping-flag.

 Big-number-processing-exit.
     EXIT.
/
 Edited-constants SECTION.
 Edited-constants-processing.

     MOVE ed-routine TO routine OF status-msg.
     MOVE acknowledge-begin TO msg-text OF status-msg.
     WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.

     WRITE print-rec FROM a-long-literal AFTER ADVANCING 1 LINE.
     WRITE print-rec FROM suite-to-literal.
     PERFORM Additional-edit-tests.
     PERFORM Fill-ed-pic-table.
     GO TO Write-ed-msg.

 Additional-edit-tests.
     MOVE val-1 OF in-vals TO val-1 OF rec-vals.
     MOVE val-1 OF in-vals TO val-2 OF rec-vals.
     MOVE val-1 OF in-vals TO val-3 OF rec-vals.
     MOVE val-1 OF in-vals TO val-6 OF rec-vals.
     MOVE all-zeros TO val-1 OF rec-vals, val-2 OF rec-vals.
     MOVE val-2 OF in-vals TO val-1 OF rec-vals.
     MOVE val-2 OF in-vals TO val-2 OF rec-vals.
     MOVE val-2 OF in-vals TO val-12 OF rec-vals.
     MOVE all-zeros TO val-1 OF rec-vals, val-2 OF rec-vals, val-12 OF rec-vals.
     MOVE val-3 OF in-vals TO val-1 OF rec-vals.
     MOVE val-3 OF in-vals TO val-12 OF rec-vals, val-2 OF rec-vals.
     MOVE val-3 OF in-vals TO val-6 OF rec-vals, val-9 OF rec-vals.
     MOVE val-4 OF in-vals TO val-4 OF rec-vals.
     MOVE all-zeros TO val-2 OF rec-vals, val-4 OF rec-vals, val-12 OF rec-vals.
     MOVE val-5 OF in-vals TO val-4 OF rec-vals, val-11 OF rec-vals.
     MOVE val-5 OF in-vals TO val-5 OF rec-vals, val-8 OF rec-vals.
     MOVE all-zeros TO val-3 OF rec-vals, val-4 OF rec-vals.
     MOVE val-6 OF in-vals TO val-4 OF rec-vals.
     MOVE val-7 OF in-vals TO val-2 OF rec-vals, val-3 OF rec-vals.
     MOVE all-zeros TO val-2 OF rec-vals, val-3 OF rec-vals.
     MOVE val-8 OF in-vals TO val-2 OF rec-vals, val-3 OF rec-vals
          val-12 OF rec-vals.
     MOVE name-1 IN in-vals TO name-1 IN rec-vals.
     MOVE name-2 IN in-vals TO name-2 IN rec-vals.
     MOVE name IN in-vals TO name IN rec-vals.
     MOVE mod-num IN in-vals TO mod-num IN rec-vals.
     MOVE id IN in-vals TO id IN rec-vals.
     MOVE owner IN in-vals TO owner IN rec-vals.
     MOVE val-9 OF in-vals TO val-13 OF rec-vals.
     MOVE val-2 OF in-vals TO val-14 OF rec-vals.
     MOVE val-6 OF in-vals TO val-15 OF rec-vals.
     MOVE val-7 OF in-vals TO val-16 OF rec-vals.
     MOVE val-3 OF in-vals TO val-16 OF rec-vals.
     MOVE val-4 OF in-vals TO val-10 OF rec-vals.
     MOVE val-6 OF in-vals TO val-17 OF rec-vals.
     MOVE bn-1 OF bn-values TO val-19 OF rec-vals.
     MOVE bn-2 OF bn-values TO val-19 OF rec-vals.
     MOVE bn-12 OF bn-destination TO val-19 OF rec-vals.
     MOVE bn-12 OF bn-values TO val-19 OF rec-vals.
     MOVE bn-13 OF bn-values TO val-19 OF rec-vals.
     MOVE val-16 OF rec-vals TO val-19 OF rec-vals.
     MOVE val-10 OF rec-vals TO val-19 OF rec-vals.
     MOVE val-4 OF rec-vals TO val-19 OF rec-vals.
     MOVE bn-12 OF bn-values TO val-13 OF rec-vals.
     MOVE bn-13 OF bn-values TO val-13 OF rec-vals.
     MOVE bn-1 OF bn-values TO val-18 OF rec-vals.
     MOVE bn-2 OF bn-values TO val-18 OF rec-vals.
     MOVE bn-24 OF bn-values TO val-20 OF rec-vals.
     MOVE bn-25 OF bn-values TO val-20 OF rec-vals.
     MOVE bn-12 OF bn-values TO val-20 OF rec-vals.
     MOVE bn-1 OF bn-values TO val-21 OF rec-vals.
     MOVE bn-1 OF bn-values TO val-11 of rec-vals.
     MOVE bn-1 OF bn-values TO val-4 OF rec-vals.

 Fill-ed-pic-table.
     MOVE edit-1 OF edit-values TO edit-1 IN line-1 OF ed-msg.
     MOVE edit-2 OF edit-values TO edit-2 IN line-1 OF ed-msg.
     MOVE edit-3 OF edit-values TO edit-3 IN line-1 OF ed-msg.
     MOVE edit-4 OF edit-values TO edit-4 IN line-1 OF ed-msg.
     MOVE edit-5 OF edit-values TO edit-5 IN line-2 OF ed-msg.
     MOVE edit-6 OF edit-values TO edit-6 IN line-2 OF ed-msg.
     MOVE edit-7 OF edit-values TO edit-7 IN line-2 OF ed-msg.
     MOVE edit-8 OF edit-values TO edit-8 IN line-2 OF ed-msg.

 Write-ed-msg.
     PERFORM Write-ed-msg-header.
     PERFORM Write-ed-msg-text.
     GO TO Ed-constants-processing-end.

 Write-ed-msg-header.
        WRITE print-rec FROM ed-msg-header.

 Write-ed-msg-text.
     WRITE print-rec FROM line-1 OF ed-msg.
     WRITE print-rec FROM line-2 OF ed-msg.

 Ed-constants-processing-end.
     MOVE acknowledge-end TO msg-text OF status-msg.
     MOVE ed-routine TO routine OF status-msg.
     WRITE print-rec FROM status-msg.
     ADD 1 TO branching-and-looping-flag.

 Ed-constants-processing-exit.
     EXIT.
/
 Various-calculations SECTION.
 Calculations.
     IF we-want-addition
        PERFORM Various-additions
     ELSE
        IF we-want-division
           PERFORM Various-divisions
        ELSE
           MOVE 2 TO program-flag
           MOVE invalid-ind TO msg-text OF status-msg
           MOVE calc-routine TO routine OF status-msg
           WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.

 Various-additions.

      MOVE add-routine TO routine OF status-msg.
      MOVE acknowledge-begin TO msg-text OF status-msg.
      WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.
      PERFORM Add-1 THRU Add-7.

      MOVE acknowledge-end TO msg-text OF status-msg.
      MOVE add-routine TO routine OF status-msg.
      WRITE print-rec FROM status-msg AFTER ADVANCING 2 LINES.
      ADD 1 TO calculations-flag.
      GO TO Various-divisions.

 Add-1.
      MOVE op-all-1s TO op-1 OF addition-data.
      MOVE op-one-1 TO op-2 OF addition-data.
      PERFORM add-numbers.

 Add-2.
      MOVE op-all-9s TO op-1 OF addition-data.
      PERFORM add-numbers.

 Add-3.
      MOVE op-all-1s TO op-2 OF addition-data.
      PERFORM add-numbers.

 Add-numbers.
      ADD op-1 OF addition-data, op-2 OF addition-data GIVING sum-of-ops
                            ON SIZE ERROR ENTER ABEND.
      MOVE op-1 OF addition-data TO op-1 OF calc-msg.
      MOVE add-sign TO op-sign OF calc-msg.
      MOVE op-2 OF addition-data TO op-2 OF calc-msg.
      IF invalid-condition
         MOVE all-stars TO bad-result
         MOVE 0 TO program-flag
      ELSE
         MOVE sum-of-ops TO result OF calc-msg.
      WRITE print-rec FROM calc-msg.

 Add-4.
      ADD decimal-1 OF addit-data, decimal-2 OF addit-data TO decimal-4 OF
          addit-data ON SIZE ERROR ENTER ABEND.
      ADD decimal-1 of addit-data, decimal-4 OF addit-data GIVING decimal-3 OF
          addit-data.
      SUBTRACT decimal-3 OF addit-data FROM decimal-4 OF addit-data GIVING
          decimal-3 OF addit-data.

 Add-5.
      ADD comp-1 OF addit-data, all-zeros GIVING display-num.
      ADD comp-2 OF addit-data, all-zeros GIVING display-num.
      ADD comp-1 OF addit-data TO comp-2 OF addit-data.
      ADD comp-2 OF addit-data, all-zeros GIVING display-num.
      ADD comp-1 OF addit-data, comp-2 OF addit-data GIVING cmp3 OF
          addit-data.
      ADD cmp3 OF addit-data, all-zeros GIVING display-num.
      SUBTRACT cmp3 OF addit-data FROM comp-2 OF addit-data GIVING
            cmp3 OF addit-data.
      ADD cmp3 OF addit-data, all-zeros GIVING display-num.

 Add-6.
      ADD synch-1 IN addit-data, all-zeros GIVING display-num.
      ADD synch-2 IN addit-data, all-zeros GIVING display-num
          ON SIZE ERROR ENTER ABEND.
      ADD synch-3 IN addit-data, all-zeros GIVING display-num.
      ADD synch-1 IN addit-data TO synch-2 IN addit-data.
      ADD synch-2 IN addit-data, all-zeros GIVING display-num.
      ADD synch-2 IN addit-data, synch-1 IN addit-data GIVING synch-3 IN
          addit-data.
      ADD all-zeros, synch-3 OF addit-data GIVING display-num.
      SUBTRACT synch-3 IN addit-data FROM synch-1 OF addit-data GIVING synch-3
             IN addit-data.
      ADD all-zeros, synch-3 OF addit-data GIVING display-num.

 Add-7.
      ADD val-2 OF in-vals, val-3 OF in-vals TO val-1 OF in-vals.
      ADD val-1 OF in-vals, comp-1 of addit-data GIVING bn-3 OF bn-values.
      ADD op-1 OF addition-data, synch-1 OF addit-data GIVING bn-3 of bn-values.
      SUBTRACT bn-3 OF bn-values FROM bn-20 OF bn-values GIVING bn-4 OF
           bn-values.

 Add-9-para.
      MOVE env TO files.

/
 VARIOUS-DIVISIONS .
      MOVE divide-routine TO routine OF status-msg.
      MOVE acknowledge-begin TO msg-text OF status-msg.
      WRITE print-rec FROM status-msg.
      PERFORM Divide-1 THRU Divide-4.

      MOVE divide-routine TO routine OF status-msg.
      MOVE acknowledge-end TO msg-text OF status-msg.
      WRITE print-rec FROM status-msg.
      ADD 1 TO calculations-flag.

 Divide-1.
      MOVE op-all-4s TO dividend.
      MOVE op-all-2s TO current-divisor.
      PERFORM Divide-numbers.

 Divide-2.
      MOVE op-one-2 TO current-divisor.
      PERFORM Divide-numbers.

 Divide-3.
      MOVE villain-divisor TO current-divisor.
      PERFORM Divide-numbers.

 Divide-numbers.
      DIVIDE dividend BY current-divisor
                      GIVING quotient
                      ON SIZE ERROR MOVE 2 TO program-flag,
                                    ENTER TAL DEBUG.

      MOVE dividend TO op-1 OF calc-msg.
      MOVE current-divisor TO op-2 OF calc-msg.
      MOVE divide-sign TO op-sign OF calc-msg.
      IF invalid-condition
         MOVE all-stars TO bad-result
         MOVE 0 TO program-flag
      ELSE
         MOVE quotient TO result OF calc-msg.

      WRITE print-rec FROM calc-msg.

 Divide-4.
      DIVIDE decimal-1 OF div-data BY decimal-2 OF div-data GIVING
            decimal-3 OF div-data.
      MULTIPLY decimal-3 OF div-data BY decimal-1 OF div-data GIVING
            decimal-4 OF div-data.
      MULTIPLY synch-3 IN addit-data BY comp-1 IN div-data GIVING decimal-3
            IN div-data.
      MULTIPLY synch-3 IN div-data BY synch-2 IN div-data GIVING decimal-4
            IN div-data.
      DIVIDE comp-1 IN div-data BY comp-2 IN div-data GIVING cmp3 IN
      div-data.  DIVIDE cmp3 IN div-data INTO comp-1 OF div-data
              GIVING synch-3 IN div-data.
      MULTIPLY synch-1 IN div-data BY synch-2 IN addit-data GIVING synch-3
              IN div-data.

 Calc-exit.
      EXIT.

 end program main-program-unit.
