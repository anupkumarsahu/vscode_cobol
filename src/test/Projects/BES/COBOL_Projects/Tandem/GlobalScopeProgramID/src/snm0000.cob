?SECTION MAIN-PROGRAM-UNIT
?MAIN MAIN-PROGRAM-UNIT
?SAVE STARTUP
?symbols
?inspect

 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      MAIN-PROGRAM-UNIT.  
 AUTHOR.
      malcolm mosher.
      Modified by Kenneth Luu for COBOL85 INSPECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INCORPRATED
      19333 VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA GROUP.
 DATE-WRITTEN.
      STARTED:   october 13, 1982
      COMPLETED: october 13, 1982
 DATE-COMPILED.


 ENVIRONMENT DIVISION.

 CONFIGURATION SECTION.

 SOURCE-COMPUTER.
      TANDEM NONSTOP.

 OBJECT-COMPUTER.
      TANDEM NONSTOP.

 INPUT-OUTPUT SECTION.

 FILE-CONTROL.
      SELECT print-file
        ASSIGN TO "#OUT"
        FILE STATUS IS print-status.

      SELECT seq1-file
* Changing reference to C000DAT? for executing the tests
* in local workspaces inherited from COBBAT. AW:01/18/06
* -------------------------------------------------------
        ASSIGN TO "C000DAT1"
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS seq1-status.

      SELECT seq2-file
        ASSIGN TO "C000DAT2"
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS seq2-status.


 I-O-CONTROL.

 RECEIVE-CONTROL.
      MESSAGE SOURCE IS receive-source
      REPORT SYSTEM MESSAGES.

/
 DATA DIVISION.


 FILE SECTION.

 FD  print-file
     LABEL RECORDS ARE OMITTED
     DATA RECORD IS print-rec
     RECORD CONTAINS 132 CHARACTERS.

 01  print-rec                       PIC X(132).


 FD  seq2-file
     LABEL RECORDS ARE OMITTED
     RECORD CONTAINS 115 CHARACTERS.

 01  seq2-rec.
     05  name                        PIC A(8).
     05  FILLER                      PIC X.
     05  works                       PIC X(7).
     05  FILLER                      PIC X.
     05  rest-of-line                PIC X(34).

 FD  seq1-file
     LABEL RECORDS ARE OMITTED
     RECORD CONTAINS 102 CHARACTERS.

 01  seq1-rec.
     05  composer                    PIC A(8).
     05  FILLER                      PIC X.
     05  title                       PIC A(13).
     05  FILLER                      PIC X.
     05  players                     PIC A(52).

/
 WORKING-STORAGE SECTION.
 01 error-code                      PIC S999 COMP.
 01 file-name                       PIC X(24).
 01 file-number                     PIC S99  COMP.

 01 subi                            PIC 99  VALUE 0.
 01 subj                            PIC 99  VALUE 0.

 01  composers.
   02 who     OCCURS 4.
      03 name                       PIC A(8).
      03 works OCCURS 5 TIMES.
         04 title                   PIC A(13).
         04 cast                    PIC X(52).
         04 char-actors REDEFINES cast.
            05 heros.
               06 hero-1            PIC A(13).
               06 hero-2            PIC A(13).
            05 villain              PIC A(13).
            05 heroine              PIC A(13).

 01 list-index  pic 999.

 01 more-composers.
   02 operatic.
      03 num-of-composers           PIC 99999  VALUE 8.
      03 list-of-works  OCCURS 1 TO 9 TIMES DEPENDING ON num-of-composers.
         04 name                    PIC A(8).
         04 works                   PIC X(7).
         04 descript   REDEFINES works.
            05 type-of-music        PIC A(5).
            05 num-composed         PIC 99.
         04 more-data            PIC X(34).
         04 specifics  REDEFINES more-data.
            05 opera-1.
               06 title             PIC A(13).
               06 date-of-comp      PIC 9(4).
            05 opera-2.
               06 title             PIC A(13).
               06 date-of-comp      PIC 9999.

 77  index-type                     pic 999.
 77  seq1-status                    PIC XX     VALUE  ZEROS.
 77  seq2-status                    PIC XX     VALUE  ZEROS.
 77  print-status                   PIC XX     VALUE  ZEROS.

* $RECEIVE feedback information.
* ------------------------------

 01  receive-source.
     05  rs-sys-flag                PIC S9   COMP.
     05  rs-entry-num               PIC 999  COMP.
     05  FILLER                     PIC X(4).
     05  rs-process-id.
         10  rs-process-name        PIC X(6).
         10  rs-cpu-pin             PIC X(2).
     05  FILLER                     PIC X(16).

 77  main-flag                      PIC 9.
     88 eof                                  VALUE 1.
     88 error-detected                       VALUE 2.
     88 invalid-data                         VALUE 3.
 77  seq1-flag                      PIC 9.
     88 seq1-file-er                         VALUE 0.
     88 seq1-file-opened                     VALUE 1.
 77  seq2-flag                      PIC 9.
     88 seq2-file-er                         VALUE 0.
     88 seq2-file-opened                     VALUE 1.

/
 PROCEDURE DIVISION.

 Main-processing SECTION.

      PERFORM Initialization THRU Initialization-exit.
      PERFORM Read-seq1-table THRU Seq1-table-exit 4 times.
      IF error-detected
         GO TO End-of-main.
      PERFORM Read-seq2-table UNTIL eof OR error-detected.
      IF error-detected
         NEXT SENTENCE
      ELSE CALL sub-program-unit-1 USING composers, more-composers.
      GO TO End-of-main.

 End-of-main.

      PERFORM End-of-processing.
      STOP RUN.

 End-of-processing.
      CLOSE seq1-file.
      CLOSE seq2-file.
      CLOSE print-file.




/
 Initialization SECTION.
      PERFORM Open-output.
      PERFORM Open-seq1-file.
      IF seq1-file-er
         DISPLAY "ERROR ON SEQ1 OPEN"
         GO TO End-of-main
      ELSE PERFORM Open-seq2-file.
      IF seq2-file-er
           DISPLAY "ERROR ON SEQ2 OPEN"
           GO TO End-of-main
      ELSE PERFORM Set-indices
             MOVE 0 TO main-flag.
      GO TO Initialization-exit.

 Open-output.
      OPEN OUTPUT print-file.
      IF print-status NOT = ZEROS
         MOVE 2 TO main-flag
         GO TO Initialization-exit.

 Open-seq1-file.
      MOVE 1 TO seq1-flag.
      OPEN INPUT seq1-file.
      IF NOT seq1-status = ZEROS
         MOVE 0 TO seq1-flag.

 Open-seq2-file.
      MOVE 1 TO seq2-flag.
      OPEN INPUT seq2-file.
      IF NOT seq2-status = ZEROS
         MOVE 0 TO SEQ2-flag.

 Set-indices.
      MOVE 1 TO list-index.


 Initialization-exit.
      EXIT.

/
 Read-seq1-table SECTION.

      READ seq1-file ; AT END MOVE 1 TO main-flag.
      IF eof
         GO TO Seq1-table-exit.

 Set-seq1-indices.
      ADD 1 TO subi.
      MOVE 1 TO subj.

 Build-comp-table.
      MOVE composer OF seq1-rec TO name OF who (subi).
      MOVE title OF seq1-rec TO title OF works OF who (subi subj).
      MOVE players OF seq1-rec TO cast OF works OF who (subi subj).
      PERFORM Read-seq1-file 4 TIMES.

 Seq1-table-exit.
      EXIT.

/
 Read-seq1-file SECTION.
      ADD 1 TO subj.
      READ seq1-file;
         AT END MOVE 1 TO main-flag.
      IF seq1-status NOT = ZEROS
         MOVE 2 TO main-flag.
      IF seq1-status = 10
         MOVE 1 TO main-flag.

 Fill-comp-tab.

      MOVE title OF seq1-rec TO title OF who (subi subj).
      MOVE players OF seq1-rec TO cast OF who (subi subj).

 Read-seq1-exit.
      EXIT.

/
 Read-seq2-table SECTION.
      READ seq2-file; AT END MOVE 1 TO main-flag.
      IF eof OR error-detected
         GO TO Table-exit. 

 Build-more-comp-table.
      MOVE name OF seq2-rec TO name OF list-of-works (list-index).
      MOVE works OF seq2-rec TO works OF list-of-works (list-index).
      MOVE rest-of-line OF seq2-rec TO more-data OF list-of-works (list-index).

 Set-list-index-up.
      add 1 to list-index.

 Table-exit.
      EXIT.

 end program main-program-unit.
/
?SECTION SUB-PROGRAM-UNIT-1
 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      SUB-PROGRAM-UNIT-1.
 AUTHOR.
      MM.
      Modified by Kenneth Luu for COBOL85 INSPECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INCORPRATED
      19333 VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA GROUP.
 DATE-WRITTEN.
      STARTED:   AUGUST 14TH,1982
      COMPLETED:
 DATE-COMPILED.

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
        ASSIGN TO "#OUT"
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

 WORKING-STORAGE SECTION.
 77  print-status                   PIC XX.

 01  composers.
   02 who.
      03 name                       PIC A(8).
      03 works.
         04 title                   PIC A(13).
         04 cast                    PIC X(52).
         04 char-actors REDEFINES cast.
            05 heros.
               06 hero-1             PIC A(13).
               06 hero-2             PIC A(13).
            05 villains              PIC A(13).
            05 heroine               PIC A(13).
      66 star    RENAMES hero-1.
      66 woman   RENAMES heroine.
      66 players RENAMES char-actors.
      66 summary RENAMES title THRU heroine.

 77 temporary                       PIC 999   VALUE 1.
 77 cond-1                          PIC X(10) VALUE "Solti     ".
 77 cond-2                          PIC A(10) VALUE "Walter    ".
 77 cond-3                          PIC A(10) VALUE "Davis     ".
 77 cond-4                          PIC X(10) VALUE "Rudel     ".
 77 sch-l                           PIC S99999 VALUE -12345.
 77 sch-listing                     PIC XXXXXXXX.
 77 label-1                         PIC AA     VALUE "LN".
 77 label-2                         PIC AA     VALUE "DG".
 77 cost                            PIC 99999  COMP VALUE 34567.
 77 i                               PIC 9      VALUE ZERO.
 77 j                               PIC 9      VALUE ZERO.
 77 x                               PIC 9      VALUE ZERO.

 01 recording-data.
    02 who OCCURS 4 TIMES.
       03 name                       PIC X(8).
       03 works  OCCURS 2 TIMES.
          04 title                   PIC A(13).
          04 listing-info.
             05 conductor            PIC A(11).
             05 schwann-listing      PIC XXX999/909.
             05 price                PIC $$$,$$9.99.
          04 dealers-code            PIC 99999 COMP.

 LINKAGE SECTION.
 01  comps.
   02 who     OCCURS 4 TIMES.
      03 name                       PIC A(8).
      03 works OCCURS 5 TIMES.
         04 title                   PIC A(13).
         04 cast                    PIC X(52).
         04 char-actors REDEFINES cast.
            05 villain.
               06 vil-1             PIC A(13).
               06 vil-2             PIC A(13).
            05 heros                PIC A(13).
            05 heroine              PIC A(13).

 01 more-comps.
   02 operatic.
      03 num-of-composers           PIC 99999.
      03 list-of-works  OCCURS 1 TO 9 TIMES DEPENDING ON num-of-composers,
                                     INDEXED BY l-ind.
         04 name                    PIC A(8).
         04 works                   PIC X(7).
         04 descript   REDEFINES works.
            05 type-of-music        PIC A(5).
            05 num-composed         PIC 99.
         04 some-more-data          PIC X(34).
         04 specifics  REDEFINES some-more-data.
            05 opera-2.
               06 title             PIC A(13).
               06 date-comp         PIC 9(4).
            05 opera-1.
               06 title             PIC A(13).
               06 date-comp         PIC 9999.

 PROCEDURE DIVISION USING comps, more-comps.

 Build-recording-data.
*
* The next 3 MOVEs are to allow this subprogram to be re-executed
* with a resume at.
*
      MOVE 0 TO i.
      MOVE 0 TO x. 
      MOVE 1 TO temporary.
      PERFORM Add-to-i THRU Get-works 4 TIMES.
      MOVE cond-1 TO conductor IN recording-data (1 1), conductor
           IN recording-data (3 1).
      MOVE cond-2 TO conductor IN recording-data (1 2), conductor
           IN recording-data (3 2).
      MOVE cond-3 TO conductor IN recording-data (2 1), conductor
           IN recording-data (4 1).
      MOVE cond-4 TO conductor IN recording-data (2 2), conductor
           IN recording-data (4 2).
      CALL SUB-PRO
-GRAM-UNIT-2  USING comps, more-comps, recording-data. 
      PERFORM Renames-test.           
      GO TO Sub-exit.   

 Add-to-i. 
      ADD 1 TO i. 
      MOVE 0 TO J. 

 Get-works.
      PERFORM Add-to-j THRU Get-stuff 2 TIMES.

 Add-to-j.
      ADD 1 TO j.
      ADD 1 TO x.
      MOVE name IN more-comps (x) TO name IN recording-data (i).

 Temp.
      IF temporary IS GREATER THAN 5
           ADD 5 TO temporary
      ELSE ADD 1 TO temporary.

 Get-stuff.
      MOVE title OF opera-1 IN more-comps (x) TO title IN recording-data (i j).
      ADD 1 TO sch-l.
      IF j IS EQUAL TO 1
          STRING label-1, sch-l DELIMITED BY SIZE INTO sch-listing
      ELSE STRING label-2, sch-l DELIMITED BY SIZE INTO sch-listing.
      MOVE sch-listing TO schwann-listing IN recording-data (i j).
      MOVE cost TO price IN recording-data (i, j).
      ADD 5 TO cost.
      MOVE cost TO dealers-code IN recording-data (i j).

 Renames-test.
      MOVE name OF recording-data (1) TO name  OF composers.
      MOVE title OF comps (1 2) TO title OF composers. 
      MOVE cast OF comps (1 2)  TO cast OF composers. 

 Sub-exit.
      EXIT.

 end program sub-program-unit-1.
/
