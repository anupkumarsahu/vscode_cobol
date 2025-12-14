?SECTION SUB-PROGRAM-UNIT-2
?symbols
?inspect

 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      SUB-PROGRAM-UNIT-2.
 AUTHOR.
      mm.
      Modified by Kenneth Luu for COBOL85 INSPECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INC.
      19333  VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA.
 DATE-WRITTEN.
      Date-started:  OCTOBER 14th, 1982
      Date-modified: October 24th, 1985
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
 01 temp                             PIC 9999  VALUE 0.
 77 print-status                     PIC XX.
 01 more-info.
    02 comps   OCCURS 2 TIMES.
       03 titles  OCCURS 2 TIMES.
          04 casts  OCCURS 2 TIMES.
             05 player               PIC A(13).
             05 subplayer1  OCCURS 2 TIMES.
                06 subplayer2  OCCURS 3 TIMES.
                   07 suplayer3  OCCURS 4 TIMES.
                      08 subplayer4  OCCURS 5 TIMES.
                         09 subplayer5  PIC A(13).
          04 title                   PIC x(13).
       03 composer                   PIC A(8).
 01 j  pic 999.


 LINKAGE SECTION.

 01 rec-data.
    02 who OCCURS 4 TIMES.
       03 name                       PIC X(8).
       03 works  OCCURS 2 TIMES.
          04 title                   PIC A(13).
          04 listing-info.
             05 conductor            PIC A(11).
             05 schwan-listing       PIC XXX999/909.
             05 price                PIC $$$,$$9.99.
          04 dealers-code            PIC 99999 COMP.

 01  op-comps.
   02 who     OCCURS 4 TIMES.
      03 name                       PIC A(8).
      03 works OCCURS 5 TIMES.
         04 title                   PIC A(13).
         04 cast                    PIC X(52).
         04 chars REDEFINES cast.
            05 heros.
               06 hero-1            PIC A(13).
               06 hero-2            PIC A(13).
            05 villain              PIC A(13).
            05 heroine              PIC A(13).

 01 more-op-comps.
   02 operatic.
      03 num-of-composers           PIC 99999.
      03 list-of-works  OCCURS 3 TO 12 TIMES DEPENDING ON num-of-composers.
         04 name                    PIC A(8).
         04 works                   PIC X(7).
         04 descript   REDEFINES works.
            05 type-of-music        PIC A(5).
            05 num-composed         PIC 99.
         04 rest-of-data            PIC X(34).
         04 specifics  REDEFINES rest-of-data.
            05 opera-1.
               06 title             PIC A(13).
               06 date-of-comp      PIC 9(4).
            05 opera-2.
               06 title             PIC A(13).
               06 date-of-comp      PIC 9999.

/
 PROCEDURE DIVISION USING op-comps, more-op-comps, rec-data.

 Get-more-info-1.
      PERFORM Set-index-1.
      MOVE name OF list-of-works (j) TO composer OF comps (1).
      MOVE title OF opera-1 OF list-of-works (j) TO title OF titles (1 1).
      MOVE title OF opera-2 OF list-of-works (j) TO title OF titles (1 2).
      MOVE hero-1 IN who (1 1) TO player OF casts (1 1 1).
      MOVE heroine IN who (1 1) TO player OF casts (1 1 2).
      MOVE hero-1 OF who (3 1) TO player OF casts (1 2 1).
      MOVE heroine OF who (3 1) TO player OF casts (1 2 2).

 Get-more-info-2.
      PERFORM Set-index-2.
      MOVE name OF list-of-works (j) TO composer OF comps (2).
      MOVE title OF opera-1 OF list-of-works (j) TO title OF titles (2 1).
      MOVE title OF opera-2 OF list-of-works (j) TO title OF titles (2 2).
      MOVE hero-1 OF who (2 1) TO player OF casts (2 1 1).
      MOVE heroine OF who (2 1) TO player OF casts (2 1 2).
      MOVE hero-1 OF who (4 1) TO player OF casts (2 2 1).
      MOVE hero-1 OF who (4 1) TO subplayer5 (2 2 2 2 3 4 5).
      MOVE heroine OF who (4 1) TO player OF casts (2 2 2).

 Set-index-1.
      move 1 to j.

 Set-index-2.
      move 2 to j.

 Start-loop.
      PERFORM Do-loop UNTIL temp > 100.

 Do-loop.
      CALL sub-program-3 USING temp.

 Sub-exit.
      EXIT.

 end program SUB-PROGRAM-UNIT-2. 











/
?SECTION SUB-PROGRAM-3
?saveabend
?symbols
?inspect
 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      SUB-PROGRAM-3.
 AUTHOR.
      mm.
      Modified by Kenneth Luu for COBOL85 INSPECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INC.
      19333  VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA.
 DATE-WRITTEN.
      DECEMBER 08, 1982.

 ENVIRONMENT DIVISION.

 CONFIGURATION SECTION.

 SOURCE-COMPUTER.
      TANDEM NONSTOP.

 OBJECT-COMPUTER.
      TANDEM NONSTOP.

 SPECIAL-NAMES.


 INPUT-OUTPUT SECTION.

 FILE-CONTROL.
      SELECT dummy-file
      ASSIGN TO "#OUT".

      SELECT seq3-file
* Changing reference to C000DAT3 for executing the tests
* in local workspaces inherited from COBBAT. AW:01/18/06
* -------------------------------------------------------
  ASSIGN to "C000DAT3"
      ORGANIZATION IS SEQUENTIAL
      ACCESS MODE IS SEQUENTIAL
      FILE STATUS IS seq3-status.

 I-O-CONTROL.


/
 DATA DIVISION.

 FILE SECTION.
 FD   dummy-file
      LABEL RECORDS ARE OMITTED
      DATA RECORD IS dum-rec.

 01   dum-rec                                       PIC A.

 FD   seq3-file
      LABEL RECORDS ARE OMITTED
      RECORD CONTAINS 68 characters.

 01   seq3-rec.
      05 cmp                                        PIC A(8).
      05 p1                                         PIC A(6).
      05 p2                                         PIC A(6).
      05 p3                                         PIC A(6).
      05 p4                                         PIC A(6).
      05 p5                                         PIC A(6).
      05 p6                                         PIC A(6).
      05 p7                                         PIC A(6).
      05 p8                                         PIC A(6).
      05 p9                                         PIC A(6).
      05 p10                                        PIC A(6).

 WORKING-STORAGE SECTION.
      01 rec-data.
         02 list OCCURS 21 TIMES.
            03 cmp                                 PIC A(8).
            03 player  OCCURS 10 TIMES             PIC A(6).

      77 seq3-status                               PIC XX  VALUE zeros.
      77 i                                         PIC 99  VALUE 0.

 LINKAGE SECTION.
      01 x                                          PIC 9999.

 PROCEDURE DIVISION using x.

 Program-flow.
      if x > 2
         GO TO add-to-x.

 Get-data.
      OPEN INPUT seq3-file.
      MOVE 0 TO i.
      PERFORM Build-table 21 times.
      GO TO close-seq3.

 Build-table.
      PERFORM read-seq3.
      MOVE cmp of seq3-rec TO cmp of list (i).
      MOVE p1 OF seq3-rec TO player (i 1).
      MOVE p2 OF seq3-rec TO player (i 2).
      MOVE p3 OF seq3-rec TO player (i 3).
      MOVE p4 OF seq3-rec TO player (i 4).
      MOVE p5 OF seq3-rec TO player (i 5).
      MOVE p6 OF seq3-rec TO player (i 6).
      MOVE p7 OF seq3-rec TO player (i 7).
      MOVE p8 OF seq3-rec TO player (i 8).
      MOVE p9 OF seq3-rec TO player (i 9).
      MOVE p10 OF seq3-rec TO player (i 10).

 Read-seq3.
      ADD 1 TO i.
      READ seq3-file ; AT END GO TO close-seq3.

 Close-seq3.
      CLOSE seq3-file.

 Add-to-x.
      ADD 1 TO x.

 Sub-exit.
      EXIT.

 end program sub-program-3.
