?symbols
?inspect
 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      Sub1.
 AUTHOR.
      Raghu Toppur.
      Modified by Kenneth Luu for COBOL85 INPSECT tests.
 INSTALLATION.
      TANDEM COMPUTERS INCORPRATED
      19333 VALLCO PARKWAY
      CUPERTINO, CA 95014

      SOFTWARE DEVELOPMENT
      QA GROUP.
 DATE-WRITTEN.
      STARTED:   October 4rd, 1984
      Modified by Kenneth Luu in 7 Oct 1985
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

 LINKAGE SECTION.
 01  parm-1                         PIC 99.

 01  subprogram-table.
     05 sub-table-entry,            PIC X
        OCCURS 10 TIMES INDEXED BY SUB-INDEX.
 01  save-sub-index                 PIC 999.

 PROCEDURE DIVISION USING parm-1,
                          subprogram-table,
                          save-sub-index.

 Start-sub2.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.
      add 1 to parm-1.





 Sub-exit.
      EXIT.

 end program sub1.
