?inspect
?symbols
 IDENTIFICATION DIVISION.
 PROGRAM-ID.
      BSORT.
 AUTHOR.
      Raghu.
      Modified by Kenneth Luu for COBOL85 INPSECT tests
 INSTALLATION.
      Tandem computers Incorporated
      19333 Vallco Parkway
      Cupertino, CA 95014

 DATE-WRITTEN.
      10 Oct 1984
       Modified by Kenneth Luu in 7 Oct 1985

 DATE-COMPILED.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.

 SOURCE-COMPUTER.
      Tandem NonStop.

 OBJECT-COMPUTER.
      Tandem NonStop.

 SPECIAL-NAMES.

 INPUT-OUTPUT SECTION.

 FILE-CONTROL.
      SELECT Print-file
      ASSIGN TO "#out"
      FILE STATUS is Print-status.

 I-O-CONTROL.
/
 DATA DIVISION.

 FILE SECTION.


 FD print-file
    LABEL RECORDS ARE OMITTED
    DATA RECORD is print-rec
    RECORD CONTAINS 132 CHARACTERS.

 01 print-rec                         PIC X(132).

 WORKING-STORAGE SECTION.
 77 print-status                      PIC XX.
 01 Array.
    02 Num          OCCURS 20 TIMES   PIC 9999.
 01 Temp                              PIC 9999.
 01 I                                 PIC 99.
 01 N                                 PIC 99.
 01 Nxt                               PIC 99.
 01 J                                 pic 99.

 LINKAGE SECTION.


 PROCEDURE DIVISION.
 Main-Processing SECTION.
      PERFORM Get-Data.
      MOVE 20 TO N.
      PERFORM Outer-loop 19 TIMES.
      PERFORM Exuent.

 Exuent SECTION.
      EXIT.

 Get-Data SECTION.


 Outer-loop SECTION.
      PERFORM Inner-loop VARYING j FROM 1 BY 1 UNTIL j = N.
      SUBTRACT 1 from N.


 Inner-loop SECTION.
      PERFORM check-and-switch.


 Check-and-switch SECTION.
      ADD j, 1 GIVING nxt.
      IF num of Array ( j ) > Num of Array ( nxt )
         MOVE Num of Array ( j ) TO temp
         MOVE Num of Array ( nxt ) TO num of Array ( j )
         MOVE temp TO num of Array ( nxt ).

  end program bsort.
