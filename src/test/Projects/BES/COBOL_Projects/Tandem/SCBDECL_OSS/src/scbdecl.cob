* For Paymentech C10-070131-3553
* Given a subprog with declaratives
* 'b subprog1' sets bpt on different address
* than what's reported by 'info subprog1'
?symbols

 IDENTIFICATION DIVISION.
 PROGRAM-ID. Test-subprog-decl.
 PROCEDURE DIVISION.
 Begin.
    CALL Subprog1.
    CALL Subprog2.
    STOP RUN.
 END PROGRAM Test-subprog-decl.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  Subprog1.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT infile ASSIGN TO declbcb
        FILE STATUS IS fsi.
 DATA DIVISION.
 FILE SECTION.
 FD  infile
     GLOBAL
     RECORD IS VARYING FROM 1 TO 255 DEPENDING ON rec-len.
 01  inrec  PIC X(255).
 WORKING-STORAGE SECTION.
 01  line-count PIC 9999 COMP VALUE 0 GLOBAL.
 01  fsi  PIC XX.
     88  fsi-good  VALUE "00".
 01  rec-len  PIC 999 COMP.
 LINKAGE SECTION.
 PROCEDURE DIVISION.
 DECLARATIVES.
 Err SECTION.
     USE AFTER STANDARD EXCEPTION PROCEDURE ON Infile.
 Err-p.
     DISPLAY "In Decl, line-count = ", line-count.
 END DECLARATIVES.
 Startt.
     OPEN INPUT infile
     PERFORM WITH TEST AFTER UNTIL fsi = "10"
         READ infile
         ADD 1 to line-count
     END-PERFORM
     CLOSE infile
     DISPLAY "Line count " line-count
     STOP RUN.
 END PROGRAM Subprog1.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  Subprog2.
 DATA DIVISION.
 LINKAGE SECTION.
 PROCEDURE DIVISION.
 Startt.
    DISPLAY "You made it to subprog2!".
 END PROGRAM Subprog2.
