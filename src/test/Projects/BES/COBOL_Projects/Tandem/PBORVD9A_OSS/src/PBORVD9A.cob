 IDENTIFICATION DIVISION.
 PROGRAM-ID. PBORVD9A.
 
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SPECIAL-NAMES.
  CONSOLE       IS CONSOLE
  MYTERM        IS MEIN-TERMINAL
  .

 INPUT-OUTPUT SECTION.
*----------------------
 FILE-CONTROL.

**** Eingabedatei.
   SELECT               DNVN90-FILE
   ASSIGN TO            NOFILE
   RESERVE              8
   ORGANIZATION         SEQUENTIAL
   ACCESS               SEQUENTIAL
   FILE STATUS IS WS-FILE-STATUS.

**** Scratchdatei.
   SELECT               DNVNSC90-FILE
   ASSIGN               NOFILE
   RESERVE              8
   ORGANIZATION         SEQUENTIAL
   ACCESS               SEQUENTIAL
   FILE STATUS IS WS-FILE-STATUS.

**** Duplikatedatei.
   SELECT               DNVNDK90-file
   ASSIGN               NOFILE
   RESERVE              8
   ORGANIZATION         SEQUENTIAL
   ACCESS               SEQUENTIAL
   FILE STATUS IS WS-FILE-STATUS.

**** Log-Datei. Ausgabe von Fehlermeldungen
   SELECT               LOG-SERVER
   ASSIGN TO            $LOG
   ORGANIZATION         SEQUENTIAL
   ACCESS               SEQUENTIAL
   FILE STATUS IS LOG-STATUS.

 DATA DIVISION.
*=============

 WORKING-STORAGE SECTION.
*-------------------------
**************************************************************************
*     COBOL  DEKLARATIONEN
**************************************************************************

 01 VN-BATCH-WORK.
   02 PROGRAMM-KENNUNG.
     03 SUBSYSTEM-ID         PIC X(2)  VALUE "OR".
     03 PROGRAMMNAME         PIC X(30) VALUE "PBORVD9A".
   02 WORK-PARAMETER.
*---------------------------------------------------------------------
* File Status Variablen
*---------------------------------------------------------------------
     03 VN-DATEI-STATI.
       04 LOG-STATUS.
         05 LOG-STATUS-1             PIC X.
         05 LOG-STATUS-2             PIC X.
       04 LOG                        PIC 9.
     03 FEHLER-NR                    PIC 9(4) COMP.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 PROCEDURE DIVISION.
*===================

 MAIN SECTION.
     MOVE "OR" TO SUBSYSTEM-ID.
  MAIN-EXIT.
  STOP RUN.
  
 1450-MOVE-PARAMETERS SECTION.
 EXIT.