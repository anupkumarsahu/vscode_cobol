?SYMBOLS
?INSPECT
 IDENTIFICATION DIVISION.
 PROGRAM-ID.
     SVMV004.
 AUTHOR.
     A. INSTRUCTOR.
 DATE-WRITTEN.
     APR 15 1994.
 DATE-COMPILED.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. T16.
 OBJECT-COMPUTER. T16.


 INPUT-OUTPUT SECTION.

 FILE-CONTROL.
     SELECT MESSAGE-IN  ASSIGN TO $RECEIVE.

     SELECT MESSAGE-OUT  ASSIGN TO $RECEIVE.

     SELECT VEHICLE-FILE ASSIGN TO "VEHICLE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS LICENSE-PLATE OF VEHICLE-RECORD
        FILE STATUS IS VEHICLE-FILE-STAT.

     SELECT PERSON-FILE ASSIGN TO "PERSON"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS PERSON-ID OF PERSON-RECORD
        ALTERNATE RECORD KEY IS PERSON-NAME-KEY OF PERSON-RECORD
           WITH DUPLICATES
        FILE STATUS IS PERSON-FILE-STAT.

     SELECT OWNER-FILE ASSIGN TO "OWNER"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS OWNER-REC-GROUP OF OWNER-RECORD
        ALTERNATE RECORD KEY IS LICENSE-PLATE OF OWNER-RECORD
           WITH DUPLICATES
        FILE STATUS IS OWNER-FILE-STAT.
 RECEIVE-CONTROL.
     TABLE OCCURS 10 TIMES
     SYNCDEPTH LIMIT IS 1
     REPLY CONTAINS MESSAGE-OUT RECORD.
/
 DATA DIVISION.
 FILE SECTION.
 FD  MESSAGE-IN
     RECORD CONTAINS 1 TO 100 CHARACTERS
     LABEL RECORDS OMITTED.

     COPY MSMV004-IN IN COBSRC.

 FD  MESSAGE-OUT
     RECORD CONTAINS 1 TO 500 CHARACTERS
     LABEL RECORDS OMITTED.

     COPY MSMV004-OUT IN COBSRC.

 01  MSMV004-ERROR-REPLY.
     05 MSMV004-ERROR-REPLY-CODE       PIC 9(4) COMP.
     05 MSMV004-ERROR-REPLY-MSG        PIC X(78).
/
 FD  VEHICLE-FILE
     LABEL RECORDS OMITTED.

     COPY VEHICLE-RECORD IN COBSRC.

 FD  PERSON-FILE
     LABEL RECORDS OMITTED.

     COPY PERSON-RECORD IN COBSRC.

 FD  OWNER-FILE
     LABEL RECORDS OMITTED.

     COPY OWNER-RECORD IN COBSRC.
/
 WORKING-STORAGE SECTION.

 01  RECEIVE-STOP-FLAG                 PIC 9(1) VALUE 0.
     88  ALL-REQUESTERS-CLOSED         VALUE 1.

 COPY PERSON-FILE-STAT IN COBLIB.

 COPY OWNER-FILE-STAT IN COBLIB.

 COPY VEHICLE-FILE-STAT IN COBLIB.

 COPY FILE-ERROR-BUFFER IN COBLIB.

 COPY COMMON-SERVER-MSGS IN COBLIB.

 COPY DATE-TIME-FIELDS IN COBLIB.

 01  WS-LICENSE-PLATE-EXISTS-MSG       PIC X(78) VALUE
     "Request to Add Vehicle Data Denied; License Plate Already Exists".

 01  WS-PERSON-EXISTS-MSG              PIC X(78) VALUE
     "Request to Add Owner Denied; This Owner Already Exists for This Vehicle".

 01  WS-NO-PERSON-EXISTS-MSG           PIC X(78) VALUE
     "Request to Add Owner Denied; This PERSON-ID Does Not Exist".

 01  WS-OK-MSG                         PIC X(78) VALUE
     "Data Inserted Successfully".

 01  WS-WAIT-TIME                      PIC 9(7)V99  COMP  VALUE 10.
/
 PROCEDURE DIVISION.

 DECLARATIVES.
********************************************************************************
* DECLARATIVES portion used to handle I/O errors.  Each file has its own section
* with a "USE AFTER ERROR PROCEDURE ON <file-name>" statement declared, causing
* the section to be implicitly performed whenever an error is encountered on the
* given <file-name>.  The error handling technique used by these routines is to
* move the physical file name, file-status identifier and GUARDIAN Error to an
* error text buffer.  The processing logic of the program then decides whether
* it can handle the error (e.g., a NO-EXISTING-RECORD for a user-supplied
* LICENSE-PLATE), or should report the error by sending the error text buffer
* back to the user in the reply message.
********************************************************************************

 VEHICLE-FILE-ERROR SECTION.
     USE AFTER ERROR PROCEDURE ON VEHICLE-FILE.
 VEHICLE-FILE-ERR.
     MOVE "VEHICLE" TO FILE-ABBREVIATION
     MOVE VEHICLE-FILE-STAT TO FILE-STATUS-DISPLAY
     MOVE GUARDIAN-ERR TO FILE-GUARDIAN-ERROR-DISPLAY.

 PERSON-FILE-ERROR SECTION.
     USE AFTER ERROR PROCEDURE ON PERSON-FILE.
 PERSON-FILE-ERR.
     MOVE "PERSON" TO FILE-ABBREVIATION
     MOVE PERSON-FILE-STAT TO FILE-STATUS-DISPLAY
     MOVE GUARDIAN-ERR TO FILE-GUARDIAN-ERROR-DISPLAY.

 OWNER-FILE-ERROR SECTION.
     USE AFTER ERROR PROCEDURE ON OWNER-FILE.
 OWNER-FILE-ERR.
     MOVE "OWNER " TO FILE-ABBREVIATION
     MOVE OWNER-FILE-STAT TO FILE-STATUS-DISPLAY
     MOVE GUARDIAN-ERR TO FILE-GUARDIAN-ERROR-DISPLAY.

 END DECLARATIVES.
/
 1000-MAIN-PROC.
********************************************************************************
* Main paragraph, performed one time upon startup.  Perform an initialize
* routine, opening all the files.  If the file-status identifiers indicate
* successful opens, then perform a message processing loop until all TCPs having
* the server process open have closed it.  If the file-status identifiers DO NOT
* indicate successful opens, then move an error text buffer to the reply message
* and write an error reply.  Execute an unconditional Stop Run at completion of
* processing.
********************************************************************************
     PERFORM 2000-INITIALIZE
     IF SUCCESS OF VEHICLE-FILE-STAT
        AND SUCCESS OF PERSON-FILE-STAT
        AND SUCCESS OF OWNER-FILE-STAT
           PERFORM 3000-PROCESS-MESSAGE
              UNTIL ALL-REQUESTERS-CLOSED
     ELSE
        READ MESSAGE-IN
        END-READ
        MOVE 1 TO MSMV004-REPLY-CODE
        MOVE FILE-ERROR-BUFFER TO MSMV004-MESSAGE
        WRITE MSMV004-ERROR-REPLY
     END-IF
     STOP RUN.
/
 2000-INITIALIZE.
********************************************************************************
* Initialize routine, performed one time at program startup, which opens all
* files.
********************************************************************************
     OPEN INPUT MESSAGE-IN
     OPEN I-O VEHICLE-FILE
        WITH TIME LIMITS, SHARED
     OPEN I-O PERSON-FILE
        WITH TIME LIMITS, SHARED
     OPEN I-O OWNER-FILE
        WITH TIME LIMITS, SHARED
     OPEN OUTPUT MESSAGE-OUT.
/
 3000-PROCESS-MESSAGE.
********************************************************************************
* Message processing loop.  Reads, processes and replies to each requester
* message until an AT END condition occurs on $RECEIVE.  If an error has been
* detected by this routine or any of the lower logic routines
* (MSMV004-REPLY-CODE = 1), write an error reply message; otherwise move a text
* message signalling success into the text portion of the reply and write the
* entire reply message.
********************************************************************************
     READ MESSAGE-IN
        AT END
           SET ALL-REQUESTERS-CLOSED TO TRUE
        NOT AT END
           PERFORM 3100-CREATE-REPLY
           IF MSMV004-REPLY-CODE = 1
              WRITE MSMV004-ERROR-REPLY
           ELSE
              MOVE WS-OK-MSG TO MSMV004-MESSAGE
              WRITE MSMV004-OUT
           END-IF.

 3100-CREATE-REPLY.
********************************************************************************
* Create a Reply Message:
* 1. Initialize the Reply Message.
* 2. Evaluate the message tran code.
* 3. Execute a case statement that will perform appropriate lower logic
*    routines based upon the message tran code.
********************************************************************************
     INITIALIZE MSMV004-OUT
     EVALUATE MSMV004-TRAN-CODE
        WHEN 1
**     (WHEN ADD-VEHICLE-AND-OWNER-DATA)
           PERFORM 4000-DO-VEHICLE-AND-OWNER-TRAN
        WHEN 2
**     (WHEN ADD-OWNER-DATA)
           PERFORM 5000-DO-OWNER-ONLY-TRAN
        WHEN OTHER
           MOVE 1 TO MSMV004-REPLY-CODE
           MOVE INVALID-TRAN-CODE-MSG TO MSMV004-MESSAGE
     END-EVALUATE.
/
 4000-DO-VEHICLE-AND-OWNER-TRAN.
********************************************************************************
* Routine that attempts to add a vehicle and its 1st owner to the MVR database.
* The steps are as follows:
* 1. Perform a routine to verify that the owner, represented by the
*    user-supplied PERSON-ID, exists on the database.
* 2. If owner exists:
*    - Move vehicle data from request message to VEHICLE file buffer.
*    - Format VEHICLE file Registration Date (todays date) and Smog Date
*      (todays date + 1 year).
*    - Attempt to Write VEHICLE file record.
*    - If not successful move an appropriate text message to the reply message;
*      otherwise, continue processing by performing a routine to add the owner
*      to the database.
********************************************************************************
     PERFORM 6000-VERIFY-PERSON
     IF SUCCESS OF PERSON-FILE-STAT
        MOVE MSMV004-LICENSE-PLATE
          TO LICENSE-PLATE OF VEHICLE-RECORD
        MOVE MSMV004-VEHICLE-TYPE
          TO VEHICLE-TYPE OF VEHICLE-RECORD
        MOVE MSMV004-YEAR TO YEAR OF VEHICLE-RECORD
        MOVE MSMV004-MAKE TO MAKE OF VEHICLE-RECORD
        MOVE MSMV004-MODEL TO MODEL OF VEHICLE-RECORD
        MOVE MSMV004-ENGINE-NUMBER
          TO ENGINE-NUMBER OF VEHICLE-RECORD
        MOVE MSMV004-SALES-PRICE
          TO SALES-PRICE OF VEHICLE-RECORD
        MOVE MSMV004-HORSEPOWER
          TO HORSEPOWER OF VEHICLE-RECORD
        MOVE MSMV004-NUMBER-OF-AXLES
          TO NUMBER-OF-AXLES OF VEHICLE-RECORD
        ACCEPT TODAYS-DATE FROM DATE
        MOVE TODAYS-DATE TO REGISTRATION-DATE OF VEHICLE-RECORD
        IF YY OF TODAYS-DATE = 99
           MOVE 0 TO YY OF TODAYS-DATE
        ELSE
           ADD 1 TO YY OF TODAYS-DATE
        END-IF
        MOVE TODAYS-DATE TO SMOG-DATE OF VEHICLE-RECORD,
                               MSMV004-SMOG-DATE
        WRITE VEHICLE-RECORD
        IF NOT SUCCESS OF VEHICLE-FILE-STAT
           MOVE 1 TO MSMV004-REPLY-CODE
           IF DUPLICATE-KEY OF VEHICLE-FILE-STAT
              MOVE WS-LICENSE-PLATE-EXISTS-MSG TO MSMV004-MESSAGE
           ELSE
              MOVE FILE-ERROR-BUFFER TO MSMV004-MESSAGE
           END-IF
        ELSE
           PERFORM 5100-ADD-OWNER
        END-IF
     END-IF.

 5000-DO-OWNER-ONLY-TRAN.
********************************************************************************
* Routine that attempts to add the 2nd and subsequent owners of a vehicle to the
* database.  The steps are as follows:
* 1. Perform a routine to verify that the owner, represented by the
*    user-supplied PERSON-ID, exists on the database.
* 2. If owner exists perform a routine to add the owner to the database.
********************************************************************************
     PERFORM 6000-VERIFY-PERSON
     IF SUCCESS OF PERSON-FILE-STAT
        PERFORM 5100-ADD-OWNER
     END-IF.

 5100-ADD-OWNER.
********************************************************************************
* Add owner routine:  Move owner data from request message to OWNER file buffer.
* Attempt to Write OWNER file record.  If not successful move an appropriate
*  text message to the reply message.
********************************************************************************
     MOVE MSMV004-LICENSE-PLATE
       TO LICENSE-PLATE OF OWNER-RECORD
     MOVE MSMV004-PERSON-ID TO PERSON-ID OF OWNER-RECORD
     WRITE OWNER-RECORD
     IF NOT SUCCESS OF OWNER-FILE-STAT
        MOVE 1 TO MSMV004-REPLY-CODE
        IF DUPLICATE-KEY OF OWNER-FILE-STAT
           MOVE WS-PERSON-EXISTS-MSG TO MSMV004-MESSAGE
        ELSE
           MOVE FILE-ERROR-BUFFER TO MSMV004-MESSAGE
        END-IF
     END-IF.

 6000-VERIFY-PERSON.
*******************************************************************************
* Verify owner routine:  READ PERSON file by primary key of user-supplied
* PERSON-ID.  If successful, move PERSON file name and address to the reply
* message; otherwise, move an appropriate text message to the reply message.
*******************************************************************************
     MOVE MSMV004-PERSON-ID TO PERSON-ID OF PERSON-RECORD
     READ PERSON-FILE
        TIME LIMIT WS-WAIT-TIME
     END-READ
     IF SUCCESS OF PERSON-FILE-STAT
        MOVE FIRST-NAME OF PERSON-RECORD TO MSMV004-FIRST-NAME
        MOVE MIDDLE-NAME OF PERSON-RECORD TO MSMV004-MIDDLE-NAME
        MOVE LAST-NAME OF PERSON-RECORD TO MSMV004-LAST-NAME
        MOVE SUFFIX OF PERSON-RECORD TO MSMV004-SUFFIX
        MOVE STREET OF PERSON-RECORD TO MSMV004-STREET
        MOVE CITY OF PERSON-RECORD TO MSMV004-CITY
        MOVE STATE OF PERSON-RECORD TO MSMV004-STATE
        MOVE ZIP OF PERSON-RECORD TO MSMV004-ZIP
     ELSE
        MOVE 1 TO MSMV004-REPLY-CODE
        IF NO-EXISTING-RECORD OF PERSON-FILE-STAT
           MOVE WS-NO-PERSON-EXISTS-MSG TO MSMV004-MESSAGE
        ELSE
           MOVE FILE-ERROR-BUFFER TO MSMV004-MESSAGE
        END-IF
     END-IF.
