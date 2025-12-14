* ============================================================================
* Filename    : B30DIRb1.cob
* Author      : sahuanu
* Created on  : Jan 17, 2020
* ============================================================================
?SECTION Source1
   MOVE "SOURCE 1"    TO PAR-NAME.
?SECTION Source2
   MOVE "SOURCE 2"    TO PAR-NAME.
?SECTION Source3
   MOVE "SOURCE 3"    TO PAR-NAME.
?SECTION Source-Data1
   01 TEST-RESULTS-Source1.
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 FEATURE                       PICTURE X(20).
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 P-OR-F                        PICTURE X(5).
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 PAR-NAME.
         10 FILLER                     PICTURE X(20).
      05 FILLER                        PICTURE X(3) VALUE SPACE.
      05 COMPUTED-A                    PICTURE X(12)   JUSTIFIED RIGHT.
      05 FILLER                        PICTURE X(8) VALUE SPACES.
      05 CORRECT-A                     PICTURE X(12)   JUSTIFIED RIGHT.
      05 FILLER                        PICTURE X(8) VALUE SPACES.
      05 RE-MARK                       PICTURE X(29)   VALUE SPACES.
?SECTION Source-Data2
   01 TEST-RESULTS-Source2.
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 FEATURE                       PICTURE X(20).
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 P-OR-F                        PICTURE X(5).
      05 FILLER                        PICTURE X   VALUE SPACE.
      05 PAR-NAME.
         10 FILLER                     PICTURE X(20).
      05 FILLER                        PICTURE X(3) VALUE SPACE.
      05 COMPUTED-A                    PICTURE X(12)   JUSTIFIED RIGHT.
      05 FILLER                        PICTURE X(8) VALUE SPACES.
      05 CORRECT-A                     PICTURE X(12)   JUSTIFIED RIGHT.
      05 FILLER                        PICTURE X(8) VALUE SPACES.
      05 RE-MARK                       PICTURE X(29)   VALUE SPACES.
?SECTION End-Section