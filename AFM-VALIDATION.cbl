       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFM-VALIDATION-SUBRTN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  AFM-IN                           PIC X(9).
       01  AFM-ARRAY REDEFINES AFM-IN.
           05 AFM-DIGIT OCCURS 9 TIMES      PIC 9.
       01  WS-VARIABLES.
           05  IDX                          PIC 99   VALUE 0.
           05  SUM-DIGITS                   PIC 9(5) VALUE 0.
           05  MULTIPLIER                   PIC 9    VALUE 8.
           05  YPOL                         PIC 99.
           05  COUNT-NUMBERS                PIC 99   VALUE 0.
           05  COUNT-CHARS                  PIC 99   VALUE 0.
           05  COUNT-SPACES                 PIC 99   VALUE 0.
           05  ERROR-SW                     PIC  9   VALUE 0.


       PROCEDURE DIVISION.
           PERFORM VALIDATE-AFM-RTN.
           PERFORM CALCULATE-AFM-RTN.
           GOBACK.
      ***------------------------------------------------------------***
      ***   PERFORMS VALIDATION OF THE INPUT-TED AFM AND ISSUES ERROR***
      ***   SPECIFIC MESSAGES                                        ***
      ***------------------------------------------------------------***
       VALIDATE-AFM-RTN.
           INITIALIZE WS-VARIABLES.

           DISPLAY 'Please provide a 9-digit TIN: '
           ACCEPT AFM-IN.
           INSPECT AFM-IN REPLACING ALL X'0D' BY SPACES
           INSPECT AFM-IN REPLACING ALL X'0A' BY SPACES
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 9
               EVALUATE TRUE
                 WHEN  AFM-DIGIT(IDX) IS NUMERIC
                   ADD 1 TO COUNT-NUMBERS
                 WHEN  AFM-DIGIT(IDX) = SPACES
                   ADD 1 TO COUNT-SPACES
                 WHEN OTHER
                   ADD 1 TO COUNT-CHARS
               END-EVALUATE
           END-PERFORM.
           EVALUATE TRUE
             WHEN COUNT-NUMBERS = 9 AND AFM-IN =  ALL '0'
                  DISPLAY  '***ZERO AFM IS NOT SUPPORTED!!!'
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0  AND
                  COUNT-SPACES > 0
                  DISPLAY  '***NO CHARS AND SPACES ALLOWED!!!'
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-SPACES > 0
                  DISPLAY '***NO SPACES ALLOWED!!!'
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0
                  DISPLAY  '***NO CHARS ALLOWED!!!'
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS = 9
                  CONTINUE
           END-EVALUATE.
           IF ERROR-SW = 1
              DISPLAY 'TIN INVALID!'
              STOP RUN
           END-IF.

      ***------------------------------------------------------------***
      ***  PERFORMS CALCULATIONS (AFM DIGITS IN THE POWER OF TWO AND ***
      ***  APPLIES MODULO 11. IF REMAINDER EQUALS TO THE 9-TH AFM DI-***
      ***  GIT, AFM IS VALID OTHERWISE IS INVALID                    ***
      ***------------------------------------------------------------***
       CALCULATE-AFM-RTN.
           MOVE 0 TO SUM-DIGITS
           MOVE 8 TO MULTIPLIER
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8
               COMPUTE SUM-DIGITS =
               SUM-DIGITS + AFM-DIGIT(IDX) * (2**(MULTIPLIER))
               SUBTRACT 1 FROM MULTIPLIER
           END-PERFORM.
           COMPUTE YPOL = FUNCTION MOD(SUM-DIGITS,11)
           IF YPOL = 10
               MOVE 0 TO YPOL
           END-IF
           IF YPOL = AFM-DIGIT(9)
              DISPLAY 'TIN VALID.'
           ELSE
              DISPLAY '***AFM CALCULATIONS LEAD TO  CHECK-DIGIT-ERROR!'
              DISPLAY 'TIN INVALID!'
           END-IF.
       END PROGRAM AFM-VALIDATION-SUBRTN.
