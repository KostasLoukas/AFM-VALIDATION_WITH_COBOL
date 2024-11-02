       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFM-VALIDATION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  AFM-IN                       PIC X(09).
       01  R-AFM REDEFINES AFM-IN.
           05 AFM-DIGITS OCCURS 9 TIMES PIC 9.
       01  IDX                          PIC 99.
       01  POWER-OF-TWO                 PIC 9     VALUE 8.
       01  POWER-RESULT                 PIC 9(20) VALUE 0.
       01  D-SUM                        PIC 9(20) VALUE 0.
       01  YPOL                         PIC 99    VALUE 0.
       01  COUNT-NUMBERS                PIC 99    VALUE 0.
       01  COUNT-CHARS                  PIC 99    VALUE 0.

       PROCEDURE DIVISION.

       MAIN.
           DISPLAY "PLEASE INSERT AFM: "
           ACCEPT AFM-IN

      *    If input contains letter (or empty character), throw error
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 9
               IF AFM-DIGITS(IDX) IS NUMERIC
                   ADD 1 TO COUNT-NUMBERS
               ELSE
                   ADD 1 TO COUNT-CHARS
           END-PERFORM
           IF COUNT-NUMBERS < 9
               DISPLAY 'ERROR! AFM MUST BE EXACTLY 9 NUMBERS!'
               STOP RUN
           END-IF
      *----We compute all digits except the ninth...
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8
               COMPUTE POWER-RESULT = (FUNCTION NUMVAL(AFM-DIGITS(IDX)))
               *(2**POWER-OF-TWO)
               ADD POWER-RESULT     TO D-SUM
               COMPUTE POWER-OF-TWO = (POWER-OF-TWO - 1)
           END-PERFORM
           COMPUTE YPOL             = FUNCTION MOD(D-SUM, 11)
           IF YPOL                  = 10
               MOVE 0 TO YPOL
           END-IF
           IF YPOL                  = AFM-DIGITS(9)
               DISPLAY "YOUR AFM IS VALID!"
           ELSE
               DISPLAY "AFM INVALID. PLEASE TRY AGAIN."
           END-IF.

       STOP-RUN.
           STOP RUN.

       END PROGRAM AFM-VALIDATION.
