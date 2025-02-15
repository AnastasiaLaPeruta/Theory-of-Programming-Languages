       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEWCIPHER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       * Encryption/Decryption section
       01 ENC-IN        PIC X(25) VALUE "OPERATION DAYBREAK".
       01 ENC-OUT       PIC X(25) VALUE SPACES.
       01 DEC-OUT       PIC X(25) VALUE SPACES.
       01 ENC-SHIFT     PIC 99    VALUE 2.
       01 IDX           PIC 99    VALUE 1.
       01 CUR-CHR       PIC X     VALUE SPACE.
       01 BASE          PIC 99    VALUE 0.
       01 NEWCODE       PIC 99    VALUE 0.
       01 LEN-ENC       PIC 99    VALUE 0.
       
       * Solve section (using predetermined text "HAL")
       01 SOL-IN        PIC X(3)  VALUE "HAL".
       01 SOL-OUT       PIC X(3)  VALUE SPACES.
       01 SOL-SHIFT     PIC 99    VALUE 0.
       01 LEN-SOL       PIC 99    VALUE 0.
       01 MAX-SHIFT     PIC 99    VALUE 26.
       
       PROCEDURE DIVISION.
       MAIN.
           * ----- ENCRYPTION -----
           DISPLAY "Encrypt:".
           COMPUTE LEN-ENC = FUNCTION LENGTH(ENC-IN).
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEN-ENC
               MOVE ENC-IN(IDX:1) TO CUR-CHR
               IF CUR-CHR >= "A" AND CUR-CHR <= "Z"
                   COMPUTE BASE = FUNCTION ORD(CUR-CHR) - FUNCTION ORD("A")
                   COMPUTE NEWCODE = (BASE + ENC-SHIFT) MOD 26
                   COMPUTE NEWCODE = NEWCODE + FUNCTION ORD("A")
                   MOVE FUNCTION CHAR(NEWCODE) TO ENC-OUT(IDX:1)
               ELSE
                   MOVE CUR-CHR TO ENC-OUT(IDX:1)
               END-IF
           END-PERFORM.
           DISPLAY ENC-OUT.
           
           * ----- DECRYPTION -----
           DISPLAY "Decrypt:".
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEN-ENC
               MOVE ENC-OUT(IDX:1) TO CUR-CHR
               IF CUR-CHR >= "A" AND CUR-CHR <= "Z"
                   COMPUTE BASE = FUNCTION ORD(CUR-CHR) - FUNCTION ORD("A")
                   COMPUTE NEWCODE = (BASE - ENC-SHIFT + 26) MOD 26
                   COMPUTE NEWCODE = NEWCODE + FUNCTION ORD("A")
                   MOVE FUNCTION CHAR(NEWCODE) TO DEC-OUT(IDX:1)
               ELSE
                   MOVE CUR-CHR TO DEC-OUT(IDX:1)
               END-IF
           END-PERFORM.
           DISPLAY DEC-OUT.
           
           * ----- SOLVE (BRUTE FORCE) -----
           DISPLAY "Solve:".
           COMPUTE LEN-SOL = FUNCTION LENGTH(SOL-IN).
           PERFORM VARYING SOL-SHIFT FROM MAX-SHIFT BY -1 UNTIL SOL-SHIFT < 0
               MOVE SPACES TO SOL-OUT
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEN-SOL
                   MOVE SOL-IN(IDX:1) TO CUR-CHR
                   IF CUR-CHR >= "A" AND CUR-CHR <= "Z"
                       COMPUTE BASE = FUNCTION ORD(CUR-CHR) - FUNCTION ORD("A")
                       COMPUTE NEWCODE = (BASE + SOL-SHIFT) MOD 26
                       COMPUTE NEWCODE = NEWCODE + FUNCTION ORD("A")
                       MOVE FUNCTION CHAR(NEWCODE) TO SOL-OUT(IDX:1)
                   ELSE
                       MOVE CUR-CHR TO SOL-OUT(IDX:1)
                   END-IF
               END-PERFORM.
               DISPLAY "Caesar " SOL-SHIFT ": " SOL-OUT
           END-PERFORM.
           STOP RUN.
