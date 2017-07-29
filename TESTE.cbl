       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT ARQ-C ASSIGN TO "DADOS.DAT"
              ORGANIZATION IS SEQUENTIAL
              ACCESS IS SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD ARQ-C
           LABEL RECORD IS STANDARD.
           01 CONTA-FILE.
              05 COD-AGN PIC S9(3) VALUES 0.
              05 COD-CON PIC 9(3) VALUES 0.
              05 NOME-CLI PIC A(15) VALUES " ".
              05 SALDO-CON PIC 9(5) VALUES 0.
           WORKING-STORAGE SECTION.
           01 WS-ARQ-C.
           05 WS-GRAVA PIC X.
           05 WS-OUTRO PIC X.
           05 OP-MENU PIC 9(1) VALUES 0.
           05 OP-CONTINUA PIC X(1) VALUES " ".
           01 WS-COD-AGN PIC 9(3).
           01 WS-COD-CON PIC 9(3).
           01 WS-NOME-CLI PIC A(15).
           01 WS-SALDO-CON PIC 9(5).
           01 WS-OP-MENU PIC 9(1).
           01 WS-EOF PIC A(1).

       PROCEDURE DIVISION.
       001-INICIO.
       002-TELA00.
       DISPLAY "======================================================="
       DISPLAY "=                  CONTA CORRENTE                     ="
       DISPLAY "======================================================="
       DISPLAY " ".
       DISPLAY "MENU".
       DISPLAY "1-INCLUIR".
       DISPLAY "2-EXCLUIR".
       DISPLAY "3-EDITAR".
       DISPLAY "4-CONSULTAR".
       DISPLAY "DIGITE SUA OPCAO".
       ACCEPT OP-MENU.

       IF OP-MENU=1 THEN
           PERFORM 005-ADICIONA
           STOP RUN
       ELSE IF OP-MENU = 2 THEN
          PERFORM 005-EXCLUI
           STOP RUN
       ELSE IF OP-MENU = 3 THEN
           PERFORM 005-EDITA
           STOP RUN
       ELSE IF OP-MENU = 4 THEN
           PERFORM 005-CONSULTA
           STOP RUN
       ELSE
           DISPLAY "OPCAO INVALIDA."
           PERFORM 002-TELA00
           STOP RUN
       END-IF.



       005-ADICIONA.
           DISPLAY "==================================================="
           DISPLAY "=              CADASTRO DE CLIENTES               ="
           DISPLAY "==================================================="
           DISPLAY "CODIGO DA AGENCIA:".
           ACCEPT COD-AGN.
           DISPLAY "CODIGO DA CONTA:".
           ACCEPT COD-CON.
           DISPLAY "NOME DO CLIENTE:".
           ACCEPT NOME-CLI.
           DISPLAY "SALDO DA CONTA:".
           ACCEPT SALDO-CON.

           OPEN EXTEND ARQ-C.
               MOVE COD-AGN TO WS-COD-AGN.
               MOVE COD-CON TO WS-COD-CON.
               MOVE NOME-CLI TO WS-NOME-CLI.
               MOVE SALDO-CON TO WS-SALDO-CON.
               WRITE CONTA-FILE
               END-WRITE.
           CLOSE ARQ-C.
           DISPLAY "DIGITE (S) SE DESEJA CONTINUAR.".
           ACCEPT OP-CONTINUA.
           IF OP-CONTINUA="S" THEN
               PERFORM 005-ADICIONA
           END-IF.
       005-EDITA.

           OPEN I-O ARQ-C.
           MOVE '584' TO COD-CON.

           READ ARQ-C
              KEY IS COD-CON
             INVALID KEY DISPLAY 'KEY IS NOT EXISTING'
           END-READ.

           MOVE 'Tim Dumais' TO NOME-CLI.
           REWRITE CONTA-FILE
           END-REWRITE.
           STOP " ".
           CLOSE ARQ-C.

       005-EXCLUI.


           OPEN I-O ARQ-C.
           MOVE 'Tim Dumais' TO NOME-CLI.

           DELETE ARQ-C RECORD
              INVALID KEY DISPLAY 'Invalid Key'
              NOT INVALID KEY DISPLAY 'Record Deleted'
           END-DELETE.
           STOP " ".
           CLOSE ARQ-C.

       005-CONSULTA.
               OPEN INPUT ARQ-C.
               PERFORM UNTIL WS-EOF='Y'
               READ ARQ-C INTO WS-ARQ-C
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-COD-AGN, " ", WS-COD-CON, " ",
               WS-NOME-CLI, " ", WS-SALDO-CON
               END-READ
               END-PERFORM.
               STOP " ".
               CLOSE ARQ-C.
       005-FINAL.
           STOP RUN.
