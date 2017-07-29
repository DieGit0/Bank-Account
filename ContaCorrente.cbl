      ******************************************************************
      * Author: Grupo os BATUTINHAS
      * Date: 10/06/2016
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CONTA-CORRENTE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MEU-PC.
       OBJECT-COMPUTER. MEU_PC.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQ ASSIGN TO DISK
       ORGANIZATION LINE SEQUENTIAL
       ACCESS MODE       SEQUENTIAL
      *RECORD KEY    CD-AG
       FILE STATUS   ARQ-OK.
      *SORT STATUS IS NOME.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ LABEL RECORD STANDARD
         DATA RECORD CTA-CORRENTE
         VALUE OF FILE-ID IS "CCORRENTE.DAT".
       01  CTA-CORRENTE.
           02 CD-AG     PIC 9(10).
           02 CD-CC     PIC 9(10).
           02 SALDO-CC  PIC S9(10)V99.
       01 NOME          PIC X(50).
       01 ENDERECO.
           02 Logradouro  PIC X(50).
           02 Numero      PIC 9(10).
           02 Cep         PIC X(100).
           02 Complemento PIC X(100).
       WORKING-STORAGE SECTION.
       01 CONTADOR  PIC 9(01).
       01 DADOS.
           05 COD-AGENC PIC X(10).
           05 COD-CTA   PIC X(10).
           05 NOME      PIC X(100).
           05 SALDO     PIC 9(10)V99.
           01 OPCAO     PIC X(1)  VALUE ZEROES.
           01 OPCAO2    PIC 9(02) VALUE 0.
           77 ARQ-OK    PIC X(02) VALUE ZEROES.

      *VALUE

      *01 CONTADOR        PIC 9(01) VALUE ZEROS.
      *    05 COD-AGENC  PIC X(10) VALUE ZEROS.
      *     05 COD-CTA    PIC X(10) VALUE ZEROS.
      *     05 NOME       PIC X(100) VALUE SPACES.
      *     05 SALDO      PIC S9(10)V99 VALUES ZEROS.
      *     01 OPCAO      PIC X(03) VALUE "NÃO".
      *     01 OPCAO      PIC X(03) VALUE SPACES.
      *     077 ARQ-OK  PIC 9(02) VALUE ZEROES.
           77 MSG-TELA PIC X(30) VALUE "PROGRAMA CONTA CORRENTE".

       SCREEN SECTION.
      *01  TELA.
      *   02 BLANK SCREEN.
      *   02 MSG-TELA.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       INICIO.
           PERFORM ABRE-ARQ
           DISPLAY "PROGRAMA CONTA CORRENTE"
           DISPLAY "======================="
      *    PERFORM ESCOLHA UNTIL OPCAO2 = "0"

      *    STOP RUN.
      *ESCOLHA.
           DISPLAY "INSERIR(1)"
           DISPLAY "ALTERAR(2)"
           DISPLAY "EXCLUIR(3)"
           DISPLAY "LISTAR (4)"
           DISPLAY "SAIR   (0)"
           DISPLAY "OPÇÃO: "

           ACCEPT OPCAO2 WITH PROMPT AUTO

      *    EVALUATE OPCAO2
      *    WHEN "1"
      *     PERFORM INSERE UNTIL 1=1
      *    WHEN "2"
      *     PERFORM ALTERA UNTIL 1=1
      *    WHEN "3"
     **     PERFORM LISTA UNTIL 1=1
      *    WHEN "4"
      *     PERFORM DEL UNTIL 1=1
      *    WHEN "0"
      *     EXIT
      *    WHEN OTHER
      *     DISPLAY "OPÇAO INVÁLIDA"
      *    END-EVALUATE.
           STOP RUN.
       INSERE.
           DISPLAY "COD AGÊNCIA: ". ACCEPT CD-AG.
           DISPLAY "COD CONTA: "  . ACCEPT CD-AG.
           DISPLAY "SALDO:".        ACCEPT CD-AG.
           STOP RUN.
       ALTERA.
           DISPLAY "COD AGÊNCIA: ". ACCEPT CD-AG.
           DISPLAY "COD CONTA: "  . ACCEPT CD-AG.
           DISPLAY "SALDO:".        ACCEPT CD-AG.
           STOP RUN.
       DEL.
           DISPLAY "COD AGÊNCIA: ". ACCEPT CD-AG.
           DISPLAY "COD CONTA: "  . ACCEPT CD-AG.
           DISPLAY "SALDO:".        ACCEPT CD-AG.
           STOP RUN.
       LISTA.
           DISPLAY "MOSTRA"
           STOP RUN.

       PROCESSO.
           DISPLAY "PROCESSO".

       ABRE-ARQ.
          OPEN I-O ARQ
           IF ARQ-OK NOT = "0" THEN
               CLOSE ARQ
             DISPLAY "STATUS ERRO : ",ARQ-OK
          ELSE
             DISPLAY "STATUS: ",ARQ-OK
              PERFORM INICIO
          END-IF.

       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
      *DISPLAY ERASE AT 0101.
      *DISPLAY INICIO AT 0101.
      *CALL "C$SLEEP" USING 2
      *     DISPLAY "Hello world"
      *ACCEPT SALDO.

            STOP RUN.
      ** add other procedures here
       END PROGRAM CONTA-CORRENTE.
