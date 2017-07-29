
       IDENTIFICATION DIVISION.
           program-id. CONTACORRENTE.

       ENVIRONMENT DIVISION.
           configuration section.
               special-names.
               decimal-point is comma.
           input-output section.
               file-control.
              select F-CADASTRO assign to disk
               organization is indexed
               access is dynamic
               record key is cod-conta
               alternate record key nome
                   with duplicates
                   FILE STATUS   ARQ-OK.

       DATA DIVISION.
       file section.
           FD F-CADASTRO LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "F-DATA.txt".
           01 DADOS-CADASTRO.
               02 cod-conta   PIC 9(5).
               02 cod-agencia PIC 9(5).
               02 nome        PIC AAAAAAAAAA value spaces.
      *         02 saldo       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
               02 saldo       PIC $---.---.---.--9,99.

       working-storage section.
       01 busca-nome    PIC AAAAAAAAAA value spaces.
       01 opcao    pic x(1) value spaces.
       01 continua pic x(1) value spaces.
       01 fim      pic x.
       01 prosseguir pic x.
       01 rodar    pic x.
       01 WS-DADOS-CADASTRO.
          02 ws-cod-conta   PIC 9(5).
          02 ws-cod-agencia PIC 9(5).
          02 ws-nome        PIC AAAAAAAAAA value spaces.
      *    02 ws-saldo       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
          02 ws-saldo       PIC $---.---.---.--9,99.
          02 ARQ-OK    PIC X(02) VALUE ZEROES.
      *   02 QQRMERDA  PIC

       screen section.
       01 TELA-INFO.
        03 BLANK SCREEN.
        03 LINE 13 COL 01 VALUE "Codigo da conta: ".
        03 LINE 14 COL 01 VALUE "Codigo da agencia: ".
        03 LINE 15 COL 01 VALUE "Dono da conta: ".
        03 LINE 16 COL 01 VALUE "Saldo na conta: ".

       01 TELA-MENU.
        03 BLANK SCREEN BACKGROUND-COLOR 3.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "               MENU                "
         HIGHLIGHT.
        03 line 03 col 01 value "***********************************".
        03 line 04 col 01 value "ESCOLHA UMA OPCAO ABAIXO:          ".
        03 line 05 col 01 value "(1) INCLUIR NOVO CADASTRO          ".
        03 line 06 col 01 value "(2) EXCLUIR CADASTRO               ".
        03 line 07 col 01 value "(3) ALTERAR CADASTRO               ".
        03 line 08 col 01 value "(4) CONSULTAR  CADASTRO            ".
        03 line 09 col 01 value "(5) LISTAR TODOS OS CADASTROS      ".
        03 line 11 col 01 value "(S) SAIR                           ".
        03 line 12 col 01 value "OPCAO:                             ".

       01 TELA-INCLUSAO.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "     INCLUSAO DE NOVO CADASTRO".
        03 line 03 col 01 value "***********************************".
        03 line 04 col 01 value "Conta Corrente: ".
        03 line 05 col 01 value "Codigo da agencia: ".
        03 line 06 col 01 value "Nome: ".
        03 line 07 col 01 value "Saldo:          R$".

       01 TELA-CONSULTA.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "              CONSULTA".
        03 line 03 col 01 value "***********************************".
        03 line 05 col 01 value "Consultar pelo codigo da conta ou".
        03 line 06 col 01 value "pelo nome do cliente?".
        03 line 08 col 01 value "     (1)    CODIGO DA CONTA".
        03 line 09 col 01 value "     (2)    NOME DO CLIENTE".
        03 line 10 col 01 value "     (S)    SAIR E VOLTAR AO MENU".
        03 line 11 col 01 value "     OPCAO: ".

       01 TELA-CONSULTA-CONTA.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "         CONSULTA POR CONTA".
        03 line 03 col 01 value "***********************************".
        03 line 05 col 01 value "Digite o codigo da conta: ".

       01 TELA-CONSULTA-NOME.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "         CONSULTA POR NOME".
        03 line 03 col 01 value "***********************************".
        03 line 05 col 01 value "Digite um nome de dono da conta: ".

       01 TELA-INFO2.
        03 BLANK SCREEN.
        03 LINE 07 COL 01 VALUE "Codigo da conta: ".
        03 LINE 08 COL 01 VALUE "Codigo da agencia: ".
        03 LINE 09 COL 01 VALUE "Dono da conta: ".
        03 LINE 10 COL 01 VALUE "Saldo na conta: ".

       01 TELA-EXCLUSAO.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "         TELA DE EXCLUSAO".
        03 line 03 col 01 value "***********************************".
        03 line 04 col 01 value "Digite o codigo da conta:".

       01 TELA-LISTAR.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "         LISTA DE CADASTROS".
        03 line 03 col 01 value "***********************************".

       01 TELA-ALTERACAO.
        03 BLANK SCREEN.
        03 line 01 col 01 value "***********************************".
        03 line 02 col 01 value "         TELA DE ALTERACAO".
        03 line 03 col 01 value "***********************************".
        03 line 04 col 01 value "     DIGITE O CODIGO DA CONTA      ".
        03 line 05 col 01 value "     A SER ALTERADA:".

       01 TELA-INFO-ALTERACAO.
        03 BLANK SCREEN.
        03 LINE 07 COL 01 VALUE
             "======================================================".
        03 LINE 08 COL 01 VALUE "  Codigo da conta:".
        03 LINE 09 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 10 COL 01 VALUE "Codigo da agencia:".
        03 LINE 11 COL 01 VALUE "       Novo valor:".
        03 LINE 12 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 13 COL 01 VALUE "    Dono da conta:".
        03 LINE 14 COL 01 VALUE "       Novo valor:".
        03 LINE 15 COL 01 VALUE
             "------------------------------------------------------".
        03 LINE 16 COL 01 VALUE "   Saldo na conta:".
        03 LINE 17 COL 01 VALUE "       Novo valor:".
        03 LINE 18 COL 01 VALUE
             "======================================================".


*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
       PROCEDURE DIVISION.
       PRINCIPAL.
       move "f" to fim.
       perform MENU-PRINCIPAL until fim = "v".
       stop run.

      ****************************** MENU PRINCIPAL *****************************
       MENU-PRINCIPAL.
       initialize opcao.
      *display erase at 0101.
       display TELA-MENU.
       perform OPCAO-MENU.

       OPCAO-MENU.
       initialize rodar.
       initialize opcao.
       accept opcao at 1208.
        evaluate opcao
         when "1" perform INCLUSAO
         when "2"  perform EXCLUSAO until rodar = "v"
         when "3"  perform ALTERACAO until rodar = "v"
         when "4"  perform CONSULTA
         when "5"  perform LISTAR
         when "S"  perform sair
         when "s"  perform sair
         when other display " Opção Inválida"
        end-evaluate.

      ******************************* INCLUSAO ************************************
       INCLUSAO.
       initialize opcao.
       initialize dados-cadastro.
       initialize ws-dados-cadastro.
       move "f" to prosseguir.
      *display erase at 0101.
       display TELA-INCLUSAO.
       OPEN output F-CADASTRO.
       IF ARQ-OK NOT = "00" THEN
           DISPLAY "Erro de Arquivo. Erro:", ARQ-OK AT 3001
           CLOSE F-CADASTRO
       ELSE
           DISPLAY "ARQUIVO OK", ARQ-OK AT 3001
       END-IF
       OPEN EXTEND F-CADASTRO.
        perform ENTRADA-CODIGO  UNTIL prosseguir = "v".
        perform ENTRADA-AGENCIA UNTIL ws-cod-agencia NOT EQUAL zeroes.
        perform ENTRADA-NOME    UNTIL ws-nome NOT EQUAL spaces.
        perform ENTRADA-SALDO.
        move "f" to prosseguir.
        perform PERGUNTA-SALVAR until prosseguir = "v".
       close F-CADASTRO.

       ENTRADA-CODIGO.
       move "v" to prosseguir.
       initialize cod-conta.
       accept cod-conta at 0420.
       if cod-conta = zeroes
       then
         display "Cadastro deve ser diferente de zero!" at 0430
         move "f" to prosseguir
       else
         display "                                    " at 0430
         READ F-CADASTRO
           NOT INVALID KEY
             display "Ja cadastrado" at 0430
           move cod-conta to ws-cod-conta

           DISPLAY "COD-CONTA    ", cod-conta
           DISPLAY "WS-COD-CONTA ", ws-cod-conta

            move "f" to prosseguir
         END-READ
       end-if.

       ENTRADA-AGENCIA.
       initialize ws-cod-agencia.
       accept ws-cod-agencia at 0520.
       if ws-cod-agencia = zeroes
       then
         display "Agencia deve ser diferente de zero!" at 0530
       else
         display "                                   " at 0530
         move ws-cod-agencia to cod-agencia
       end-if.

       ENTRADA-NOME.
       initialize ws-nome.
       accept ws-nome at 0620.
       if ws-nome = spaces
       then
         display "Nome esta em branco!" at 0730
       else
         display "                    " at 0730
         move ws-nome to nome
       end-if.

       ENTRADA-SALDO.
       initialize ws-saldo.
       accept ws-saldo at 0720.
       move ws-saldo to saldo.

       PERGUNTA-SALVAR.
       initialize opcao.
       display "Salvar os dados? (S\N):  " at 0910.
       accept opcao at 0935.
       if opcao = "s" or "S"
       then
          perform ESCREVER-DADOS
      *     DISPLAY "Cadastrado com Sucesso"
           move "v" to prosseguir
       else
           if opcao equals "n" or "N"
           then
               display " Dados nao foram salvos" at 0937
               move "v" to prosseguir
           else
               display " Opcao invalida!       " at 0937
               move "f" to prosseguir
           end-if
       end-if.

       ESCREVER-DADOS.
       write DADOS-CADASTRO
       invalid key
           display "!!??" at 1215
               not invalid key
               display "Registrado com sucesso! " at 1210
      *        PERFORM MOSTRAR-DADOS-SALVOS
               accept continua
       end-write.

       MOSTRAR-DADOS.
       read F-CADASTRO record into WS-DADOS-CADASTRO
           key is cod-conta
       end-read.
       display TELA-INFO.
       display ws-cod-conta at 1320.
       display ws-cod-agencia at 1420.
       display ws-nome at 1520.
       display ws-saldo at 1620.

       MOSTRAR-DADOS-SALVOS.
       read F-CADASTRO record into WS-DADOS-CADASTRO
           key is cod-conta
       end-read.
       display spaces.
       display spaces.
       display "======================================================".
       display "Codigo da conta:       " ws-cod-conta.
       display "Codigo da agencia:     " ws-cod-agencia.
       display "Nome do dono da conta: " ws-nome.
       display "Saldo da conta:        " ws-saldo.
       display "======================================================".
       initialize WS-DADOS-CADASTRO.
       display spaces.
       display "Registrado com sucesso! ".
       display "Enter para continuar.".

      *******************************************************************

       RODAR-ALTERACAO.
       move "f" to rodar.
       perform ALTERACAO.


       ALTERACAO.
       move "f" to rodar.
       initialize opcao.
       move "f" to prosseguir.
      *display erase at 0101.
       display TELA-ALTERACAO.
       open i-o F-CADASTRO.
           perform until prosseguir = "v"
               initialize cod-conta
               initialize WS-DADOS-CADASTRO
               accept cod-conta at 0522
               read F-CADASTRO into WS-DADOS-CADASTRO
                 key is cod-conta
                 invalid key
                  display "Codigo invalido!" at 0530
                  perform PERGUNTA-SAIR-ALTERACAO until prosseguir = "v"
                  if rodar = "f" then
                       move "v" to prosseguir
                  else
                       move "f" to prosseguir
                 not invalid key
                   display "                " at 0530
                   perform ALTERAR-CONTA until prosseguir = "v"
                   move "v" to prosseguir
               end-read
            end-perform.
       close F-CADASTRO.
       move "v" to rodar.


       PERGUNTA-SAIR-ALTERACAO.
       move "f" to  prosseguir.
       display         "Deseja voltar ao menu? (S/N):" at 0601.
       initialize opcao.
       accept opcao at 0631.
       if opcao = "S" or "s" then
           move "v" to prosseguir
           move "f" to rodar
       else
           if opcao = "N" or "n" then
               move "v" to prosseguir
               move "v" to rodar
               display "Entre com uma conta valida!       " at 0601
           else
               display "Opcao invalida"
               move "f" to prosseguir
               move "v" to rodar
           end-if
       end-if.

       ALTERAR-CONTA.
       display spaces.
       display TELA-INFO-ALTERACAO.
       display ws-cod-conta at 0820.
       display ws-cod-agencia at 1020.
       move "f" to prosseguir.
       perform until prosseguir = "v"
      *    move "f" to prosseguir
           accept ws-cod-agencia at 1120
           if ws-cod-agencia equals zeroes then
               display "Codigo deve ser diferente de zero!" at 1127
               move "f" to prosseguir
           else
               display "                                  " at 1127
               move "v" to prosseguir
           end-if
       end-perform.
       display ws-nome at 1320.
       move "f" to prosseguir.
       perform until prosseguir = "v"
           move "f" to prosseguir
           accept ws-nome at 1420
           if ws-nome equals spaces then
               display "O nome esta em branco!" at 1435
               move "f" to prosseguir
           else
               display "                      " at 1435
               move "v" to prosseguir
           end-if
       end-perform.
       display ws-saldo at 1620.
       accept ws-saldo at 1720.
       move "f" to prosseguir.
       perform PERTGUNTA-ALTERAR until prosseguir = "v".
       display spaces.
       display "ENTER para continuar."
       accept continua.
       move "v" to prosseguir.

       PERTGUNTA-ALTERAR.
       move "f" to prosseguir.
       display "Tem certeza que quer alterar esta conta (S/N)?" at 1901.
       initialize opcao.
       accept opcao at 1947.
       if opcao = "s" or "S" then
           MOVE WS-DADOS-CADASTRO TO DADOS-CADASTRO
           REWRITE DADOS-CADASTRO
           END-REWRITE
           display "Os dados foram salvos!    " at 2001
           move "v" to prosseguir
       else
           if opcao = "n" or "N" then
               display "Os dados nao foram salvos." at 2001
               move "v" to prosseguir
           else
               display "Comando invalido!!!       " at 2001
               move "f" to prosseguir
           end-if
       end-if.

      *******************************************************************


       EXCLUSAO.
       move "f" to rodar.
       initialize opcao.
       move "f" to prosseguir.
      *display erase at 0101.
       display TELA-EXCLUSAO.
       open I-O F-CADASTRO.
           perform until prosseguir = "v"
               initialize cod-conta
               initialize WS-DADOS-CADASTRO
               accept cod-conta at 0427
               read F-CADASTRO into WS-DADOS-CADASTRO
                 key is cod-conta
                 invalid key
                   display "Conta inexistente!" at 0435
                   perform PERGUNTA-SAIR-EXCLUSAO until prosseguir = "v"
                   if rodar = "f" then
                       move "v" to prosseguir
                   else
                       move "f" to prosseguir
                 not invalid key
                   display "                  " at 0435
                   perform PERGUNTA-EXCLUIR until prosseguir = "v"
                   move "v" to prosseguir
               end-read
            end-perform.
       close F-CADASTRO.
       move "v" to rodar.
       display " ENTER para sair.".
       accept continua.

       PERGUNTA-SAIR-EXCLUSAO.
       move "f" to  prosseguir.
       display         "Deseja voltar ao menu? (S/N):" at 0501.
       initialize opcao.
       accept opcao at 0531.
       if opcao = "S" or "s" then
           move "v" to prosseguir
           move "f" to rodar
       else
           if opcao = "N" or "n" then
               move "v" to prosseguir
               move "v" to rodar
               display "Entre com uma conta valida!       " at 0501
           else
               display "Opcao invalida"
               move "f" to prosseguir
               move "v" to rodar
           end-if
       end-if.

       PERGUNTA-EXCLUIR.
       move "f" to  prosseguir.
       display "                                  " at 0501
       display "Tem certeza que quer excluir esta conta (S/N)?" at 0601.
       display spaces.
       display spaces.
       display spaces.
       perform MOSTRAR-EXCLUSAO.
       initialize opcao.
       accept opcao at 0650.
       if opcao = "n" or "N" then
        display "Conta nao sera excluida! " at 0701
        move "v" to prosseguir
       else
        if opcao = "s" or "S" then
        display "Conta excluida!          " at 0701
        DELETE F-CADASTRO RECORD
        move "v" to prosseguir
       else
        display " Opcao invalida          " at 0701.

       MOSTRAR-EXCLUSAO.
       display "======================================================".
       display "Codigo da conta:       " ws-cod-conta.
       display "Codigo da agencia:     " ws-cod-agencia.
       display "Nome do dono da conta: " ws-nome.
       display "Saldo da conta:        " ws-saldo.
       display "======================================================".




      *******************************************************************
       CONSULTA.
       initialize opcao.
      *display erase at 0101.
       display TELA-CONSULTA.
       perform OPCAOCONSULTA until opcao = "1" or "2" or "s" or "S".

       OPCAOCONSULTA.
           initialize opcao.
           accept opcao at 1113.
           evaluate opcao
               when "1"  perform CONSULTAR-CONTA until rodar = "f"
               when "2"  perform CONSULTAR-NOME  until rodar = "f"
               when "s"  display "saindo"
               when "S"  display "saindo"
               when other display "   comando nao existe".

       CONSULTAR-CONTA.
      *    display erase at 0101.
           display TELA-CONSULTA-CONTA.
           move "f" to prosseguir.
           perform CONTA-EXISTE until prosseguir = "v".
           display "ENTER para continuar" at 1111.
           accept continua.
           initialize opcao.
          perform menu-principal.

       CONTA-EXISTE.
           initialize WS-DADOS-CADASTRO.
           initialize DADOS-CADASTRO.
           open input F-CADASTRO.
               accept cod-conta at 0527.
               read F-CADASTRO record into WS-DADOS-CADASTRO
                   key is cod-conta
                   invalid key
                       display "Conta inexistesnte!" at 0535
                   not invalid key
                       perform MOSTRAR-CONSULTA-CONTA
                       move "v" to prosseguir
                       display "                   " at 0535
               end-read.
           close F-CADASTRO.

       MOSTRAR-CONSULTA-CONTA.
       display TELA-INFO2.
       display ws-cod-conta at 0720.
       display ws-cod-agencia at 0820.
       display ws-nome at 0920.
       display ws-saldo at 1020.

       CONSULTAR-NOME.
       open input F-CADASTRO.
      * display erase at 0101.
        display TELA-CONSULTA-NOME.
        move "f" to prosseguir.
        perform NOME-EXISTE until prosseguir = "v".
       close F-CADASTRO.

       NOME-EXISTE.
       move "v" to prosseguir.
       initialize WS-DADOS-CADASTRO.
       initialize DADOS-CADASTRO.
       initialize busca-nome.
       accept nome at 0535.
       move nome to busca-nome.
       start F-CADASTRO key is = nome
           invalid key
               display "Nome nao possui conta!" at 0635
               move "f" to prosseguir
           not invalid key
               display "                      " at 0635
               perform LOOP-NOME
               move "v" to prosseguir
        end-start.

       LOOP-NOME.
       move "f" to prosseguir.
       move nome to busca-nome.
       display spaces.
       display "Contas pertencentes a " busca-nome.
       display spaces.
       perform until prosseguir = "v"
           read F-CADASTRO next record into WS-DADOS-CADASTRO
               at end move "v" to prosseguir
               not at end
                   if nome = busca-nome then
                     perform MOSTRAR-CONSULTA-NOME
                   else
                       move "v" to prosseguir
                   end-if
               end-read
       end-perform.
       display "======================================================".
       display "Fim da lista".
       display "ENTER para continuar ".
       accept continua.

       MOSTRAR-CONSULTA-NOME.
       display "======================================================".
       display "Codigo da conta:       " ws-cod-conta.
       display "Codigo da agencia:     " ws-cod-agencia.
       display "Nome do dono da conta: " ws-nome.
       display "Saldo da conta:        " ws-saldo.

      *******************************************************************
       LISTAR.
       initialize WS-DADOS-CADASTRO.
       initialize DADOS-CADASTRO.
      *display erase at 0101.
       display TELA-LISTAR.
       display spaces.
       move "f" to prosseguir.
       open input F-CADASTRO.
        perform until prosseguir = "v"
         read F-CADASTRO next record into WS-DADOS-CADASTRO
          at end
           move "v" to prosseguir
          not at end
           perform MOSTRAR-CADASTROS
         end-read
        end-perform.
       close F-CADASTRO.
       display "======================================================".
       display "Fim da lista".
       display "ENTER para continuar ".
       accept continua.

       MOSTRAR-CADASTROS.
       display "======================================================".
       display "Codigo da conta:       " ws-cod-conta.
       display "Codigo da agencia:     " ws-cod-agencia.
       display "Nome do dono da conta: " ws-nome.
       display "Saldo da conta:        " ws-saldo.

      *******************************************************************
       SAIR.
           move "v" to fim.
           display " saindo...                      ".
