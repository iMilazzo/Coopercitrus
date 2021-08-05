CLASS zcl_agco_so_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_agco_global
  CREATE PUBLIC .

  PUBLIC SECTION.

    ALIASES enviar
      FOR zif_agco~enviar .
    ALIASES preencher_saida
      FOR zif_agco~preencher_saida .
    ALIASES processar
      FOR zif_agco~processar .
    ALIASES ty_t_centros
      FOR zif_agco~ty_t_centros .
    ALIASES ty_t_materiais
      FOR zif_agco~ty_t_materiais .

    TYPES:
      ty_r_auart TYPE RANGE OF auart .
    TYPES:
      BEGIN OF ty_s_ordem,
        vbeln  TYPE vbeln,
        erdat  TYPE erdat,
        vdatu  TYPE edatu_vbak,
        auart  TYPE auart,
        kunnr  TYPE kunnr,
        vbtyp  TYPE vbtyp,
        posnr  TYPE posnr,
        matnr  TYPE matnr,
        werks  TYPE werks_d,
        kwmeng TYPE kwmeng,
        netpr  TYPE netpr,
        kzwi6  TYPE kzwi6,
        mwsbp  TYPE mwsbp,
        waerk  TYPE waerk,
        uvall  TYPE uvall,
        uvfak  TYPE uvfak,
      END OF ty_s_ordem .
    TYPES:
      ty_t_ordens TYPE SORTED TABLE OF ty_s_ordem
                        WITH UNIQUE KEY vbeln
                        WITH NON-UNIQUE SORTED KEY material COMPONENTS matnr werks
                        WITH NON-UNIQUE SORTED KEY centro COMPONENTS werks .
    TYPES:
      BEGIN OF ty_s_item,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        matnr  TYPE matnr,
        werks  TYPE werks_d,
        kwmeng TYPE kwmeng,
        netpr  TYPE netpr,
        kzwi6  TYPE kzwi6,
        mwsbp  TYPE mwsbp,
        waerk  TYPE waerk,
        uvall  TYPE uvall,
        uvfak  TYPE uvfak,
      END OF ty_s_item .
    TYPES:
      ty_t_itens TYPE SORTED TABLE OF ty_s_item
                       WITH UNIQUE KEY vbeln posnr .
    TYPES:
      BEGIN OF ty_s_fatura,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        fkdat TYPE fkdat,
      END OF ty_s_fatura .
    TYPES:
      ty_t_faturas TYPE SORTED TABLE OF ty_s_fatura
                         WITH UNIQUE KEY vbeln posnr .
    TYPES:
      BEGIN OF ty_s_cliente,
        kunnr TYPE kunnr,
        stcd1 TYPE stcd1,
      END OF ty_s_cliente .
    TYPES:
      ty_t_clientes TYPE SORTED TABLE OF ty_s_cliente WITH UNIQUE KEY kunnr .
    TYPES:
      BEGIN OF MESH ty_m_ordens,
        ordens    TYPE ty_t_ordens ASSOCIATION cliente TO clientes
                                            ON kunnr = kunnr USING KEY primary_key
                                   ASSOCIATION fatura TO faturas
                                            ON vbeln = vbeln
                                           AND posnr = posnr USING KEY primary_key
                                   ASSOCIATION material TO materiais
                                            ON matnr = matnr USING KEY primary_key
                                   ASSOCIATION centro TO centros
                                            ON matnr = matnr
                                           AND werks = werks USING KEY primary_key,
*        itens     TYPE ty_t_itens   ASSOCIATION ordem TO ordens
*                                            ON vbeln = vbeln USING KEY primary_key
*                                   ASSOCIATION fatura TO faturas
*                                            ON vbeln = vbeln
*                                           AND posnr = posnr USING KEY primary_key
*                                   ASSOCIATION material TO materiais
*                                            ON matnr = matnr USING KEY primary_key
*                                   ASSOCIATION centro TO centros
*                                            ON matnr = matnr
*                                           AND werks = werks USING KEY primary_key,
        materiais TYPE ty_t_materiais,
        centros   TYPE ty_t_centros,
        faturas   TYPE ty_t_faturas,
        clientes  TYPE ty_t_clientes,

      END OF MESH ty_m_ordens .

    DATA m_ordens TYPE ty_m_ordens .

    METHODS zif_agco~carregar_dados
        REDEFINITION .
    METHODS zif_agco~definir_criterios
        REDEFINITION .
    METHODS zif_agco~preencher_saida
        REDEFINITION .
protected section.
private section.

  aliases LT_CONSTANTES
    for ZIF_AGCO~T_CONSTANTES .
  aliases LT_PARCEIROS
    for ZIF_AGCO~T_PARCEIROS .
  aliases RL_LGORT
    for ZIF_AGCO~R_LGORT .
  aliases RL_MATKL
    for ZIF_AGCO~R_MATKL .
  aliases RL_MFRNR
    for ZIF_AGCO~R_MFRNR .
  aliases RL_NFTYPE
    for ZIF_AGCO~R_NFTYPE .
  aliases V_DATA
    for ZIF_AGCO~V_DATA .
  aliases V_HORA
    for ZIF_AGCO~V_HORA .
  aliases V_MESES
    for ZIF_AGCO~V_MESES .
  aliases V_RTIME
    for ZIF_AGCO~V_RTIME .
  aliases V_TESTE
    for ZIF_AGCO~V_TESTE .
  aliases V_TOKEN
    for ZIF_AGCO~V_TOKEN .
  aliases AUTENTICAR
    for ZIF_AGCO~AUTENTICAR .
  aliases CARREGAR_DADOS
    for ZIF_AGCO~CARREGAR_DADOS .
  aliases CRIAR_RANGE
    for ZIF_AGCO~CRIAR_RANGE .
  aliases CRIAR_RANGE_CENTROS
    for ZIF_AGCO~CRIAR_RANGE_CENTROS .
  aliases DEFINIR_CRITERIOS
    for ZIF_AGCO~DEFINIR_CRITERIOS .
  aliases FORMATAR_CNPJ
    for ZIF_AGCO~FORMATAR_CNPJ .
  aliases LER_CENTROS
    for ZIF_AGCO~LER_CENTROS .
  aliases LER_CONSTANTES
    for ZIF_AGCO~LER_CONSTANTES .
  aliases LER_FORNECEDOR_EDI
    for ZIF_AGCO~LER_FORNECEDOR_EDI .
  aliases LER_MATERIAIS
    for ZIF_AGCO~LER_MATERIAIS .
  aliases LER_PARCEIROS
    for ZIF_AGCO~LER_PARCEIROS .
  aliases LER_TIMESTAMP
    for ZIF_AGCO~LER_TIMESTAMP .
  aliases TY_R_LGORT
    for ZIF_AGCO~TY_R_LGORT .
  aliases TY_R_MATKL
    for ZIF_AGCO~TY_R_MATKL .
  aliases TY_R_MFRNR
    for ZIF_AGCO~TY_R_MFRNR .
  aliases TY_R_NFTYPE
    for ZIF_AGCO~TY_R_NFTYPE .
  aliases TY_S_CENTRO
    for ZIF_AGCO~TY_S_CENTRO .
  aliases TY_S_DESCRIPTION
    for ZIF_AGCO~TY_S_DESCRIPTION .
  aliases TY_S_MATERIAL
    for ZIF_AGCO~TY_S_MATERIAL .
  aliases TY_S_PARCEIRO
    for ZIF_AGCO~TY_S_PARCEIRO .
  aliases TY_S_TVARVC
    for ZIF_AGCO~TY_S_TVARVC .
  aliases TY_T_DESCRIPTION
    for ZIF_AGCO~TY_T_DESCRIPTION .
  aliases TY_T_PARCEIROS
    for ZIF_AGCO~TY_T_PARCEIROS .
  aliases TY_T_TVARVC
    for ZIF_AGCO~TY_T_TVARVC .

  data LV_DIAS type INT4 .
  data LV_ERDAT type DATS .
  data RL_AUART type TY_R_AUART .

  methods LER_ORDENS
    importing
      !IV_ERDAT type ERDAT
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_ORDENS) type TY_T_ORDENS .
  methods LER_ITENS
    importing
      !IT_ORDENS type TY_T_ORDENS
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_ITENS) type TY_T_ITENS .
  methods LER_DADOS_COMERCIAIS
    importing
      !IT_ORDENS type TY_T_ORDENS
    returning
      value(RT_FATURAS) type TY_T_FATURAS .
  methods LER_CLIENTES
    importing
      !IT_ORDENS type TY_T_ORDENS
    returning
      value(RT_CLIENTES) type TY_T_CLIENTES .
ENDCLASS.



CLASS ZCL_AGCO_SO_DATA IMPLEMENTATION.


  METHOD ler_clientes.
    CHECK it_ordens IS NOT INITIAL.
    SELECT DISTINCT
           k~kunnr, k~stcd1
      FROM @it_ordens AS o
      JOIN kna1       AS k ON k~kunnr = o~kunnr
      INTO TABLE @rt_clientes.
  ENDMETHOD.


  METHOD ler_dados_comerciais.
    CHECK it_ordens IS NOT INITIAL.
    SELECT d~vbeln, d~posnr, d~fkdat
      FROM @it_ordens AS i
      JOIN vbkd AS d ON d~vbeln = i~vbeln
                    AND d~posnr = i~posnr
      INTO TABLE @rt_faturas.
  ENDMETHOD.


  METHOD ler_itens.
    CHECK it_ordens IS NOT INITIAL.
    SELECT p~vbeln, p~posnr, p~matnr, p~werks, p~kwmeng, p~netpr,
           p~kzwi6, p~mwsbp, p~waerk, p~uvall, p~uvfak
      FROM @it_ordens AS o
      JOIN vbap AS p ON p~vbeln = o~vbeln
      INTO TABLE @rt_itens.
  ENDMETHOD.


  METHOD ler_ordens.
    SELECT k~vbeln, k~erdat, k~vdatu, k~auart, k~kunnr, CASE WHEN k~vbtyp = 'C' THEN 'S'
                                                             WHEN k~vbtyp = 'H' THEN 'R'
                                                             ELSE ' '
                                                         END AS vbtyp,
           p~posnr, p~matnr, p~werks, p~kwmeng, p~netpr,
           p~kzwi6, p~mwsbp, p~waerk, p~uvall,  p~uvfak
      FROM @it_centros AS c
      JOIN vbap        AS p ON p~matnr = c~matnr
                           AND p~werks = c~werks
      JOIN vbak        AS k ON k~vbeln = p~vbeln
     WHERE k~erdat >= @iv_erdat
      INTO TABLE @rt_ordens.
  ENDMETHOD.


  METHOD zif_agco~carregar_dados.
    "Carrega todos parceiros configurados (AGCO no caso)
    lt_parceiros = ler_parceiros( ).
    IF lt_parceiros IS NOT INITIAL.

      MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( lt_parceiros ) ) 'PARCEIROS'.

      "Carrega materiais por tipo e fornecedor
      m_ordens-materiais = ler_materiais( it_tipos = rl_matkl
                                              it_fornecedores = rl_mfrnr ).

      IF m_ordens-materiais IS NOT INITIAL.

        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_ordens-materiais ) ) 'MATERIAIS'.
        "Carrega materiais por centro
        m_ordens-centros = ler_centros( it_materiais = m_ordens-materiais
                                          it_centros = criar_range_centros( lt_parceiros ) ).

        IF m_ordens-centros IS NOT INITIAL.

          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_ordens-centros ) ) 'CENTROS'.
          m_ordens-ordens = ler_ordens( it_centros = m_ordens-centros
                                        iv_erdat = lv_erdat ).
*          m_ordens-itens = ler_itens( m_ordens-ordens ).
          m_ordens-faturas = ler_dados_comerciais( m_ordens-ordens ).
          m_ordens-clientes = ler_clientes( m_ordens-ordens ).

        ELSE.
          RAISE EXCEPTION TYPE zcx_agco MESSAGE e014(zpmm_agco).
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_agco MESSAGE e015(zpmm_agco).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_agco MESSAGE e014(zpmm_agco).
    ENDIF.
  ENDMETHOD.


  method ZIF_AGCO~DEFINIR_CRITERIOS.

    MESSAGE s004(zpmm_agco).
    "Definir tipo de material e quais fornecedores
    DATA(lr_matkl) = criar_range( iv_name = |ZAGCO_MATKL| iv_data_element = |MATKL| ).
    ASSIGN lr_matkl->* TO FIELD-SYMBOL(<fs_matkl>).
    rl_matkl = <fs_matkl>.

    DATA(lr_mfrnr) = criar_range( iv_name = |ZAGCO_MFRNR| iv_data_element = |MFRNR| ).
    ASSIGN lr_mfrnr->* TO FIELD-SYMBOL(<fs_mfrnr>).
    rl_mfrnr = <fs_mfrnr>.

    "Carrega materiais por depósito
    DATA(lr_lgort) = criar_range( iv_name = |ZAGCO_ID_LGORT| iv_data_element = |LGORT_D| ).
    ASSIGN lr_lgort->* TO FIELD-SYMBOL(<fs_lgort>).
    rl_lgort = <fs_lgort>.

    "Definir tipo de documento para venda "unusual"
    DATA(lr_auart) = criar_range( iv_name = |ZAGCO_AUART| iv_data_element = |AUART| ).
    ASSIGN lr_auart->* TO FIELD-SYMBOL(<fs_auart>).
    rl_auart = <fs_auart>.

    v_meses  = VALUE i( lt_constantes[ name = |ZAGCO_MONTHS| type = |P| numb = |0000| ]-low DEFAULT |-36| ).
    lv_erdat = NEW cl_hrpad_date_computations( )->add_months_to_date( EXPORTING start_date = sy-datum
                                                                                    months = v_meses ).

  endmethod.


  METHOD zif_agco~preencher_saida.
    TYPES:
      BEGIN OF ty_s_payload,
        msgguid TYPE sxmsmsglst-msgguid,
        pid     TYPE sxmsmsglst-pid,
        payload TYPE xstring,
      END OF ty_s_payload,
      ty_t_payload TYPE SORTED TABLE OF ty_s_payload  WITH NON-UNIQUE KEY msgguid pid,

      BEGIN OF ty_s_sdata,
        order_date            TYPE c LENGTH 50,
        order_type            TYPE c LENGTH 10,
        order_id              TYPE c LENGTH 50,
        customer_legal_number TYPE c LENGTH 50,
        delivery_type         TYPE c LENGTH 8,
        dealer_filter1        TYPE c LENGTH 50,
        dealer_filter2        TYPE c LENGTH 50,
        dealer_filter3        TYPE c LENGTH 50,
      END OF ty_s_sdata,
      ty_t_log_hdr TYPE SORTED TABLE OF zmm_agco_log_hdr
                   WITH UNIQUE KEY interface cnpj data hora rastreio,
      ty_t_log_itm TYPE SORTED TABLE OF zmm_agco_log_itm
                   WITH UNIQUE KEY interface cnpj data hora rastreio item.


    DATA:
      lv_rtime   TYPE p DECIMALS 3,
      lt_log_hdr TYPE ty_t_log_hdr,
      lt_log_itm TYPE ty_t_log_itm,
      lt_pedidos TYPE zagcott_po_orders,
      lt_items   TYPE zagcott_po_items.

    GET RUN TIME FIELD DATA(lv_rtime_ini).

    TRY.

        DATA(lo_sender) = NEW zco_si_sales_outbound( ).
        DATA(ls_saida) = VALUE zsend_sales(
                                send_sales = VALUE #(
                                             token = v_token
                                              data = VALUE #(
                              extraction_date_time = ler_timestamp( ) ) ) ) .

        LOOP AT  m_ordens-ordens USING KEY centro
                                 ASSIGNING FIELD-SYMBOL(<fs_key>)
                                 GROUP BY ( werks = <fs_key>-werks
                                             size = GROUP SIZE
                                            index = GROUP INDEX )
                                 ASSIGNING FIELD-SYMBOL(<fs_members>).

          ls_saida-send_sales-data-dealer_legal_number = formatar_cnpj( CONV #( lt_parceiros[ partner = <fs_members>-werks ]-taxnum ) ).
          MESSAGE s019(zpmm_agco) WITH <fs_members>-size |inventários| ls_saida-send_sales-data-dealer_legal_number.

          LOOP AT GROUP <fs_members> ASSIGNING FIELD-SYMBOL(<fs_ordem>).

            DATA(ls_so) = VALUE zsales_data_order(
                                  order_date = <fs_ordem>-erdat
                                  order_type = |S|
                                    order_id = <fs_ordem>-vbeln
                       customer_legal_number = formatar_cnpj( VALUE #( m_ordens-ordens\cliente[ <fs_ordem> ]-stcd1 OPTIONAL ) )
                               delivery_type = |REGULAR|
                                  items = VALUE #(
                                          LET l_reserved = COND #( WHEN <fs_ordem>-uvall <> |B| AND
                                                                        <fs_ordem>-uvall <> |C| AND
                                                                        <fs_ordem>-uvfak <> |B| AND
                                                                        <fs_ordem>-uvfak <> |C| THEN <fs_ordem>-kwmeng )
                                           IN
                                        ( order_line_number = <fs_ordem>-posnr
                                                part_number = <fs_ordem>-matnr
                                            first_pass_fill = abap_false
                                                demand_date = <fs_ordem>-vdatu
                                             requested_date = <fs_ordem>-erdat
                                          reserved_quantity = l_reserved
                                           shipped_quantity = <fs_ordem>-kwmeng
                                               shipped_date = VALUE #( m_ordens-ordens\fatura[ <fs_ordem> ]-fkdat OPTIONAL )
                                                line_status = COND #( WHEN <fs_ordem>-kwmeng > 0 THEN |CLOSED|
                                                                      WHEN l_reserved > 0 THEN |RESERVED| )
                                               unusual_sale = COND abap_bool( WHEN <fs_ordem>-auart IN rl_auart[] THEN abap_false ELSE abap_true )
                                                  net_value = <fs_ordem>-netpr
                                                total_value = COND #( WHEN <fs_ordem>-kwmeng > 0 THEN <fs_ordem>-kzwi6 / <fs_ordem>-kwmeng  )
                                                      taxes = <fs_ordem>-mwsbp
                                                   currency = <fs_ordem>-waerk ) ) ) .

            ls_saida-send_sales-data-order = ls_so.

            DATA: lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id.

            lo_sender->si_sales_outbound( EXPORTING output = ls_saida
                                          IMPORTING  input = DATA(ls_input)  ).

            lo_protocol_messageid ?= lo_sender->get_protocol( if_wsprotocol=>message_id ).

            lt_log_hdr = VALUE ty_t_log_hdr(
                          BASE lt_log_hdr
                            (  interface = |04|
                                    cnpj = lt_parceiros[ 1 ]-taxnum
                                    data = v_data
                                    hora = v_hora
                                rastreio = ls_input-response_sales-meta-tracking_id
                                  status = ls_input-response_sales-meta-status
                              message_id = lo_protocol_messageid->get_message_id( )
                                 tamanho = 318
                                   sdata = CONV ty_s_sdata( CORRESPONDING #( ls_saida-send_sales-data-order ) ) ) ).

            IF ls_input-response_sales-meta-status <> |200| AND
               ls_input-response_sales-meta-status <> |201|.

              lt_log_itm = VALUE ty_t_log_itm(
                            BASE lt_log_itm
                             FOR i = 1 THEN i + 1 UNTIL i > lines( ls_input-response_sales-errors )
                             LET ls_error = ls_input-response_sales-errors[ i ]
                              IN (
                                interface = |04|
                                     cnpj = lt_parceiros[ 1 ]-taxnum
                                     data = v_data
                                     hora = v_hora
                                 rastreio = ls_input-response_sales-meta-tracking_id
                                     item = i
                                    campo = ls_error-source-pointer
                                    valor = REDUCE #(
                                              INIT l_value TYPE string
                                               FOR <fs_value> IN ls_error-source-values
                                              NEXT l_value = |{ l_value }, { <fs_value> }| )
                                  detalhe = ls_error-detail
                                     erro = ls_error-error_code ) ).

            ENDIF.

          ENDLOOP.

        ENDLOOP.

        INSERT zmm_agco_log_hdr FROM TABLE lt_log_hdr.
        INSERT zmm_agco_log_itm FROM TABLE lt_log_itm.
        COMMIT WORK.

      CATCH cx_ai_system_fault INTO DATA(lo_exception).
      CATCH zcx_fault_response_out INTO DATA(lo_fault).

    ENDTRY.

    GET RUN TIME FIELD DATA(lv_rtime_fim).
    lv_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
    LOG-POINT ID zagco_log FIELDS lv_rtime_ini lv_rtime_fim lv_rtime.

  ENDMETHOD.
ENDCLASS.
