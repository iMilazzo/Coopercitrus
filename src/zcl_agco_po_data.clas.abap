class ZCL_AGCO_PO_DATA definition
  public
  inheriting from ZCL_AGCO_GLOBAL
  create public .

public section.

  aliases PREENCHER_SAIDA
    for ZIF_AGCO~PREENCHER_SAIDA .
  aliases PROCESSAR
    for ZIF_AGCO~PROCESSAR .

  types:
    BEGIN OF ty_s_nota,
        docnum     TYPE j_1bdocnum,
        docdat     TYPE j_1bdocdat,
        pstdat     TYPE j_1bpstdat,
        belnr      TYPE belnr_d,
        gjahr      TYPE gjahr,
        bukrs      TYPE bukrs,
        nfenum     TYPE j_1bnfnum9,
        series     TYPE j_1bseries,
        parid      TYPE j_1bparid,
        cnpj_bupla TYPE j_1b_cnpj_bupla,
        name1      TYPE name1_gp,
        waerk      TYPE waerk,
      END OF ty_s_nota .
  types:
    ty_t_notas TYPE SORTED TABLE OF ty_s_nota WITH UNIQUE KEY docnum
                   WITH NON-UNIQUE SORTED KEY belnr COMPONENTS belnr gjahr bukrs .
  types:
    BEGIN OF ty_s_fatura,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        bukrs TYPE bukrs,
        buzei TYPE buzei,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        menge TYPE menge_d,
        wrbtr TYPE wrbtr,
      END OF ty_s_fatura .
  types:
    ty_t_faturas TYPE SORTED TABLE OF ty_s_fatura WITH UNIQUE KEY belnr gjahr bukrs buzei
                                                        WITH NON-UNIQUE SORTED KEY pedido COMPONENTS ebeln ebelp .
  types:
    BEGIN OF ty_s_pedido,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        matnr TYPE matnr,
        menge TYPE menge_d,
      END OF ty_s_pedido .
  types:
    ty_t_pedidos TYPE SORTED TABLE OF ty_s_pedido WITH UNIQUE KEY ebeln ebelp .
  types:
    BEGIN OF ty_s_imposto,
        docnum TYPE j_1bdocnum,
        taxtyp TYPE j_1btaxtyp,
        base   TYPE j_1bbase,
        taxval TYPE j_1btaxval,
        excbas TYPE j_1bexcbas,
        othbas TYPE j_1bothbas,
      END OF ty_s_imposto .
  types:
    ty_t_impostos TYPE SORTED TABLE OF ty_s_imposto WITH UNIQUE KEY docnum taxtyp .
  types:
    ty_s_saida TYPE TABLE OF zagcos_parts_data WITH DEFAULT KEY .
  types:
    BEGIN OF MESH ty_m_pedidos,
        notas    TYPE ty_t_notas ASSOCIATION fatura TO faturas
                                          ON belnr = belnr
                                         AND gjahr = gjahr
                                         AND bukrs = bukrs USING KEY primary_key
                                 ASSOCIATION imposto TO impostos
                                          ON docnum = docnum USING KEY primary_key,
        impostos TYPE ty_t_impostos,
        faturas  TYPE ty_t_faturas ASSOCIATION pedido TO pedidos
                                           ON ebeln = ebeln USING KEY primary_key,
        pedidos  TYPE ty_t_pedidos,

      END OF MESH ty_m_pedidos .
  types:
    ty_r_branch TYPE RANGE OF J_1BBRANC_ .

  data RL_BRANCH type TY_R_BRANCH .
  data M_PEDIDOS type TY_M_PEDIDOS .

  methods ZIF_AGCO~CARREGAR_DADOS
    redefinition .
  methods ZIF_AGCO~DEFINIR_CRITERIOS
    redefinition .
  methods ZIF_AGCO~ENVIAR
    redefinition .
  methods ZIF_AGCO~PREENCHER_SAIDA
    redefinition .
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
  aliases ENVIAR
    for ZIF_AGCO~ENVIAR .
  aliases FORMATAR_CNPJ
    for ZIF_AGCO~FORMATAR_CNPJ .
  aliases LER_CENTROS
    for ZIF_AGCO~LER_CENTROS .
  aliases LER_CONSTANTES
    for ZIF_AGCO~LER_CONSTANTES .
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
  aliases TY_S_DESCRIPTION
    for ZIF_AGCO~TY_S_DESCRIPTION .
  aliases TY_S_MATERIAL
    for ZIF_AGCO~TY_S_MATERIAL .
  aliases TY_S_PARCEIRO
    for ZIF_AGCO~TY_S_PARCEIRO .
  aliases TY_S_TVARVC
    for ZIF_AGCO~TY_S_TVARVC .
  aliases TY_T_CENTROS
    for ZIF_AGCO~TY_T_CENTROS .
  aliases TY_T_DESCRIPTION
    for ZIF_AGCO~TY_T_DESCRIPTION .
  aliases TY_T_PARCEIROS
    for ZIF_AGCO~TY_T_PARCEIROS .
  aliases TY_T_TVARVC
    for ZIF_AGCO~TY_T_TVARVC .

  data LV_DOCTYP type J_1BDOCTYP .
  data LV_DIRECT type J_1BDIRECT .
  data LV_BUKRS type BUKRS .
  data LV_DIAS type INT4 .
  data LV_DOCDAT type J_1BDOCDAT .
  data LV_BRANCH type J_1BBRANC_ .

  methods LER_NOTAS
    importing
      !IV_DOCTYP type J_1BDOCTYP
      !IV_DIRECT type J_1BDIRECT
      !IV_DOCDAT type J_1BDOCDAT
      !IV_BUKRS type BUKRS
      !IT_BRANCH type TY_R_BRANCH
    returning
      value(RT_NOTAS) type TY_T_NOTAS .
  methods LER_FATURAS
    importing
      !IT_NOTAS type TY_T_NOTAS
    returning
      value(RT_FATURAS) type TY_T_FATURAS .
  methods LER_PEDIDOS
    importing
      !IT_FATURAS type TY_T_FATURAS
    returning
      value(RT_PEDIDOS) type TY_T_PEDIDOS .
  methods LER_IMPOSTOS
    importing
      !IT_NOTAS type TY_T_NOTAS
    returning
      value(RT_IMPOSTOS) type TY_T_IMPOSTOS .
  methods CRIAR_RANGE_BRANCH
    importing
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_BRANCH) type TY_R_BRANCH .
ENDCLASS.



CLASS ZCL_AGCO_PO_DATA IMPLEMENTATION.


  method CRIAR_RANGE_BRANCH.
    LOOP AT it_centros ASSIGNING FIELD-SYMBOL(<fs_centro>)
                       USING KEY centro GROUP BY ( werks = <fs_centro>-werks ).
      rt_branch = VALUE #(
                   BASE rt_branch
                 ( sign = |I|
                 option = |EQ|
                    low = |0{ <fs_centro>-werks+1(3) }| ) ).
    ENDLOOP.
  endmethod.


  METHOD ler_faturas.
    CHECK it_notas IS NOT INITIAL.
    SELECT belnr, gjahr, bukrs, buzei, ebeln, ebelp, menge, wrbtr
      INTO TABLE @rt_faturas
      FROM rseg
       FOR ALL ENTRIES IN @it_notas
     WHERE belnr = @it_notas-belnr
       AND gjahr = @it_notas-gjahr
       AND bukrs = @it_notas-bukrs.
  ENDMETHOD.


  METHOD ler_impostos.
    CHECK it_notas IS NOT INITIAL.
    SELECT x~docnum, x~taxtyp,
           SUM( base ) AS base, SUM( excbas ) AS excbas, SUM( othbas )
      FROM @it_notas AS n
      JOIN j_1bnfstx AS x ON x~docnum = n~docnum
     GROUP BY x~docnum, x~taxtyp
      INTO TABLE @rt_impostos.
  ENDMETHOD.


  METHOD ler_notas.
    SELECT docnum, docdat, pstdat, belnr, gjahr, bukrs,
           nfenum, series, parid, cnpj_bupla, name1
      INTO TABLE @rt_notas
      FROM j_1bnfdoc
     WHERE doctyp = @iv_doctyp
       AND direct = @iv_direct
       AND docdat >= @iv_docdat
       AND bukrs  = @iv_bukrs
       AND branch IN @it_branch.
  ENDMETHOD.


  METHOD ler_pedidos.
    CHECK it_faturas IS NOT INITIAL.
    SELECT ebeln, ebelp, matnr, menge
      INTO TABLE @rt_pedidos
      FROM ekpo
       FOR ALL ENTRIES IN @it_faturas
     WHERE ebeln = @it_faturas-ebeln.
  ENDMETHOD.


  METHOD zif_agco~carregar_dados.
    MESSAGE s003(zpmm_agco).
    GET RUN TIME FIELD DATA(lv_rtime_ini).
    "Carrega todos parceiros configurados (AGCO no caso)
    lt_parceiros = ler_parceiros( ).
    IF lt_parceiros IS NOT INITIAL.

      MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( lt_parceiros ) ) 'PARCEIROS'.

      DATA(lt_materiais) = ler_materiais( it_tipos = rl_matkl
                                   it_fornecedores = rl_mfrnr ).
      IF lt_materiais IS NOT INITIAL.

        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( lt_materiais ) ) 'MATERIAIS'.
        rl_branch = criar_range_branch( ler_centros( it_materiais = lt_materiais
                                                       it_centros = criar_range_centros( lt_parceiros ) ) ).
        m_pedidos-notas = ler_notas( iv_doctyp = lv_doctyp
                                     iv_direct = lv_direct
                                     iv_docdat = lv_docdat
                                      iv_bukrs = lv_bukrs
                                     it_branch = rl_branch ).
        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pedidos-notas ) ) 'NOTAS'.

        m_pedidos-faturas = ler_faturas( m_pedidos-notas ).
        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pedidos-faturas ) ) 'FATURAS'.

        m_pedidos-pedidos = ler_pedidos( m_pedidos-faturas ).
        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pedidos-pedidos ) ) 'PEDIDOS'.

      ELSE.
        RAISE EXCEPTION TYPE zcx_agco MESSAGE e015(zpmm_agco).
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_agco MESSAGE e014(zpmm_agco).
    ENDIF.
  ENDMETHOD.


  METHOD zif_agco~definir_criterios.
    message s004(zpmm_agco).
    lv_doctyp = VALUE doctyp( lt_constantes[ name = |ZAGCO_PO_DOCTYP| type = |P| numb = |0000| ]-low DEFAULT |6| ).
    lv_direct = VALUE direct( lt_constantes[ name = |ZAGCO_PO_DIRECT| type = |P| numb = |0000| ]-low DEFAULT |2| ).
    lv_bukrs  = VALUE  bukrs( lt_constantes[ name = |ZAGCO_PO_BUKRS|  type = |P| numb = |0000| ]-low DEFAULT |2| ).
    lv_dias   = VALUE   int4( lt_constantes[ name = |ZAGCO_PO_DOCDAT| type = |P| numb = |0000| ]-low DEFAULT |2| ).

    v_meses  = VALUE i( lt_constantes[ name = |ZAGCO_MONTHS| type = |P| numb = |0000| ]-low DEFAULT |-36| ).
    lv_docdat = NEW cl_hrpad_date_computations( )->add_months_to_date( EXPORTING start_date = sy-datum
                                                                                    months = v_meses ).
  ENDMETHOD.


  method ZIF_AGCO~ENVIAR.
*CALL METHOD SUPER->ZIF_AGCO~ENVIAR
*  EXPORTING
*    IS_INVENTORY_DATA =
*    .
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
        order_id                      TYPE c LENGTH 50,
        order_agco                    TYPE c LENGTH 10,
        order_id_original             TYPE c LENGTH 50,
        delivered_dealer_legal_number TYPE c LENGTH 50,
        order_date                    TYPE c LENGTH 8,
        order_type                    TYPE c LENGTH 50,
        supplier_legal_number         TYPE c LENGTH 50,
        supplier_name                 TYPE c LENGTH 100,
        filter1                       TYPE c LENGTH 50,
        filter2                       TYPE c LENGTH 50,
        filter3                       TYPE c LENGTH 50,
      END OF ty_s_sdata,
      ty_t_log_hdr TYPE SORTED TABLE OF zmm_agco_log_hdr
                   WITH UNIQUE KEY interface cnpj data hora rastreio,
      ty_t_log_itm TYPE SORTED TABLE OF zmm_agco_log_itm
                   WITH UNIQUE KEY interface cnpj data hora rastreio item.

    DATA:
      lt_log_hdr TYPE ty_t_log_hdr,
      lt_log_itm TYPE ty_t_log_itm,
      lt_pedidos TYPE zagcott_po_orders,
      lt_items   TYPE zagcott_po_items.

    MESSAGE s008(zpmm_agco).

    GET RUN TIME FIELD DATA(lv_rtime_ini).

    TRY.

        MESSAGE s009(zpmm_agco) WITH 'ZCO_SI_PO_OUTBOUND'.
        DATA(lo_sender) = NEW zco_si_purchase_outbound( ).
        DATA(ls_saida) = VALUE zsend_purchase(
                                send_purchase = VALUE #(
                                                token = v_token
                                                 data = VALUE #(
                                         extraction_date_time = ler_timestamp( ) ) ) ) .

        LOOP AT m_pedidos-notas ASSIGNING FIELD-SYMBOL(<fs_nota>).

          LOOP AT m_pedidos-notas\fatura[ <fs_nota> ] ASSIGNING FIELD-SYMBOL(<fs_fatura>).

            DATA(ls_po) = VALUE zpurchase_data_order(
                            order_id = |{ <fs_nota>-nfenum }{ <fs_nota>-series }{ <fs_nota>-parid }|
                   order_id_original = <fs_fatura>-ebeln
       delivered_dealer_legal_number = formatar_cnpj( CONV #( lt_parceiros[ 1 ]-taxnum ) )
                          order_date = <fs_nota>-docdat
                          order_type = |STOCK_ORDER|
               supplier_legal_number = <fs_nota>-cnpj_bupla
                       supplier_name = <fs_nota>-name1 ).


            DATA(lt_item) = VALUE zpurchase_data_items_tab(
                                         FOR <fs_pedido> IN m_pedidos-faturas\pedido[ <fs_fatura> ]
                                         LET l_pendente = <fs_pedido>-menge - <fs_fatura>-menge
                                          IN
                         ( order_line_number = <fs_pedido>-ebelp
                                 part_number = <fs_pedido>-matnr
                               received_date = <fs_nota>-pstdat
                          requested_quantity = <fs_pedido>-menge
                           received_quantity = <fs_fatura>-menge
                               open_quantity = l_pendente
                                 line_status = COND #( WHEN l_pendente > 0 THEN |OPEN| ELSE |CLOSED| )
                                   net_value = COND #( WHEN <fs_fatura>-menge > 0 THEN <fs_fatura>-wrbtr / <fs_fatura>-menge ELSE 0 )
                                    currency = COND #( WHEN <fs_nota>-waerk IS INITIAL THEN |BRL| ELSE <fs_nota>-waerk  ) ) ) .

            ls_po-items = lt_item[].
            ls_saida-send_purchase-data-order = ls_po.

            DATA: lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id.
            lo_protocol_messageid ?= lo_sender->get_protocol( if_wsprotocol=>message_id ).

            lo_sender->si_purchase_outbound( EXPORTING output = ls_saida
                                             IMPORTING  input = DATA(ls_input)  ).

            lt_log_hdr = VALUE ty_t_log_hdr(
                          BASE lt_log_hdr
                            (  interface = |03|
                                    cnpj = lt_parceiros[ 1 ]-taxnum
                                    data = v_data
                                    hora = v_hora
                                rastreio = ls_input-response_purchase-meta-tracking_id
                                  status = ls_input-response_purchase-meta-status
                              message_id = lo_protocol_messageid->get_message_id( )
                                 tamanho = 518
                                   sdata = CONV ty_s_sdata( CORRESPONDING #( ls_saida-send_purchase-data-order ) ) ) ).

            IF ls_input-response_purchase-meta-status <> |200| AND
               ls_input-response_purchase-meta-status <> |201|.

              lt_log_itm = VALUE ty_t_log_itm(
                            BASE lt_log_itm
                             FOR i = 1 THEN i + 1 UNTIL i > lines( ls_input-response_purchase-errors )
                             LET ls_error = ls_input-response_purchase-errors[ i ]
                              IN (
                                interface = |03|
                                     cnpj = lt_parceiros[ 1 ]-taxnum
                                     data = v_data
                                     hora = v_hora
                                 rastreio = ls_input-response_purchase-meta-tracking_id
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
    v_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
    MESSAGE s013(zpmm_agco) WITH v_rtime.

  ENDMETHOD.
ENDCLASS.
