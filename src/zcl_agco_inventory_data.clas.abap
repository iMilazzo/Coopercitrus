class ZCL_AGCO_INVENTORY_DATA definition
  public
  inheriting from ZCL_AGCO_GLOBAL
  create public .

public section.

  aliases R_WERKS
    for ZIF_AGCO~R_WERKS .
  aliases PREENCHER_SAIDA
    for ZIF_AGCO~PREENCHER_SAIDA .
  aliases PROCESSAR
    for ZIF_AGCO~PROCESSAR .
  aliases TY_R_WERKS
    for ZIF_AGCO~TY_R_WERKS .
  aliases TY_T_MATERIAIS
    for ZIF_AGCO~TY_T_MATERIAIS .

  types:
    BEGIN OF ty_s_centro,
        matnr TYPE matnr,
        werks TYPE werks_d,
      END OF ty_s_centro .
  types:
    ty_t_centros TYPE SORTED TABLE OF ty_s_centro
                   WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF ty_s_deposito,
        matnr TYPE matnr,
        werks TYPE werks_d,
        labst TYPE labst,
      END OF ty_s_deposito .
  types:
    ty_t_depositos TYPE SORTED TABLE OF ty_s_deposito
                           WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF ty_s_reserva,
        matnr TYPE matnr,
        werks TYPE werks_d,
        bdmng TYPE bdmng,
      END OF ty_s_reserva .
  types:
    ty_t_reservas TYPE SORTED TABLE OF ty_s_reserva WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF ty_s_join,
        matnr TYPE matnr,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        labst TYPE labst,
        bdmng TYPE bdmng,
        saldo TYPE bdmng,
      END OF ty_s_join .
  types:
    ty_t_join TYPE SORTED TABLE OF ty_s_join WITH UNIQUE KEY matnr werks lgort .
  types:
    BEGIN OF ty_s_remessa,
        matnr TYPE matnr,
        werks TYPE werks_d,
        lfimg TYPE lfimg,
      END OF ty_s_remessa .
  types:
    ty_t_remessas TYPE SORTED TABLE OF ty_s_remessa WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF ty_s_pedido,
        matnr TYPE matnr,
        werks TYPE werks_d,
        menge TYPE menge_d,
      END OF ty_s_pedido .
  types:
    ty_t_pedidos TYPE SORTED TABLE OF ty_s_pedido WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF ty_s_nota,
        matnr TYPE matnr,
        werks TYPE werks_d,
        menge TYPE menge_d,
      END OF ty_s_nota .
  types:
    ty_t_notas TYPE SORTED TABLE OF ty_s_nota WITH UNIQUE KEY matnr werks .
  types:
    BEGIN OF MESH ty_m_inventario,

        materiais TYPE ty_t_materiais ASSOCIATION centro TO centros
                                               ON matnr = matnr  USING KEY primary_key
                                      ASSOCIATION deposito TO depositos
                                               ON matnr = matnr USING KEY primary_key
                                      ASSOCIATION reserva TO reservas
                                               ON matnr = matnr USING KEY primary_key
                                      ASSOCIATION remessa TO remessas
                                               ON matnr = matnr USING KEY primary_key
                                      ASSOCIATION pedido TO pedidos
                                               ON matnr = matnr USING KEY primary_key
                                      ASSOCIATION nota TO notas
                                               ON matnr = matnr USING KEY primary_key,

        centros   TYPE ty_t_centros   ASSOCIATION material TO materiais
                                               ON matnr = matnr USING KEY primary_key
                                      ASSOCIATION deposito TO depositos
                                               ON matnr = matnr
                                              AND werks = werks USING KEY primary_key
                                      ASSOCIATION reserva TO reservas
                                               ON matnr = matnr
                                              AND werks = werks USING KEY primary_key
                                      ASSOCIATION remessa TO remessas
                                               ON matnr = matnr
                                              AND werks = werks USING KEY primary_key
                                      ASSOCIATION pedido TO pedidos
                                               ON matnr = matnr
                                              AND werks = werks USING KEY primary_key
                                      ASSOCIATION nota TO notas
                                               ON matnr = matnr
                                              AND werks = werks USING KEY primary_key,
        depositos TYPE ty_t_depositos,
        reservas  TYPE ty_t_reservas,
        remessas  TYPE ty_t_remessas,
        pedidos   TYPE ty_t_pedidos,
        notas     TYPE ty_t_notas,
      END OF MESH ty_m_inventario .

  data M_INVENTARIO type TY_M_INVENTARIO .
  data LV_MES type LFMON .
  data LV_ANO type LFGJA .
  data LV_WBSTA type WBSTA .
  data LV_KNTTP type KNTTP .
  data LV_LOEKZ type LOEKZ .
  data LV_DOCTYP type J_1BDOCTYP .
  data LV_DIRECT type J_1BDIRECT .

  methods ZIF_AGCO~CARREGAR_DADOS
    redefinition .
  methods ZIF_AGCO~DEFINIR_CRITERIOS
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
  aliases TY_T_PARCEIROS
    for ZIF_AGCO~TY_T_PARCEIROS .

  methods LER_DEPOSITOS
    importing
      !IT_CENTROS type TY_T_CENTROS
      !IT_LGORT type TY_R_LGORT
      !IV_ANO type LFGJA
      !IV_MES type LFMON
    returning
      value(RT_DEPOSITOS) type TY_T_DEPOSITOS .
  methods LER_RESERVAS
    importing
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_RESERVAS) type TY_T_RESERVAS .
  methods LER_REMESSAS
    importing
      !IT_CENTROS type TY_T_CENTROS
      !IV_WBSTA type WBSTA
    returning
      value(RT_REMESSAS) type TY_T_REMESSAS .
  methods LER_PEDIDOS
    importing
      !IT_CENTROS type TY_T_CENTROS
      !IV_KNTTP type KNTTP
      !IV_LOEKZ type LOEKZ
    returning
      value(RT_PEDIDOS) type TY_T_PEDIDOS .
  methods LER_NOTAS
    importing
      !IT_CENTROS type TY_T_CENTROS
      !IT_NFTYPE type TY_R_NFTYPE
      !IV_DOCTYP type J_1BDOCTYP
      !IV_DIRECT type J_1BDIRECT
    returning
      value(RT_NOTAS) type TY_T_NOTAS .
ENDCLASS.



CLASS ZCL_AGCO_INVENTORY_DATA IMPLEMENTATION.


  METHOD ler_depositos.
    CHECK it_centros IS NOT INITIAL.
    SELECT m~matnr, m~werks, SUM( m~labst )
      FROM @it_centros AS c
      JOIN mard AS m ON m~matnr = c~matnr
                    AND m~werks = c~werks
     WHERE m~lgort IN @it_lgort
       AND m~lfgja = @iv_ano
       AND m~lfmon = @iv_mes
       AND m~lvorm = @abap_false
     GROUP BY m~matnr, m~werks
      INTO TABLE @rt_depositos.
  ENDMETHOD.


  METHOD ler_notas.
    CHECK it_centros IS NOT INITIAL.
*    SELECT d~matnr, d~werks, SUM( d~menge )
*      FROM @it_centros AS c
*      JOIN zvagco_notas AS d ON d~matnr = c~matnr
*                            AND d~bwkey = c~werks
*     WHERE d~nftype IN @it_nftype
*       AND d~doctyp = @iv_doctyp
*       AND d~direct = @iv_direct
*       AND d~cancel = @abap_false
*     GROUP BY d~matnr, d~werks
*      INTO TABLE @rt_notas.
    SELECT l~matnr, l~werks, SUM( l~menge )
      FROM @it_centros AS c
      JOIN j_1bnflin AS l ON l~matnr = c~matnr
                         AND l~bwkey = c~werks
      JOIN j_1bnfdoc AS d ON d~docnum = l~docnum
     WHERE d~nftype IN @it_nftype
       AND d~doctyp = @iv_doctyp
       AND d~direct = @iv_direct
       AND d~cancel = @abap_false
     GROUP BY l~matnr, l~werks
      INTO TABLE @rt_notas.
  ENDMETHOD.


  METHOD ler_pedidos.
    CHECK it_centros IS NOT INITIAL.
    SELECT p~matnr, p~werks, SUM( menge )
      FROM @it_centros AS c
      JOIN ekpo AS p ON p~matnr = c~matnr
                    AND p~werks = c~werks
     WHERE p~knttp = @iv_knttp
       AND p~loekz = @iv_loekz
       AND p~elikz = @abap_true
     GROUP BY p~matnr, p~werks
      INTO TABLE @rt_pedidos.
  ENDMETHOD.


  METHOD ler_remessas.
    CHECK it_centros IS NOT INITIAL.
    SELECT l~matnr, l~werks, SUM( lfimg )
      FROM @it_centros AS c
      JOIN lips AS l ON l~matnr = c~matnr
                    AND l~werks = c~werks
     WHERE l~wbsta <> @iv_wbsta
     GROUP BY l~matnr, l~werks
      INTO TABLE @rt_remessas.
  ENDMETHOD.


  METHOD ler_reservas.
    CHECK it_centros IS NOT INITIAL.
    SELECT r~matnr, r~werks, SUM( r~bdmng )
      FROM @it_centros AS c
      JOIN resb AS r ON r~matnr = c~matnr
                    AND r~werks = c~werks
     WHERE r~kzear <> @abap_true
       AND r~xloek <> @abap_true
     GROUP BY r~matnr, r~werks
      INTO TABLE @rt_reservas.
  ENDMETHOD.


  METHOD zif_agco~carregar_dados.

    MESSAGE s003(zpmm_agco).
    GET RUN TIME FIELD DATA(lv_rtime_ini).

    "Carrega todos parceiros configurados (AGCO no caso)
    lt_parceiros = ler_parceiros( ).
    IF lt_parceiros IS NOT INITIAL.

      MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( lt_parceiros ) ) 'PARCEIROS'.

      "Carrega materiais por tipo e fornecedor
      m_inventario-materiais = ler_materiais( it_tipos = rl_matkl
                                              it_fornecedores = rl_mfrnr ).

      IF m_inventario-materiais IS NOT INITIAL.

        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-materiais ) ) 'MATERIAIS'.
        "Carrega materiais por centro
        m_inventario-centros = ler_centros( it_materiais = m_inventario-materiais
                                            it_centros = criar_range_centros( lt_parceiros ) ).

        IF m_inventario-centros IS NOT INITIAL.

          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-centros ) ) 'CENTROS'.

          "Carrega materiais por depósito
          m_inventario-depositos = ler_depositos( it_centros = m_inventario-centros
                                                  it_lgort = rl_lgort
                                                  iv_ano = lv_ano
                                                  iv_mes = lv_mes ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-depositos ) ) 'DEPÓSITOS'.

          "Carrega reservas
          m_inventario-reservas = ler_reservas( m_inventario-centros ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-reservas ) ) 'DESCRIÇÕES'.

          "Carrega remessas
          m_inventario-remessas = ler_remessas( it_centros = m_inventario-centros
                                                iv_wbsta = lv_wbsta ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-remessas ) ) 'REMESSAS'.

          "Carrega pedidos de compras
          m_inventario-pedidos = ler_pedidos( it_centros = m_inventario-centros
                                              iv_knttp = lv_knttp
                                              iv_loekz = lv_loekz ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-pedidos ) ) 'PEDIDOS'.

          "Carrega notas fiscais
          m_inventario-notas = ler_notas( it_centros = m_inventario-centros
                                          it_nftype = rl_nftype
                                          iv_doctyp = lv_doctyp
                                          iv_direct = lv_direct ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_inventario-notas ) ) 'NOTAS'.

          GET RUN TIME FIELD DATA(lv_rtime_fim).
          v_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
          MESSAGE s013(zpmm_agco) WITH v_rtime.

        ELSE.
          RAISE EXCEPTION TYPE zcx_agco MESSAGE e016(zpmm_agco).
        ENDIF.

      ELSE.
        RAISE EXCEPTION TYPE zcx_agco MESSAGE e015(zpmm_agco).
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_agco MESSAGE e014(zpmm_agco).
    ENDIF.

  ENDMETHOD.


  METHOD zif_agco~definir_criterios.

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

    "Definir período
    lv_ano = sy-datum+0(4).
    lv_mes = sy-datum+4(2).
    lv_wbsta = VALUE knttp( lt_constantes[ name = |ZAGCO_ID_WBSTA| type = |P| numb = |0000| ]-low DEFAULT |C| ).

    "Definir pedidos de compras
    lv_knttp = VALUE knttp( lt_constantes[ name = |ZAGCO_ID_KNTTP| type = |P| numb = |0000| ]-low DEFAULT |H| ).
    lv_loekz = VALUE knttp( lt_constantes[ name = |ZAGCO_ID_LOEKZ| type = |P| numb = |0000| ]-low DEFAULT |L| ).

    "Definir notas fiscais
    DATA(lr_nftype) = criar_range( iv_name = |ZAGCO_ID_NFTYPE| iv_data_element = |J_1BNFTYPE| ).
    ASSIGN lr_nftype->* TO FIELD-SYMBOL(<fs_nftype>).
    rl_nftype = <fs_nftype>.

    lv_doctyp = VALUE knttp( lt_constantes[ name = |ZAGCO_ID_DOCTYP| type = |P| numb = |0000| ]-low DEFAULT |6| ).
    lv_direct = VALUE knttp( lt_constantes[ name = |ZAGCO_ID_DIRECT| type = |P| numb = |0000| ]-low DEFAULT |2| ).
  ENDMETHOD.


  METHOD zif_agco~preencher_saida.

    TYPES:
      BEGIN OF ty_s_sdata,
        part_number                    TYPE c LENGTH 50,
        available_quantity             TYPE c LENGTH 10,
        on_order_quantity              TYPE c LENGTH 10,
        reserved_quantity_work_orders  TYPE c LENGTH 10,
        reserved_quantity_part_tickets TYPE c LENGTH 10,
        open_customer_orders           TYPE c LENGTH 10,
        quantity_returned_by_dealer    TYPE c LENGTH 10,
        quantity_returned_by_customer1 TYPE c LENGTH 10,
        quantity_returned_by_customer  TYPE c LENGTH 10,
        quantity_returned_total        TYPE c LENGTH 10,
      END OF ty_s_sdata,
      ty_t_log_hdr TYPE SORTED TABLE OF zmm_agco_log_hdr
                   WITH UNIQUE KEY interface cnpj data hora rastreio,
      ty_t_log_itm TYPE SORTED TABLE OF zmm_agco_log_itm
                   WITH UNIQUE KEY interface cnpj data hora rastreio item.

    DATA:
      lt_log_hdr TYPE ty_t_log_hdr,
      lt_log_itm TYPE ty_t_log_itm.

    MESSAGE s008(zpmm_agco).

    GET RUN TIME FIELD DATA(lv_rtime_ini).

    TRY.

        MESSAGE s009(zpmm_agco) WITH 'ZCO_SI_INVENTORY_OUTBOUND'.
        DATA(lo_sender) = NEW zco_si_inventory_outbound( ).
        DATA(ls_saida) = VALUE zsend_inventory_out(
                                send_inventory_out = VALUE #( token = v_token
                                                               data = VALUE #(
                                               extraction_date_time = ler_timestamp( ) ) ) ) .

        LOOP AT m_inventario-centros ASSIGNING FIELD-SYMBOL(<fs_key>)
                                GROUP BY ( werks = <fs_key>-werks
                                            size = GROUP SIZE
                                           index = GROUP INDEX )
                                ASSIGNING FIELD-SYMBOL(<fs_member>).

          ls_saida-send_inventory_out-data-dealer_legal_number = formatar_cnpj( CONV #( lt_parceiros[ partner = <fs_member>-werks ]-taxnum ) ).

          MESSAGE s019(zpmm_agco) WITH <fs_member>-size |inventários| ls_saida-send_inventory_out-data-dealer_legal_number.

          LOOP AT GROUP <fs_member> ASSIGNING FIELD-SYMBOL(<fs_centro>).

            ASSIGN m_inventario-centros\material[ <fs_centro> ] TO FIELD-SYMBOL(<fs_material>).

            DATA(ls_peca) = VALUE zpart(
                              LET lv_labst = REDUCE labst(
                                               INIT l_labst TYPE labst
                                                FOR <fs_deposito> IN m_inventario-centros\deposito[ <fs_centro> ]
                                               NEXT l_labst = l_labst + <fs_deposito>-labst )
                                  lv_bdmng = REDUCE bdmng(
                                               INIT l_bdmng  TYPE bdmng
                                                FOR <fs_reservas> IN  m_inventario-centros\reserva[ <fs_centro> ]
                                               NEXT l_bdmng  = l_bdmng  + <fs_reservas>-bdmng  )
                                  lv_lfimg = REDUCE lfimg(
                                               INIT l_lfimg TYPE lfimg
                                                FOR <fs_remessa>  IN  m_inventario-centros\remessa[ <fs_centro> ]
                                               NEXT l_lfimg = l_lfimg + <fs_remessa>-lfimg )
                                  lv_menge = REDUCE menge_d(
                                               INIT l_menge TYPE menge_d
                                                FOR <fs_pedido> IN  m_inventario-centros\pedido[ <fs_centro> ]
                                               NEXT l_menge = l_menge + <fs_pedido>-menge )
                                  lv_meng2 = REDUCE menge_d(
                                               INIT l_menge TYPE menge_d
                                                FOR <fs_nota> IN  m_inventario-centros\nota[ <fs_centro> ]
                                               NEXT l_menge = l_menge + <fs_nota>-menge )
                               IN
                               part_number = <fs_centro>-matnr
                        available_quantity = lv_labst - lv_bdmng - lv_lfimg
                         on_order_quantity = lv_menge
             reserved_quantity_work_orders = lv_bdmng
               quantity_returned_by_dealer = lv_meng2 ).

            IF v_teste IS INITIAL.
              ls_saida-send_inventory_out-data-part = ls_peca.
              DATA: lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id.

              lo_sender->si_inventory_outbound( EXPORTING output = ls_saida
                                                IMPORTING input = DATA(ls_input)  ).

              lo_protocol_messageid ?= lo_sender->get_protocol( if_wsprotocol=>message_id ).

              lt_log_hdr = VALUE ty_t_log_hdr(
                            BASE lt_log_hdr
                              (  interface = |01|
                                      cnpj = lt_parceiros[ 1 ]-taxnum
                                      data = v_data
                                      hora = v_hora
                                  rastreio = ls_input-response_inventory_out-meta-tracking_id
                                    status = ls_input-response_inventory_out-meta-status
                                message_id = lo_protocol_messageid->get_message_id( )
                                   tamanho = 140
                                     sdata = CONV ty_s_sdata( CORRESPONDING #( ls_saida-send_inventory_out-data-part ) ) ) ).

              IF ls_input-response_inventory_out-meta-status <> |200| AND
                 ls_input-response_inventory_out-meta-status <> |201|.

                lt_log_itm = VALUE ty_t_log_itm(
                              BASE lt_log_itm
                               FOR i = 1 THEN i + 1 UNTIL i > lines( ls_input-response_inventory_out-errors )
                               LET ls_error = ls_input-response_inventory_out-errors[ i ]
                                IN (
                                  interface = |01|
                                       cnpj = lt_parceiros[ 1 ]-taxnum
                                       data = v_data
                                       hora = v_hora
                                   rastreio = ls_input-response_inventory_out-meta-tracking_id
                                       item = i
                                      campo = ls_error-source-pointer
                                      valor = ls_error-source-value
                                    detalhe = ls_error-detail
                                       erro = ls_error-error_code ) ).

              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        MESSAGE s010(zpmm_agco).

        IF v_teste IS INITIAL.

          INSERT zmm_agco_log_hdr FROM TABLE lt_log_hdr.
          INSERT zmm_agco_log_itm FROM TABLE lt_log_itm.
          COMMIT WORK.

        ENDIF.

      CATCH cx_ai_system_fault INTO DATA(lo_exception).

    ENDTRY.


    GET RUN TIME FIELD DATA(lv_rtime_fim).
    v_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
    MESSAGE s013(zpmm_agco) WITH v_rtime.

  ENDMETHOD.
ENDCLASS.
