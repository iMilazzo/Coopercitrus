class ZCL_AGCO_CUSTOMER_DATA definition
  public
  inheriting from ZCL_AGCO_GLOBAL
  create public .

public section.

  aliases ENVIAR
    for ZIF_AGCO~ENVIAR .
  aliases PREENCHER_SAIDA
    for ZIF_AGCO~PREENCHER_SAIDA .
  aliases PROCESSAR
    for ZIF_AGCO~PROCESSAR .

  types:
    BEGIN OF ty_s_cliente,
        kunnr TYPE kunnr,
        land1 TYPE land1_gp,
        name1 TYPE name1_gp,
        ort01 TYPE ort01,
        regio TYPE regio,
        stcd1 TYPE stcd1,
      END OF ty_s_cliente .
  types:
    ty_t_clientes TYPE SORTED TABLE OF ty_s_cliente WITH UNIQUE KEY kunnr .
  types:
    BEGIN OF ty_s_venda,
        kunnr    TYPE kunnr,
        cre_timestamp TYPE j_1bcre_timestamp,
      END OF ty_s_venda .
  types:
    ty_t_vendas TYPE SORTED TABLE OF ty_s_venda WITH UNIQUE KEY kunnr .
  types:
    BEGIN OF MESH ty_m_clientes,
        clientes TYPE ty_t_clientes ASSOCIATION venda TO vendas
                                             ON kunnr = kunnr USING KEY primary_key,
        vendas   TYPE ty_t_vendas,

      END OF MESH ty_m_clientes .

  data M_CLIENTES type TY_M_CLIENTES .

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
  aliases LV_CENTRO
    for ZIF_AGCO~V_CENTRO .
  aliases RL_LGORT
    for ZIF_AGCO~R_LGORT .
  aliases RL_MATKL
    for ZIF_AGCO~R_MATKL .
  aliases RL_MFRNR
    for ZIF_AGCO~R_MFRNR .
  aliases RL_NFTYPE
    for ZIF_AGCO~R_NFTYPE .
  aliases V_TOKEN
    for ZIF_AGCO~V_TOKEN .
  aliases V_DATA
    for ZIF_AGCO~V_DATA .
  aliases V_HORA
    for ZIF_AGCO~V_HORA .
  aliases AUTENTICAR
    for ZIF_AGCO~AUTENTICAR .
  aliases CARREGAR_DADOS
    for ZIF_AGCO~CARREGAR_DADOS .
  aliases CRIAR_RANGE
    for ZIF_AGCO~CRIAR_RANGE .
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
  aliases TY_T_CENTROS
    for ZIF_AGCO~TY_T_CENTROS .
  aliases TY_T_DESCRIPTION
    for ZIF_AGCO~TY_T_DESCRIPTION .
  aliases TY_T_MATERIAIS
    for ZIF_AGCO~TY_T_MATERIAIS .
  aliases TY_T_PARCEIROS
    for ZIF_AGCO~TY_T_PARCEIROS .
  aliases TY_T_TVARVC
    for ZIF_AGCO~TY_T_TVARVC .

  methods LER_CLIENTES
    returning
      value(RT_CLIENTES) type TY_T_CLIENTES .
  methods LER_ULTIMA_VENDA
    importing
      !IT_CLIENTES type TY_T_CLIENTES
    returning
      value(RT_VENDAS) type TY_T_VENDAS .
  methods CONVERTER_TIMESTAMP
    importing
      !IV_CRE_TIMESTAMP type J_1BCRE_TIMESTAMP
    returning
      value(R_TIMESTAMP) type XSDDATETIME_Z .
ENDCLASS.



CLASS ZCL_AGCO_CUSTOMER_DATA IMPLEMENTATION.


  METHOD converter_timestamp.

    DATA(lv_cre_timestamp) = iv_cre_timestamp.

    IF iv_cre_timestamp IS INITIAL.

      CONVERT DATE '19000101' TIME '010101' INTO TIME STAMP lv_cre_timestamp TIME ZONE sy-zonlo.

    ENDIF.

    NEW cl_bs_soa_convert_xsddatetime( )->map_xsddatetime_z_out(
                    EXPORTING
                      iv_timestamp = lv_cre_timestamp
                      iv_timezone = sy-zonlo
                    IMPORTING
                      ev_xsd_datetime = r_timestamp ).

  ENDMETHOD.


  METHOD LER_CLIENTES.
    SELECT kunnr, land1, name1, ort01, regio,  stcd1
      FROM kna1
      INTO TABLE @rt_clientes.
  ENDMETHOD.


  METHOD ler_ultima_venda.
    CHECK it_clientes IS NOT INITIAL.
    SELECT kunnr, MAX( cre_timestamp )
      FROM @it_clientes AS k
      JOIN j_1bnfdoc AS d ON d~parid = k~kunnr
     WHERE d~direct = 2
     GROUP BY kunnr
      INTO TABLE @rt_vendas.
  ENDMETHOD.


  METHOD zif_agco~carregar_dados.
    "Carrega todos parceiros configurados (AGCO no caso)
    lt_parceiros = ler_parceiros( ).
    m_clientes-clientes = ler_clientes( ).
    m_clientes-vendas = ler_ultima_venda( m_clientes-clientes ).
  ENDMETHOD.


  method ZIF_AGCO~DEFINIR_CRITERIOS.
    lt_constantes = ler_constantes( ).
  endmethod.


  METHOD zif_agco~preencher_saida.
    TYPES:
      BEGIN OF ty_s_sdata,
        customer_legal_number TYPE c LENGTH 50,
        customer_id           TYPE c LENGTH 50,
        customer_name         TYPE c LENGTH 100,
        country_code          TYPE c LENGTH 60,
        state_code            TYPE c LENGTH 60,
        city_name             TYPE c LENGTH 60,
        type                  TYPE c LENGTH 60,
        last_sales_date       TYPE c LENGTH 16,
      END OF ty_s_sdata,
      ty_t_log_hdr TYPE SORTED TABLE OF zmm_agco_log_hdr
                   WITH UNIQUE KEY interface cnpj data hora rastreio,
      ty_t_log_itm TYPE SORTED TABLE OF zmm_agco_log_itm
                   WITH UNIQUE KEY interface cnpj data hora rastreio item.

    DATA:
      lv_rtime   TYPE p DECIMALS 3,
      lt_log_hdr TYPE ty_t_log_hdr,
      lt_log_itm TYPE ty_t_log_itm.

    GET RUN TIME FIELD DATA(lv_rtime_ini).

    TRY.

        DATA(lo_sender) = NEW zco_si_customer_outbound( ).

        formatar_cnpj( EXPORTING
                          iv_input = CONV #( lt_parceiros[ 1 ]-taxnum )
                       IMPORTING
                          ev_formatted = DATA(lv_cnpj_formatted) ).

        DATA(ls_saida) = VALUE zsend_customer(
                 send_customer = VALUE #(
                                 token = v_token
                                  data = VALUE #(
                           dealer_legal_number = lv_cnpj_formatted
                          extraction_date_time = ler_timestamp( ) ) ) ) .

        LOOP AT m_clientes-clientes ASSIGNING FIELD-SYMBOL(<fs_cliente>).

          formatar_cnpj( EXPORTING
                            iv_input = CONV #( lt_parceiros[ 1 ]-taxnum )
                         IMPORTING
                            ev_formatted = lv_cnpj_formatted ).

          DATA(ls_customer) = VALUE zcustomer_data_customer1(
              customer_legal_number = lv_cnpj_formatted
                        customer_id = <fs_cliente>-kunnr
                      customer_name = <fs_cliente>-name1
                       country_code = <fs_cliente>-land1
                         state_code = |{ <fs_cliente>-land1 }-{ <fs_cliente>-regio }|
                          city_name = <fs_cliente>-ort01
                               type = |RC|
                    last_sales_date = converter_timestamp( VALUE #( m_clientes-clientes\venda[ <fs_cliente> ]-cre_timestamp OPTIONAL ) ) ).

          ls_saida-send_customer-data-customer = ls_customer.
          DATA: lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id.

          lo_sender->si_customer_outbound( EXPORTING output = ls_saida
                                           IMPORTING input = DATA(ls_input)  ).

          lo_protocol_messageid ?= lo_sender->get_protocol( if_wsprotocol=>message_id ).

          lt_log_hdr = VALUE ty_t_log_hdr(
                        BASE lt_log_hdr
                          (  interface = |06|
                                  cnpj = lt_parceiros[ 1 ]-taxnum
                                  data = v_data
                                  hora = v_hora
                              rastreio = ls_input-response_customer-meta-tracking_id
                                status = ls_input-response_customer-meta-status
                            message_id = lo_protocol_messageid->get_message_id( )
                               tamanho = 456
                                 sdata = CONV ty_s_sdata( CORRESPONDING #( ls_saida-send_customer-data-customer ) ) ) ).

          IF ls_input-response_customer-meta-status <> |200| AND
             ls_input-response_customer-meta-status <> |201|.

            lt_log_itm = VALUE ty_t_log_itm(
                          BASE lt_log_itm
                           FOR i = 1 THEN i + 1 UNTIL i > lines( ls_input-response_customer-errors )
                           LET ls_error = ls_input-response_customer-errors[ i ]
                            IN (
                              interface = |06|
                                   cnpj = lt_parceiros[ 1 ]-taxnum
                                   data = v_data
                                   hora = v_hora
                               rastreio = ls_input-response_customer-meta-tracking_id
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
