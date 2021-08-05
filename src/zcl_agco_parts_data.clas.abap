CLASS zcl_agco_parts_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_agco_global
  CREATE PUBLIC .

  PUBLIC SECTION.

    ALIASES processar
      FOR zif_agco~processar .
    ALIASES ty_r_werks
      FOR zif_agco~ty_r_werks .
    ALIASES ty_t_centros
      FOR zif_agco~ty_t_centros .
    ALIASES ty_t_materiais
      FOR zif_agco~ty_t_materiais .

    TYPES:
      BEGIN OF ty_s_descricao,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF ty_s_descricao .
    TYPES:
      ty_t_descricoes TYPE SORTED TABLE OF ty_s_descricao WITH UNIQUE KEY matnr .
    TYPES:
      BEGIN OF ty_s_avaliacao,
        matnr TYPE matnr,
        bwkey TYPE bwkey,
        verpr TYPE p LENGTH 14 DECIMALS 2,
        peinh TYPE int4,
      END OF ty_s_avaliacao .
    TYPES:
      ty_t_avaliacoes TYPE SORTED TABLE OF ty_s_avaliacao WITH UNIQUE KEY matnr bwkey .
    TYPES:
      BEGIN OF ty_s_documento,
        matnr TYPE matnr,
        werks TYPE werks_d,
        budat TYPE budat,
      END OF ty_s_documento .
    TYPES:
      ty_t_documentos TYPE SORTED TABLE OF ty_s_documento WITH UNIQUE KEY matnr .
    TYPES:
      BEGIN OF ty_s_abertos,
        matnr TYPE matnr,
        werks TYPE werks_d,
        menge TYPE menge_d,
        glmng TYPE glmng,
      END OF ty_s_abertos .
    TYPES:
      ty_t_abertos TYPE SORTED TABLE OF ty_s_abertos WITH UNIQUE KEY matnr .
    TYPES:
      ty_s_saida TYPE TABLE OF zagcos_parts_data WITH DEFAULT KEY .
    TYPES:
      BEGIN OF MESH ty_m_pecas,
        materiais  TYPE ty_t_materiais  ASSOCIATION centro TO centros
                                                 ON matnr = matnr  USING KEY primary_key
                                        ASSOCIATION descricao TO descricoes
                                                 ON matnr = matnr USING KEY primary_key,
        centros    TYPE ty_t_centros    ASSOCIATION material TO materiais
                                                 ON matnr = matnr USING KEY primary_key
                                        ASSOCIATION descricao TO descricoes
                                                 ON matnr = matnr USING KEY primary_key
                                        ASSOCIATION avaliacao TO avaliacoes
                                                 ON matnr = matnr
                                                AND bwkey = werks USING KEY primary_key
                                        ASSOCIATION aberto TO abertos
                                                 ON matnr = matnr USING KEY primary_key
                                        ASSOCIATION documento TO documentos
                                                 ON matnr = matnr USING KEY primary_key,
        descricoes TYPE ty_t_descricoes,
        avaliacoes TYPE ty_t_avaliacoes,
        abertos    TYPE ty_t_abertos,
        documentos TYPE ty_t_documentos,

      END OF MESH ty_m_pecas .

    DATA m_pecas TYPE ty_m_pecas .

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
  aliases R_WERKS
    for ZIF_AGCO~R_WERKS .
  aliases V_DATA
    for ZIF_AGCO~V_DATA .
  aliases V_HORA
    for ZIF_AGCO~V_HORA .
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
  aliases PREENCHER_SAIDA
    for ZIF_AGCO~PREENCHER_SAIDA .
  aliases TY_R_MATKL
    for ZIF_AGCO~TY_R_MATKL .
  aliases TY_R_MFRNR
    for ZIF_AGCO~TY_R_MFRNR .
  aliases TY_R_NFTYPE
    for ZIF_AGCO~TY_R_NFTYPE .
  aliases TY_S_DESCRIPTION
    for ZIF_AGCO~TY_S_DESCRIPTION .

  methods LER_DESCRICOES
    importing
      !IT_MATERIAIS type TY_T_MATERIAIS
    returning
      value(RT_DESCRICOES) type TY_T_DESCRICOES .
  methods LER_DOCUMENTOS
    importing
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_DOCUMENTOS) type TY_T_DOCUMENTOS .
  methods LER_AVALIACOES
    importing
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_AVALIACOES) type TY_T_AVALIACOES .
  methods LER_PO_ABERTAS
    importing
      !IT_CENTROS type TY_T_CENTROS
    returning
      value(RT_ABERTOS) type TY_T_ABERTOS .
ENDCLASS.



CLASS ZCL_AGCO_PARTS_DATA IMPLEMENTATION.


  METHOD ler_avaliacoes.
    CHECK it_centros IS NOT INITIAL.
    SELECT m~matnr, m~bwkey, SUM( m~verpr ), SUM( m~peinh )
      FROM @it_centros AS c
      JOIN mbew AS m ON m~matnr = c~matnr
                    AND m~bwkey = c~werks
     GROUP BY m~matnr, m~bwkey
      INTO TABLE @rt_avaliacoes.
*
*
*    SELECT matnr, SUM( cast( verpr as fltp )  ) , SUM( peinh ), div( cast( verpr as dec ), peinh ) AS custo
*      FROM @rt_avaliacoes AS r
*     GROUP BY matnr.


    ENDMETHOD.


  method LER_DESCRICOES.
    select matnr, maktx
      into table @rt_descricoes
      from makt
       FOR ALL ENTRIES IN @it_materiais
     where matnr = @it_materiais-matnr
       and spras = @sy-langu.
  endmethod.


  METHOD ler_documentos.
    CHECK it_centros IS NOT INITIAL.
    SELECT s~matnr, s~werks, MIN( budat_mkpf ) AS budat
      FROM @it_centros AS c
      JOIN mseg AS s ON s~matnr = c~matnr
                    AND s~werks = c~werks
     GROUP BY s~matnr, s~werks
      INTO TABLE @rt_documentos.
  ENDMETHOD.


  METHOD ler_po_abertas.
    CHECK it_centros IS NOT INITIAL.
    SELECT c~matnr, c~werks, SUM( t~menge ), SUM( t~glmng )
      FROM @it_centros AS c
      JOIN ekpo AS p ON p~matnr = c~matnr
                    AND p~werks = c~werks
      JOIN eket AS t ON t~ebeln = p~ebeln
                    AND t~ebelp = p~ebelp
     WHERE t~menge > t~glmng
     GROUP BY c~matnr, c~werks
      INTO TABLE @rt_abertos.
  ENDMETHOD.


  METHOD zif_agco~carregar_dados.
    MESSAGE s003(zpmm_agco).
    GET RUN TIME FIELD DATA(lv_rtime_ini).
    "Carrega todos parceiros configurados (AGCO no caso)
    lt_parceiros = ler_parceiros( ).
    IF lt_parceiros IS NOT INITIAL.

      MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( lt_parceiros ) ) 'PARCEIROS'.

      "Carrega materiais por tipo e fornecedor
      m_pecas-materiais = ler_materiais( it_tipos = rl_matkl
                                         it_fornecedores = rl_mfrnr ).
      IF m_pecas-materiais IS NOT INITIAL.

        MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-materiais ) ) 'MATERIAIS'.

        m_pecas-centros = ler_centros( it_materiais = m_pecas-materiais
                                       it_centros = criar_range_centros( lt_parceiros ) ).

        IF m_pecas-centros IS NOT INITIAL.

          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-centros ) ) 'CENTROS'.

          m_pecas-descricoes = ler_descricoes( m_pecas-materiais ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-descricoes ) ) 'DESCRIÇÕES'.

          m_pecas-avaliacoes = ler_avaliacoes( m_pecas-centros ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-avaliacoes ) ) 'AVALIAÇÕES'.

          m_pecas-abertos    = ler_po_abertas( m_pecas-centros  ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-abertos ) ) 'PEDIDOS EM ABERTO'.

          m_pecas-documentos = ler_documentos( m_pecas-centros  ).
          MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( m_pecas-documentos ) ) 'DOCUMENTOS DE COMPRA'.

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
    message s004(zpmm_agco).
    "Definir tipo de material e quais fornecedores
    IF it_werks IS SUPPLIED.
      r_werks = it_werks.
    ENDIF.
    DATA(lr_matkl) = criar_range( iv_name = |ZAGCO_MATKL| iv_data_element = |MATKL| ).
    ASSIGN lr_matkl->* TO FIELD-SYMBOL(<fs_matkl>).
    rl_matkl = <fs_matkl>.

    DATA(lr_mfrnr) = criar_range( iv_name = |ZAGCO_MFRNR| iv_data_element = |MFRNR| ).
    ASSIGN lr_mfrnr->* TO FIELD-SYMBOL(<fs_mfrnr>).
    rl_mfrnr = <fs_mfrnr>.
  ENDMETHOD.


  METHOD zif_agco~preencher_saida.
    TYPES:
      BEGIN OF ty_s_sdata,
        part_number                    TYPE c LENGTH 50,
        agco_part_number               TYPE c LENGTH 50,
        original_part                  TYPE c LENGTH 1,
        description                    TYPE c LENGTH 60,
        dealer_note                    TYPE c LENGTH 60,
        stockable                      TYPE c LENGTH 1,
        last_sale_date                 TYPE c LENGTH 8,
        main_bin_location              TYPE c LENGTH 60,
        alt_bin_location               TYPE c LENGTH 60,
        minimum_threshold              TYPE c LENGTH 12,
        maximum_threshold              TYPE c LENGTH 12,
        preferred_supplier_legal_numbe TYPE c LENGTH 50,
        preferred_supplier_legal_name  TYPE c LENGTH 100,
        net_price                      TYPE c LENGTH 16,
        average_cost                   TYPE c LENGTH 16,
        unit_of_measure                TYPE c LENGTH 3,
        dealer_parts_per_package       TYPE c LENGTH 16,
        currency_code                  TYPE c LENGTH 3,
        last_purchase_price            TYPE c LENGTH 16,
        open_purchase_orders           TYPE c LENGTH 16,
        vendor_package_quantity        TYPE c LENGTH 10,
        segmentation_code01            TYPE c LENGTH 30,
        segmentation_code02            TYPE c LENGTH 30,
        segmentation_code03            TYPE c LENGTH 30,
        segmentation_code04            TYPE c LENGTH 30,
        first_available_date           TYPE c LENGTH 8,
        first_purchase                 TYPE c LENGTH 1,
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

        MESSAGE s009(zpmm_agco) WITH 'ZCO_SI_PARTS_OUTBOUND'.
        DATA(lo_sender) = NEW zco_si_parts_outbound( ).

        DATA(ls_saida) = VALUE zsend_parts(
                                send_parts = VALUE #(
                                             token = v_token
                                              data = VALUE #(
                              extraction_date_time = ler_timestamp( ) ) ) ).

        IF m_pecas-centros IS NOT INITIAL.

          LOOP AT m_pecas-centros ASSIGNING FIELD-SYMBOL(<fs_key>)
                                  GROUP BY ( werks = <fs_key>-werks
                                              size = GROUP SIZE
                                             index = GROUP INDEX )
                                  ASSIGNING FIELD-SYMBOL(<fs_member>).

            ls_saida-send_parts-data-dealer_legal_number = formatar_cnpj( CONV #( lt_parceiros[ partner = <fs_member>-werks ]-taxnum ) ).
            MESSAGE s019(zpmm_agco) WITH <fs_member>-size |peças| ls_saida-send_parts-data-dealer_legal_number.

            LOOP AT GROUP <fs_member> ASSIGNING FIELD-SYMBOL(<fs_centro>).

              ASSIGN m_pecas-centros\material[ <fs_centro> ] TO FIELD-SYMBOL(<fs_material>).

              DATA(ls_peca) = VALUE zparts_data_part(
                                LET lv_descricao = VALUE #( m_pecas-materiais\descricao[ <fs_material> ]-maktx OPTIONAL )
                                    ls_avaliacao = REDUCE ty_s_avaliacao(
                                                     INIT l_avaliacao TYPE ty_s_avaliacao
                                                      FOR GROUPS <fs_grupo> OF <fs_chave> IN m_pecas-centros\avaliacao[ <fs_centro> ]
                                                          GROUP BY ( matnr = <fs_chave>-matnr count = GROUP SIZE )
                                                      NEXT l_avaliacao-matnr = <fs_grupo>-matnr
                                                           l_avaliacao-bwkey = abap_false
                                                           l_avaliacao-peinh = REDUCE #( INIT l_peinh TYPE int4
                                                                                  FOR m IN GROUP <fs_grupo>
                                                                                 NEXT l_peinh = l_peinh + m-peinh )
                                                           l_avaliacao-verpr = REDUCE #( INIT l_verpr TYPE ty_s_avaliacao-verpr
                                                                                  FOR m IN GROUP <fs_grupo>
                                                                                 NEXT l_verpr = l_verpr + m-verpr ) )
                                      ls_abertos = VALUE #( m_pecas-centros\aberto[ <fs_centro> ] OPTIONAL )
                              lv_disponibilidade = VALUE #( m_pecas-centros\documento[ <fs_centro> ]-budat DEFAULT |19000101| )
                                 IN
                                     part_number = <fs_material>-matnr
                                agco_part_number = <fs_material>-mfrpn
                                   original_part = abap_true
                                     description = lv_descricao
                                       stockable = abap_true
                                  last_sale_date = |19000101|
                  preferred_supplier_legal_numbe = |55.962.369/0009-24|
                                    average_cost = COND #( WHEN ls_avaliacao-peinh > 0 THEN ls_avaliacao-verpr / ls_avaliacao-peinh )
                                 unit_of_measure = <fs_material>-meins
                        dealer_parts_per_package = 1
                                   currency_code = |BRL|
                            open_purchase_orders = ls_abertos-menge - ls_abertos-glmng
                            first_available_date = lv_disponibilidade  )  .

              IF v_teste IS INITIAL.

                ls_saida-send_parts-data-part = ls_peca.

                DATA: lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id.
                lo_sender->si_parts_outbound( EXPORTING output = ls_saida
                                              IMPORTING input = DATA(ls_input)  ).
                lo_protocol_messageid ?= lo_sender->get_protocol( if_wsprotocol=>message_id ).

                lt_log_hdr = VALUE ty_t_log_hdr(
                              BASE lt_log_hdr
                                (  interface = |02|
                                        cnpj = lt_parceiros[ 1 ]-taxnum
                                        data = v_data
                                        hora = v_hora
                                    rastreio = ls_input-response_parts-meta-tracking_id
                                      status = ls_input-response_parts-meta-status
                                  message_id = lo_protocol_messageid->get_message_id( )
                                     tamanho = 749
                                       sdata = CONV ty_s_sdata( CORRESPONDING #( ls_saida-send_parts-data-part ) ) ) ).

                IF ls_input-response_parts-meta-status <> |200| AND
                   ls_input-response_parts-meta-status <> |201|.

                  lt_log_itm = VALUE ty_t_log_itm(
                                BASE lt_log_itm
                                 FOR i = 1 THEN i + 1 UNTIL i > lines( ls_input-response_parts-errors )
                                 LET ls_error = ls_input-response_parts-errors[ i ]
                                  IN (
                                    interface = |02|
                                         cnpj = lt_parceiros[ 1 ]-taxnum
                                         data = v_data
                                         hora = v_hora
                                     rastreio = ls_input-response_parts-meta-tracking_id
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

        ELSE.

          RAISE EXCEPTION TYPE zcx_agco MESSAGE e018(zpmm_agco).

        ENDIF.

      CATCH cx_ai_system_fault INTO DATA(lo_exception).

    ENDTRY.


    GET RUN TIME FIELD DATA(lv_rtime_fim).
    v_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
    MESSAGE s013(zpmm_agco) WITH v_rtime.

  ENDMETHOD.
ENDCLASS.
