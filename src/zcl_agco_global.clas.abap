class ZCL_AGCO_GLOBAL definition
  public
  create private

  global friends ZCL_AGCO_CUSTOMER_DATA
                 ZCL_AGCO_INVENTORY_DATA
                 ZCL_AGCO_PARTS_DATA
                 ZCL_AGCO_PO_DATA
                 ZCL_AGCO_SO_DATA .

public section.

  interfaces ZIF_AGCO .
  PROTECTED SECTION.
private section.

  aliases LT_CONSTANTES
    for ZIF_AGCO~T_CONSTANTES .
  aliases LT_PARCEIROS
    for ZIF_AGCO~T_PARCEIROS .
  aliases R_MATNR
    for ZIF_AGCO~R_MATNR .
  aliases R_WERKS
    for ZIF_AGCO~R_WERKS .
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
  aliases DEFINIR_MODO_TESTE
    for ZIF_AGCO~DEFINIR_MODO_TESTE .
  aliases ENVIAR
    for ZIF_AGCO~ENVIAR .
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
  aliases PROCESSAR
    for ZIF_AGCO~PROCESSAR .
  aliases TY_R_LGORT
    for ZIF_AGCO~TY_R_LGORT .
  aliases TY_R_MATKL
    for ZIF_AGCO~TY_R_MATKL .
  aliases TY_R_MFRNR
    for ZIF_AGCO~TY_R_MFRNR .
  aliases TY_R_NFTYPE
    for ZIF_AGCO~TY_R_NFTYPE .
  aliases TY_R_WERKS
    for ZIF_AGCO~TY_R_WERKS .
  aliases TY_S_CENTRO
    for ZIF_AGCO~TY_S_CENTRO .
  aliases TY_S_DESCRIPTION
    for ZIF_AGCO~TY_S_DESCRIPTION .
  aliases TY_S_MATERIAL
    for ZIF_AGCO~TY_S_MATERIAL .
  aliases TY_S_TVARVC
    for ZIF_AGCO~TY_S_TVARVC .
  aliases TY_T_DESCRIPTION
    for ZIF_AGCO~TY_T_DESCRIPTION .
  aliases TY_T_MATERIAIS
    for ZIF_AGCO~TY_T_MATERIAIS .
  aliases TY_T_TVARVC
    for ZIF_AGCO~TY_T_TVARVC .
ENDCLASS.



CLASS ZCL_AGCO_GLOBAL IMPLEMENTATION.


  METHOD zif_agco~autenticar.
    MESSAGE s005(zpmm_agco).
    GET RUN TIME FIELD DATA(lv_rtime_ini).
    "Ler usuário e senha para autenticar na AGCO
    DATA(lv_username) = VALUE string( lt_constantes[ name = |ZAGCO_USERNAME| type = |P| numb = |0000| ]-low OPTIONAL ).
    DATA(lv_password) = VALUE string( lt_constantes[ name = |ZAGCO_PASSWORD| type = |P| numb = |0000| ]-low OPTIONAL ).


    DATA(lt_aut_request) = VALUE zmt_authentication_request_out(
                                  mt_authentication_request_out = VALUE zdt_authentication_request_out(
                                                       username = lv_username
                                                       password = lv_password ) ).


    DATA(lo_authentication) = NEW zco_si_authentication_out( ).
    TRY .
        lo_authentication->si_authentication_out(
                                      EXPORTING
                                        output = lt_aut_request
                                      IMPORTING
                                        input  = DATA(lt_aut_response)  ).

        v_token = lt_aut_response-mt_authentication_response_out-token_id.
        IF v_token IS INITIAL.
          RAISE EXCEPTION TYPE zcx_agco MESSAGE e007(zpmm_agco).
        ENDIF.
        MESSAGE s006(zpmm_agco).
        GET RUN TIME FIELD DATA(lv_rtime_fim).
        v_rtime = ( lv_rtime_fim - lv_rtime_ini ) / 1000000 .
        MESSAGE s013(zpmm_agco) WITH v_rtime.
      CATCH cx_ai_application_fault INTO DATA(lr_error) .
        DATA(lv_erro) = lr_error->get_text( ).
        MESSAGE e000(zpmm_agco) WITH lv_erro.
      CATCH cx_ai_system_fault INTO DATA(lr_system_fault).
        lv_erro = lr_system_fault->errortext.
        MESSAGE e000(zpmm_agco) WITH lv_erro.
    ENDTRY.
  ENDMETHOD.


  method ZIF_AGCO~CARREGAR_DADOS.
  endmethod.


  method ZIF_AGCO~CRIAR_RANGE.
    DATA:
      tl_table_range     TYPE REF TO data,
      tl_component_table TYPE abap_component_tab,
      tl_table_data      TYPE REF TO data,

      wl_line_range      TYPE REF TO data,
      wl_line_data       TYPE REF TO data,
      wl_componemt_struc TYPE abap_componentdescr.

    FIELD-SYMBOLS:
      <fs_table_sel> TYPE STANDARD TABLE.

** Create dynamic range table
    wl_componemt_struc-name = 'SIGN'.
    wl_componemt_struc-type ?= cl_abap_datadescr=>describe_by_name( 'SIGN' ).
    APPEND wl_componemt_struc TO tl_component_table.

    wl_componemt_struc-name = 'OPTION'.
    wl_componemt_struc-type ?= cl_abap_datadescr=>describe_by_name( 'OPTION' ).
    APPEND wl_componemt_struc TO tl_component_table.

    wl_componemt_struc-name = 'LOW'.
    wl_componemt_struc-type ?= cl_abap_datadescr=>describe_by_name( iv_data_element ).
    APPEND wl_componemt_struc TO tl_component_table.

    wl_componemt_struc-name = 'HIGH'.
    wl_componemt_struc-type ?= cl_abap_datadescr=>describe_by_name( iv_data_element ).
    APPEND wl_componemt_struc TO tl_component_table.

*->Creating Dynamic Structure based on Field Catalog
    CALL METHOD cl_abap_structdescr=>create
      EXPORTING
        p_components = tl_component_table
        p_strict     = cl_abap_structdescr=>false
      RECEIVING
        p_result     = DATA(wl_struct_descr).

**Create table type based on the structure
    CALL METHOD cl_abap_tabledescr=>create
      EXPORTING
        p_line_type  = wl_struct_descr
        p_table_kind = cl_abap_tabledescr=>tablekind_std
      RECEIVING
        p_result     = DATA(tl_dynamic_table).

    CREATE DATA rt_range TYPE HANDLE tl_dynamic_table.
    ASSIGN rt_range->* TO <fs_table_sel>.

    CREATE DATA wl_line_range TYPE HANDLE wl_struct_descr.
    ASSIGN wl_line_range->* TO FIELD-SYMBOL(<fs_line_sel>).

    TYPES:
      BEGIN OF ty_s_filter,
        name TYPE rvari_vnam,
        type TYPE rsscr_kind,
      END OF ty_s_filter,
      ty_t_filter TYPE HASHED TABLE OF ty_s_filter WITH UNIQUE KEY name type.

    DATA(lt_filter) = VALUE ty_t_filter( ( name = iv_name type = |S| ) ).
    DATA(lt_result) = FILTER #( lt_constantes IN lt_filter WHERE name = name AND type = type ).

*** Populate dynamic range
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_constante>).

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_line_sel> TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc EQ 0.
        <fs_field> = <fs_constante>-sign.
      ENDIF.
      UNASSIGN <fs_field>.

      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_line_sel> TO <fs_field>.
      IF sy-subrc EQ 0.
        <fs_field> = <fs_constante>-opti.
      ENDIF.
      UNASSIGN <fs_field>.

      ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_line_sel> TO <fs_field>.
      IF sy-subrc EQ 0.
        <fs_field> = <fs_constante>-low.
      ENDIF.
      UNASSIGN <fs_field>.

      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_line_sel> TO <fs_field>.
      IF sy-subrc EQ 0.
        <fs_field> = <fs_constante>-high.
      ENDIF.
      UNASSIGN <fs_field>.

      APPEND <fs_line_sel> TO <fs_table_sel>.

    ENDLOOP.
  endmethod.


  METHOD zif_agco~criar_range_centros.
    rt_centros = VALUE #(
                   FOR <fs_parceiro> IN it_parceiros
                ( sign = |I|
                option = |EQ|
                   low = <fs_parceiro>-partner ) ).
*    IF rt_centros[] IS INITIAL.
*      rt_centros = VALUE #( ( sign = |I| option = |CP| low = |C*| ) ).
*    ENDIF.
*    IF r_werks[] IS INITIAL.
*      rt_centros = VALUE #( ( sign = |I| option = |CP| low = |C*| ) ).
*    ELSE.
*      rt_centros = r_werks.
*    ENDIF.
  ENDMETHOD.


  method ZIF_AGCO~DEFINIR_CRITERIOS.
  endmethod.


  method ZIF_AGCO~DEFINIR_MATERIAIS.
    r_matnr = it_matnr.
  endmethod.


  METHOD zif_agco~definir_modo_teste.
    IF iv_teste IS SUPPLIED.
      v_teste = iv_teste.
      IF v_teste = abap_true.
        MESSAGE s020(zpmm_agco).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method ZIF_AGCO~ENVIAR.
  endmethod.


  METHOD zif_agco~formatar_cnpj.
    CALL FUNCTION 'HR_BR_CHECK_CGC_FORMAT'
      EXPORTING
        cgc_number               = iv_input
      IMPORTING
        cgc_number_formatted     = r_output
*        cgc_number_raw           = r_output
      EXCEPTIONS
        cgc_format_not_supported = 1
        cgc_check_digit          = 2
        OTHERS                   = 3.
  ENDMETHOD.


  method ZIF_AGCO~GRAVAR_LOG.
  endmethod.


  METHOD zif_agco~ler_centros.
    CHECK it_materiais IS NOT INITIAL AND it_centros IS NOT INITIAL.
    SELECT c~matnr, c~werks
      FROM @it_materiais AS m
      JOIN marc AS c ON c~matnr = m~matnr
     WHERE c~werks IN @it_centros
       AND c~lvorm = @abap_false
      INTO TABLE @rt_centros.
  ENDMETHOD.


  METHOD zif_agco~ler_constantes.
    MESSAGE s002(zpmm_agco).
    SELECT name, type, numb, sign, opti, low, high
      INTO TABLE @rt_constantes
      FROM tvarvc
     WHERE name LIKE 'ZAGCO%'.
    IF sy-subrc = 0.
      MESSAGE s011(zpmm_agco) WITH CONV numc10( lines( rt_constantes ) ) 'CONSTANTES'.
    ELSE.
      RAISE EXCEPTION TYPE zcx_agco MESSAGE e017(zpmm_agco).
    ENDIF.
  ENDMETHOD.


  method ZIF_AGCO~LER_FORNECEDOR_EDI.
  endmethod.


  METHOD zif_agco~ler_materiais.
    SELECT matnr, meins, mfrpn
      FROM mara
     WHERE matnr IN @it_materiais
       AND matkl IN @it_tipos
       AND mfrnr IN @it_fornecedores
       AND lvorm = @abap_false
      INTO TABLE @rt_materiais.
  ENDMETHOD.


  METHOD zif_agco~ler_parceiros.
    DATA:
      rl_taxnum  TYPE RANGE OF bptaxnum,
      rl_taxtype TYPE RANGE OF bptaxtype.

    DATA(lr_taxnum) = criar_range( iv_name = |ZAGCO_TAXNUM| iv_data_element = |BPTAXNUM| ).
    ASSIGN lr_taxnum->* TO FIELD-SYMBOL(<fs_taxnum>).
    rl_taxnum = <fs_taxnum>.

    DATA(lr_taxtype) = criar_range( iv_name = |ZAGCO_TAXTYPE| iv_data_element = |BPTAXTYPE| ).
    ASSIGN lr_taxtype->* TO FIELD-SYMBOL(<fs_taxtype>).
    rl_taxtype = <fs_taxtype>.

    SELECT partner, taxnum
      INTO TABLE @rt_parceiros
      FROM dfkkbptaxnum
     WHERE taxnum  IN @rl_taxnum
       AND taxtype IN @rl_taxtype.

    IF sy-subrc EQ 0.
      IF r_werks[] IS NOT INITIAL.
        DELETE rt_parceiros WHERE partner NOT IN r_werks.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agco~ler_timestamp.

    GET TIME STAMP FIELD DATA(lv_timestamp).
    CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo INTO DATE v_data TIME v_hora.

    NEW cl_bs_soa_convert_xsddatetime( )->map_xsddatetime_z_out(
                    EXPORTING
                      iv_date = sy-datum
                      iv_time = sy-uzeit
                      iv_timestamp = lv_timestamp
                      iv_timezone = sy-zonlo
                    IMPORTING
                      ev_xsd_datetime = r_timestamp ).
*    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP r_timestamp TIME ZONE sy-zonlo.

  ENDMETHOD.


  method ZIF_AGCO~PREENCHER_SAIDA.
  endmethod.


  METHOD zif_agco~processar.
    TRY.
        MESSAGE s001(zpmm_agco).
        definir_modo_teste( iv_teste ).
        lt_constantes = ler_constantes( ).
        definir_criterios( it_werks[] ).
        carregar_dados( ).
        autenticar( ).
        preencher_saida( ).
      CATCH zcx_agco INTO DATA(lx_agco).
        MESSAGE lx_agco TYPE 'I' DISPLAY LIKE 'E'.
      CATCH cx_ai_system_fault INTO DATA(lx_system_fault).
      CATCH cx_bs_soa_exception INTO DATA(lx_soa_exc).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
