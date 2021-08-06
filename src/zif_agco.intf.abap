interface ZIF_AGCO
  public .


  types:
    BEGIN OF ty_s_tvarvc,
      name TYPE rvari_vnam,
      type TYPE rsscr_kind,
      numb TYPE tvarv_numb,
      sign TYPE tvarv_sign,
      opti TYPE tvarv_opti,
      low  TYPE rvari_val_255,
      high TYPE rvari_val_255,
    END OF ty_s_tvarvc .
  types:
    ty_t_tvarvc TYPE SORTED TABLE OF ty_s_tvarvc
                           WITH UNIQUE KEY name type numb .
  types:
    BEGIN OF ty_s_description,
      matnr TYPE matnr,
      matkl TYPE matkl,
    END OF ty_s_description .
  types:
    BEGIN OF ty_s_material,
      matnr TYPE matnr,
      meins TYPE meins,
      mfrpn TYPE mfrpn,
    END OF ty_s_material .
  types:
    BEGIN OF ty_s_centro,
      matnr TYPE matnr,
      werks TYPE werks_d,
    END OF ty_s_centro .
  types:
    ty_t_centros TYPE SORTED TABLE OF ty_s_centro
                   WITH UNIQUE KEY matnr werks
                   WITH NON-UNIQUE SORTED KEY centro COMPONENTS werks .
  types:
    ty_t_description TYPE SORTED TABLE OF ty_s_description WITH UNIQUE KEY matnr .
  types:
    ty_t_materiais TYPE SORTED TABLE OF ty_s_material WITH UNIQUE KEY matnr .
  types:
    ty_r_matkl TYPE RANGE OF matkl .
  types:
    ty_r_mfrnr TYPE RANGE OF mfrnr .
  types:
    ty_r_werks TYPE RANGE OF werks_d .
  types:
    ty_r_lgort TYPE RANGE OF lgort_d .
  types:
    ty_r_nftype TYPE RANGE OF j_1bnftype .
  types:
    ty_r_matnr type RANGE OF matnr .
  types:
    BEGIN OF ty_s_parceiro,
      partner TYPE bu_partner,
      taxnum  TYPE bptaxnum,
    END OF ty_s_parceiro .
  types:
    ty_t_parceiros TYPE SORTED TABLE OF ty_s_parceiro WITH UNIQUE KEY partner
                   WITH NON-UNIQUE SORTED KEY cnpj COMPONENTS taxnum .

  data R_MATNR type TY_R_MATNR .
  data R_MATKL type TY_R_MATKL .
  data R_MFRNR type TY_R_MFRNR .
  data T_CONSTANTES type TY_T_TVARVC .
  data R_LGORT type TY_R_LGORT .
  data R_NFTYPE type TY_R_NFTYPE .
  data T_PARCEIROS type TY_T_PARCEIROS .
  data V_TOKEN type STRING .
  data V_DATA type DATUM .
  data V_HORA type UZEIT .
  data V_MESES type I .
  data R_WERKS type TY_R_WERKS .
  data:
    v_rtime TYPE p DECIMALS 3 .
  data V_TESTE type ABAP_BOOL .

  methods LER_PARCEIROS
    returning
      value(RT_PARCEIROS) type TY_T_PARCEIROS .
  methods PROCESSAR
    importing
      !IT_WERKS type TY_R_WERKS optional
      !IV_TESTE type ABAP_BOOL optional .
  methods CARREGAR_DADOS
    raising
      ZCX_AGCO .
  methods LER_MATERIAIS
    importing
      !IT_TIPOS type TY_R_MATKL
      !IT_FORNECEDORES type TY_R_MFRNR
      !IT_MATERIAIS type TY_R_MATNR optional
    returning
      value(RT_MATERIAIS) type TY_T_MATERIAIS .
  methods LER_CONSTANTES
    returning
      value(RT_CONSTANTES) type TY_T_TVARVC
    raising
      ZCX_AGCO .
  methods PREENCHER_SAIDA
    raising
      ZCX_AGCO
      CX_AI_SYSTEM_FAULT
      CX_AI_APPLICATION_FAULT
      CX_BS_SOA_EXCEPTION .
  methods CRIAR_RANGE
    importing
      !IV_NAME type RVARI_VNAM
      !IV_DATA_ELEMENT type FIELDNAME
    returning
      value(RT_RANGE) type ref to DATA .
  methods LER_TIMESTAMP
    returning
      value(R_TIMESTAMP) type XSDDATETIME_Z
    raising
      CX_BS_SOA_EXCEPTION .
  methods DEFINIR_CRITERIOS
    importing
      !IT_WERKS type TY_R_WERKS optional .
  methods ENVIAR
    importing
      !IS_OUTPUT type ANY .
  methods LER_FORNECEDOR_EDI
    importing
      !IV_FORNECEDOR type ZDESD_EDIFORN .
  methods FORMATAR_CNPJ
    importing
      !IV_INPUT type PBR99_CGC
    returning
      value(R_OUTPUT) type PBR99_CGC .
  methods LER_CENTROS
    importing
      !IT_MATERIAIS type TY_T_MATERIAIS
      !IT_CENTROS type TY_R_WERKS
    returning
      value(RT_CENTROS) type TY_T_CENTROS .
  methods AUTENTICAR
    raising
      ZCX_AGCO
      CX_AI_SYSTEM_FAULT
      CX_AI_APPLICATION_FAULT .
  methods LER_TIMESTAMP_LOCAL
    importing
      !R_TIMESTAMP type XSDDATETIME_Z .
  methods CRIAR_RANGE_CENTROS
    importing
      !IT_PARCEIROS type TY_T_PARCEIROS
    returning
      value(RT_CENTROS) type TY_R_WERKS .
  methods DEFINIR_MODO_TESTE
    importing
      !IV_TESTE type ABAP_BOOL optional .
  methods GRAVAR_LOG
    importing
      !IV_CNPJ type ZDEMM_AGCO_DLN
      !IV_MESSAGE_ID type SXMSMGUID
      !IS_OUTPUT type ref to DATA
      !IS_INPUT type ref to DATA .
  methods DEFINIR_MATERIAIS
    importing
      !IT_MATNR type TY_R_MATNR .
endinterface.
