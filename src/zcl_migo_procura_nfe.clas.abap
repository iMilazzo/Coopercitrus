class ZCL_MIGO_PROCURA_NFE definition
  public
  final
  create public .

public section.

  methods SEARCH_NFE
    importing
      !IS_GOHEAD type GOHEAD
      !IS_GOITEM type GOITEM
    changing
      !CT_BAPIRET2 type BAPIRET2_T .
private section.

  types TY_S_SPAR type SPAR .
  types:
    ty_t_spar TYPE TABLE OF ty_s_spar WITH DEFAULT KEY .

  data LS_NF_DOC type J_1BNFDOC .

  methods BRANCH_DETERMINE
    importing
      !I_COMPANY type BUKRS optional
      !I_PLANT type WERKS_D optional
    exporting
      !E_BRANCH type J_1BBRANC_
      !E_COMPANY type BUKRS .
  methods NFTYPE_READ
    importing
      !I_NFTYPE type J_1BNFTYPE
      !I_WERKS type WERKS_D
      !I_BWART type BWART
      !I_LIFNR type LIFNR
      !I_DEFAULTS type ABAP_BOOL
    returning
      value(RS_PARAMETERS) type J_1BAA .
  methods READ_T001W
    importing
      !I_WERKS type WERKS_D
    returning
      value(RS_T001W) type T001W .
  methods READ_T156
    importing
      !I_BWART type BWART
    returning
      value(RS_T156) type T156 .
  methods NF_NUMBER_SEPARATE
    importing
      !I_REF_NUMBER type XBLNR1
      !I_NFEFLAG type ABAP_BOOL
    exporting
      !E_REF_NUMBER type XBLNR1
      !E_NF_NUMBER type J_1BNFNUMB
      !E_SERIES type J_1BSERIES
      !E_SUBSERIES type J_1BSUBSER
      !E_NFENUM type J_1BNFNUM9 .
  methods SELECT_NFE
    importing
      !I_PARID type J_1BPARID
      !I_PARTYP type J_1BPARTYP
      !I_NFENUM type J_1BNFNUM9
      !I_SERIES type J_1BSERIES
    returning
      value(RS_NFDOC) type J_1BNFDOC .
ENDCLASS.



CLASS ZCL_MIGO_PROCURA_NFE IMPLEMENTATION.


  METHOD branch_determine.
    CALL FUNCTION 'J_1B_BRANCH_DETERMINE'
      EXPORTING
        company = i_company
        plant   = i_plant
      IMPORTING
        company = e_company
        branch  = e_branch
      EXCEPTIONS
        OTHERS  = 8.
  ENDMETHOD.


  METHOD nftype_read.
    CALL FUNCTION 'J_1B_NFTYPE_READ'
      EXPORTING
        nf_type            = i_nftype
        i_werks            = i_werks
        i_bwart            = i_bwart
        i_lifnr            = i_lifnr
        i_nfedefaults      = i_defaults
      IMPORTING
        nf_type_parameters = rs_parameters
      EXCEPTIONS
        OTHERS             = 8.
  ENDMETHOD.


  METHOD nf_number_separate.
    CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
      EXPORTING
        ref_number = i_ref_number
        i_nfeflag  = i_nfeflag
      IMPORTING
        ref_number = e_ref_number
        nf_number  = e_nf_number
        series     = e_series
        subseries  = e_subseries
        nf_number9 = e_nfenum
      EXCEPTIONS
        OTHERS     = 8.
  ENDMETHOD.


  METHOD read_t001w.
    SELECT SINGLE *
      FROM t001w
      INTO rs_t001w
     WHERE werks = i_werks.
  ENDMETHOD.


  METHOD read_t156.
    SELECT SINGLE *
      INTO @rs_t156
      FROM t156
     WHERE bwart = @i_bwart.
  ENDMETHOD.


  METHOD search_nfe.
    DATA(lv_answer) = abap_false.

* Exemplo:
*    DATA(ls_mkpf) = VALUE mkpf( xblnr = |005019769-004| bldat = p_bldat ).
*    DATA(ls_mseg) = VALUE mseg( bwart = |861|
*                                vbeln_im = |0080012486|
*                                werks = |C014|
*                                bukrs = |1001|
*                                umwrk = |C002| ).
    DATA(ls_mkpf) = VALUE mkpf( xblnr = is_gohead-xblnr bldat = is_gohead-bldat ).
    DATA(ls_mseg) = VALUE mseg( bwart = is_goitem-bwart
                                werks = is_goitem-werks
                                bukrs = is_goitem-bukrs_for_stock
                                umwrk = is_gohead-reswk ).

    branch_determine( EXPORTING i_company = ls_mseg-bukrs
                                i_plant   = ls_mseg-werks
                      IMPORTING e_branch  = DATA(lv_branch)
                                e_company = DATA(lv_company) ).

    DATA(lv_t001w) = read_t001w( ls_mseg-werks ).
    DATA(ls_t156)  = read_t156( ls_mseg-bwart ).

    DATA(ls_j1baa) = nftype_read( EXPORTING i_nftype = ls_t156-j_1bnftype
                                            i_werks = ls_mseg-werks
                                            i_bwart = ls_mseg-bwart
                                            i_lifnr = ||
                                            i_defaults = || ).

    branch_determine( EXPORTING i_plant   = ls_mseg-umwrk
                      IMPORTING e_branch  = lv_branch
                                e_company = lv_company ).

    DATA(lv_partner_id) = CONV lifnr( lv_company ).
    MOVE lv_branch TO lv_partner_id+4(4).
    DATA(lv_partner_function) = ls_t156-j_1bparvw.
    DATA(lv_partner_type)     = ls_t156-j_1bpartyp.


    DATA(ls_nf_doc) = VALUE j_1bnfdoc( parid  = lv_partner_id
                                       parvw  = lv_partner_function
                                       partyp = lv_partner_type ).
    ls_nf_doc = CORRESPONDING #( BASE ( ls_nf_doc ) ls_j1baa ).

    nf_number_separate( EXPORTING i_ref_number = ls_mkpf-xblnr
                                  i_nfeflag    = ls_nf_doc-nfe
                        IMPORTING e_ref_number = DATA(lv_ref_number)
                                  e_nf_number  = ls_nf_doc-nfnum
                                  e_series     = ls_nf_doc-series
                                  e_subseries  = ls_nf_doc-subser
                                  e_nfenum     = ls_nf_doc-nfenum ).

    DATA(ls_nfdoc) = select_nfe( i_parid = lv_partner_id
                                 i_partyp = lv_partner_type
                                 i_nfenum = ls_nf_doc-nfenum
                                 i_series = ls_nf_doc-series ).

    IF ls_nfdoc IS NOT INITIAL.

      IF ls_nfdoc-docdat <> is_gohead-bldat.

        ct_bapiret2 = VALUE #(
                       BASE ct_bapiret2
                     ( type = |E| id = |8B|       number = |264| message_v1 = ls_nfdoc-nfenum message_v2 = ls_nfdoc-parid )
                     ( type = |E| id = |ZMM_MIGO| number = |000| message_v1 = ls_nfdoc-docnum message_v2 = |{ ls_nfdoc-docdat DATE = USER }| ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD select_nfe.
    SELECT *
      INTO TABLE @DATA(lt_nfdocs)
      FROM j_1bnfdoc
     WHERE parid  =  @i_parid
       AND partyp =  @i_partyp
       AND direct =  '1'
       AND nfenum =  @i_nfenum
       AND series =  @i_series
       AND doctyp <> '5'
       AND cancel =  @space.
    IF sy-subrc = 0.
      rs_nfdoc = lt_nfdocs[ 1 ].
    ENDIF.
  ENDMETHOD.
ENDCLASS.
