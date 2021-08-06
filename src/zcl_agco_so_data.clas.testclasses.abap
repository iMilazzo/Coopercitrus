CLASS ltc_agco_so_test DEFINITION DEFERRED.
CLASS zcl_agco_so_data DEFINITION LOCAL FRIENDS ltc_agco_so_test.

CLASS ltc_agco_so_test DEFINITION FOR TESTING
  DURATION MEDIUM
     INHERITING FROM zcl_agco_so_data  RISK LEVEL DANGEROUS
     FRIENDS  zcl_agco_so_data.
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Agco_so_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AGCO_SO_DATA
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      t_vbak TYPE STANDARD TABLE OF vbak WITH EMPTY KEY,
      t_vbap TYPE STANDARD TABLE OF vbap WITH EMPTY KEY,
      t_vbkd TYPE STANDARD TABLE OF vbkd WITH EMPTY KEY,
      t_kna1 TYPE STANDARD TABLE OF kna1 WITH EMPTY KEY,
      f_cut  TYPE REF TO zcl_agco_so_data.  "class under test

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup,
                   class_teardown.

    METHODS: setup.
    METHODS: teardown.
    METHODS: executar FOR TESTING.
ENDCLASS.       "ltc_Agco_So_Test


CLASS ltc_agco_so_test IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( |vbak| )
                                                                                 ( |vbap| )
                                                                                 ( |vbkd| )
                                                                                 ( |kna1| ) ) ).
  ENDMETHOD.
  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.
  METHOD setup.
    environment->clear_doubles( ).
    t_vbak = VALUE #( ( vbeln = |0001000001| erdat = |20210731| vdatu = |20210801| auart = |ZBOR| kunnr = |1000000001| vbtyp = |C| )
                      ( vbeln = |0001000002| erdat = |20210701| vdatu = |20210802| auart = |ZBOR| kunnr = |1000000002| vbtyp = |C| )
                      ( vbeln = |0001000003| erdat = |20210702| vdatu = |20210803| auart = |ZBOR| kunnr = |1000000003| vbtyp = |C| )
                      ( vbeln = |0001000004| erdat = |20210703| vdatu = |20210804| auart = |ZBOD| kunnr = |1000000002| vbtyp = |C| )
                      ( vbeln = |0001000005| erdat = |20210704| vdatu = |20210805| auart = |ZBOD| kunnr = |1000000001| vbtyp = |C| ) ).
    environment->insert_test_data( t_vbak ).

    environment->clear_doubles( ).
    t_vbap = VALUE #( ( vbeln = |0001000001| posnr = |000010| matnr = |000000000001000001| kwmeng = 1000 netpr = 1001 kzwi6 = 1001 mwsbp = 11 waerk = |BRL| uvall = |A| uvfak = |B| )
                      ( vbeln = |0001000001| posnr = |000020| matnr = |000000000001000002| kwmeng = 1000 netpr = 1001 kzwi6 = 1001 mwsbp = 11 waerk = |BRL| uvall = |C| uvfak = |A| )
                      ( vbeln = |0001000002| posnr = |000010| matnr = |000000000001000003| kwmeng = 2000 netpr = 2002 kzwi6 = 2002 mwsbp = 22 waerk = |BRL| uvall = |B| uvfak = |C| )
                      ( vbeln = |0001000002| posnr = |000020| matnr = |000000000001000004| kwmeng = 2000 netpr = 2002 kzwi6 = 2002 mwsbp = 22 waerk = |BRL| uvall = |A| uvfak = |A| )
                      ( vbeln = |0001000003| posnr = |000010| matnr = |000000000001000005| kwmeng = 3000 netpr = 3003 kzwi6 = 3003 mwsbp = 33 waerk = |BRL| uvall = |C| uvfak = |A| )
                      ( vbeln = |0001000003| posnr = |000020| matnr = |000000000001000006| kwmeng = 3000 netpr = 3003 kzwi6 = 3003 mwsbp = 33 waerk = |BRL| uvall = |B| uvfak = |C| )
                      ( vbeln = |0001000004| posnr = |000010| matnr = |000000000001000007| kwmeng = 4000 netpr = 4004 kzwi6 = 4004 mwsbp = 44 waerk = |BRL| uvall = |A| uvfak = |B| )
                      ( vbeln = |0001000004| posnr = |000020| matnr = |000000000001000008| kwmeng = 4000 netpr = 4004 kzwi6 = 4004 mwsbp = 44 waerk = |BRL| uvall = |A| uvfak = |A| )
                      ( vbeln = |0001000005| posnr = |000010| matnr = |000000000001000009| kwmeng = 5000 netpr = 5005 kzwi6 = 5005 mwsbp = 55 waerk = |BRL| uvall = |B| uvfak = |C| )
                      ( vbeln = |0001000005| posnr = |000020| matnr = |000000000001000010| kwmeng = 5000 netpr = 5005 kzwi6 = 5005 mwsbp = 55 waerk = |BRL| uvall = |A| uvfak = |A| ) ).
    environment->insert_test_data( t_vbap ).

    environment->clear_doubles( ).
    t_vbkd = VALUE #( ( vbeln = |0001000001| posnr = |000010| fkdat = |20210930| )
                      ( vbeln = |0001000001| posnr = |000020| fkdat = |20210901| )
                      ( vbeln = |0001000002| posnr = |000010| fkdat = |20210902| )
                      ( vbeln = |0001000002| posnr = |000020| fkdat = |20210903| )
                      ( vbeln = |0001000003| posnr = |000010| fkdat = |20210904| )
                      ( vbeln = |0001000003| posnr = |000020| fkdat = |20210930| )
                      ( vbeln = |0001000004| posnr = |000010| fkdat = |20210901| )
                      ( vbeln = |0001000004| posnr = |000020| fkdat = |20210902| )
                      ( vbeln = |0001000005| posnr = |000010| fkdat = |20210903| )
                      ( vbeln = |0001000005| posnr = |000020| fkdat = |20210904| ) ).
    environment->insert_test_data( t_vbap ).
    environment->clear_doubles( ).
    t_kna1 = VALUE #( ( kunnr = |1000000001| stcd1 = |49210685000190| )
                      ( kunnr = |1000000002| stcd1 = |89347953000134| )
                      ( kunnr = |1000000003| stcd1 = |94675583000102| ) ).
    environment->insert_test_data( t_vbap ).
    CREATE OBJECT f_cut.
  ENDMETHOD.
  METHOD teardown.
  ENDMETHOD.
  METHOD executar.

*    f_cut->m_ordens = VALUE #(
*            ordens = VALUE #(
*                          ( vbeln = |0001000001| erdat = |20210731| vdatu = |20210801| auart = |ZBOR| kunnr = |1000000001| vbtyp = |C| )
*                          ( vbeln = |0001000002| erdat = |20210701| vdatu = |20210802| auart = |ZBOR| kunnr = |1000000002| vbtyp = |C| )
*                          ( vbeln = |0001000003| erdat = |20210702| vdatu = |20210803| auart = |ZBOR| kunnr = |1000000003| vbtyp = |C| )
*                          ( vbeln = |0001000004| erdat = |20210703| vdatu = |20210804| auart = |ZBOD| kunnr = |1000000002| vbtyp = |C| )
*                          ( vbeln = |0001000005| erdat = |20210704| vdatu = |20210805| auart = |ZBOD| kunnr = |1000000001| vbtyp = |C| ) )
*            itens = VALUE #(
*                          ( vbeln = |0001000001| posnr = |000010| matnr = |000000000001000001| kwmeng = 1000 netpr = 1001 kzwi6 = 1001 mwsbp = 11 waerk = |BRL| uvall = |A| uvfak = |B| )
*                          ( vbeln = |0001000001| posnr = |000020| matnr = |000000000001000002| kwmeng = 1000 netpr = 1001 kzwi6 = 1001 mwsbp = 11 waerk = |BRL| uvall = |C| uvfak = |A| )
*                          ( vbeln = |0001000002| posnr = |000010| matnr = |000000000001000003| kwmeng = 2000 netpr = 2002 kzwi6 = 2002 mwsbp = 22 waerk = |BRL| uvall = |B| uvfak = |C| )
*                          ( vbeln = |0001000002| posnr = |000020| matnr = |000000000001000004| kwmeng = 2000 netpr = 2002 kzwi6 = 2002 mwsbp = 22 waerk = |BRL| uvall = |A| uvfak = |A| )
*                          ( vbeln = |0001000003| posnr = |000010| matnr = |000000000001000005| kwmeng = 3000 netpr = 3003 kzwi6 = 3003 mwsbp = 33 waerk = |BRL| uvall = |C| uvfak = |A| )
*                          ( vbeln = |0001000003| posnr = |000020| matnr = |000000000001000006| kwmeng = 3000 netpr = 3003 kzwi6 = 3003 mwsbp = 33 waerk = |BRL| uvall = |B| uvfak = |C| )
*                          ( vbeln = |0001000004| posnr = |000010| matnr = |000000000001000007| kwmeng = 4000 netpr = 4004 kzwi6 = 4004 mwsbp = 44 waerk = |BRL| uvall = |A| uvfak = |B| )
*                          ( vbeln = |0001000004| posnr = |000020| matnr = |000000000001000008| kwmeng = 4000 netpr = 4004 kzwi6 = 4004 mwsbp = 44 waerk = |BRL| uvall = |A| uvfak = |A| )
*                          ( vbeln = |0001000005| posnr = |000010| matnr = |000000000001000009| kwmeng = 5000 netpr = 5005 kzwi6 = 5005 mwsbp = 55 waerk = |BRL| uvall = |B| uvfak = |C| )
*                          ( vbeln = |0001000005| posnr = |000020| matnr = |000000000001000010| kwmeng = 5000 netpr = 5005 kzwi6 = 5005 mwsbp = 55 waerk = |BRL| uvall = |A| uvfak = |A| ) )
*            faturas = VALUE #(
*                          ( vbeln = |0001000001| posnr = |000010| fkdat = |20210930| )
*                          ( vbeln = |0001000001| posnr = |000020| fkdat = |20210901| )
*                          ( vbeln = |0001000002| posnr = |000010| fkdat = |20210902| )
*                          ( vbeln = |0001000002| posnr = |000020| fkdat = |20210903| )
*                          ( vbeln = |0001000003| posnr = |000010| fkdat = |20210904| )
*                          ( vbeln = |0001000003| posnr = |000020| fkdat = |20210930| )
*                          ( vbeln = |0001000004| posnr = |000010| fkdat = |20210901| )
*                          ( vbeln = |0001000004| posnr = |000020| fkdat = |20210902| )
*                          ( vbeln = |0001000005| posnr = |000010| fkdat = |20210903| )
*                          ( vbeln = |0001000005| posnr = |000020| fkdat = |20210904| ) )
*            clientes = VALUE #(
*                          ( kunnr = |1000000001| stcd1 = |49210685000190| )
*                          ( kunnr = |1000000002| stcd1 = |89347953000134| )
*                          ( kunnr = |1000000003| stcd1 = |94675583000102| ) ) ).
*
*    f_cut->lt_constantes = f_cut->ler_constantes( ).
*    f_cut->lt_parceiros = f_cut->ler_parceiros( ).
*
*    f_cut->autenticar( ).
*    IF f_cut->v_token IS NOT INITIAL.
*      f_cut->preencher_saida( ).
*    ENDIF.
*
*    cl_abap_unit_assert=>assert_equals(
*                                   EXPORTING
*                                        act = 0
*                                        exp = 0
*                                        msg = | linhas processadas|
*                                       quit = if_aunit_constants=>quit-no
*                                      level = if_aunit_constants=>severity-low ).

  ENDMETHOD.

ENDCLASS.
