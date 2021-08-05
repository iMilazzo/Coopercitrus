CLASS ltc_agco_po_test DEFINITION DEFERRED.
CLASS zcl_agco_po_data DEFINITION LOCAL FRIENDS ltc_agco_po_test.

CLASS ltc_agco_po_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS
                       INHERITING FROM zcl_agco_po_data
                       FRIENDS  zcl_agco_po_data.

*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Agco_PO_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AGCO_PO_DATA
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
      t_mara TYPE STANDARD TABLE OF mara WITH EMPTY KEY,
      t_marc TYPE STANDARD TABLE OF marc WITH EMPTY KEY,
      t_doc  TYPE STANDARD TABLE OF j_1bnfdoc WITH EMPTY KEY,
      t_fat  TYPE STANDARD TABLE OF rseg WITH EMPTY KEY,
      t_ped  TYPE STANDARD TABLE OF ekpo WITH EMPTY KEY,
      f_cut  TYPE REF TO zcl_agco_po_data.  "class under test

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup,
      class_teardown.

    METHODS: setup.
    METHODS: teardown.
    METHODS: executar FOR TESTING.
ENDCLASS.       "ltc_Agco_PO_Test


CLASS ltc_agco_po_test IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( |mara| )
                                                                                 ( |marc| )
                                                                                 ( |j_1bnfdoc| )
                                                                                 ( |rseg| )
                                                                                 ( |ekpo| ) ) ).
  ENDMETHOD.
  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.
  METHOD setup.
    environment->clear_doubles( ).
    t_mara = VALUE #( ( mandt = sy-mandt matnr = |000000000001000000| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000001| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000002| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000003| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000004| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000005| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000006| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000007| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000008| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false )
                      ( mandt = sy-mandt matnr = |000000000001000009| meins = |PC| mfrpn = |0002401075| matkl = |000000053| mfrnr = |0002401075| lvorm = abap_false ) ).
    environment->insert_test_data( t_mara ).

    t_marc = VALUE #( ( matnr = |000000000001000000| werks = |C015| lvorm = abap_false )
                      ( matnr = |000000000001000001| werks = |C016| lvorm = abap_false )
                      ( matnr = |000000000001000002| werks = |C018| lvorm = abap_false )
                      ( matnr = |000000000001000003| werks = |C029| lvorm = abap_false )
                      ( matnr = |000000000001000004| werks = |C034| lvorm = abap_false )
                      ( matnr = |000000000001000005| werks = |C035| lvorm = abap_false )
                      ( matnr = |000000000001000006| werks = |C050| lvorm = abap_false )
                      ( matnr = |000000000001000007| werks = |C052| lvorm = abap_false )
                      ( matnr = |000000000001000008| werks = |C065| lvorm = abap_false )
                      ( matnr = |000000000001000009| werks = |C067| lvorm = abap_false ) ).
    environment->insert_test_data( t_marc ).

    t_doc = VALUE #(
    ( docnum = |0000000001| doctyp = |YB| direct = 2 branch = |0015| docdat = |20210101| pstdat = |20210701| belnr = |1000000001| gjahr = |2021| bukrs = |1001| nfenum = |000001001| series = |1| parid = |0000100001| cnpj_bupla = |01.234.567/0000-01|
name1 = |Fornecedor 01| )
    ( docnum = |0000000002| doctyp = |YB| direct = 2 branch = |0016| docdat = |20210102| pstdat = |20210702| belnr = |1000000002| gjahr = |2021| bukrs = |1001| nfenum = |000001002| series = |1| parid = |0000100002| cnpj_bupla = |01.234.567/0000-02|
name1 = |Fornecedor 02| )
    ( docnum = |0000000003| doctyp = |YB| direct = 2 branch = |0018| docdat = |20210103| pstdat = |20210703| belnr = |1000000003| gjahr = |2021| bukrs = |1001| nfenum = |000001003| series = |1| parid = |0000100003| cnpj_bupla = |01.234.567/0000-03|
name1 = |Fornecedor 03| )
    ( docnum = |0000000004| doctyp = |YB| direct = 2 branch = |0029| docdat = |20210104| pstdat = |20210704| belnr = |1000000004| gjahr = |2021| bukrs = |1001| nfenum = |000001004| series = |1| parid = |0000100004| cnpj_bupla = |01.234.567/0000-04|
name1 = |Fornecedor 04| )
    ( docnum = |0000000005| doctyp = |YB| direct = 2 branch = |0034| docdat = |20210105| pstdat = |20210705| belnr = |1000000005| gjahr = |2021| bukrs = |1001| nfenum = |000001005| series = |1| parid = |0000100005| cnpj_bupla = |01.234.567/0000-05|
name1 = |Fornecedor 05| )
    ( docnum = |0000000006| doctyp = |YB| direct = 2 branch = |0050| docdat = |20210106| pstdat = |20210706| belnr = |1000000006| gjahr = |2021| bukrs = |1001| nfenum = |000001006| series = |1| parid = |0000100006| cnpj_bupla = |01.234.567/0000-06|
name1 = |Fornecedor 06| )
    ( docnum = |0000000007| doctyp = |YB| direct = 2 branch = |0065| docdat = |20210107| pstdat = |20210707| belnr = |1000000007| gjahr = |2021| bukrs = |1001| nfenum = |000001007| series = |1| parid = |0000100007| cnpj_bupla = |01.234.567/0000-07|
name1 = |Fornecedor 07| )
    ( docnum = |0000000008| doctyp = |YB| direct = 2 branch = |0067| docdat = |20210108| pstdat = |20210708| belnr = |1000000008| gjahr = |2021| bukrs = |1001| nfenum = |000001008| series = |1| parid = |0000100008| cnpj_bupla = |01.234.567/0000-08|
name1 = |Fornecedor 08| ) ).
    environment->insert_test_data( t_doc ).

    t_fat = VALUE #(
    ( belnr = |1000000001| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000001| ebelp = |000010| menge = 100 wrbtr = 1101 )
    ( belnr = |1000000001| gjahr = |2021| bukrs = |1001| buzei = |000020| ebeln = |0100000001| ebelp = |000020| menge = 200 wrbtr = 1201 )
    ( belnr = |1000000002| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000002| ebelp = |000010| menge = 100 wrbtr = 2101 )
    ( belnr = |1000000002| gjahr = |2021| bukrs = |1001| buzei = |000020| ebeln = |0100000002| ebelp = |000020| menge = 200 wrbtr = 2201 )
    ( belnr = |1000000002| gjahr = |2021| bukrs = |1001| buzei = |000030| ebeln = |0100000002| ebelp = |000030| menge = 300 wrbtr = 2301 )
    ( belnr = |1000000003| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000003| ebelp = |000010| menge = 100 wrbtr = 3001 )
    ( belnr = |1000000004| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000004| ebelp = |000010| menge = 100 wrbtr = 4001 )
    ( belnr = |1000000005| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000005| ebelp = |000010| menge = 100 wrbtr = 5001 )
    ( belnr = |1000000006| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000006| ebelp = |000010| menge = 100 wrbtr = 6001 )
    ( belnr = |1000000007| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000007| ebelp = |000010| menge = 100 wrbtr = 7001 )
    ( belnr = |1000000008| gjahr = |2021| bukrs = |1001| buzei = |000010| ebeln = |0100000008| ebelp = |000010| menge = 100 wrbtr = 8001 ) ).
    environment->insert_test_data( t_fat ).

    t_ped = VALUE #(
    ( ebeln = |0100000001| ebelp = |000010| menge = 1100 matnr = |1000000001| )
    ( ebeln = |0100000001| ebelp = |000020| menge = 1200 matnr = |1000000002| )
    ( ebeln = |0100000002| ebelp = |000010| menge = 2100 matnr = |1000000003| )
    ( ebeln = |0100000002| ebelp = |000020| menge = 2200 matnr = |1000000004| )
    ( ebeln = |0100000002| ebelp = |000030| menge = 2300 matnr = |1000000005| )
    ( ebeln = |0100000003| ebelp = |000010| menge = 3000 matnr = |1000000006| )
    ( ebeln = |0100000004| ebelp = |000010| menge = 4000 matnr = |1000000007| )
    ( ebeln = |0100000004| ebelp = |000020| menge = 4000 matnr = |1000000008| )
    ( ebeln = |0100000005| ebelp = |000010| menge = 5000 matnr = |1000000009| )
    ( ebeln = |0100000005| ebelp = |000020| menge = 5000 matnr = |1000000010| )
    ( ebeln = |0100000005| ebelp = |000030| menge = 5000 matnr = |1000000011| )
    ( ebeln = |0100000006| ebelp = |000010| menge = 6000 matnr = |1000000012| )
    ( ebeln = |0100000006| ebelp = |000020| menge = 6000 matnr = |1000000013| )
    ( ebeln = |0100000007| ebelp = |000010| menge = 7000 matnr = |1000000014| )
    ( ebeln = |0100000008| ebelp = |000010| menge = 7000 matnr = |1000000015| ) ) .
    environment->insert_test_data( t_ped ).

    CREATE OBJECT f_cut.
  ENDMETHOD.
  METHOD teardown.
  ENDMETHOD.

  METHOD executar.

    f_cut->processar( ).

  ENDMETHOD.


ENDCLASS.
