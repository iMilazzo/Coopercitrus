CLASS ltc_agco_parts_test DEFINITION DEFERRED.
CLASS zcl_agco_parts_data DEFINITION LOCAL FRIENDS ltc_agco_parts_test.

CLASS ltc_agco_parts_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS
     INHERITING FROM zcl_agco_parts_data
     FRIENDS  zcl_agco_parts_data
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Agco_Parts_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AGCO_PARTS_DATA
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
      t_makt TYPE STANDARD TABLE OF makt WITH EMPTY KEY,
      t_mbew TYPE STANDARD TABLE OF mbew WITH EMPTY KEY,
      t_ekpo TYPE STANDARD TABLE OF ekpo WITH EMPTY KEY,
      t_eket TYPE STANDARD TABLE OF eket WITH EMPTY KEY,
      t_mseg TYPE STANDARD TABLE OF mseg WITH EMPTY KEY,
      f_cut  TYPE REF TO zcl_agco_parts_data.  "class under test

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup,
      class_teardown.

    METHODS: setup.
    METHODS: teardown.
    METHODS: executar FOR TESTING.
ENDCLASS.       "ltc_Agco_Inventory_Test


CLASS ltc_agco_parts_test IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( |mara| )
                                                                                 ( |marc| )
                                                                                 ( |makt| )
                                                                                 ( |mbew| )
                                                                                 ( |ekpo| )
                                                                                 ( |eket| )
                                                                                 ( |mseg| ) ) ).
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

    t_makt = VALUE #( ( matnr = |000000000001000000| maktx = |Descrição Material 1| spras = |PT| )
                      ( matnr = |000000000001000001| maktx = |Descrição Material 2| spras = |PT| )
                      ( matnr = |000000000001000002| maktx = |Descrição Material 3| spras = |PT| )
                      ( matnr = |000000000001000003| maktx = |Descrição Material 4| spras = |PT| )
                      ( matnr = |000000000001000004| maktx = |Descrição Material 5| spras = |PT| )
                      ( matnr = |000000000001000005| maktx = |Descrição Material 6| spras = |PT| )
                      ( matnr = |000000000001000006| maktx = |Descrição Material 7| spras = |PT| )
                      ( matnr = |000000000001000007| maktx = |Descrição Material 8| spras = |PT| )
                      ( matnr = |000000000001000008| maktx = |Descrição Material 9| spras = |PT| ) ).
    environment->insert_test_data( t_makt ).

    t_mbew = VALUE #( ( matnr = |000000000001000000| bwkey = |C015| verpr = 1000 peinh = 100 )
                      ( matnr = |000000000001000001| bwkey = |C016| verpr = 2000 peinh = 100 )
                      ( matnr = |000000000001000002| bwkey = |C018| verpr = 3000 peinh = 100 )
                      ( matnr = |000000000001000003| bwkey = |C029| verpr = 4000 peinh = 100 )
                      ( matnr = |000000000001000004| bwkey = |C034| verpr = 5000 peinh = 100 )
                      ( matnr = |000000000001000005| bwkey = |C035| verpr = 6000 peinh = 100 )
                      ( matnr = |000000000001000006| bwkey = |C050| verpr = 7000 peinh = 100 )
                      ( matnr = |000000000001000007| bwkey = |C052| verpr = 8000 peinh = 100 )
                      ( matnr = |000000000001000008| bwkey = |C065| verpr = 9000 peinh = 100 ) ).
    environment->insert_test_data( t_mbew ).

    t_ekpo = VALUE #( ( ebeln = |0100000000| ebelp = |00001| matnr = |000000000001000000| werks = |C015| )
                      ( ebeln = |0100000001| ebelp = |00001| matnr = |000000000001000001| werks = |C016| )
                      ( ebeln = |0100000002| ebelp = |00001| matnr = |000000000001000002| werks = |C018| )
                      ( ebeln = |0100000003| ebelp = |00001| matnr = |000000000001000003| werks = |C029| )
                      ( ebeln = |0100000004| ebelp = |00001| matnr = |000000000001000004| werks = |C034| )
                      ( ebeln = |0100000005| ebelp = |00001| matnr = |000000000001000005| werks = |C035| )
                      ( ebeln = |0100000006| ebelp = |00001| matnr = |000000000001000006| werks = |C050| )
                      ( ebeln = |0100000008| ebelp = |00001| matnr = |000000000001000007| werks = |C052| ) ).
    environment->insert_test_data( t_ekpo ).

    t_eket = VALUE #( ( ebeln = |0100000000| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000001| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000002| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000003| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000004| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000005| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000006| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 )
                      ( ebeln = |0100000008| ebelp = |00001| etenr = |0001| menge = 1000 glmng = 100 ) ).
    environment->insert_test_data( t_eket ).

    t_mseg = VALUE #( ( mblnr = |0100000000| mjahr = |1001| zeile = |00001| matnr = |000000000001000000| werks = |C015| budat_mkpf = |20210101| )
                      ( mblnr = |0100000001| mjahr = |1001| zeile = |00001| matnr = |000000000001000001| werks = |C016| budat_mkpf = |20210102| )
                      ( mblnr = |0100000002| mjahr = |1001| zeile = |00001| matnr = |000000000001000002| werks = |C018| budat_mkpf = |20210103| )
                      ( mblnr = |0100000003| mjahr = |1001| zeile = |00001| matnr = |000000000001000003| werks = |C029| budat_mkpf = |20210104| )
                      ( mblnr = |0100000004| mjahr = |1001| zeile = |00001| matnr = |000000000001000004| werks = |C034| budat_mkpf = |20210105| )
                      ( mblnr = |0100000005| mjahr = |1001| zeile = |00001| matnr = |000000000001000005| werks = |C035| budat_mkpf = |20210106| )
                      ( mblnr = |0100000006| mjahr = |1001| zeile = |00001| matnr = |000000000001000006| werks = |C050| budat_mkpf = |20210107| )
                      ( mblnr = |0100000008| mjahr = |1001| zeile = |00001| matnr = |000000000001000007| werks = |C052| budat_mkpf = |20210108| ) ).
    environment->insert_test_data( t_mseg ).

    CREATE OBJECT f_cut.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.

  METHOD executar.

    f_cut->processar( ).

  ENDMETHOD.


ENDCLASS.
