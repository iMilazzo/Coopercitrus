CLASS ltc_agco_customer_test DEFINITION DEFERRED.
CLASS zcl_agco_customer_data DEFINITION LOCAL FRIENDS ltc_agco_customer_test.

CLASS ltc_agco_customer_test DEFINITION FOR TESTING
  DURATION MEDIUM
     INHERITING FROM zcl_agco_customer_data  RISK LEVEL DANGEROUS
     FRIENDS  zcl_agco_customer_data.
  .
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Agco_Customer_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AGCO_CUSTOMER_DATA
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_agco_customer_data.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: executar FOR TESTING.
ENDCLASS.       "ltc_Agco_Customer_Test


CLASS ltc_agco_customer_test IMPLEMENTATION.

  METHOD setup.


    CREATE OBJECT f_cut.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD executar.

    f_cut->m_clientes = VALUE #(
                            clientes = VALUE #(
                                          ( kunnr = |0001000000| land1 = |BR| name1 = |Nome cliente 1| ort01 = |Rua do cliente 1| regio = |PR| stcd1 = |37479253000108| )
                                          ( kunnr = |0001000001| land1 = |BR| name1 = |Nome cliente 2| ort01 = |Rua do cliente 2| regio = |PR| stcd1 = |16746992000130| )
                                          ( kunnr = |0001000002| land1 = |BR| name1 = |Nome cliente 3| ort01 = |Rua do cliente 3| regio = |PR| stcd1 = |87067194000120| )
                                          ( kunnr = |0001000003| land1 = |BR| name1 = |Nome cliente 4| ort01 = |Rua do cliente 4| regio = |PR| stcd1 = |29638028000130| )
                                          ( kunnr = |0001000004| land1 = |BR| name1 = |Nome cliente 5| ort01 = |Rua do cliente 5| regio = |PR| stcd1 = |77972787000188| )
                                          ( kunnr = |0001000005| land1 = |BR| name1 = |Nome cliente 6| ort01 = |Rua do cliente 6| regio = |PR| stcd1 = |46424544000181| ) )
                            vendas = VALUE #(
                                          ( kunnr = |0001000000| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                                          ( kunnr = |0001000001| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                                          ( kunnr = |0001000002| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                                          ( kunnr = |0001000003| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                                          ( kunnr = |0001000004| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                                          ( kunnr = |0001000005| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) ) ) ).

    f_cut->lt_constantes = f_cut->ler_constantes( ).
    f_cut->lt_parceiros = f_cut->ler_parceiros( ).

    f_cut->autenticar( ).
    IF f_cut->v_token IS NOT INITIAL.
      f_cut->preencher_saida( ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
                                   EXPORTING
                                        act = 0
                                        exp = 0
                                        msg = | linhas processadas|
                                       quit = if_aunit_constants=>quit-no
                                      level = if_aunit_constants=>severity-low ).
  ENDMETHOD.




ENDCLASS.
