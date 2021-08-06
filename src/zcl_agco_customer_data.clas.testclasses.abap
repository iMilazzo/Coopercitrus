CLASS ltc_agco_customer_test DEFINITION DEFERRED.
CLASS zcl_agco_customer_data DEFINITION LOCAL FRIENDS ltc_agco_customer_test.

CLASS ltc_agco_customer_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS
     INHERITING FROM zcl_agco_customer_data
     FRIENDS  zcl_agco_customer_data.

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
      t_kna1 TYPE STANDARD TABLE OF kna1 WITH EMPTY KEY,
      t_doc  TYPE STANDARD TABLE OF j_1bnfdoc WITH EMPTY KEY,
      f_cut  TYPE REF TO zcl_agco_customer_data.  "class under test

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup,
                   class_teardown.

    METHODS: setup.
    METHODS: teardown.
    METHODS: executar FOR TESTING.
ENDCLASS.       "ltc_Agco_Customer_Test


CLASS ltc_agco_customer_test IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( |kna1| )
                                                                                 ( |j_1bnfdoc| ) ) ).
  ENDMETHOD.
  method class_teardown.
    environment->destroy( ).
  endmethod.
  METHOD setup.

    environment->clear_doubles( ).

    t_kna1 = VALUE #( ( kunnr = |0001000000| land1 = |BR| name1 = |Nome cliente 1| ort01 = |Rua do cliente 1| regio = |PR| stcd1 = |37479253000108| )
                      ( kunnr = |0001000001| land1 = |BR| name1 = |Nome cliente 2| ort01 = |Rua do cliente 2| regio = |PR| stcd1 = |16746992000130| )
                      ( kunnr = |0001000002| land1 = |BR| name1 = |Nome cliente 3| ort01 = |Rua do cliente 3| regio = |PR| stcd1 = |87067194000120| )
                      ( kunnr = |0001000003| land1 = |BR| name1 = |Nome cliente 4| ort01 = |Rua do cliente 4| regio = |PR| stcd1 = |29638028000130| )
                      ( kunnr = |0001000004| land1 = |BR| name1 = |Nome cliente 5| ort01 = |Rua do cliente 5| regio = |PR| stcd1 = |77972787000188| )
                      ( kunnr = |0001000005| land1 = |BR| name1 = |Nome cliente 6| ort01 = |Rua do cliente 6| regio = |PR| stcd1 = |46424544000181| ) ).
    environment->insert_test_data( t_kna1 ).
    t_doc = VALUE #( ( docnum = |0001000000| direct = 2 parid = |0001000000| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                     ( docnum = |0001000001| direct = 2 parid = |0001000001| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                     ( docnum = |0001000002| direct = 2 parid = |0001000002| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                     ( docnum = |0001000003| direct = 2 parid = |0001000003| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                     ( docnum = |0001000004| direct = 2 parid = |0001000004| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) )
                     ( docnum = |0001000005| direct = 2 parid = |0001000005| cre_timestamp = CONV j_1bcre_timestamp( 20191218053001 ) ) ).
    environment->insert_test_data( t_doc ).

    CREATE OBJECT f_cut.
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD executar.
    f_cut->processar( ).
  ENDMETHOD.

ENDCLASS.
