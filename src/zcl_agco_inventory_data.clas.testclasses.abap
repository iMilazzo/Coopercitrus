CLASS ltc_agco_inventory_test DEFINITION DEFERRED.
CLASS zcl_agco_inventory_data DEFINITION LOCAL FRIENDS ltc_agco_inventory_test.

CLASS ltc_agco_inventory_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS
                              INHERITING FROM zcl_agco_inventory_data
                              FRIENDS  zcl_agco_inventory_data.

  PRIVATE SECTION.

    DATA:
      t_mara TYPE STANDARD TABLE OF mara WITH EMPTY KEY,
      t_marc TYPE STANDARD TABLE OF marc WITH EMPTY KEY,
      t_mard TYPE STANDARD TABLE OF mard WITH EMPTY KEY,
      t_resb TYPE STANDARD TABLE OF resb WITH EMPTY KEY,
      t_lips TYPE STANDARD TABLE OF lips WITH EMPTY KEY,
      t_ekpo TYPE STANDARD TABLE OF ekpo WITH EMPTY KEY,
      t_doc  TYPE STANDARD TABLE OF j_1bnfdoc WITH EMPTY KEY,
      t_item TYPE STANDARD TABLE OF j_1bnflin WITH EMPTY KEY,

      f_cut  TYPE REF TO zcl_agco_inventory_data.  "class under test

    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup,
      class_teardown.
    METHODS: setup,
      executar FOR TESTING.

ENDCLASS.       "ltc_Agco_Inventory_Test


CLASS ltc_agco_inventory_test IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( |mara| )
                                                                                 ( |marc| )
                                                                                 ( |mard| )
                                                                                 ( |resb| )
                                                                                 ( |lips| )
                                                                                 ( |ekpo| )
                                                                                 ( |j_1bnfdoc| )
                                                                                 ( |j_1bnflin| ) ) ).
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

    t_marc = VALUE #( ( matnr = |000000000001000000| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000001| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000002| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000003| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000004| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000005| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000006| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000007| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000008| werks = |C001| lvorm = abap_false )
                      ( matnr = |000000000001000009| werks = |C001| lvorm = abap_false ) ).
    environment->insert_test_data( t_marc ).

    t_mard = VALUE #( ( matnr = |000000000001000000| werks = |C001| labst = 100 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000001| werks = |C001| labst = 101 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000002| werks = |C001| labst = 102 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000003| werks = |C001| labst = 103 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000004| werks = |C001| labst = 104 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000005| werks = |C001| labst = 105 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000006| werks = |C001| labst = 106 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false )
                      ( matnr = |000000000001000007| werks = |C001| labst = 107 lgort = |C001| lfgja = |2021| lfmon = |08| lvorm = abap_false ) ).
    environment->insert_test_data( t_mard ).

    t_resb = VALUE #( ( rsnum = |0001000000| rspos = |0001| rsart = || matnr = |000000000001000000| werks = |C001| bdmng = 10 kzear = abap_false xloek = abap_false )
                      ( rsnum = |0001000001| rspos = |0001| rsart = || matnr = |000000000001000001| werks = |C001| bdmng = 11 kzear = abap_false xloek = abap_false )
                      ( rsnum = |0001000002| rspos = |0001| rsart = || matnr = |000000000001000002| werks = |C001| bdmng = 12 kzear = abap_false xloek = abap_false )
                      ( rsnum = |0001000003| rspos = |0001| rsart = || matnr = |000000000001000003| werks = |C001| bdmng = 13 kzear = abap_false xloek = abap_false )
                      ( rsnum = |0001000004| rspos = |0001| rsart = || matnr = |000000000001000006| werks = |C001| bdmng = 14 kzear = abap_false xloek = abap_false ) ).
    environment->insert_test_data( t_resb ).

    t_lips = VALUE #(
                  ( vbeln = |0001000000| posnr = |000010| matnr = |000000000001000000| werks = |C001| lfimg = 1 wbsta = |A| )
                  ( vbeln = |0001000001| posnr = |000010| matnr = |000000000001000001| werks = |C001| lfimg = 2 wbsta = |B| )
                  ( vbeln = |0001000002| posnr = |000010| matnr = |000000000001000002| werks = |C001| lfimg = 3 wbsta = |A| )
                  ( vbeln = |0001000003| posnr = |000010| matnr = |000000000001000003| werks = |C001| lfimg = 4 wbsta = |B| )
                  ( vbeln = |0001000004| posnr = |000010| matnr = |000000000001000007| werks = |C001| lfimg = 5 wbsta = |A| ) ).
    environment->insert_test_data( t_lips ).

    t_ekpo = VALUE #(
                  ( ebeln = |0001000000| ebelp = |000010| matnr = |000000000001000000| werks = |C001| menge = 1 knttp = |H| loekz = |L| elikz = abap_true )
                  ( ebeln = |0001000001| ebelp = |000010| matnr = |000000000001000001| werks = |C001| menge = 2 knttp = |H| loekz = |L| elikz = abap_true )
                  ( ebeln = |0001000002| ebelp = |000010| matnr = |000000000001000002| werks = |C001| menge = 3 knttp = |H| loekz = |L| elikz = abap_true )
                  ( ebeln = |0001000003| ebelp = |000010| matnr = |000000000001000003| werks = |C001| menge = 4 knttp = |H| loekz = |L| elikz = abap_true ) ).
    environment->insert_test_data( t_ekpo ).

    t_doc = VALUE #( ( docnum = |0000000100| nftype = |YB| doctyp = |6| direct = |2| cancel = abap_false )
                     ( docnum = |0000000101| nftype = |YB| doctyp = |6| direct = |2| cancel = abap_false )
                     ( docnum = |0000000102| nftype = |YB| doctyp = |6| direct = |2| cancel = abap_false )
                     ( docnum = |0000000103| nftype = |YB| doctyp = |6| direct = |2| cancel = abap_false )
                     ( docnum = |0000000104| nftype = |YB| doctyp = |6| direct = |2| cancel = abap_false ) ).
    environment->insert_test_data( t_doc ).

    t_item = VALUE #( ( docnum = |0000000100| itmnum = |000010| matnr = |000000000001000000| bwkey = |C001| werks = |C001| menge = 1 )
                      ( docnum = |0000000101| itmnum = |000010| matnr = |000000000001000001| bwkey = |C001| werks = |C001| menge = 2 )
                      ( docnum = |0000000102| itmnum = |000010| matnr = |000000000001000002| bwkey = |C001| werks = |C001| menge = 3 )
                      ( docnum = |0000000103| itmnum = |000010| matnr = |000000000001000003| bwkey = |C001| werks = |C001| menge = 4 )
                      ( docnum = |0000000104| itmnum = |000010| matnr = |000000000001000005| bwkey = |C001| werks = |C001| menge = 5 ) ).
    environment->insert_test_data( t_item ).
    CREATE OBJECT f_cut.
  ENDMETHOD.
  METHOD executar.

    f_cut->processar( ).

    cl_abap_unit_assert=>assert_initial(
                                   EXPORTING
                                        act = f_cut->v_token
                                        msg = |Não autenticou|
                                       quit = if_aunit_constants=>quit-no
                                      level = if_aunit_constants=>severity-low ).

    cl_abap_unit_assert=>assert_not_initial(
                                   EXPORTING
                                        act = lines( f_cut->m_inventario-materiais )
                                        msg = |Dados carregados|
                                       quit = if_aunit_constants=>quit-no
                                      level = if_aunit_constants=>severity-low ).

  ENDMETHOD.


ENDCLASS.
