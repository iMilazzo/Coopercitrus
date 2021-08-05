class ZCO_SI_PURCHASE_OUTBOUND definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods SI_PURCHASE_OUTBOUND
    importing
      !OUTPUT type ZSEND_PURCHASE
    exporting
      !INPUT type ZRESPONSE_PURCHASE1
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZCO_SI_PURCHASE_OUTBOUND IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZCO_SI_PURCHASE_OUTBOUND'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method SI_PURCHASE_OUTBOUND.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'SI_PURCHASE_OUTBOUND'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
