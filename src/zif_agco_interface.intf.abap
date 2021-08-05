interface ZIF_AGCO_INTERFACE
  public .


  data GV_CENTRO type WERKS_D .

  methods DEFINIR_CENTRO .
  methods LER_TAXNUM
    returning
      value(R_CNPJ) type CHAR50 .
endinterface.
