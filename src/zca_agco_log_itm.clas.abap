class ZCA_AGCO_LOG_ITM definition
  public
  inheriting from ZCB_AGCO_LOG_ITM
  final
  create private .

public section.

  class-data AGENT type ref to ZCA_AGCO_LOG_ITM read-only .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCA_AGCO_LOG_ITM IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
***BUILD 090501
************************************************************************
* Purpose        : Initialize the 'class'.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Singleton is created.
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 1999-09-20   : (OS) Initial Version
* - 2000-03-06   : (BGR) 2.0 modified REGISTER_CLASS_AGENT
************************************************************************
* GENERATED: Do not modify
************************************************************************

  create object AGENT.

  call method AGENT->REGISTER_CLASS_AGENT
    exporting
      I_CLASS_NAME          = 'ZCL_AGCO_LOG_ITM'
      I_CLASS_AGENT_NAME    = 'ZCA_AGCO_LOG_ITM'
      I_CLASS_GUID          = '42010AFEF70F1EEBBBD18D84FFEFDC27'
      I_CLASS_AGENT_GUID    = '42010AFEF70F1EEBBBD18DA700629C27'
      I_AGENT               = AGENT
      I_STORAGE_LOCATION    = 'ZMM_AGCO_LOG_ITM'
      I_CLASS_AGENT_VERSION = '2.0'.

           "CLASS_CONSTRUCTOR
  endmethod.
ENDCLASS.
