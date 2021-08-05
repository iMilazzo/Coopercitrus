class ZCL_AGCO_LOG_ITM definition
  public
  final
  create protected

  global friends ZCB_AGCO_LOG_ITM .

public section.

  interfaces IF_OS_STATE .

  methods GET_DETAIL
    returning
      value(RESULT) type ZDEMM_AGCO_DETAIL
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_ERROR_CODE
    returning
      value(RESULT) type ZDEMM_AGCO_ERROR_CODE
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_ITEM
    returning
      value(RESULT) type ZDEMM_AGCO_ITEM
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_POINTER
    returning
      value(RESULT) type ZDEMM_AGCO_POINTER
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_TRACKING_ID
    returning
      value(RESULT) type ZDEMM_AGCO_TRACKING_ID
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_VALUE
    returning
      value(RESULT) type ZDEMM_AGCO_VALUE
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_DETAIL
    importing
      !I_DETAIL type ZDEMM_AGCO_DETAIL
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_ERROR_CODE
    importing
      !I_ERROR_CODE type ZDEMM_AGCO_ERROR_CODE
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_POINTER
    importing
      !I_POINTER type ZDEMM_AGCO_POINTER
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_VALUE
    importing
      !I_VALUE type ZDEMM_AGCO_VALUE
    raising
      CX_OS_OBJECT_NOT_FOUND .
protected section.

  data TRACKING_ID type ZDEMM_AGCO_TRACKING_ID .
  data ITEM type ZDEMM_AGCO_ITEM .
  data POINTER type ZDEMM_AGCO_POINTER .
  data VALUE type ZDEMM_AGCO_VALUE .
  data DETAIL type ZDEMM_AGCO_DETAIL .
  data ERROR_CODE type ZDEMM_AGCO_ERROR_CODE .
private section.
ENDCLASS.



CLASS ZCL_AGCO_LOG_ITM IMPLEMENTATION.


  method GET_DETAIL.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute DETAIL
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = DETAIL.

           " GET_DETAIL
  endmethod.


  method GET_ERROR_CODE.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute ERROR_CODE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = ERROR_CODE.

           " GET_ERROR_CODE
  endmethod.


  method GET_ITEM.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute ITEM
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = ITEM.

           " GET_ITEM
  endmethod.


  method GET_POINTER.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute POINTER
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = POINTER.

           " GET_POINTER
  endmethod.


  method GET_TRACKING_ID.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute TRACKING_ID
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = TRACKING_ID.

           " GET_TRACKING_ID
  endmethod.


  method GET_VALUE.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute VALUE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = VALUE.

           " GET_VALUE
  endmethod.


  method IF_OS_STATE~GET.
***BUILD 090501
     " returning result type ref to object
************************************************************************
* Purpose        : Get state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  create object STATE_OBJECT.
  call method STATE_OBJECT->SET_STATE_FROM_OBJECT( ME ).
  result = STATE_OBJECT.

  endmethod.


  method IF_OS_STATE~HANDLE_EXCEPTION.
***BUILD 090501
     " importing I_EXCEPTION type ref to IF_OS_EXCEPTION_INFO optional
     " importing I_EX_OS type ref to CX_OS_OBJECT_NOT_FOUND optional
************************************************************************
* Purpose        : Handles exceptions during attribute access.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : If an exception is raised during attribut access,
*                  this method is called and the exception is passed
*                  as a paramater. The default is to raise the exception
*                  again, so that the caller can handle the exception.
*                  But it is also possible to handle the exception
*                  here in the callee.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
* - 2000-08-02   : (SB)  OO Exceptions
************************************************************************
* Modify if you like
************************************************************************

  if i_ex_os is not initial.
    raise exception i_ex_os.
  endif.

  endmethod.


  method IF_OS_STATE~INIT.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Initialisation of the transient state partition.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Transient state is initial.
*
* OO Exceptions  : -
*
* Implementation : Caution!: Avoid Throwing ACCESS Events.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~INVALIDATE.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Do something before all persistent attributes are
*                  cleared.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : Whatever you like to do.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~SET.
***BUILD 090501
     " importing I_STATE type ref to object
************************************************************************
* Purpose        : Set state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  STATE_OBJECT ?= I_STATE.
  call method STATE_OBJECT->SET_OBJECT_FROM_STATE( ME ).

  endmethod.


  method SET_DETAIL.
***BUILD 090501
     " importing I_DETAIL
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute DETAIL
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_DETAIL <> DETAIL ).

    DETAIL = I_DETAIL.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_DETAIL <> DETAIL )

           " GET_DETAIL
  endmethod.


  method SET_ERROR_CODE.
***BUILD 090501
     " importing I_ERROR_CODE
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute ERROR_CODE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_ERROR_CODE <> ERROR_CODE ).

    ERROR_CODE = I_ERROR_CODE.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_ERROR_CODE <> ERROR_CODE )

           " GET_ERROR_CODE
  endmethod.


  method SET_POINTER.
***BUILD 090501
     " importing I_POINTER
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute POINTER
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_POINTER <> POINTER ).

    POINTER = I_POINTER.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_POINTER <> POINTER )

           " GET_POINTER
  endmethod.


  method SET_VALUE.
***BUILD 090501
     " importing I_VALUE
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute VALUE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_VALUE <> VALUE ).

    VALUE = I_VALUE.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_VALUE <> VALUE )

           " GET_VALUE
  endmethod.
ENDCLASS.
