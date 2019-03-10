class ZCL_FUNCTION_MODULE_WRAPPER definition
  public
  abstract
  create public .

public section.

  methods THROW_EXCEPTION_ON_ERROR_FROM
    importing
      !ID_FUNCTION_MODULE type RS38L_FNAM .
  methods REMOVE_EXISTING_MESSAGES .
  methods STORE_EXCEPTION_DETAILS_FROM
    importing
      !ID_FUNCTION_MODULE type RS38L_FNAM .
protected section.
private section.

  data FUNCTION_NAME type RS38L_FNAM .
  data RETURN_CODE type SY-SUBRC .
  data EXCEPTION_NAME type RS38L_PAR_ .
  data EXCEPTION_TEXT type PARAMTEXT .
  data CALL_STACK_TABLE type SYS_CALLST .

  methods NO_T100_MESSAGE_WAS_RAISED
    returning
      value(RF_NO_T100_MESSAGE_WAS_RAISED) type ABAP_BOOL .
  methods THE_EXCEPTION_MAPPED_TO_THE
    importing
      !ID_SUBRC type SY-SUBRC
    returning
      value(RD_EXCEPTION_NAME) type RS38L_PAR_ .
  methods DESCRIPTION_OF
    importing
      !ID_EXCEPTION_NAME type PARAMETER
    returning
      value(RD_EXCEPTION_TEXT) type PARAMTEXT .
ENDCLASS.



CLASS ZCL_FUNCTION_MODULE_WRAPPER IMPLEMENTATION.


METHOD description_of.

    SELECT SINGLE stext
      INTO  rd_exception_text
      FROM  funct
      WHERE funcname  EQ function_name
      AND   parameter EQ id_exception_name
      AND   kind      EQ 'X'.

    IF sy-subrc <> 0.
      rd_exception_text = id_exception_name.
      RETURN.
    ENDIF.

    IF rd_exception_text IS INITIAL.
      rd_exception_text = id_exception_name.
      RETURN.
    ENDIF.

  ENDMETHOD.


METHOD no_t100_message_was_raised.

    rf_no_t100_message_was_raised = abap_false.

    CHECK sy-msgid IS INITIAL OR
          sy-msgno IS INITIAL.

    rf_no_t100_message_was_raised = abap_true.

  ENDMETHOD.


METHOD remove_existing_messages.

    CLEAR: sy-msgid,
           sy-msgno,
           sy-msgty,
           sy-msgv1,
           sy-msgv2,
           sy-msgv3,
           sy-msgv4.

  ENDMETHOD.


METHOD store_exception_details_from.

    return_code = sy-subrc.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = call_stack_table.

    DELETE call_stack_table WHERE progname CS 'ZCL_FUNCTION_MODULE_WRAPPER'.

    function_name  = id_function_module.
    exception_name = the_exception_mapped_to_the( return_code ).
    exception_text = description_of( exception_name ).

  ENDMETHOD.


METHOD the_exception_mapped_to_the.

    SELECT  parameter
      FROM  fupararef UP TO 1 ROWS
      INTO  rd_exception_name
      WHERE funcname  = function_name
      AND   r3state   = 'A'
      AND   paramtype = 'X' "It's an exception
      AND   pposition = id_subrc.
    ENDSELECT.

    IF sy-subrc <> 0.
      CLEAR rd_exception_name.
      RETURN.
    ENDIF.

  ENDMETHOD.


METHOD throw_exception_on_error_from.

    store_exception_details_from( id_function_module ).

    IF no_t100_message_was_raised( ).
      RAISE EXCEPTION TYPE zcx_function_module_error
        EXPORTING
          function_module  = id_function_module
          exception_name   = exception_name
          exception_text   = exception_text
          no_t100_message  = abap_true
          call_stack_table = call_stack_table.
    ELSE.
      RAISE EXCEPTION TYPE zcx_function_module_error
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          function_module  = id_function_module
          exception_name   = exception_name
          exception_text   = exception_text
          no_t100_message  = abap_false
          call_stack_table = call_stack_table.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
