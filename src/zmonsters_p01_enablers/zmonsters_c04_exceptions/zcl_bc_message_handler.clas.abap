class ZCL_BC_MESSAGE_HANDLER definition
  public
  create public .

public section.

  data MS_ERROR_LOG type ZBC_S_ERROR_LOG .
  data MT_ERROR_LOG type ZBC_T_ERROR_LOG .

  class-methods GET_INSTANCE
    returning
      value(RO_MESSAGE_HANDLER) type ref to ZCL_BC_MESSAGE_HANDLER .
  methods CLEAR_ERROR_LOG .
  methods APPEND_ERROR_LOG
    importing
      !ID_TEXT type ANY optional
      !IS_RETURN type BAPIRET2 optional
      !ID_OBJECT type ANY optional
      !ID_F1 type ANY optional
      !ID_F2 type ANY optional
      !ID_F3 type ANY optional
      !ID_F4 type ANY optional
      !ID_F5 type ANY optional
      !IF_SYSTEM_MESSAGE type ABAP_BOOL optional
      !IT_BAPI_MESSAGES type BAPIRET2_T optional
      !IT_BDC_MESSAGES type ZTT_BC_BDCMSGCOLL optional .
  methods SHOW_ERROR_LOG
    importing
      !ID_TITLE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_MESSAGE_HANDLER IMPLEMENTATION.


METHOD append_error_log.
* Local Variables
  DATA: lt_call_stack    TYPE sys_callst,
        ls_call_stack    LIKE LINE OF lt_call_stack,
        lt_bapi_messages TYPE bapiret2_t,
        ls_bapi_messages TYPE bapiret2.

* Precondtions : If no data imported, exit
  IF id_text            IS INITIAL AND
     is_return          IS INITIAL AND
     it_bapi_messages[] IS INITIAL AND
     it_bdc_messages[]  IS INITIAL AND
     if_system_message  EQ abap_false.
    RETURN.
  ENDIF.

* We have a table of BDC messages passsed in
  IF it_bdc_messages[] IS NOT INITIAL.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
      TABLES
        imt_bdcmsgcoll = it_bdc_messages[]
        ext_return     = lt_bapi_messages[].
  ELSE.
    lt_bapi_messages[] = it_bapi_messages[].
  ENDIF.

* We have a table of BAPI messages passed in
  IF lt_bapi_messages[] IS NOT INITIAL.
    LOOP AT lt_bapi_messages INTO ls_bapi_messages.
      "Recursive Call
      append_error_log( EXPORTING is_return = ls_bapi_messages ).
    ENDLOOP.
    RETURN.
  ENDIF.

  IF if_system_message = abap_true.
* Copy system data
    ms_error_log-type       = sy-msgty.
    ms_error_log-id         = sy-msgid.
    ms_error_log-znumber    = sy-msgno.
    ms_error_log-message_v1 = sy-msgv1.
    ms_error_log-message_v2 = sy-msgv2.
    ms_error_log-message_v3 = sy-msgv3.
    ms_error_log-message_v4 = sy-msgv4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ms_error_log-message.
  ELSE.
* Copy relevant data
    ms_error_log-message    = is_return-message.
    ms_error_log-type       = is_return-type.
    ms_error_log-id         = is_return-id.
    ms_error_log-znumber    = is_return-number.
    ms_error_log-message_v1 = is_return-message_v1.
    ms_error_log-message_v2 = is_return-message_v2.
    ms_error_log-message_v3 = is_return-message_v3.
    ms_error_log-message_v4 = is_return-message_v4.
  ENDIF.
* Set traffic light based on error type
  CASE ms_error_log-type.
    WHEN 'S' OR 'I'.
      ms_error_log-zz_traffic = icon_led_green.
    WHEN 'W'.
      ms_error_log-zz_traffic = icon_led_yellow.
    WHEN 'E' OR 'A' OR 'X'.
      ms_error_log-zz_traffic = icon_led_red.
    WHEN OTHERS.
      ms_error_log-zz_traffic = icon_led_green.
  ENDCASE.
* Set Question Mark
  ms_error_log-zz_question = icon_system_help.
* Copy object data as needed
  ms_error_log-object_type = id_object.
  ms_error_log-field1      = id_f1.
  ms_error_log-field2      = id_f2.
  ms_error_log-field3      = id_f3.
  ms_error_log-field4      = id_f4.
  ms_error_log-field5      = id_f5.
* Override BAPI message with any text imported
  IF id_text IS NOT INITIAL.
    ms_error_log-message = id_text.
  ENDIF.

* Read Call Stack
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      et_callstack = lt_call_stack.

  LOOP AT lt_call_stack INTO ls_call_stack.
    CHECK ls_call_stack-progname NS 'ZCL_BC_MESSAGE_HANDLER'."i.e. we did not call ourself
    EXIT."From Loop
  ENDLOOP.

  IF ls_call_stack-progname IS NOT INITIAL.
    ms_error_log-zz_progname  = ls_call_stack-progname.
    ms_error_log-zz_eventtype = ls_call_stack-eventtype.
    ms_error_log-zz_eventname = ls_call_stack-eventname.
  ENDIF.

* Add entry to error log
  APPEND ms_error_log TO mt_error_log.

ENDMETHOD.


METHOD clear_error_log.

  REFRESH mt_error_log.
  FREE    mt_error_log.

ENDMETHOD.


METHOD get_instance.
* Local Variables
  DATA: ld_object    TYPE balobj_d  VALUE 'ZAU',
        ld_subobject TYPE balsubobj VALUE 'BSS',
        ld_extnumber TYPE balnrext.

  CREATE OBJECT ro_message_handler.

* Initialize instance
*  CALL METHOD ro_message_handler->init
*    EXPORTING
*      id_object    = ld_object
*      id_subobject = ld_subobject
*      id_extnumber = ld_extnumber
*      id_activity  = '01'
*    EXCEPTIONS
*      OTHERS       = 0.

ENDMETHOD.


METHOD show_error_log.
* If the system is running in the background, send any errors
* to the job log
  IF sy-batch = 'X'.
    LOOP AT mt_error_log INTO ms_error_log.
      CHECK ms_error_log-znumber IS NOT INITIAL.
      MESSAGE ID ms_error_log-id TYPE 'S' NUMBER ms_error_log-znumber
      WITH ms_error_log-message_v1 ms_error_log-message_v2
           ms_error_log-message_v3 ms_error_log-message_v4.
    ENDLOOP.
  ENDIF.

* If system is running in the background i.e. no user then it is
* fairly pointless to output an error log. All you get is a short
* dump
  CHECK sy-batch IS INITIAL.

* If we are running a unit test, do not pop up a box
  CHECK sy-title <> 'Internal Session for Isolated Test Class Execution'(001).

  CALL FUNCTION 'ZBC_SHOW_ERROR_LOG'
    EXPORTING
      id_title     = id_title
    TABLES
      it_error_log = mt_error_log.

ENDMETHOD.
ENDCLASS.
