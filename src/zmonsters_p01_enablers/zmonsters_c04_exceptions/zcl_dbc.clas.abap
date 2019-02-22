class ZCL_DBC definition
  public
  create public .

public section.

  class-methods ENSURE
    importing
      !WHICH_IS_TRUE_IF type ABAP_BOOL
      !THAT type ITEX132 .
  class-methods REQUIRE
    importing
      !WHICH_IS_TRUE_IF type ABAP_BOOL
      !THAT type ITEX132 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DBC IMPLEMENTATION.


METHOD ensure.
* Local Variables
  DATA: lt_call_stack     TYPE sys_callst,
        ls_call_stack     LIKE LINE OF lt_call_stack,
        ld_client_program TYPE dbglprog,
        ld_server_program TYPE dbglprog,
        ld_client_routine TYPE dbglevent,"Calling Routine
        ld_server_routine TYPE dbglevent,"Routine where exception raised
        ld_count          TYPE i,
        lo_log            TYPE REF TO zcl_bc_message_handler.

* Preconditions
  CHECK which_is_true_if                            = abap_false.
  CHECK zcl_bc_system_environment=>is_production( ) = abap_false.

  lo_log = zcl_bc_message_handler=>get_instance( ).

* Read Call Stack
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      et_callstack = lt_call_stack.

  "The call stack works backwards
  LOOP AT lt_call_stack INTO ls_call_stack.
    CHECK ls_call_stack-progname NS 'ZCL_DBC'."i.e. we did not call ourself
    ADD 1 TO ld_count.
    CASE ld_count.
      WHEN 1.
        ld_server_program = ls_call_stack-progname.
        ld_server_routine = ls_call_stack-eventname.
      WHEN 2.
        ld_client_program = ls_call_stack-progname.
        ld_client_routine = ls_call_stack-eventname.
      WHEN OTHERS.
        EXIT."From Loop
    ENDCASE.
  ENDLOOP.

*--------------------------------------------------------------------*
* Listing 04.14 - Building an Error Message
*--------------------------------------------------------------------*
  lo_log->append_error_log( :
  id_text = |{ 'This Diagnosis is intended for IT'(011) }| ),
  id_text = |{ 'Routine'(001) } { ld_server_routine } { 'of program'(002) }{ ld_server_program } { 'has a contract'(003) } | ),
  id_text = |{ 'with calling routine'(004) } { ld_client_routine } { 'of program'(002) } { ld_client_program } | ),
  id_text = |{ 'Routine'(001) } { ld_server_routine } { 'agrees to carry out a certain task'(005) } | ),
  id_text = |{ 'The task is to ensure that'(012) } { that } | ),
  id_text = |{ 'That task has not been fulfilled by routine'(013) } { ld_server_routine } | ),
  id_text = |{ 'Therefore there is an error (bug) in routine'(008) } { ld_server_routine } { 'that needs to be corrected'(009) } | ).

  RAISE EXCEPTION TYPE zcx_violated_postcondition
    EXPORTING
      md_condition = that
      mo_error_log = lo_log.

ENDMETHOD.


METHOD require.
* Local Variables
  DATA: lt_call_stack     TYPE sys_callst,
        ls_call_stack     LIKE LINE OF lt_call_stack,
        ld_client_program TYPE dbglprog,
        ld_server_program TYPE dbglprog,
        ld_client_routine TYPE dbglevent,"Calling Routine
        ld_server_routine TYPE dbglevent,"Routine where exception raised
        ld_count          TYPE i,
        lo_log            TYPE REF TO zcl_bc_message_handler.

* Preconditions
  CHECK which_is_true_if                            = abap_false.
  CHECK zcl_bc_system_environment=>is_production( ) = abap_false.

  lo_log = zcl_bc_message_handler=>get_instance( ).

* Read Call Stack
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      et_callstack = lt_call_stack.

  "The call stack works backwards
  LOOP AT lt_call_stack INTO ls_call_stack.
    CHECK ls_call_stack-progname NS 'ZCL_DBC'."i.e. we did not call ourself
    ADD 1 TO ld_count.
    CASE ld_count.
      WHEN 1.
        ld_server_program = ls_call_stack-progname.
        ld_server_routine = ls_call_stack-eventname.
      WHEN 2.
        ld_client_program = ls_call_stack-progname.
        ld_client_routine = ls_call_stack-eventname.
      WHEN OTHERS.
        EXIT."From Loop
    ENDCASE.
  ENDLOOP.

  lo_log->append_error_log( :
  id_text = | { 'This Diagnosis is intended for IT'(011) }| ),
  id_text = | { 'Routine'(001) } { ld_server_routine } { 'of program'(002) } { ld_server_program } { 'has a contract'(003) }| ),
  id_text = | { 'with routine'(015) } { ld_client_routine } { 'of program'(002) } { ld_client_program }| ),
  id_text = | { 'Routine'(001) } { ld_server_routine } { 'agrees to carry out a certain task'(005) }| ),
  id_text = | { 'This requires that'(006) } { that }| ),
  id_text = | { 'That condition has not been fulfilled by calling routine'(007) } { ld_client_routine }| ),
  id_text = | { 'and so therefore there is an error (bug) in routine'(014) } { ld_client_routine } { 'that needs to be corrected'(009) }| ).

  RAISE EXCEPTION TYPE zcx_violated_precondition
    EXPORTING
      md_condition = that
      mo_error_log = lo_log.

ENDMETHOD.
ENDCLASS.
