*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZDS_LAUGHING_MONSTER</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Laughing Monster Debugger Script</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                                               "prolog

  METHOD init.
*--------------------------------------------------------------------*
* Listing 06.07 - INIT Method
*--------------------------------------------------------------------*
* Local Variables
    DATA(log_header_data) = VALUE bal_s_log(
            object    = 'MONSTER'
            extnumber = '12345' ).

    TRY.
        "The APPL_LOG object is inherited from the superclass
        appl_log = cl_tpda_script_messages=>create_appl_log( log_header_data ).

      CATCH cx_tpda_application_log .
        "No point going on / Return Control to Debugger
        me->break( ).
    ENDTRY.

  ENDMETHOD.                                               "init

  METHOD script.
*--------------------------------------------------------------------*
* Listing 06.08 - SCRIPT Method for Laughing Example
*--------------------------------------------------------------------*
* Local Variables
    DATA: current_number TYPE sy-tabix,
          variable_name  TYPE tpda_var_name,
          info_text      TYPE char255,
          message_type   TYPE char01.

* What line are we on?
    TRY.
        DATA(current_line) = abap_source->line( ).

      CATCH cx_tpda_src_info .
        RETURN.
      CATCH cx_tpda_src_descr_invalidated .
        RETURN.
    ENDTRY.

* Get Current Variable Values
    variable_name = 'CURRENT_NUMBER'.

    TRY.
        DATA(variable_value) =
          cl_tpda_script_data_descr=>get_simple_value( variable_name ).

        current_number = variable_value.

      CATCH cx_tpda_varname .
        RETURN.
      CATCH cx_tpda_script_no_simple_type .
        RETURN.
    ENDTRY.

* Analyse current situation
    CASE current_line.
      WHEN 29."Laugh Count Changed
        info_text = |Laugh trigger { current_number } is supposed to be exactly divisible by 3|.
        IF current_number MOD 3 <> 0.
          CONCATENATE info_text 'and it is not'
          INTO info_text SEPARATED BY space.
          message_type = 'E'.
        ELSE.
          CONCATENATE info_text 'and it is'
          INTO info_text SEPARATED BY space.
          message_type = 'S'.
        ENDIF.
      WHEN 33."Lightning Count Changed
        info_text = |Lightning trigger { current_number } is supposed to be exactly divisible by 5|.
        IF current_number MOD 5 <> 0.
          CONCATENATE info_text 'and it is not'
          INTO info_text SEPARATED BY space.
          message_type = 'E'.
        ELSE.
          CONCATENATE info_text 'and it is'
          INTO info_text SEPARATED BY space.
          message_type = 'S'.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

* Write the information into the application log
    TRY.
        appl_log->add_message_free_text(
            p_mess_type = message_type
            p_text      = info_text ).
      CATCH cx_tpda_application_log .
        RETURN.
    ENDTRY.

    "When done, return control to the real debugger
    IF current_number = 10.
      me->break( ).
    ENDIF.

  ENDMETHOD.                                               "script

  METHOD end.
*---------------------------------------------------------------------*
* Listing 06.09 - END
*--------------------------------------------------------------------*
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here
    TRY.
        appl_log->display_log( p_title = 'Laughing Monster Log' ).

      CATCH cx_tpda_application_log .
        RETURN.
    ENDTRY.

  ENDMETHOD.                                               "end
ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
