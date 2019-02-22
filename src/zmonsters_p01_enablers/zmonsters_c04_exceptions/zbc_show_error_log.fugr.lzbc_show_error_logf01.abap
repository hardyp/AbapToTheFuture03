*----------------------------------------------------------------------*
***INCLUDE LZBC_SHOW_ERROR_LOGF01.
*----------------------------------------------------------------------*
*-----------------------------------------------------------------------
* LCL_DIALOG_BOX: definition
*-----------------------------------------------------------------------
CLASS lcl_dialog_box DEFINITION FINAL.

  PUBLIC SECTION.
*   open and display dialog box
    METHODS: open,
             set_column_attributes,
             on_link_click
             FOR EVENT link_click OF cl_salv_events_table
             IMPORTING
               row
               column.

*   attributes
    DATA mo_alv_table TYPE REF TO cl_salv_table.              "ALV grid

ENDCLASS.                    "lcl_dialog_box DEFINITION

*-----------------------------------------------------------------------
* LCL_DIALOG_BOX: implementation
*-----------------------------------------------------------------------
CLASS lcl_dialog_box IMPLEMENTATION.
*   open and display dialog box-----------------------------------------
  METHOD open.
* Local Variables
    DATA: lr_functions  TYPE REF TO cl_salv_functions.

* Create SALV object
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv_table
          CHANGING
            t_table      = it_error_log[] ).
      CATCH cx_salv_msg.
        MESSAGE s290(zbc_horizontool_mc01)."Report in Trouble
        RETURN.
    ENDTRY.

* Display Basic Toolbar
    lr_functions = mo_alv_table->get_functions( ).
    lr_functions->set_all( if_salv_c_bool_sap=>true ).

* Make sure this is in fact a dialog box
    CALL METHOD mo_alv_table->set_screen_popup
      EXPORTING
        start_column = 5
        end_column   = 100
        start_line   = 5
        end_line     = gd_end_line.

* Set Hotspots, rename and hide columns etc
    set_column_attributes( ).

* Display ALV report
    mo_alv_table->display( ).

  ENDMETHOD.                    "open

*&---------------------------------------------------------------------*
*&      Form  SET_COLUMN_ATTRIBUTES
*&---------------------------------------------------------------------*
  METHOD set_column_attributes.
* Local Variables
    DATA: lo_columns   TYPE REF TO cl_salv_columns_table,
          lo_column    TYPE REF TO cl_salv_column_table,
          lt_columns   TYPE        salv_t_column_ref,
          ls_columns   TYPE        salv_s_column_ref,
          ld_tabix     TYPE sy-tabix,
          lf_blank     TYPE abap_bool,
          lo_events    TYPE REF TO cl_salv_events_table,
          ls_error_log LIKE LINE OF it_error_log.

    FIELD-SYMBOLS: <ld_component> TYPE any.

    TRY.
**********************************************************************
* Optimise Column Width
**********************************************************************
        lo_columns = mo_alv_table->get_columns( ).
        lo_columns->set_optimize( if_salv_c_bool_sap=>true ).

**********************************************************************
* Set Hotspots
**********************************************************************
        lo_column ?= lo_columns->get_column( 'ZZ_QUESTION' ).
        lo_column->set_tooltip( 'Show message long text'(003) ).

        CALL METHOD lo_column->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.

        lo_column ?= lo_columns->get_column( 'FIELD1' ).
        lo_column->set_tooltip( 'Document number for drill down'(002) ).

        CALL METHOD lo_column->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.

        lo_events = mo_alv_table->get_event( ).

        "Event Handler
        SET HANDLER on_link_click FOR lo_events.

        lo_column ?= lo_columns->get_column( 'OBJECT_TYPE' ).
        lo_column->set_tooltip( 'Transaction or object for drill down'(005) ).

**********************************************************************
* Change Names
**********************************************************************
        lo_column ?= lo_columns->get_column( 'ZZ_EVENTNAME' ).
        lo_column->set_long_text( 'Subroutine'(004) ).

**********************************************************************
* Hide Columns
**********************************************************************
* Check each column of the internal table to see if it is totally empty
        lt_columns = lo_columns->get( ).

        LOOP AT lt_columns INTO ls_columns.
          lf_blank = abap_true.
          LOOP AT it_error_log INTO ls_error_log.
            lf_blank = abap_true.
            ASSIGN COMPONENT ls_columns-columnname OF STRUCTURE ls_error_log TO <ld_component>.
            CHECK sy-subrc = 0.
            IF <ld_component> IS NOT INITIAL.
              lf_blank = abap_false.
            ENDIF.
          ENDLOOP."Error Table
* If all the entries are blank, hide the column
          IF lf_blank = abap_true.
            lo_column ?= lo_columns->get_column( ls_columns-columnname ).
            lo_column->set_visible( if_salv_c_bool_sap=>false ).
* We only want to show the first ten columns
          ELSEIF ld_tabix > 10.
            lo_column ?= lo_columns->get_column( ls_columns-columnname ).
            lo_column->set_visible( if_salv_c_bool_sap=>false ).
          ELSE.
            ld_tabix = ld_tabix + 1."Do not have more than ten fields
          ENDIF.
        ENDLOOP."Columns in the ALV output

      CATCH cx_salv_not_found.
        MESSAGE 'Report in trouble' TYPE 'S'.
      CATCH cx_salv_data_error.
        MESSAGE 'Report in trouble' TYPE 'S'.
    ENDTRY.

  ENDMETHOD.                  " SET_COLUMN_ATTRIBUTES
**********************************************************************
* METHOD on_link_click
*             FOR EVENT link_click OF cl_salv_events_table
*             IMPORTING
*               row
*               column.
**********************************************************************
  METHOD on_link_click.
* Local Variables
    DATA: ls_error_log LIKE LINE OF it_error_log.

    READ TABLE it_error_log INTO ls_error_log INDEX row.
    CHECK sy-subrc = 0.

    CASE column.
      WHEN 'ZZ_QUESTION'.
        CHECK ls_error_log-znumber IS NOT INITIAL.
        MESSAGE ID ls_error_log-id TYPE 'I' NUMBER ls_error_log-znumber
        WITH ls_error_log-message_v1 ls_error_log-message_v2
             ls_error_log-message_v3 ls_error_log-message_v4.
      WHEN 'FIELD1'.
        CHECK ls_error_log-field1      IS NOT INITIAL.
        CHECK ls_error_log-object_type IS NOT INITIAL.
*        zcl_bc_drill_down=>call_transaction( EXPORTING id_transaction = ls_error_log-object_type
*                                                       id_f1          = ls_error_log-field1
*                                                       id_f2          = ls_error_log-field2
*                                                       id_f3          = ls_error_log-field3
*                                                       id_f4          = ls_error_log-field4
*                                                       id_f5          = ls_error_log-field5 ).
      WHEN OTHERS.
        EXIT.
    ENDCASE.

  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "lcl_dialog_box IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  START_LIST_VIEWER
*&---------------------------------------------------------------------*
FORM start_list_viewer.
* Local Variables
  DATA: lo_dialog_box TYPE REF TO lcl_dialog_box.

  IF id_title IS SUPPLIED.
    SET TITLEBAR '100' WITH id_title.
  ENDIF.

* Create and open dialog box
  CREATE OBJECT lo_dialog_box.
  lo_dialog_box->open( ).

  LEAVE TO SCREEN 0.

ENDFORM.                    " START_LIST_VIEWER
