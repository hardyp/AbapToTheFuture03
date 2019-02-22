*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_MONITOR_IO1
*&---------------------------------------------------------------------*
* Local Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD main.
* Local Variables
    DATA: ld_report_name TYPE string.

    CONCATENATE sy-tcode sy-title INTO ld_report_name
    SEPARATED BY ' : '.

    CREATE OBJECT mo_model.
    CREATE OBJECT mo_view.
    CREATE OBJECT mo_controller
      EXPORTING
        io_model = mo_model
        io_view  = mo_view.

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_ouput( ).

    "It is bad news to pass system variables as parameters
    DATA(ld_repid) = sy-repid.

    IF sy-batch IS INITIAL.
*--------------------------------------------------------------------*
* Listing 10.35 - Calling a SALV report whilst creating a container
*                 automatically
*--------------------------------------------------------------------*
* Program flow is as follows:-
* ZCL_BC_VIEW_SALV_TABLE->CREATE_CONTAINER_PREPARE_DATA
* Function ZSALV_CSQT_CREATE_CONTAINER
* ZSALV_CSQT_CREATE_CONTAINER->FILL_CONTAINER_CONTENT
* ZCL_BC_VIEW_SALV_TABLE->PREPARE_DISPLAY_DATA
* --> Initialise
* --> Application Specific Changes
* --> Display
*--------------------------------------------------------------------*
      mo_view->create_container_prep_display(
      EXPORTING
        id_report_name        = ld_repid                                                                                                                                             " Calling program
        if_start_in_edit_mode = abap_false
        is_layout             = mo_model->ms_layout
        id_edit_control_field = mo_model->md_edit_control_field
        it_editable_fields    = mo_model->mt_editable_fields
        it_technicals         = mo_model->mt_technicals
        it_hidden             = mo_model->mt_hidden
        it_hotspots           = mo_model->mt_hotspots
        it_checkboxes         = mo_model->mt_checkboxes
        it_subtotal_fields    = mo_model->mt_subtotal_fields
        it_field_texts        = mo_model->mt_field_texts                                                                                                                                    " Display Variant as specified by user
        it_user_commands      = mo_model->mt_user_commands
      CHANGING
        ct_data_table         = mo_model->mt_output_data ).

    ELSE.
* If this is running in the background there is no way
* in the world we want/need a container, as there is no
* chance for the user to press any user command buttons or
* edit the data, as there is no user, and no screen for the
* container to live on for that matter
      mo_view->prepare_display_data(
        EXPORTING
          id_report_name     = ld_repid
          it_technicals      = mo_model->mt_technicals
          it_hidden          = mo_model->mt_hidden
          it_hotspots        = mo_model->mt_hotspots
          it_checkboxes      = mo_model->mt_checkboxes
          it_subtotal_fields = mo_model->mt_subtotal_fields
          it_field_texts     = mo_model->mt_field_texts
          it_user_commands   = mo_model->mt_user_commands
        CHANGING
          ct_data_table      = mo_model->mt_output_data ).
    ENDIF."Are we running in the background?

    IF go_selections->p_email IS NOT INITIAL.
      mo_controller->send_email( ).
    ENDIF.

  ENDMETHOD.                                               "main

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_numbr = is_numbr.
    s_name  = is_name.
    p_bhf   = ip_bhf.
    p_vari  = ip_vari.
    p_edit  = ip_edit.
    p_macro = ip_macro.
    p_send  = ip_send.
    p_email = ip_email.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

  METHOD application_specific_changes.
*--------------------------------------------------------------------*
* Listing 10.08 - Application Specific Changes Method in Calling Program
*--------------------------------------------------------------------*
    TRY.
        optimise_column_width( ).

        set_column_attributes( :
        id_field_name  = 'KEY'
        if_is_technical = abap_true ),
        id_field_name  = 'PARENT_KEY'
        if_is_technical = abap_true ),
        id_field_name  = 'ROOT_KEY'
        if_is_technical = abap_true ),
        id_field_name  = 'MONSTER_NUMBER'
        if_is_hotspot  = abap_true
        id_tooltip     = |'Click here to see the Monster Master Record'| ),
        id_field_name  = 'HAT_SIZE'
        if_is_visible  = abap_false ),
        id_field_name  = 'MONSTER_COUNT'
        id_long_text   = 'Monster Count'
        if_is_subtotal = abap_true ).

      CATCH cx_salv_not_found INTO DATA(lo_not_found).
        DATA(lf_error_occurred) = abap_true.
        "Object = Column
        "Key    = Field Name e.g. VBELN
        zcl_dbc=>require( that             = |{ lo_not_found->object } { lo_not_found->key } must exist|
                          which_is_true_if = boolc( lf_error_occurred = abap_false ) ).
      CATCH cx_salv_data_error INTO DATA(lo_data_error).
        DATA(ls_error) = lo_data_error->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
      CATCH cx_salv_msg INTO DATA(lo_error).
        ls_error = lo_error->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
    ENDTRY.

  ENDMETHOD."Application Specific Changes

  METHOD ida_demo.
*--------------------------------------------------------------------*
* Listing 10.39 - Coding SALV with IDA with Selection Criteria
*--------------------------------------------------------------------*
    TRY.
        "Tell the ALV what database table we want
        DATA(alv_display_object) = cl_salv_gui_table_ida=>create(
        iv_table_name = 'ZTMONSTER_HEADER' ).
        "Prepare the select-options
        DATA(range_table_collector) = NEW cl_salv_range_tab_collector( ).
        range_table_collector->add_ranges_for_name(
        iv_name = 'MONSTER_NUMBER' it_ranges = go_selections->s_numbr[] ).
        range_table_collector->add_ranges_for_name(
        iv_name = 'NAME' it_ranges = go_selections->s_name[] ).
        range_table_collector->get_collected_ranges(
        IMPORTING et_named_ranges = DATA(selection_option_table) ).
        "Prepare any parameters from the selection screen
        DATA(parameter_factory) = alv_display_object->condition_factory( ).
        DATA(parameter_object)  = parameter_factory->equals( name = 'BED_HIDER_FLAG'
        value = go_selections->p_bhf ).
        "Tell the ALV about these restrictions
        alv_display_object->set_select_options(
        it_ranges    = selection_option_table
        io_condition = parameter_object ).
        "Off we go!
        alv_display_object->fullscreen( )->display( ).
      CATCH cx_salv_db_connection.
      CATCH cx_salv_db_table_not_supported.
      CATCH cx_salv_ida_contract_violation.
    ENDTRY.

*--------------------------------------------------------------------*
* Listing 10.40 : Coding SALV with IDA without Selection Criteria
*--------------------------------------------------------------------*
    cl_salv_gui_table_ida=>create(
    iv_table_name = 'ZMONSTER_HEADER' )->fullscreen( )->display( ).

*--------------------------------------------------------------------*
* Listing 10.41 : Creating IDA SALV for CDS View (Acronym Soup)
*--------------------------------------------------------------------*
    cl_salv_gui_table_ida=>create_for_cds_view(
    'ZCDS_MONSTER_DELIVERIES' )->fullscreen( )->display( ).

  ENDMETHOD.

ENDCLASS."Local View

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT mo_monster_model.

  ENDMETHOD.                    "constructor

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*
* Local Variables
    DATA: lt_selections TYPE ztt_bc_coseltab,
          ls_selections LIKE LINE OF lt_selections,
          ls_numbers    LIKE LINE OF go_selections->s_numbr,
          ls_names      LIKE LINE OF go_selections->s_name.

    LOOP AT go_selections->s_numbr INTO ls_numbers.
      ls_selections-field  = 'MONSTER_NUMBER'.
      ls_selections-sign   = ls_numbers-sign.
      ls_selections-option = ls_numbers-option.
      ls_selections-low    = ls_numbers-low.
      ls_selections-high   = ls_numbers-high.
      APPEND ls_selections TO lt_selections.
    ENDLOOP.

    LOOP AT go_selections->s_name INTO ls_names.
      ls_selections-field  = 'NAME'.
      ls_selections-sign   = ls_names-sign.
      ls_selections-option = ls_names-option.
      ls_selections-low    = ls_names-low.
      ls_selections-high   = ls_names-high.
      APPEND ls_selections TO lt_selections.
    ENDLOOP.

    IF lt_selections[] IS INITIAL.
      ls_selections-field  = 'MONSTER_NUMBER'.
      ls_selections-sign   = 'I'.
      ls_selections-option = 'GT'.
      ls_selections-low    = '0000000001'.
      APPEND ls_selections TO lt_selections.
    ENDIF.

    DATA: lt_monster_headers TYPE zttyp_monster_header.

    mo_monster_model->retrieve_headers_by_attribute(
    EXPORTING it_selections      = lt_selections
    IMPORTING et_monster_headers = lt_monster_headers ).

    FIELD-SYMBOLS: <ls_monster_headers> LIKE LINE OF lt_monster_headers,
                   <ls_output_data>     LIKE LINE OF et_output_data.

    "The two table are identical, except the ALV one has the CELLTAB
    "addition, to make cells editable
    LOOP AT lt_monster_headers ASSIGNING <ls_monster_headers>.
      APPEND INITIAL LINE TO et_output_data ASSIGNING <ls_output_data>.
      MOVE-CORRESPONDING <ls_monster_headers> TO <ls_output_data>.
    ENDLOOP.

  ENDMETHOD.                                               "get_data

ENDCLASS.                    "lcl_persistency_layer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
* When creating the model for real we do not fill the import parameter
* and thus the data is read for real
* When creating the model within a unit test, we pass in a reference to
* the fake database access class
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    IF io_access_class IS SUPPLIED.
      mo_persistency_layer = io_access_class.
    ELSE.
      CREATE OBJECT mo_persistency_layer.
    ENDIF.

    fill_user_commands( ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    mo_persistency_layer->get_data( IMPORTING et_output_data = mt_output_data ).

  ENDMETHOD.                                               "data_retrieval

**********************************************************************
* METHOD prepare_data_for_output
**********************************************************************
* Get text names of objects, mapping, etc etc
*----------------------------------------------------------------------*
  METHOD prepare_data_for_ouput.

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.
*--------------------------------------------------------------------*
* Listing 10.27 - Method in the Model Class to Define User Commands
*--------------------------------------------------------------------*
* Local Variables
    DATA: ls_user_commands LIKE LINE OF mt_user_commands.

    REFRESH mt_user_commands.

    IF go_selections->p_edit = abap_true.

      CLEAR ls_user_commands.
      ls_user_commands-function  = 'ZEDIT'.
      ls_user_commands-icon      = icon_change.
      ls_user_commands-butn_type = 0.                      "Normal Button
      ls_user_commands-text      = 'Edit Grid'.
      APPEND ls_user_commands TO mt_user_commands.

      CLEAR ls_user_commands.
      ls_user_commands-function  = 'ZSAVE'.
      ls_user_commands-icon      = icon_system_save.
      ls_user_commands-butn_type = 0.                      "Normal Button
      ls_user_commands-text      = 'Save Data'.
      APPEND ls_user_commands TO mt_user_commands.

    ENDIF."Do we want to edit the data?

    CLEAR ls_user_commands.
    ls_user_commands-function  = 'ZEXCEL'.
    ls_user_commands-icon      = icon_xls.
    ls_user_commands-butn_type = 0.                        "Normal Button
    ls_user_commands-text      = 'Spreadsheet'.
    APPEND ls_user_commands TO mt_user_commands.

  ENDMETHOD.                                               "fill_user_commands

  METHOD user_command.
* Local Variables
    DATA: underlying_alv_grid TYPE REF TO cl_gui_alv_grid,
          layout_info         TYPE lvc_s_layo,
          field_catalog_table TYPE lvc_t_fcat.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF mt_output_data.

    CASE id_user_command.
      WHEN '&IC1'.
        READ TABLE mt_output_data ASSIGNING <ls_output> INDEX id_row.
        CHECK sy-subrc = 0.
        CASE id_column.
          WHEN 'VBELN'.
          WHEN OTHERS.
            RETURN.
        ENDCASE."What column was selected for drill down?

      WHEN OTHERS.
        RETURN.
    ENDCASE."What user command was chosen?

  ENDMETHOD."User Command / Model

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_model = io_model.
    mo_view  = io_view.

    "Make the controller react to the views events
    SET HANDLER on_user_command FOR mo_view.
    "If the model changes some data, then it needs to
    "tell the controller, so it can tell the view
    "to refresh the data
    SET HANDLER on_data_changed FOR mo_model.

  ENDMETHOD.                    "constructor

*--------------------------------------------------------------------*
* METHOD send_email
*--------------------------------------------------------------------*
  METHOD send_email.
* Local Variables
    DATA: ld_program_name TYPE sy-repid,
          ld_short_title  TYPE zexcel_sheet_title,
          ld_long_title   TYPE char80,
          ld_tcode        TYPE sy-tcode,
          lo_excel        TYPE REF TO zcl_excel.

* Preconditions
    CHECK go_selections->p_send EQ abap_true.

    ld_short_title  = sy-title.
    ld_long_title   = sy-title.
    ld_program_name = sy-repid.                            "bad news to pass this directly
    ld_tcode        = 'Z_MONSTER_MONITOR'.

    "Step 1 - convert internal table into an EXCEL object
*    CALL METHOD zcl_excel_emailer=>convert_salv_to_excel
*      EXPORTING
*        id_title = ld_short_title
*        io_salv  = mo_view->mo_alv_grid
*        it_table = mo_model->mt_output_data[]
*      CHANGING
*        co_excel = lo_excel.
*
*    "Step 2 - make any changes to the spreadsheet object that relate to
*    "this report
*
*    "Step 3 - send the amended EXCEL object as an email
*    CALL METHOD zcl_excel_emailer=>send_excel_object
*      EXPORTING
*        io_excel        = lo_excel
*        id_tcode        = ld_tcode
*        id_report_title = ld_long_title
*        id_program_name = ld_program_name
*        id_email        = go_selections->p_email.

  ENDMETHOD.                                               "send_email

  METHOD on_user_command.
*--------------------------------------------------------------------*
* Listing 10.36 - User Command to Make a SALV Grid Editable
*--------------------------------------------------------------------*
* FOR EVENT added_function OF cl_salv_events
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*
* Local Variables
    DATA: ls_layout TYPE lvc_s_layo,
          lf_valid  TYPE abap_bool ##needed,
          ld_answer TYPE char01.

    DATA(underlying_alv_grid) = mo_view->get_alv_grid( ).

    CASE ed_user_command.
      WHEN 'ZEDIT'."A command to make the grid editable

        IF underlying_alv_grid IS NOT BOUND.
          RETURN.
        ENDIF.

        underlying_alv_grid->get_frontend_fieldcatalog(
          IMPORTING et_fieldcatalog = DATA(field_catalog_table) ).

        "We only want two of the columns to be editable
        make_column_editable( :
        EXPORTING id_column_name = 'AGE'
        CHANGING  ct_fcat        = field_catalog_table ),
        EXPORTING id_column_name = 'NO_OF_HEADS'
        CHANGING  ct_fcat        = field_catalog_table ).

        underlying_alv_grid->set_frontend_fieldcatalog( field_catalog_table ).

        ls_layout-stylefname = 'CELLTAB'.

        underlying_alv_grid->set_frontend_layout( ls_layout ).

        underlying_alv_grid->refresh_table_display( ).

        cl_gui_cfw=>flush( ).

      WHEN 'ZSAVE'."A command to save your changes
        "Trick to update the internal table
        CALL METHOD underlying_alv_grid->check_changed_data
          IMPORTING
            e_valid = lf_valid.
        "Code to save the data
        MESSAGE 'Data Saved' TYPE 'I'.

      WHEN 'ZEXCEL'.
        DATA: lo_converter TYPE REF TO zcl_excel_converter,
              lo_excel     TYPE REF TO zcl_excel,
              lo_exception TYPE REF TO zcx_excel.

        CREATE OBJECT lo_converter.

        TRY.
            "Need to make the age figures negative so the
            "conditional formatting with traffic lights works
            FIELD-SYMBOLS: <ls_monsters> LIKE LINE OF mo_model->mt_output_data.

*            LOOP AT mo_model->mt_output_data ASSIGNING <ls_monsters>.
*              <ls_monsters>- = <ls_monsters>-age * -1.
*            ENDLOOP.

*--------------------------------------------------------------------*
* Listing 11.28 - Uploading an Excel Template
* Retrieve template from the database
*--------------------------------------------------------------------*
            DATA: ls_template     TYPE ztexcel_template,
                  lo_excel_reader TYPE REF TO zif_excel_reader,
                  lo_worksheet    TYPE REF TO zcl_excel_worksheet.
*
*            IF go_selections->p_macro = abap_true.
*
*              CREATE OBJECT lo_excel_reader TYPE zcl_excel_reader_xlsm.
*
*              SELECT SINGLE *
*                FROM ztexcel_template
*                INTO CORRESPONDING FIELDS OF ls_template
*                WHERE template_name = 'MONSTER_EXAMPLE'.
*
*              lo_excel = lo_excel_reader->load( ls_template-raw_data ).
*
*              lo_worksheet = lo_excel->get_active_worksheet( ).
*
*            ENDIF."Do we want to use a macro?

*--------------------------------------------------------------------*
* Listing 11.05 - Transforming a Report Object into an Excel Object
* AND
* Listing 11.29 - Filling the Macro-Enabled Worksheet with Data
*--------------------------------------------------------------------*
* Convert SALV object into EXCEL object
*--------------------------------------------------------------------*
            lo_converter->convert(
              EXPORTING
                io_alv        = mo_view->mo_alv_grid
                it_table      = mo_model->mt_output_data[]
                i_table       = abap_true
                i_style_table = zcl_excel_table=>builtinstyle_medium2
                io_worksheet  = lo_worksheet
              CHANGING
                co_excel      = lo_excel ).

*--------------------------------------------------------------------*
* Listing 11.06 - Data Declarations for Accounting Formatting
*--------------------------------------------------------------------*
* Set numeric cells to accounting convention format
*--------------------------------------------------------------------*
            DATA: ld_no_of_columns  TYPE zexcel_cell_column,
                  ld_no_of_rows     TYPE zexcel_cell_row,
                  ld_column_alpha   TYPE zexcel_cell_column_alpha,
                  ld_column_integer TYPE zexcel_cell_column,
                  ld_row_integer    TYPE zexcel_cell_row,
                  ls_stylemapping   TYPE zexcel_s_stylemapping,
                  ld_cell_style     TYPE zexcel_cell_style.

*--------------------------------------------------------------------*
* Listing 11.07 - Looping through all the cells in a spreadsheet
*--------------------------------------------------------------------*
            lo_worksheet = lo_excel->get_active_worksheet( ).

            ld_no_of_rows    = lo_worksheet->get_highest_row( ).
            ld_no_of_columns = lo_worksheet->get_highest_column( ).

            DO ld_no_of_columns TIMES.
              ld_column_integer = ld_column_integer + 1.
              ld_column_alpha   = zcl_excel_common=>convert_column2alpha( ld_column_integer ).
              ld_row_integer    = 0.
              DO ld_no_of_rows TIMES.
                ADD 1 TO ld_row_integer.
*--------------------------------------------------------------------*
* Listing 11.08 - Finding the Style of a Spreadsheet Cell
*--------------------------------------------------------------------*
                lo_worksheet->get_cell( EXPORTING ip_column = ld_column_alpha
                                                  ip_row    = ld_row_integer
                                        IMPORTING ep_guid   = ld_cell_style ).

                TRY.
                    ls_stylemapping = lo_worksheet->excel->get_style_to_guid( ld_cell_style ).
                  CATCH zcx_excel.
                    CLEAR ls_stylemapping.
                ENDTRY.
*--------------------------------------------------------------------*
* Listing 11.09 - Setting a Cell to be Formatted Appropriately
*--------------------------------------------------------------------*
                IF ls_stylemapping-complete_style-number_format-format_code = '#,##0.00'.
                  "This is a currency amount, use the accounting conventions
                  "which are to have negative numbers as red in brackets, and show zero values
                  "as dashes, so as to focus the eye on the real numbers
                  ls_stylemapping-complete_style-number_format-format_code = '$#,##0.00;[Red]($#,##0.00);-'.
                  lo_worksheet->change_cell_style(
                  ip_column                    = ld_column_alpha
                  ip_row                       = ld_row_integer
                  ip_number_format_format_code = ls_stylemapping-complete_style-number_format-format_code ).
                ENDIF.
              ENDDO."Rows
            ENDDO."Columns

*--------------------------------------------------------------------*
* Listing 11.10 - Programtically Changing the Print Orientation to
*                 Landscape (amongst other things)
*--------------------------------------------------------------------*
* Print Settings
*--------------------------------------------------------------------*
            "Page printing settings
            "Margins are to be set to the values for "narrow". I just copy
            "the values in the "narrow" option on the print preview
            lo_worksheet->sheet_setup->set_page_margins( ip_top    = '1.91'
                                                         ip_bottom = '1.91'
                                                         ip_left   = '0.64'
                                                         ip_right  = '0.64'
                                                         ip_header = '0.76'
                                                         ip_footer = '0.76'
                                                         ip_unit   = 'cm' ).
            lo_worksheet->sheet_setup->black_and_white   = 'X'.

            "Requirement is to fit all columns on one sheet
            lo_worksheet->sheet_setup->fit_to_page         = 'X'.  " you should turn this on to activate fit_to_height and fit_to_width
            lo_worksheet->sheet_setup->fit_to_width        = 1.  " used only if ip_fit_to_page = 'X'
            lo_worksheet->sheet_setup->orientation         = zcl_excel_sheet_setup=>c_orientation_landscape.
            lo_worksheet->sheet_setup->page_order          = zcl_excel_sheet_setup=>c_ord_downthenover.
            lo_worksheet->sheet_setup->paper_size          = zcl_excel_sheet_setup=>c_papersize_a4.
            lo_worksheet->sheet_setup->scale               = 80.  " used only if ip_fit_to_page = SPACE
            lo_worksheet->sheet_setup->horizontal_centered = abap_true.

*--------------------------------------------------------------------*
* Listing 11.11 - Coding Header and Footer Print Settings
*--------------------------------------------------------------------*
            DATA: ls_header TYPE zexcel_s_worksheet_head_foot,
                  ls_footer TYPE zexcel_s_worksheet_head_foot,
                  ld_string TYPE string.

            "Put Tab Name in Header Centre
            ls_header-center_value     = lo_worksheet->get_title( ).
            ls_header-center_font      = ls_header-right_font.
            ls_header-center_font-size = 8.
            ls_header-center_font-name = zcl_excel_style_font=>c_name_arial.

            "Put last save date on footer left
            CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ld_string SEPARATED BY '/'.
            ls_footer-left_value = ld_string.
            ls_footer-left_font  = ls_header-center_font.

            "Put Spreadsheet path and name in Footer Centre
            ls_footer-center_value = '&Z&F'.               "Path / Filename
            ls_footer-center_font  = ls_header-center_font.

            "Put page X of Y on Footer Right
            ls_footer-right_value = 'page &P of &N' ##no_text.  "page x of y
            ls_footer-right_font  = ls_header-center_font.

            lo_worksheet->sheet_setup->set_header_footer(
            ip_odd_header = ls_header
            ip_odd_footer = ls_footer ).

*--------------------------------------------------------------------*
* Listing 11.12 - Making the Header Row Repeat on Every Printed Sheet
*--------------------------------------------------------------------*
            lo_worksheet->zif_excel_sheet_printsettings~set_print_repeat_rows(
            iv_rows_from = 1
            iv_rows_to   = 1 ).

*--------------------------------------------------------------------*
* Conditional Formatting - Coloured Cells
*--------------------------------------------------------------------*
* Listing 11.13 - Strength Column
*--------------------------------------------------------------------*
            CONSTANTS: lc_strength_column TYPE zexcel_cell_column_alpha VALUE 'E'.

*--------------------------------------------------------------------*
* Listing 11.14 - Creating Colors to Use in a Spreadsheet
*--------------------------------------------------------------------*
            DATA: lo_style_conditional TYPE REF TO zcl_excel_style_conditional,
                  lo_style_1           TYPE REF TO zcl_excel_style,
                  lo_style_2           TYPE REF TO zcl_excel_style,
                  lo_style_3           TYPE REF TO zcl_excel_style,
                  ld_green_guid        TYPE zexcel_cell_style,
                  ld_red_guid          TYPE zexcel_cell_style,
                  ld_yellow_guid       TYPE zexcel_cell_style,
                  ls_cellis            TYPE zexcel_conditional_cellis.

            lo_style_1                    = lo_excel->add_new_style( ).
            lo_style_1->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
            lo_style_1->fill->bgcolor-rgb = zcl_excel_style_color=>c_green.
            ld_green_guid                 = lo_style_1->get_guid( ).

            lo_style_2                    = lo_excel->add_new_style( ).
            lo_style_2->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
            lo_style_2->fill->bgcolor-rgb = 'FFFF99FF'.    "Soft Red (Pink Really)
            ld_red_guid                   = lo_style_2->get_guid( ).

            lo_style_3                    = lo_excel->add_new_style( ).
            lo_style_3->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
            lo_style_3->fill->bgcolor-rgb = 'FFFF9900'.    "Orange - see ZDEMO_EXCEL21
            ld_yellow_guid                = lo_style_3->get_guid( ).

*--------------------------------------------------------------------*
* Listing 11.15 - Creating Conditional Formatting
*--------------------------------------------------------------------*
*            DATA: ld_first_data_row TYPE sy-tabix,
*                  ld_last_data_row  TYPE sy-tabix.
*
*            ld_first_data_row = 2.                         "i.e. first row after header
*            ld_last_data_row  = lines( mo_model->mt_output_data[] ) + 1.
*
*            "High Strength Monster - Green for 'Good'
*            lo_style_conditional              = lo_worksheet->add_new_conditional_style( ).
*            lo_style_conditional->rule        = zcl_excel_style_conditional=>c_rule_cellis.
*            ls_cellis-formula                 = '"REALLY STRONG"'.
*            ls_cellis-operator                = zcl_excel_style_conditional=>c_operator_equal.
*            ls_cellis-cell_style              = ld_green_guid.
*            lo_style_conditional->mode_cellis = ls_cellis.
*            lo_style_conditional->priority    = 1.
*            lo_style_conditional->set_range( ip_start_column = lc_strength_column
*                                             ip_start_row    = ld_first_data_row
*                                             ip_stop_column  = lc_strength_column
*                                             ip_stop_row     = ld_last_data_row ).
*
*            "Low Strength Monster - Red for 'Bad'
*            lo_style_conditional              = lo_worksheet->add_new_conditional_style( ).
*            lo_style_conditional->rule        = zcl_excel_style_conditional=>c_rule_cellis.
*            ls_cellis-formula                 = '"LOW"'.
*            ls_cellis-operator                = zcl_excel_style_conditional=>c_operator_equal.
*            ls_cellis-cell_style              = ld_red_guid.
*            lo_style_conditional->mode_cellis = ls_cellis.
*            lo_style_conditional->priority    = 2.
*            lo_style_conditional->set_range( ip_start_column = lc_strength_column
*                                             ip_start_row    = ld_first_data_row
*                                             ip_stop_column  = lc_strength_column
*                                             ip_stop_row     = ld_last_data_row ).
*
*            "Medium Strength Monster - Yellow for 'nothing special'
*            lo_style_conditional              = lo_worksheet->add_new_conditional_style( ).
*            lo_style_conditional->rule        = zcl_excel_style_conditional=>c_rule_cellis.
*            ls_cellis-formula                 = '"MEDIUM"'.
*            ls_cellis-operator                = zcl_excel_style_conditional=>c_operator_equal.
*            ls_cellis-cell_style              = ld_yellow_guid.
*            lo_style_conditional->mode_cellis = ls_cellis.
*            lo_style_conditional->priority    = 3.
*            lo_style_conditional->set_range( ip_start_column = lc_strength_column
*                                             ip_start_row    = ld_first_data_row
*                                             ip_stop_column  = lc_strength_column
*                                             ip_stop_row     = ld_last_data_row ).
*
*--------------------------------------------------------------------*
* Listing 11.20 - Conditional Formatting - Traffic Lights
*--------------------------------------------------------------------*
*            CONSTANTS: lc_age_column TYPE zexcel_cell_column_alpha VALUE 'F'.
*
*            DATA: ls_iconset3         TYPE zexcel_conditional_iconset,
*                  ld_number_as_string TYPE string.
*
*            "Green if below 7 days
*            ls_iconset3-cfvo1_type  = zcl_excel_style_conditional=>c_cfvo_type_number.
*            ls_iconset3-cfvo1_value = '-9999'.
*            "Red if above 14 days
*            ls_iconset3-cfvo2_type = zcl_excel_style_conditional=>c_cfvo_type_number.
*            ld_number_as_string    = 14.
*            ld_number_as_string    = '-' && ld_number_as_string.
*            CONDENSE ld_number_as_string.
*            ls_iconset3-cfvo2_value = ld_number_as_string.
*            "Yellow otherwise
*            ls_iconset3-cfvo3_type = zcl_excel_style_conditional=>c_cfvo_type_number.
*            ld_number_as_string    = 7.
*            ld_number_as_string    = '-' && ld_number_as_string.
*            CONDENSE ld_number_as_string.
*            ls_iconset3-cfvo3_value = ld_number_as_string.
*            "Show the value as well as the ICON
*            ls_iconset3-showvalue   = zcl_excel_style_conditional=>c_showvalue_true.
*
*            lo_style_conditional               = lo_worksheet->add_new_conditional_style( ).
*            "We are going to show ICONS
*            lo_style_conditional->rule     = zcl_excel_style_conditional=>c_rule_iconset.
*            lo_style_conditional->priority = 1.
*            "The ICONS are going to look like Traffic Lights
*            ls_iconset3-iconset                = zcl_excel_style_conditional=>c_iconset_3trafficlights.
*            lo_style_conditional->mode_iconset = ls_iconset3.
*            lo_style_conditional->set_range( ip_start_column = lc_age_column
*                                             ip_start_row    = ld_first_data_row
*                                             ip_stop_column  = lc_age_column
*                                             ip_stop_row     = ld_last_data_row ).

*--------------------------------------------------------------------*
* Listing 11.21 - Changing the Fomratting to Make Negatives Look Positive
*--------------------------------------------------------------------*
*            "We made all the ages negative earlier
*            "Now we have to show them as positive on the spreadsheet
*            DATA: ld_sheet_row TYPE zexcel_cell_row.
*            ld_sheet_row = 1.
*            WHILE ld_sheet_row LE ld_last_data_row.
*              ADD 1 TO ld_sheet_row.
*              ls_stylemapping-complete_style-number_format-format_code = '#,##0;#,##0'.
*              lo_worksheet->change_cell_style( ip_column                    = lc_age_column
*                                               ip_row                       = ld_sheet_row
*                                               ip_number_format_format_code = ls_stylemapping-complete_style-number_format-format_code ).
*            ENDWHILE.
*
*            "If we are using our example Macro then the Macro will
*            "set the name of the worksheet
*            IF go_selections->p_macro EQ abap_false.
*
*              lo_worksheet->set_title( 'Monster Header Data' ).
*
*            ENDIF.

*--------------------------------------------------------------------*
* Listing 11.23 - Setting Up the Pie Chart Data Worksheet
* Create a Pie Chart
*--------------------------------------------------------------------*
            "AT time of writing Pie Charts do not work with macros
            IF go_selections->p_macro EQ abap_false.

              lo_worksheet = lo_excel->add_new_worksheet( ).

              lo_worksheet->set_title( 'Pie Chart Values' ).

              "In real life you would loop over an internal table to
              "populate the data values
              "Pie Chart - Monster Types
              lo_worksheet->set_cell( ip_column = 'A' ip_row = 1 ip_value = 'Blue Monsters' ).
              lo_worksheet->set_cell( ip_column = 'A' ip_row = 2 ip_value = 'Red Monsters' ).
              lo_worksheet->set_cell( ip_column = 'A' ip_row = 3 ip_value = 'Green Monsters' ).
              lo_worksheet->set_cell( ip_column = 'A' ip_row = 4 ip_value = 'Sky Blue Pink Monsters' ).

              "Pie Chart - Number of each Monster Type
              lo_worksheet->set_cell( ip_column = 'B' ip_row = 1 ip_value = 5 ).
              lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 10 ).
              lo_worksheet->set_cell( ip_column = 'B' ip_row = 3 ip_value = 15 ).
              lo_worksheet->set_cell( ip_column = 'B' ip_row = 4 ip_value = 20 ).

*--------------------------------------------------------------------*
* Listing 11.24 - Setting Up the Pie Chart Worksheet
*--------------------------------------------------------------------*
              "Add the worksheet with the actual pie chart on it
              lo_worksheet = lo_excel->add_new_worksheet( ).

              lo_worksheet->set_title( 'Monster Pie Chart' ).

              DATA: lo_drawing TYPE REF TO zcl_excel_drawing,
                    lo_pie     TYPE REF TO zcl_excel_graph_pie.

              CREATE OBJECT lo_pie.

              "Tell the Pie Chart where it gets it's data from
              CALL METHOD lo_pie->create_serie
                EXPORTING
                  ip_order        = 0
                  ip_sheet        = 'Pie Chart Values'       "the sheet the data comes from
                  "Range where the labels live
                  ip_lbl_from_col = 'A'
                  ip_lbl_from_row = '1'
                  ip_lbl_to_col   = 'A'
                  ip_lbl_to_row   = '4'
                  "Range where the data values live
                  ip_ref_from_col = 'B'
                  ip_ref_from_row = '1'
                  ip_ref_to_col   = 'B'
                  ip_ref_to_row   = '4'
                  ip_sername      = 'Monsters by Color'.

              lo_pie->set_style( zcl_excel_graph=>c_style_15 ).

              "Let us show the category names next to the pie chart
              lo_pie->set_show_cat_name( zcl_excel_graph_pie=>c_show_true ).

              lo_drawing = lo_worksheet->excel->add_new_drawing(
              ip_type    = zcl_excel_drawing=>type_chart
              ip_title   = 'Monster Pie Chart' ).

              lo_drawing->graph      = lo_pie.
              lo_drawing->graph_type = zcl_excel_drawing=>c_graph_pie.

              DATA: ls_upper TYPE zexcel_drawing_location,
                    ls_lower TYPE zexcel_drawing_location.

              ls_lower-row = 20.
              ls_lower-col = 10.
              lo_drawing->set_position2(
                EXPORTING
                  ip_from = ls_upper
                  ip_to   = ls_lower ).

              lo_drawing->set_media(
                EXPORTING
                  ip_media_type = zcl_excel_drawing=>c_media_type_xml ).

              lo_worksheet->add_drawing( lo_drawing ).

              "The value sheet for the pie chart is hidden by default
              lo_excel->set_active_sheet_index( 2 ).
              lo_worksheet                                    = lo_excel->get_active_worksheet( ).
              lo_worksheet->zif_excel_sheet_properties~hidden = zif_excel_sheet_properties=>c_hidden.

            ENDIF."Are we using Macros?

            "We want the user to start on the first worksheet
            lo_excel->set_active_sheet_index( 1 ).

*--------------------------------------------------------------------*
* Listing 11.32 - Inserting Hyperlinks in a Spreadsheet
*--------------------------------------------------------------------*
* Hyperlinks
*--------------------------------------------------------------------*
*            CONSTANTS: lc_monster_column TYPE zexcel_cell_column_alpha VALUE 'A'.
*
*            DATA: lo_hyperlink TYPE REF TO zcl_excel_hyperlink,
*                  ld_url       TYPE string,
*                  ld_parameter TYPE string,
*                  ls_monsters  LIKE LINE OF mo_model->mt_output_data.
*
*            lo_worksheet = lo_excel->get_active_worksheet( ).
*
** Now we loop through the spreadsheet, adding hyperlinks so the user can drill
** down into the original document in SAP
*            ld_sheet_row = ld_first_data_row.
*
*            LOOP AT mo_model->mt_output_data INTO ls_monsters.
** Drill down into the monster master record
*              IF ls_monsters-monster_number IS NOT INITIAL.
*
*                ld_parameter = 'S_NUMBR-LOW=' && ls_monsters-monster_number && ';'.
*                ld_url       = build_hyperlink_url( id_transaction = 'ZMONSTER'
*                                                    id_parameters  = ld_parameter
*                                                    id_ok_code     = '=ONLI' ).
*                lo_hyperlink = zcl_excel_hyperlink=>create_external_link( iv_url = ld_url ).
*                lo_worksheet->set_cell( ip_column    = lc_monster_column
*                                        ip_row       = ld_sheet_row
*                                        ip_value     = ls_monsters-monster_number
*                                        ip_hyperlink = lo_hyperlink ).
*                lo_worksheet->change_cell_style( ip_column         = lc_monster_column
*                                                 ip_row            = ld_sheet_row
*                                                 ip_font_color_rgb = zcl_excel_style_color=>c_blue
*                                                 ip_font_underline = abap_true ).
*              ENDIF.
*              ADD 1 TO ld_sheet_row.
*            ENDLOOP."Monster Table
*--------------------------------------------------------------------*
* Listing 11.04 - Downloading an EXCEL Spreadsheet
*--------------------------------------------------------------------*
* Choose where to download EXCEL spreadsheet to front end
*--------------------------------------------------------------------*
            DATA: lt_rawdata   TYPE solix_tab,
                  ld_bytecount TYPE i,
                  ld_filename  TYPE string,
                  ld_path      TYPE string,
                  ld_fullpath  TYPE string.

* From now on this is all bog standard SAP
            cl_gui_frontend_services=>file_save_dialog(
              EXPORTING
                 window_title         = 'Choose where to save file'
                 default_extension    = 'XLSX'                                                   " Default Extension
              CHANGING
                 filename             = ld_filename                                                " File Name to Save
                 path                 = ld_path                                                    " Path to File
                 fullpath             = ld_fullpath                                                " Path + File Name
              EXCEPTIONS
                 cntl_error           = 1
                 error_no_gui         = 2
                 not_supported_by_gui = 3
                 OTHERS               = 4 ).

            IF sy-subrc <> 0.
              RETURN.
            ENDIF.

*--------------------------------------------------------------------*
* Listing 11.30 - Making sure the Spreadsheet File is Saved Correctly
*--------------------------------------------------------------------*
* Convert to XML
*--------------------------------------------------------------------*
            DATA: ld_xml_file      TYPE xstring,
                  lo_excel_writer  TYPE REF TO zif_excel_writer,
                  lf_macro_enabled TYPE abap_bool.

            IF lo_excel->zif_excel_book_vba_project~codename_pr IS NOT INITIAL.
              lf_macro_enabled = abap_true.
            ELSE.
              lf_macro_enabled = abap_false.
            ENDIF.

            IF ld_fullpath CS 'XLSM' AND lf_macro_enabled = abap_false.
              REPLACE 'XLSM' WITH 'XLSX' INTO ld_fullpath.
            ELSEIF ld_fullpath CS 'XLSX' AND lf_macro_enabled = abap_true.
              REPLACE 'XLSX' WITH 'XLSM' INTO ld_fullpath.
            ELSEIF ld_fullpath CS 'XLSX' OR
                   ld_fullpath CS 'XLSM'.
              "Do nothing - everything is fine
            ELSEIF ld_fullpath CS 'XLS' AND lf_macro_enabled = abap_false.
              REPLACE 'XLS' WITH 'XLSX' INTO ld_fullpath.
            ELSEIF ld_fullpath CS 'XLS' AND lf_macro_enabled = abap_true.
              REPLACE 'XLS' WITH 'XLSM' INTO ld_fullpath.
            ENDIF.

            IF ld_fullpath CS 'XLSM'.
              CREATE OBJECT lo_excel_writer TYPE zcl_excel_writer_xlsm.
            ELSE.
              CREATE OBJECT lo_excel_writer TYPE zcl_excel_writer_2007.
            ENDIF.

            ld_xml_file = lo_excel_writer->write_file( lo_excel ).

*--------------------------------------------------------------------*
* Download to Front End
*--------------------------------------------------------------------*
            lt_rawdata   = cl_bcs_convert=>xstring_to_solix( iv_xstring = ld_xml_file ).
            ld_bytecount = xstrlen( ld_xml_file ).

            cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = ld_bytecount
                                                              filename     = ld_fullpath
                                                              filetype     = 'BIN'
                                                    CHANGING data_tab      = lt_rawdata ).

          CATCH zcx_excel INTO lo_exception.
            "Raise Fatal Exception
        ENDTRY.

*        "We reversed the polarity of the ages earlier for spreadsheet purposes
*        "Now we have to change them back again
*        LOOP AT mo_model->mt_output_data ASSIGNING <ls_monsters>.
*          <ls_monsters>-age = <ls_monsters>-age * -1.
*        ENDLOOP.
    ENDCASE.

    mo_model->user_command(
      EXPORTING
        id_user_command = ed_user_command                                              " Function code that PAI triggered
        id_column       = ed_column                                                    " Selected Column
        id_row          = ed_row ).                                                    " Selected Row

    mo_view->refresh_display( ).

  ENDMETHOD."User Command / Controller

  METHOD build_hyperlink_url.
*--------------------------------------------------------------------*
* Listing 11.33 - Building the Hyperlink URL
*--------------------------------------------------------------------*
* This builds up a URL to log onto an SICF service. The SICF service will
* redirect to a java system which will authenticate the user via the
* Kerebros logon ticket created when the user logged onto windows in
* th morning. After authentication the java stack will redirect back
* to the SICF service which will then log the user on without asking
* for their user name or password, just like normal SAP SSO
* If the java service is not there, the user is asked for their name
* and password, not the end of the world
*--------------------------------------------------------------------*
* Local Variables
    DATA: ld_logical_system  TYPE t000-logsys,
          ld_rfc_destination TYPE rfcdest,
          ld_server_name     TYPE rfchost_ext,
          ld_url             TYPE string.

* T000 - Fully Buffered
    SELECT SINGLE logsys
      FROM t000
      INTO ld_logical_system
      WHERE mandt = sy-mandt.

    CHECK sy-subrc = 0.

    ld_rfc_destination = ld_logical_system.

* Make sure RFC destination (SM58) for current system has
* "save as hostname" option taken
    CALL FUNCTION 'DEST_RFC_ABAP_READ'
      EXPORTING
        name         = ld_rfc_destination
      IMPORTING
        server_name  = ld_server_name
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* First half of the URL - SICF path and first parameter i.e.
* transaction code
    rs_url =
    'http://' &&
    ld_server_name &&
    ':8000/sap/bc/gui/sap/its/webgui?' && "8000 is the standard port for HTTP
    '~transaction=*' &&
    id_transaction.

* Second half of the URL i.e. what data to be pass to the
* transaction code
    ld_url =
    id_parameters  &&" e.g. USR02-BNAME=hardyp;
    'DYNP_OKCODE=' &&
    id_ok_code     &&
    '&sap-client=' &&
    sy-mandt       .

    CONCATENATE rs_url ld_url INTO rs_url SEPARATED BY space.

  ENDMETHOD."Build Hyperlink URL

  METHOD on_data_changed.
* Local Variables
    DATA: ls_stable TYPE lvc_s_stbl.

    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    mo_view->mo_alv_grid->refresh( s_stable = ls_stable ).

  ENDMETHOD.                                               "on_data_changed

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* Listng 10.38 - MAKE_COLUMN_EDITABLE Method
*--------------------------------------------------------------------*
* IMPORTING id_column_name TYPE dd03l-fieldname
* CHANGING  ct_fcat        TYPE lvc_t_fcat.
*--------------------------------------------------------------------*
* Local Variables
    DATA :editable_cell_structure TYPE lvc_s_styl,
          row_to_read             TYPE sy-tabix.

    LOOP AT mo_model->mt_output_data ASSIGNING FIELD-SYMBOL(<alv_output_row>).

      READ TABLE <alv_output_row>-celltab ASSIGNING FIELD-SYMBOL(<editable_cell_structure>)
      WITH KEY fieldname = id_column_name.

      IF sy-subrc <> 0.
        row_to_read                       = sy-tabix.
        editable_cell_structure-fieldname = id_column_name.
        INSERT editable_cell_structure INTO <alv_output_row>-celltab INDEX row_to_read.
        READ TABLE <alv_output_row>-celltab ASSIGNING <editable_cell_structure> WITH KEY fieldname = id_column_name.
      ENDIF.

      IF <editable_cell_structure>-style EQ cl_gui_alv_grid=>mc_style_enabled.
        <editable_cell_structure>-style = cl_gui_alv_grid=>mc_style_disabled.
      ELSE.
        <editable_cell_structure>-style = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.

    ENDLOOP.

    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<field_catalogue>) WHERE fieldname = id_column_name.
      <field_catalogue>-edit = abap_true.
    ENDLOOP.

  ENDMETHOD."make  column editable

ENDCLASS.                    "lcl_controller IMPLEMENTATION
