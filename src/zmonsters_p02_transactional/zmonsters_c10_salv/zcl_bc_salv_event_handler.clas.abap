class ZCL_BC_SALV_EVENT_HANDLER definition
  public
  create public .

public section.

  types:
    M_TT_SALV_OBJECTS TYPE STANDARD TABLE OF REF TO cl_salv_table .

  data MT_SALV type M_TT_SALV_OBJECTS .
  data MD_EDIT_CONTROL_FIELD type LVC_FNAME .
  data MT_EDITABLE_FIELDS type LVC_T_FNAM .

  methods ON_AFTER_REFRESH
    for event AFTER_REFRESH of CL_GUI_ALV_GRID
    importing
      !SENDER .
  methods ON_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE
      !SENDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_SALV_EVENT_HANDLER IMPLEMENTATION.


METHOD on_after_refresh."of ZCL_BC_SALV_EVENT_HANDLER
*------------------------------------------------------------*
* Listing 10.34 : Handling the initial “ON AFTER REFRESH” event
*------------------------------------------------------------*
* What we are doing here is enabling the SALV GRID to open in
* editable mode. When each such SALV was created it registered
* to be made editable at such time it's grid got created and
* was ready to appear on the screen
*------------------------------------------------------------*
* Local Variables
  DATA: layout_information TYPE lvc_s_layo,
        salv_model         TYPE REF TO cl_salv_model.

  TRY .
      LOOP AT mt_salv INTO DATA(registered_salv_object).
        "Narrow casting
        "CL_SALV_MODEL is a superclass of CL_SALV_TABLE
        "Target = SALV_MODEL  = CL_SALV_MODEL
        "Source = MO_ALV_GRID = CL_SALV_TABLE
        salv_model ?= registered_salv_object.

        "Object to access underlying CL_GUI_ALV_GRID
        DATA(underlying_grid_getter) = NEW zcl_salv_model( salv_model ).

        DATA(underlying_grid_object) = underlying_grid_getter->get_alv_grid( ).
        "The SENDER is the grid that just got created and has raised an
        "event to that effect
        CHECK underlying_grid_object EQ sender.

        "Deregister the event handler
        "i.e. we do not want to keep calling this every time
        "the user refreshes the display.
        "Once the report is running the user can control whether
        "the grid is editable by using the icons at the top of the screen
        SET HANDLER me->on_after_refresh
          FOR ALL INSTANCES
          ACTIVATION space.

        "Set editable
        IF md_edit_control_field IS NOT INITIAL.
          "Make certain fields editable based on FIELDCAT
          layout_information-stylefname = md_edit_control_field.
          underlying_grid_object->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(field_catalog_table) ).
          LOOP AT mt_editable_fields INTO DATA(editable_field).
            READ TABLE field_catalog_table ASSIGNING FIELD-SYMBOL(<field_catalog_info>) WITH KEY fieldname = editable_field.
            IF sy-subrc = 0.
              <field_catalog_info>-edit = abap_true.
            ENDIF.
          ENDLOOP."Editable Fields
          underlying_grid_object->set_frontend_fieldcatalog( field_catalog_table ).
        ELSE.
          "Make everything editable
          layout_information-edit = 'X'.
        ENDIF.
        underlying_grid_object->set_frontend_layout( layout_information ).
        underlying_grid_object->set_ready_for_input( 1 ).
      ENDLOOP."SALV Objects
    CATCH cx_salv_error.
* Raise a whacking great fatal exception. If we are here, it's a bug.
  ENDTRY.

ENDMETHOD."of ZCL_BC_SALV_EVENT_HANDLER


METHOD on_toolbar.
*--------------------------------------------------------------------*
* We are making the SALV grid editable, and SALV does not like that
* So we need to manually the add the icons at the top that you would
* get when making a CL_GUI_ALV_GRID editable
*--------------------------------------------------------------------*
* Local Variables
  DATA: underlying_grid_object TYPE REF TO cl_gui_alv_grid,
        toolbar_button_table   TYPE ttb_button,
        toolbar_button_info    LIKE LINE OF toolbar_button_table,
        registered_salv_object TYPE REF TO cl_salv_table,
        salv_model             TYPE REF TO cl_salv_model,
        underlying_grid_getter TYPE REF TO zcl_salv_model.

  TRY.
      LOOP AT mt_salv INTO registered_salv_object.
        "Narrow casting
        "CL_SALV_MODEL is a superclass of CL_SALV_TABLE
        "Target = salv_model = CL_SALV_MODEL
        "Source = MO_ALV_GRID   = CL_SALV_TABLE
        salv_model ?= registered_salv_object.

        "Object to access underlying CL_GUI_ALV_GRID
        CREATE OBJECT underlying_grid_getter
          EXPORTING
            io_model = salv_model.

        underlying_grid_object = underlying_grid_getter->get_alv_grid( ).
        IF underlying_grid_object EQ sender.
          EXIT.
        ELSE.
          CLEAR underlying_grid_object.
        ENDIF.
      ENDLOOP.
    CATCH cx_salv_msg.
      EXIT.
  ENDTRY.

  CHECK underlying_grid_object IS BOUND.
  CHECK underlying_grid_object->is_ready_for_input( ) = 1.

*... Toolbar Button CHECK
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_check.
  toolbar_button_info-quickinfo = text-053.                         "Eingaben prfen
  toolbar_button_info-icon      = icon_check.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.


* Toolbar Separator ...
* Up till now I was told it was impossible to have a separator in
* a CL_SALV_TABLE....
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = '&&SEP01'.
  toolbar_button_info-butn_type = 3.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button CUT
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_cut.
  toolbar_button_info-quickinfo = text-046.                         "Ausschneiden
  toolbar_button_info-icon      = icon_system_cut.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button COPY
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_copy.
  toolbar_button_info-quickinfo = text-045.                         " Kopieren
  toolbar_button_info-icon      = icon_system_copy.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button PASTE OVER ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_paste.
  toolbar_button_info-quickinfo = text-047.
  toolbar_button_info-icon      = icon_system_paste.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button PASTE NEW ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  toolbar_button_info-quickinfo = text-063.
  toolbar_button_info-icon      = icon_system_paste.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button UNDO
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_undo.
  toolbar_button_info-quickinfo = text-052.                         "Rckgngig
  toolbar_button_info-icon      = icon_system_undo.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Separator
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = '&&SEP02'.
  toolbar_button_info-butn_type = 3.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button APPEND ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
  toolbar_button_info-quickinfo = text-054.                         "Zeile anhngen
  toolbar_button_info-icon      = icon_create.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button INSERT ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  toolbar_button_info-quickinfo = text-048.                         "Zeile einfgen
  toolbar_button_info-icon      = icon_insert_row.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button DELETE ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  toolbar_button_info-quickinfo = text-049.                         "Zeile lschen
  toolbar_button_info-icon      = icon_delete_row.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Button COPY ROW
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  toolbar_button_info-quickinfo = text-051.                         "Duplizieren
  toolbar_button_info-icon      = icon_copy_object.
  toolbar_button_info-disabled  = space.
  APPEND toolbar_button_info TO toolbar_button_table.

*... Toolbar Separator
  CLEAR toolbar_button_info.
  toolbar_button_info-function  = '&&SEP03'.
  toolbar_button_info-butn_type = 3.
  APPEND toolbar_button_info TO toolbar_button_table.

  APPEND LINES OF toolbar_button_table TO e_object->mt_toolbar.

ENDMETHOD.
ENDCLASS.
