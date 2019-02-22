class ZCL_BC_MODEL definition
  public
  create public .

public section.

  interfaces ZIF_BC_MODEL .

  aliases FILL_CHECKBOX_FIELDS
    for ZIF_BC_MODEL~FILL_CHECKBOX_FIELDS .
  aliases FILL_EDITABLE_FIELDS
    for ZIF_BC_MODEL~FILL_EDITABLE_FIELDS .
  aliases FILL_FIELD_TEXTS
    for ZIF_BC_MODEL~FILL_FIELD_TEXTS .
  aliases FILL_HIDDEN_FIELDS
    for ZIF_BC_MODEL~FILL_HIDDEN_FIELDS .
  aliases FILL_HOTSPOT_FIELDS
    for ZIF_BC_MODEL~FILL_HOTSPOT_FIELDS .
  aliases FILL_SUBTOTAL_FIELDS
    for ZIF_BC_MODEL~FILL_SUBTOTAL_FIELDS .
  aliases FILL_TECHNICAL_FIELDS
    for ZIF_BC_MODEL~FILL_TECHNICAL_FIELDS .
  aliases FILL_USER_COMMANDS
    for ZIF_BC_MODEL~FILL_USER_COMMANDS .
  aliases MD_EDIT_CONTROL_FIELD
    for ZIF_BC_MODEL~MD_EDIT_CONTROL_FIELD .
  aliases MD_SUCCESS_MESSAGE
    for ZIF_BC_MODEL~MD_SUCCESS_MESSAGE .
  aliases MS_LAYOUT
    for ZIF_BC_MODEL~MS_LAYOUT .
  aliases MT_CHECKBOXES
    for ZIF_BC_MODEL~MT_CHECKBOXES .
  aliases MT_EDITABLE_FIELDS
    for ZIF_BC_MODEL~MT_EDITABLE_FIELDS .
  aliases MT_FIELD_TEXTS
    for ZIF_BC_MODEL~MT_FIELD_TEXTS .
  aliases MT_HIDDEN
    for ZIF_BC_MODEL~MT_HIDDEN .
  aliases MT_HOTSPOTS
    for ZIF_BC_MODEL~MT_HOTSPOTS .
  aliases MT_SORT_CRITERIA
    for ZIF_BC_MODEL~MT_SORT_CRITERIA .
  aliases MT_SUBTOTAL_FIELDS
    for ZIF_BC_MODEL~MT_SUBTOTAL_FIELDS .
  aliases MT_TECHNICALS
    for ZIF_BC_MODEL~MT_TECHNICALS .
  aliases MT_USER_COMMANDS
    for ZIF_BC_MODEL~MT_USER_COMMANDS .
  aliases SET_EDIT_CONTROL_FIELD
    for ZIF_BC_MODEL~SET_EDIT_CONTROL_FIELD .
  aliases USER_COMMAND
    for ZIF_BC_MODEL~USER_COMMAND .
  aliases FILL_LAYOUT_DATA
    for ZIF_BC_MODEL~FILL_LAYOUT_DATA .
  aliases FILL_SORT_CRITERIA
    for ZIF_BC_MODEL~FILL_SORT_CRITERIA .
  aliases DATA_CHANGED
    for ZIF_BC_MODEL~DATA_CHANGED .
protected section.
private section.

  aliases MD_SELECTED_COLUMN
    for ZIF_BC_MODEL~MD_SELECTED_COLUMN .
  aliases MD_SELECTED_ROW
    for ZIF_BC_MODEL~MD_SELECTED_ROW .
ENDCLASS.



CLASS ZCL_BC_MODEL IMPLEMENTATION.


METHOD zif_bc_model~fill_checkbox_fields.
* Redefine when you want a checkbox (you can have more than one but
* I ave never had that requirement yet)
ENDMETHOD.


METHOD zif_bc_model~fill_editable_fields.
* There may not be any editable fields
* If there are, this method will be redefined to fill table
* MT_EDITABLE_FIELDS
ENDMETHOD.


METHOD zif_bc_model~fill_field_texts.
* Redefine when you want to rename at least one field
ENDMETHOD.


METHOD zif_bc_model~fill_hidden_fields.
* Redefine for reports with hidden fields
ENDMETHOD.


METHOD zif_bc_model~fill_hotspot_fields.
* Redefine for reports with hotspots (which is 99% of them)
ENDMETHOD.


  METHOD zif_bc_model~fill_layout_data.
* Gets Redefined if calling program wants to do something clever
* That usually involves dynamically changing the list header at run-time
    ms_layout = VALUE #( list_header       = id_list_header
                         variant           = id_variant
                         colwidth_optimize = if_colwidth_optimize
                         striped_pattern   = if_striped_pattern
                         no_cell_merging   = if_no_cell_merging ).

  ENDMETHOD.


  METHOD zif_bc_model~fill_sort_criteria.
* Redefine when report has sort criteria (most do)
  ENDMETHOD.


METHOD zif_bc_model~fill_subtotal_fields.
* Redefine for reports with subtotals (the majority)
  ENDMETHOD.


METHOD zif_bc_model~fill_technical_fields.
* Redefine for reports where you need to suppress fields used
* purely for technical purposes e.g. to derive another field
* you actually want to show the user
  ENDMETHOD.


METHOD zif_bc_model~fill_user_commands.
* This will always be redefined
* Unless there are no custom user commands....
ENDMETHOD.


METHOD zif_bc_model~set_edit_control_field.
  md_edit_control_field = id_edit_control_field.
ENDMETHOD.


METHOD zif_bc_model~user_command.
* This always gets redefined
* Unless there are no custom user commands.....
ENDMETHOD.
ENDCLASS.
