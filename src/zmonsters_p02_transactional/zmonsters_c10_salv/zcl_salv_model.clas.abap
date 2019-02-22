class ZCL_SALV_MODEL definition
  public
  inheriting from CL_SALV_MODEL_LIST
  create public .

public section.

  methods GET_ALV_GRID
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods CONSTRUCTOR
    importing
      !IO_MODEL type ref to CL_SALV_MODEL .
  class-methods SET_EDITABLE
    importing
      !IO_SALV type ref to CL_SALV_TABLE
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME optional
      !IT_EDITABLE_FIELDS type LVC_T_FNAM optional .
protected section.
private section.

  data MO_MODEL type ref to CL_SALV_MODEL .
  class-data MO_EVENT_HANDLER type ref to OBJECT .
ENDCLASS.



CLASS ZCL_SALV_MODEL IMPLEMENTATION.


METHOD constructor.
*--------------------------------------------------------------------*
* listing 10.29 : Adding the Standard SALV Model to a Custom Class
*--------------------------------------------------------------------*
  super->constructor( ).
  mo_model = io_model.

ENDMETHOD.


METHOD get_alv_grid."of ZCL_SALV_MODEL
*--------------------------------------------------------------------*
* Listing 10.33 : Returning an ALV Grid Object from ZCL_SALV_MODEL
*--------------------------------------------------------------------*
* Local Variables
  DATA: full_screen_adapter TYPE REF TO cl_salv_fullscreen_adapter,
        container_adapter   TYPE REF TO cl_salv_grid_adapter,
        salv_adapter        TYPE REF TO cl_salv_adapter.

  DATA(salv_controller) = mo_model->r_controller.

  "Target = SALV_ADAPTER type CL_SALV_ADAPTER
  "Source = R_ADAPTER    type CL_SALV_ADAPTER
  "R_ADAPTER may actually be a subclass however
  salv_adapter ?= salv_controller->r_adapter.

  TRY.
      "Presume full screen mode (No Container)
      "Fullscreen Adapter (Down Casting)
      "Target = FULL_SCREEN_ADAPTER = CL_SALV_FULLSCREEN_ADAPTER
      "Source = MO_ADAPTER          = CL_SALV_ADAPTER
      "CL_SALV_FULLSCREEN is a subclass of CL_SALV_ADAPTER
      full_screen_adapter ?= salv_adapter.
      "Get the Grid
      ro_alv_grid = full_screen_adapter->get_grid( ).
    CATCH cx_sy_move_cast_error.
      "We must be in container mode
      "CL_SALV_GRID_ADAPTER is a subclass of CL_SALV_ADAPTER
      container_adapter ?= salv_adapter.
      ro_alv_grid = container_adapter->get_grid( ).
  ENDTRY.

ENDMETHOD."GET_ALV_GRID of ZCL_SALV_MODEL


METHOD set_editable."of ZCL_SALV_MODEL
*--------------------------------------------------------------------*
* Listing 10.31 : Preparing for the Grid “Refresh” event
*--------------------------------------------------------------------*
* Local Variables
  DATA: event_one_handler TYPE REF TO zcl_bc_salv_event_handler.

  "Ensure one, and only one, static event handler exists
  IF zcl_salv_model=>mo_event_handler IS NOT BOUND.
    CREATE OBJECT zcl_salv_model=>mo_event_handler
      TYPE zcl_bc_salv_event_handler.
  ENDIF.

  "After this assignment, we will be dealing with the static
  "event handler
  event_one_handler ?= zcl_salv_model=>mo_event_handler.

  event_one_handler->md_edit_control_field = id_edit_control_field.
  event_one_handler->mt_editable_fields    = it_editable_fields.

  "We list every SALV object where we wish to make the fields
  "editable
  APPEND io_salv TO event_one_handler->mt_salv.

  "At such time as any SALV object is displayed, call the
  "after refresh event to make the grid editable
  SET HANDLER event_one_handler->on_after_refresh
    FOR ALL INSTANCES
    ACTIVATION 'X'.

  "Sometimes the icons needed for an editable grid do not
  "display, so we have to force the issue
  IF io_salv->get_display_object( ) = 3.
    SET HANDLER event_one_handler->on_toolbar
      FOR ALL INSTANCES
      ACTIVATION 'X'.
  ENDIF.

ENDMETHOD."SET_EDITABLE of ZCL_SALV_MODEL
ENDCLASS.
