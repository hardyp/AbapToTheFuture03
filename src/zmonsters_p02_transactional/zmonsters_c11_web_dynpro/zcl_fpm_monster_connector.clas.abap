class ZCL_FPM_MONSTER_CONNECTOR definition
  public
  create public .

public section.

  interfaces IF_FPM_CONNECTOR .
  interfaces IF_FPM_CONNECTOR_DEF .
  interfaces IF_FPM_CONNECTOR_RUN .

  class-data MO_CONTAINER type ref to ZCL_FPM_MONSTER_CONTAINER .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM_MONSTER_CONNECTOR IMPLEMENTATION.


  METHOD class_constructor.

    if_fpm_connector_def~sv_namespace = 'MONSTER_NAMESPACE'.

  ENDMETHOD.


  method IF_FPM_CONNECTOR_DEF~GET_PARAMETER_LIST.

  endmethod.


  method IF_FPM_CONNECTOR_DEF~GET_PARAMETER_VALUE_SET.

  endmethod.


  method IF_FPM_CONNECTOR_DEF~INITIALIZE.

  endmethod.


  METHOD if_fpm_connector_def~set_input.
* The feeder class ZCL_FPM_MONSTER_LIST_FEEDER "outports" the
* it's container object is method GET_OUTPORT_DATA and this
* container object ends up here as an importing parameter
    mo_container ?= io_input.

  ENDMETHOD.


  method IF_FPM_CONNECTOR_RUN~CREATE_ENTITY.

  endmethod.


  METHOD if_fpm_connector_run~get_output.
* This will end up in the "form feeder" class ZCL_FPM_MONSTER_DETAIL_FEEDER
    ro_output = mo_container.

  ENDMETHOD.


  method IF_FPM_CONNECTOR_RUN~IS_CREATE_ALLOWED.

  endmethod.
ENDCLASS.
