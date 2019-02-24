class ZCL_FPM_MONSTER_DETAIL_FEEDER definition
  public
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_FORM .
  interfaces IF_FPM_FEEDER_MODEL .

  data MO_CONNECTOR type ref to ZCL_FPM_MONSTER_CONNECTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM_MONSTER_DETAIL_FEEDER IMPLEMENTATION.


  method IF_FPM_FEEDER_MODEL~GET_INPORT_KEY.

  endmethod.


  METHOD if_fpm_feeder_model~get_namespace.

    rv_namespace = 'MONSTER_NAMESPACE'.

  ENDMETHOD.


  method IF_FPM_FEEDER_MODEL~GET_OUTPORTS.

  endmethod.


  method IF_FPM_FEEDER_MODEL~GET_OUTPORT_DATA.

  endmethod.


  METHOD if_fpm_feeder_model~set_connector.

    mo_connector ?= io_connector.

  ENDMETHOD.


  method IF_FPM_GUIBB_FORM~CHECK_CONFIG.
  endmethod.


  method IF_FPM_GUIBB_FORM~FLUSH.
  endmethod.


  METHOD if_fpm_guibb_form~get_data.

    CASE io_event->mv_event_id.
      WHEN 'FPM_GUIBB_LIST_ON_LEAD_SELECTI'.
        cs_data         = mo_connector->mo_container->ms_monster_header.
        ev_data_changed = abap_true.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  method IF_FPM_GUIBB_FORM~GET_DEFAULT_CONFIG.
  endmethod.


  method IF_FPM_GUIBB_FORM~GET_DEFINITION.
    eo_field_catalog ?= cl_abap_structdescr=>describe_by_name( 'ZSC_MONSTER_HEADER' ).
  endmethod.


  method IF_FPM_GUIBB_FORM~PROCESS_EVENT.
  endmethod.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


  method IF_FPM_GUIBB~INITIALIZE.
  endmethod.
ENDCLASS.
