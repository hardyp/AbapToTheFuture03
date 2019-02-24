class ZCL_FPM_MONSTER_LIST_FEEDER definition
  public
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_LIST .
  interfaces IF_FPM_FEEDER_MODEL .

  data MT_MONSTER_HEADERS type ZTT_MONSTER_HEADER .
  data MO_CONTAINER type ref to ZCL_FPM_MONSTER_CONTAINER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM_MONSTER_LIST_FEEDER IMPLEMENTATION.


  method IF_FPM_FEEDER_MODEL~GET_INPORT_KEY.
  endmethod.


  METHOD if_fpm_feeder_model~get_namespace.
    rv_namespace = 'MONSTER_NAMESPACE'.
  ENDMETHOD.


  METHOD if_fpm_feeder_model~get_outports.

    DATA: object_key      TYPE string VALUE 'MONSTER_DISPLAY',
          object_key_data TYPE REF TO data.

    APPEND INITIAL LINE TO et_outport ASSIGNING FIELD-SYMBOL(<ls_outport>).
    GET REFERENCE OF object_key INTO object_key_data.
    <ls_outport>-object_key  = object_key_data.
    <ls_outport>-type        = 'LS'.
    <ls_outport>-identifier  = 'MONSTER'.
    <ls_outport>-description = 'Monster Information'.

  ENDMETHOD.


  METHOD if_fpm_feeder_model~get_outport_data.

    ro_data = mo_container.

  ENDMETHOD.


  method IF_FPM_FEEDER_MODEL~SET_CONNECTOR.
  endmethod.


  method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
  endmethod.


  method IF_FPM_GUIBB_LIST~FLUSH.
  endmethod.


  METHOD if_fpm_guibb_list~get_data.
    CASE iv_eventid->mv_event_id.
      WHEN 'FPM_START'.
        SELECT * FROM ztmonster_header
          INTO CORRESPONDING FIELDS OF TABLE mt_monster_headers.
          ct_data = mt_monster_headers.
          ev_data_changed = abap_true.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.


  method IF_FPM_GUIBB_LIST~GET_DEFAULT_CONFIG.
  endmethod.


  METHOD if_fpm_guibb_list~get_definition.
    eo_field_catalog ?= cl_abap_tabledescr=>describe_by_name( 'ZTT_MONSTER_HEADER' ).
  ENDMETHOD.


  METHOD if_fpm_guibb_list~process_event.

    CASE io_event->mv_event_id.
      WHEN 'FPM_GUIBB_LIST_ON_LEADESELECTI'.
        READ TABLE mt_monster_headers ASSIGNING FIELD-SYMBOL(<ls_monster_header>)
                                      INDEX iv_lead_index.
        CHECK sy-subrc EQ 0.
        mo_container->ms_monster_header = <ls_monster_header>.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
* Hello!
  endmethod.


  METHOD if_fpm_guibb~initialize.

    CREATE OBJECT mo_container.

  ENDMETHOD.
ENDCLASS.
