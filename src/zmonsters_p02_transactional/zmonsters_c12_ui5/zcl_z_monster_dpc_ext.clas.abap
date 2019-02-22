class ZCL_Z_MONSTER_DPC_EXT definition
  public
  inheriting from ZCL_Z_MONSTER_DPC
  create public .

public section.

  methods CONSTRUCTOR .
protected section.

  methods MONSTERITEMS_GET_ENTITYSET
    redefinition .
  methods MONSTERS_GET_ENTITYSET
    redefinition .
  methods MONSTERS_DELETE_ENTITY
    redefinition .
private section.

  data MO_MONSTER_MODEL type ref to ZCL_MONSTER_MODEL .
ENDCLASS.



CLASS ZCL_Z_MONSTER_DPC_EXT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT mo_monster_model.

  ENDMETHOD.


  METHOD monsteritems_get_entityset.

    DATA: monster_header LIKE LINE OF et_entityset.

    io_tech_request_context->get_converted_source_keys(
    IMPORTING es_key_values = monster_header ).

    TRY.

        mo_monster_model->retrieve_monster_record(
          EXPORTING
            id_monster_number = monster_header-monster_number
          IMPORTING
            et_monster_items  = et_entityset[] ).

      CATCH zcx_monster_exceptions INTO DATA(monster_exception).
* Create Bottle...
        DATA(message_container) =
       /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

* Put message in bottle....
* Use CONV to convert from string to TEXT220
        message_container->add_message_text_only(
          EXPORTING
          iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
          iv_msg_text = CONV #( monster_exception->get_text( ) ) ).

* Throw bottle into sea....
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = message_container.

    ENDTRY.

  ENDMETHOD.


  METHOD monsters_delete_entity.
*--------------------------------------------------------------------*
* In real life you would outsource the DELETE operation to the
* Monster Model class, get an error message back (maybe) and if
* so convert that into a SEGW framework specific message here
*--------------------------------------------------------------------*
    DATA(message_container) =
     /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    message_container->add_message_text_only(
    EXPORTING
    iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
    iv_msg_text = `This monster does not want to be deleted` ).

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = message_container.

  ENDMETHOD.


  METHOD monsters_get_entityset.
* See if we have any selection criteria passed in
    DATA(odata_filter) = io_tech_request_context->get_filter( ).
    DATA(odata_filter_select_options) =
    odata_filter->get_filter_select_options( ).

* We adapt the ODATA structure to the generic COSEL
* selection structure used throughout ABAP
    DATA: abap_select_options TYPE ztt_bc_coseltab.

    IF odata_filter_select_options[] IS NOT INITIAL.
      LOOP AT odata_filter_select_options
       INTO DATA(odata_select_option_structure).
        APPEND INITIAL LINE TO abap_select_options
        ASSIGNING FIELD-SYMBOL(<abap_select_option>).
        <abap_select_option>-field =
       odata_select_option_structure-property.
        LOOP AT odata_select_option_structure-select_options
          INTO DATA(odata_select_option).
          <abap_select_option>-option = odata_select_option-option.
          <abap_select_option>-sign   = odata_select_option-sign.
          <abap_select_option>-low    = odata_select_option-low.
          <abap_select_option>-high   = odata_select_option-high.
        ENDLOOP."Selection options for field being queried
      ENDLOOP."List of fields being queried
    ELSE.
* No selection criteria have been passed in
* Set selection criteria so that all records are returned
      APPEND INITIAL LINE TO abap_select_options
      ASSIGNING <abap_select_option>.
      <abap_select_option>-field   = 'MONSTER_NUMBER'.
      <abap_select_option>-option  = 'GT'.
      <abap_select_option>-sign    = 'I'.
      <abap_select_option>-low     = '0000000001'.
    ENDIF."Were any selection criteria passed in?

* The below needs buffering, in case of client side paging
    mo_monster_model->retrieve_headers_by_attribute(
      EXPORTING
        it_selections      = abap_select_options
      IMPORTING
        et_monster_headers = DATA(table_of_filtered_monsters) ).

* Extract any instructions how to sort the result list from
* the incoming request
    DATA(odata_sort_order_fields) =
    io_tech_request_context->get_orderby( ).

* Now we build a dynamic table which we will then use to
* sort the result a la SORTCAT in the ALV
    DATA: abap_sort_order_fields TYPE abap_sortorder_tab.

    LOOP AT odata_sort_order_fields
      ASSIGNING FIELD-SYMBOL(<odata_sort_order>).
      APPEND INITIAL LINE TO abap_sort_order_fields
      ASSIGNING FIELD-SYMBOL(<abap_sort_order>).
      <abap_sort_order>-name = <odata_sort_order>-property.
      IF <odata_sort_order>-order = 'desc'.
        <abap_sort_order>-descending = abap_true.
      ENDIF.
      IF <odata_sort_order>-property = 'NAME'  OR
         <odata_sort_order>-property = 'COLOR' OR
         <odata_sort_order>-property = 'STRENGTH'.
        <abap_sort_order>-astext = abap_true.
      ENDIF.
    ENDLOOP."Sort Order from Incoming Request

    SORT table_of_filtered_monsters BY (abap_sort_order_fields).

* Query the incoming URL to see if we have to start from
* a specific point, and how many rows to display
    DATA(paging_skip) = io_tech_request_context->get_skip( ).

    "The URL may contain text like "$skip=5"
    IF paging_skip IS NOT INITIAL.
      DATA(start_row) = paging_skip + 1.
    ELSE.
      start_row = 1.
    ENDIF.

    DATA(paging_top) = io_tech_request_context->get_top( ).

    "The URL may contain text like "$top=10"
    IF paging_top IS NOT INITIAL.
      DATA(end_row) = paging_skip + paging_top.
    ELSE.
      end_row = lines( table_of_filtered_monsters ).
    ENDIF.

* Export the final result
    LOOP AT table_of_filtered_monsters FROM start_row TO end_row
      ASSIGNING FIELD-SYMBOL(<filtered_monster_header>).
      APPEND INITIAL LINE TO et_entityset
      ASSIGNING FIELD-SYMBOL(<monster_header_record>).
      MOVE-CORRESPONDING <filtered_monster_header>
      TO <monster_header_record>.
    ENDLOOP.

* Change security settings to allow local testing as per
* http://scn.sap.com/community/gateway/blog/2014/09/23/solve-cors-with-gateway-and-chrome
    DATA(http_name_value_pair) = VALUE ihttpnvp(
    name = 'Access-Control-Allow-Origin'
    value = '*' ).

    /iwbep/if_mgw_conv_srv_runtime~set_header( http_name_value_pair ).

  ENDMETHOD.
ENDCLASS.
