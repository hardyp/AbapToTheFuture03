class ZCL_MONSTER_MODEL_PERS_BOPF definition
  public
  create public .

public section.

  interfaces ZIF_MONSTER_MODEL_PERS_LAYER .

  aliases CREATE_MONSTER_RECORD
    for ZIF_MONSTER_MODEL_PERS_LAYER~CREATE_MONSTER_RECORD .
  aliases RETRIEVE_HEADERS_BY_ATTRIBUTE
    for ZIF_MONSTER_MODEL_PERS_LAYER~RETRIEVE_HEADERS_BY_ATTRIBUTE .
  aliases RETRIEVE_MONSTER_RECORD
    for ZIF_MONSTER_MODEL_PERS_LAYER~RETRIEVE_MONSTER_RECORD .
  aliases UPDATE_MONSTER_RECORD
    for ZIF_MONSTER_MODEL_PERS_LAYER~UPDATE_MONSTER_RECORD .

  methods CONSTRUCTOR
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods GET_BOPF_KEY_4_MONSTER_NUMBER
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RD_BOPF_KEY) type /BOBF/CONF_KEY .
protected section.
private section.

  data MO_TRANSACTION_MANAGER type ref to /BOBF/IF_TRA_TRANSACTION_MGR .
  data MO_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERVICE_MANAGER .
  data MO_OBJECT_CONFIGURATION type ref to /BOBF/IF_FRW_CONFIGURATION .
  data MO_BOPF_PL_HELPER type ref to ZCL_BC_BOPF_PL_HELPER .

  methods EXTERNAL_HEADERS_VIEW
    importing
      !IT_BOPF_HEADER_RECORDS type ZTT_MONSTER_HEADER
    returning
      value(RT_EXTERNAL_HEADER_RECORDS) type ZTTYP_MONSTER_HEADER .
  methods EXTERNAL_HEADER_VIEW
    importing
      !IS_BOPF_HEADER_RECORD type ZSC_MONSTER_HEADER
    returning
      value(RS_EXTERNAL_HEADER_RECORD) type ZSTR_MONSTER_HEADER .
  methods EXTERNAL_ITEM_VIEW
    importing
      !IT_BOPF_ITEM_RECORDS type ZTT_MONSTER_ITEMS
    returning
      value(RT_EXTERNAL_ITEM_RECORDS) type ZTTYP_MONSTER_ITEMS .
ENDCLASS.



CLASS ZCL_MONSTER_MODEL_PERS_BOPF IMPLEMENTATION.


METHOD constructor.
*--------------------------------------------------------------------*
* Listing 08.02 - BOPF Specific Persistency Layer Constructor
*--------------------------------------------------------------------*
  TRY.

      mo_transaction_manager =
      /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

      mo_service_manager =
      /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
      zif_monster_c=>sc_bo_key ).

      mo_object_configuration =
      /bobf/cl_frw_factory=>get_configuration(
      zif_monster_c=>sc_bo_key ).

      CREATE OBJECT mo_bopf_pl_helper
        EXPORTING
          io_transaction_manager  = mo_transaction_manager
          io_service_manager      = mo_service_manager
          io_object_configuration = mo_object_configuration.

    CATCH /bobf/cx_frw INTO DATA(bopf_exception).
      RAISE EXCEPTION TYPE zcx_monster_exceptions
        EXPORTING
          textid = bopf_exception->if_t100_message~t100key.
  ENDTRY.

ENDMETHOD."Constructor


METHOD EXTERNAL_HEADERS_VIEW.
* Local Variables
  FIELD-SYMBOLS: <bopf_header_record>     LIKE LINE OF it_bopf_header_records,
                 <external_header_record> LIKE LINE OF rt_external_header_records.

* In 7.4 you can do this without the loops & field symbols
  LOOP AT it_bopf_header_records ASSIGNING <bopf_header_record>.
    APPEND INITIAL LINE TO rt_external_header_records ASSIGNING <external_header_record>.
    MOVE-CORRESPONDING <bopf_header_record> TO <external_header_record>.
  ENDLOOP.

ENDMETHOD.


METHOD EXTERNAL_HEADER_VIEW.

  MOVE-CORRESPONDING is_bopf_header_record TO rs_external_header_record.

ENDMETHOD.


METHOD EXTERNAL_ITEM_VIEW.
* Local Variables
  FIELD-SYMBOLS: <bopf_item_record>     LIKE LINE OF it_bopf_item_records,
                 <external_item_record> LIKE LINE OF rt_external_item_records.

* In 7.4 you can do this without the loops & field symbols
  LOOP AT it_bopf_item_records ASSIGNING <bopf_item_record>.
    APPEND INITIAL LINE TO rt_external_item_records ASSIGNING <external_item_record>.
    MOVE-CORRESPONDING <bopf_item_record> TO <external_item_record>.
  ENDLOOP.

ENDMETHOD.


METHOD GET_BOPF_KEY_4_MONSTER_NUMBER.
*---------------------------------------------------------------*
* IMPORTING id_monster_number TYPE ZDE_MONSTER_NUMBER
* RETURN    rd_bopf_key       TYPE /BOBF/CONF_KEY
*---------------------------------------------------------------*
* Listing 08.04 - Using a Custom Query to turn a Monster Number into a Key
*---------------------------------------------------------------*
* Local Variables
  DATA : bopf_selection_parameters TYPE /bobf/t_frw_query_selparam.

* This builds the dynamic WHERE clause for the database read
  APPEND INITIAL LINE TO bopf_selection_parameters
  ASSIGNING FIELD-SYMBOL(<bopf_selection_parameter>).

  <bopf_selection_parameter> = VALUE #(
  attribute_name =
  zif_monster_c=>sc_query_attribute-monster_header-select_by_elements-monster_number
  sign   = 'I'
  option = 'EQ'
  low    = id_monster_number ).

* This builds a fully dynamic SQL Statement
  CALL METHOD mo_service_manager->query
    EXPORTING
      iv_query_key            = zif_monster_c=>sc_query-monster_header-select_by_elements
      it_selection_parameters = bopf_selection_parameters
    IMPORTING
      et_key                  = DATA(monster_keys).

* The return is always a table, but we have a unique key, so
* as someone once said THERE CAN BE ONLY ONE!
  READ TABLE monster_keys INDEX 1
  ASSIGNING FIELD-SYMBOL(<monster_key>).

  CHECK sy-subrc EQ 0.

  rd_bopf_key = <monster_key>-key.

ENDMETHOD.


METHOD zif_monster_model_pers_layer~create_monster_record.
*--------------------------------------------------------------------*
* Listing 08.20 - Creating a New Monster Record
*--------------------------------------------------------------------*
* Local Variables
    DATA : bopf_monster_header_record TYPE REF TO zsc_monster_header.

    FIELD-SYMBOLS: <bopf_monster_header_record> TYPE zsc_monster_header.

    CLEAR ef_creation_successful.

*--------------------------------------------------------------------*
* Create Header Record
*--------------------------------------------------------------------*
    TRY.
        "The data component of the change to be made is typed as TYPE REF TO DATA
        CREATE DATA bopf_monster_header_record.

        ASSIGN bopf_monster_header_record->* TO <bopf_monster_header_record>.

        MOVE-CORRESPONDING is_monster_header TO <bopf_monster_header_record>.

        "I've got a brand new pair of roller skates, you've got a brand new key
        <bopf_monster_header_record>-key = /bobf/cl_frw_factory=>get_new_key( ).

        DATA(all_changes_to_be_made) = VALUE /bobf/t_frw_modification( (
        node        = zif_monster_c=>sc_node-monster_header
        change_mode = /bobf/if_frw_c=>sc_modify_create
        key         = bopf_monster_header_record->key
        data        = bopf_monster_header_record ) ).

*--------------------------------------------------------------------*
* Time for the item table
*--------------------------------------------------------------------*
        DATA: bopf_monster_item_record TYPE REF TO zsc_monster_items.

        FIELD-SYMBOLS: <bopf_monster_item_record> TYPE zsc_monster_items.

        LOOP AT it_monster_items INTO DATA(monster_item_record).

          CREATE DATA bopf_monster_item_record.

          ASSIGN bopf_monster_item_record->* TO <bopf_monster_item_record>.

          MOVE-CORRESPONDING monster_item_record TO <bopf_monster_item_record>.
          <bopf_monster_item_record>-key = /bobf/cl_frw_factory=>get_new_key( ).

          APPEND VALUE #(
          node        = zif_monster_c=>sc_node-monster_items
          change_mode = /bobf/if_frw_c=>sc_modify_create
          source_node = zif_monster_c=>sc_node-monster_header
          association =
          zif_monster_c=>sc_association-monster_header-monster_items
          source_key = bopf_monster_header_record->key
          key        = bopf_monster_item_record->key
          data       = bopf_monster_item_record )
          TO all_changes_to_be_made.

        ENDLOOP."Monster Items

*--------------------------------------------------------------------*
* Here We Go!
*--------------------------------------------------------------------*
        mo_bopf_pl_helper->change_data_in_memory(
          EXPORTING it_changes_to_be_made     = all_changes_to_be_made
          IMPORTING ef_data_in_memory_changed = ef_creation_successful ).

        CHECK ef_creation_successful = abap_true.

        mo_bopf_pl_helper->change_data_in_database( ).

      CATCH /bobf/cx_frw INTO DATA(bobf_exception).
        ef_creation_successful = abap_false.
        "Need to extract the actual error information from the BOBF error
        "Both the BOBF exception and the monster exception implement the
        "T100 interface, so we can just pass the information on directly
        RAISE EXCEPTION TYPE zcx_monster_exceptions
          EXPORTING
            textid = bobf_exception->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD."Create Monster Record / ZCL_MONSTER_MODEL_PERS_BOPF


METHOD zif_monster_model_pers_layer~retrieve_headers_by_attribute.
* Local variables
  DATA bopf_selection_parameters TYPE /bobf/t_frw_query_selparam.

* Adapt generic query structure to specific BOPF query structure
  LOOP AT it_selections INTO DATA(generic_selection_parameter).

    APPEND INITIAL LINE TO bopf_selection_parameters
    ASSIGNING FIELD-SYMBOL(<bopf_selection_parameter>).

    <bopf_selection_parameter>-attribute_name = generic_selection_parameter-field.
    <bopf_selection_parameter>-sign           = generic_selection_parameter-sign.
    <bopf_selection_parameter>-option         = generic_selection_parameter-option.
    <bopf_selection_parameter>-low            = generic_selection_parameter-low.
    <bopf_selection_parameter>-high           = generic_selection_parameter-high.

  ENDLOOP.

  CHECK bopf_selection_parameters[] IS NOT INITIAL.

  DATA: bopf_monster_header_records TYPE ztt_monster_header.

  CALL METHOD mo_service_manager->query
    EXPORTING
      iv_query_key            = zif_monster_c=>sc_query-monster_header-select_by_elements
      it_selection_parameters = bopf_selection_parameters
      iv_fill_data            = abap_true
    IMPORTING
      et_data                 = bopf_monster_header_records.

  et_monster_headers = external_headers_view( bopf_monster_header_records ).

  LOOP AT et_monster_headers ASSIGNING FIELD-SYMBOL(<ls_headers>).
    IF <ls_headers>-creation_date IS INITIAL.
      "Gateway dumps if a date is initial
      <ls_headers>-creation_date = sy-datum.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD zif_monster_model_pers_layer~retrieve_monster_record.
*-----------------------------------------------------------------*
* Listing 08.03 - Model Class Retrieval Method
*-----------------------------------------------------------------*
* We could do this the traditional way...
*
*  SELECT SINGLE * FROM ztmonster_header
*    INTO CORRESPONDING FIELDS OF es_monster_header
*    WHERE monster_number = id_monster_number.
*
* But that is just what they are EXPECTING us to do!
* Instead, let us go down the BOPF path...
*-----------------------------------------------------------------*
* Clear Export Parameters
    CLEAR: es_monster_header,
           et_monster_items.

* Use adapter pattern to translate human readable CRUD standard to
* the BOPF equivalent
    DATA(bopf_edit_mode) = SWITCH /bobf/conf_edit_mode( id_edit_mode
      WHEN 'R' THEN /bobf/if_conf_c=>sc_edit_read_only  "Read
      WHEN 'U' THEN /bobf/if_conf_c=>sc_edit_exclusive  "Update
      ELSE THROW zcx_monster_exceptions( ) ).           "Unexpected Situation

    TRY.
* To get a BOPF object, we need a key, not a number
        DATA(monster_key) = get_bopf_key_4_monster_number( id_monster_number ).

* The header record lives in the header(root) node
        DATA(bopf_monster_header) = CAST zsc_monster_header(
        mo_bopf_pl_helper->get_node_row(
        id_object_key      = monster_key
        id_node_type       = zif_monster_c=>sc_node-monster_header
        id_node_row_number = 1
        id_edit_mode       = bopf_edit_mode ) ).

        FIELD-SYMBOLS: <bopf_monster_header> TYPE zsc_monster_header.

        ASSIGN bopf_monster_header->* TO <bopf_monster_header>.

        es_monster_header = external_header_view( <bopf_monster_header> ).

* The item table records live in one or more child nodes of the
* header level node
        DATA(bopf_monster_items) = CAST ztt_monster_items(
        mo_bopf_pl_helper->get_child_node_table(
        id_object_key       = monster_key
        id_parent_node_type = zif_monster_c=>sc_node-monster_header
        id_child_node_type  = zif_monster_c=>sc_association-monster_header-monster_items ) ).

        FIELD-SYMBOLS: <bopf_monster_items> TYPE ztt_monster_items.

        ASSIGN bopf_monster_items->* TO <bopf_monster_items>.

        et_monster_items = external_item_view( <bopf_monster_items> ).

      CATCH /bobf/cx_frw INTO DATA(bopf_exception).
        RAISE EXCEPTION TYPE zcx_monster_exceptions
          EXPORTING
            textid = bopf_exception->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD."Retrieve Monster Record of ZCL_MONSTER_MODEL_PERS_BOPF


METHOD zif_monster_model_pers_layer~update_monster_record.
*--------------------------------------------------------------------*
* Listing 08.23 - Updating (Changing) an Existing BOPF Record
*--------------------------------------------------------------------*
* Local Variables
  FIELD-SYMBOLS: <bopf_monster_header> TYPE zsc_monster_header.

  DATA : bopf_monster_header TYPE REF TO zsc_monster_header.

*--------------------------------------------------------------------*
* Update Header
*--------------------------------------------------------------------*
  TRY.
      CREATE DATA bopf_monster_header.

      ASSIGN bopf_monster_header->* TO <bopf_monster_header>.

      MOVE-CORRESPONDING is_monster_header TO <bopf_monster_header>.

      DATA(all_changes_to_be_made) = VALUE /bobf/t_frw_modification( (
      node        = zif_monster_c=>sc_node-monster_header
      change_mode = /bobf/if_frw_c=>sc_modify_update
      key         = bopf_monster_header->key
      data        = bopf_monster_header ) ).

*--------------------------------------------------------------------*
* Update Items
*--------------------------------------------------------------------*
      DATA: bopf_monster_item_record TYPE REF TO zsc_monster_items.

      FIELD-SYMBOLS: <bopf_monster_item_record> TYPE zsc_monster_items.

      LOOP AT it_monster_items INTO DATA(monster_item_record).

        CREATE DATA bopf_monster_item_record.

        ASSIGN bopf_monster_item_record->* TO <bopf_monster_item_record>.

        MOVE-CORRESPONDING monster_item_record TO <bopf_monster_item_record>.

        APPEND VALUE #(
        node        = zif_monster_c=>sc_node-monster_items
        change_mode = /bobf/if_frw_c=>sc_modify_update
        source_node = zif_monster_c=>sc_node-monster_header
        association =
        zif_monster_c=>sc_association-monster_header-monster_items
        source_key = bopf_monster_header->key
        key        = bopf_monster_item_record->key
        data       = bopf_monster_item_record )
        TO all_changes_to_be_made.

      ENDLOOP."Monster Items

      mo_bopf_pl_helper->change_data_in_memory(
        EXPORTING it_changes_to_be_made     = all_changes_to_be_made
        IMPORTING ef_data_in_memory_changed = ef_update_successful ).

      CHECK ef_update_successful = abap_true.

      mo_bopf_pl_helper->change_data_in_database( ).

    CATCH /bobf/cx_frw INTO DATA(bopf_exception).
      ef_update_successful = abap_false.
      RAISE EXCEPTION TYPE zcx_monster_exceptions
        EXPORTING
          textid = bopf_exception->if_t100_message~t100key.
  ENDTRY.

ENDMETHOD."Update Monster Record / ZCL_MONSTER_MODEL_PERS_BOPF
ENDCLASS.
