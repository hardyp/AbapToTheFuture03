class ZCL_V_MONSTER_HEAD_CONS_CHECK definition
  public
  inheriting from /BOBF/CL_LIB_V_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_VALIDATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_V_MONSTER_HEAD_CONS_CHECK IMPLEMENTATION.


METHOD /bobf/if_frw_validation~execute.
*--------------------------------------------------------------------*
* Listing 08.15 - Executing a Validation
*--------------------------------------------------------------------*
* Local Variables
    DATA: bopf_monster_header_records    TYPE ztt_monster_header,
          external_monster_header_record TYPE zstr_monster_header.

* Clear Exporting Parameters
    CLEAR: eo_message,
           et_failed_key.

* Get the current header values
    io_read->retrieve(
    EXPORTING iv_node       = zif_monster_c=>sc_node-monster_header
              it_key        = it_key
    IMPORTING et_data       = bopf_monster_header_records
              et_failed_key = et_failed_key
              eo_message    = eo_message ).

    CHECK et_failed_key[] IS INITIAL.

    READ TABLE bopf_monster_header_records INTO DATA(bopf_monster_header_record) INDEX 1.

    CHECK sy-subrc EQ 0.

    TRY.
* Use the model to actually perform the logic check
        DATA(monster_model) =
        zcl_monster_model=>get_instance( bopf_monster_header_record-monster_number ).

        MOVE-CORRESPONDING bopf_monster_header_record TO external_monster_header_record.

        monster_model->validate_monster_header( external_monster_header_record ).

      CATCH zcx_monster_exceptions INTO DATA(monster_exception).

        DATA(monster_key) = it_key[ 1 ]."Only One Line

        "This key (node) has failed at the job of being consistent
        "Shame upon it
        INSERT monster_key INTO TABLE et_failed_key.

* Now we send the error message in the format the BOPF Framework desires
        DATA(origin_location_information) = VALUE /bobf/s_frw_location(
         node_key = is_ctx-node_key
         key      = monster_key-key )."I heard you the first time

        DATA(message_in_a_bottle) = NEW /bobf/cm_frw_core(
            textid             = monster_exception->if_t100_message~t100key
            severity           = /bobf/cm_frw=>co_severity_error
            symptom            = /bobf/if_frw_message_symptoms=>co_bo_inconsistency
            lifetime           = /bobf/if_frw_c=>sc_lifetime_set_by_bopf
            ms_origin_location = origin_location_information ).

        zcl_bc_bopf_pl_helper=>put_message_in_bottle(
         EXPORTING i_hope_that_someone_gets_my = message_in_a_bottle
         CHANGING  co_bottle                   = eo_message ).

    ENDTRY.

  ENDMETHOD."EXECUTE of ZCL_V_MONSTER_HEAD_CONS_CHECK
ENDCLASS.
