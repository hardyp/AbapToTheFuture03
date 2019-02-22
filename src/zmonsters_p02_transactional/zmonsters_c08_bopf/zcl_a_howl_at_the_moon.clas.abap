class ZCL_A_HOWL_AT_THE_MOON definition
  public
  inheriting from /BOBF/CL_LIB_A_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_ACTION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_A_HOWL_AT_THE_MOON IMPLEMENTATION.


METHOD /bobf/if_frw_action~execute.
*--------------------------------------------------------------------*
* Listing 08.17 - Executing an Action
*--------------------------------------------------------------------*
* Local Variables
    DATA: monster_header_records TYPE ztt_monster_header.

* Clear Exporting Parameters
    CLEAR: eo_message,
           et_failed_key.

* Get the current header values
    io_read->retrieve(
    EXPORTING iv_node = zif_monster_c=>sc_node-monster_header
              it_key  = it_key
    IMPORTING et_data = monster_header_records ).

    DATA(monster_header_record) = monster_header_records[ 1 ].

    CHECK monster_header_record-monster_number IS NOT INITIAL.

* Get the model
    TRY.
        DATA(monster_model) =
        zcl_monster_model=>get_instance( monster_header_record-monster_number ).
      CATCH zcx_monster_exceptions.
        RETURN.
    ENDTRY.

* Transform generic import structure into concrete structure
    DATA: howl_request_record TYPE zsa_howl_at_the_moon.

    FIELD-SYMBOLS: <howl_request_record> TYPE any.

    ASSIGN is_parameters->* TO <howl_request_record>.

    IF <howl_request_record> IS ASSIGNED.
      howl_request_record = <howl_request_record>.
    ELSE.
      "No specific request for an exact number of howls
      howl_request_record-no_of_howls = 1.
    ENDIF.

* Off we go!
    TRY.
        monster_model->howl_at_the_moon( howl_request_record ).

* Error Handling Time. The Monster class sends a framework agnostic
* error in the form of an exception class. We will adapt this to a
* BOPF specific exception
      CATCH zcx_monster_exceptions INTO DATA(monster_exception).

        DATA(monster_key) = it_key[ 1 ]."Only one line

        "This key (node) has failed at the job of performing the action
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

  ENDMETHOD."Execute Action of ZCL_A_HOWL_AT_THE_MOON
ENDCLASS.
