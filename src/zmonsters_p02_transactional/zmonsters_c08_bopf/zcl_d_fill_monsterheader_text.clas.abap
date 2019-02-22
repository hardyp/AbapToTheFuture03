class ZCL_D_FILL_MONSTERHEADER_TEXT definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~CHECK
    redefinition .
  methods /BOBF/IF_FRW_DETERMINATION~CHECK_DELTA
    redefinition .
  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_D_FILL_MONSTERHEADER_TEXT IMPLEMENTATION.


METHOD /bobf/if_frw_determination~check.
*--------------------------------------------------------------------*
* Listing 08.11 : Coding CHECK Method for Determination
*--------------------------------------------------------------------*
    TRY.
        "Set specific table type for generic return table
        DATA: bopf_monster_header_records TYPE ztt_monster_header.

        io_read->retrieve(
        EXPORTING iv_node = zif_monster_c=>sc_node-monster_header
                  it_key  = ct_key
        IMPORTING et_data = bopf_monster_header_records ).

        READ TABLE bopf_monster_header_records INTO
        DATA(bopf_monster_header_record) INDEX 1.

        CHECK sy-subrc EQ 0.

DATA(monster_model) = zcl_monster_model=>get_instance(
bopf_monster_header_record-monster_number ).

"Adapt BOPF-specific structure to generic monster structure
DATA: external_monster_header_record TYPE zstr_monster_header.

MOVE-CORRESPONDING bopf_monster_header_record TO
external_monster_header_record.

DATA(is_determination_needed) =
monster_model->are_values_derivation_relevant( external_monster_header_record ).

IF is_determination_needed = abap_false.
   DELETE ct_key WHERE key = bopf_monster_header_record-key.
ENDIF.

CATCH zcx_monster_exceptions.
  RETURN.
ENDTRY.

ENDMETHOD."CHECK of ZCL_D_FILL_MONSTERHEADER_TEXT


METHOD /bobf/if_frw_determination~check_delta.
*--------------------------------------------------------------------*
* Listing 08.10 - Checking Changed Fields
*-----------------------------------------------------------*
* We’re looking to see if any field in the monster header
* record has changed, which may require a derived field being
* updated as a result e.g., hat size changes, hat size description
* to be updated
*------------------------------------------------------------*

* (1) See what changes there are

"Look at the current state of the header record
io_read->compare(
EXPORTING iv_node_key        = zif_monster_c=>sc_node-monster_header
          it_key             = ct_key
          iv_fill_attributes = abap_true
IMPORTING eo_change          = DATA(this_header_record) ).

IF this_header_record->has_changes( ) = abap_true.

  this_header_record->get_changes( IMPORTING et_change = DATA(changed_header_records) ).

  "Set specific table table, for generic table returned by BOPF
  DATA: bopf_monster_header_records TYPE ztt_monster_header.

  io_read->retrieve(
    EXPORTING iv_node = zif_monster_c=>sc_node-monster_header
               it_key  = ct_key
    IMPORTING et_data = bopf_monster_header_records ).

* (2) Ask the model if the changes are such that we
* need to run the determination
  READ TABLE bopf_monster_header_records
  INTO DATA(bopf_monster_header_record) INDEX 1.

  CHECK sy-subrc EQ 0.

  TRY.
      DATA(monster_model) = zcl_monster_model=>get_instance(
       bopf_monster_header_record-monster_number ).
    CATCH zcx_monster_exceptions.
      RETURN.
  ENDTRY.

  "Adapt BOPF-specific structure to generic change structure
  DATA: for_the_changed_fields TYPE bal_t_fld,
        changed_field          LIKE LINE OF for_the_changed_fields.

  LOOP AT changed_header_records INTO DATA(changed_record).
    LOOP AT changed_record-attributes INTO DATA(field_from_attribute).
      changed_field = field_from_attribute.
      INSERT changed_field INTO TABLE for_the_changed_fields.
    ENDLOOP.
  ENDLOOP.

  DATA(is_determination_needed) =
  monster_model->is_derivation_relevant( for_the_changed_fields ).

ENDIF."Are there any changes?

* (3) If no need for determination, delete header from table
* of nodes to be changed
IF is_determination_needed = abap_false.
  DELETE ct_key WHERE key = bopf_monster_header_record-key.
ENDIF.

ENDMETHOD."Check Delta of ZCL_D_FILL_MONSTERHEADER_TEXT


  METHOD /bobf/if_frw_determination~execute.
*--------------------------------------------------------------------*
* Listing 08.12 : Executing Determination
*--------------------------------------------------------------------*
* Local variables
    DATA: bopf_monster_header_records    TYPE ztt_monster_header,
          external_monster_header_record TYPE zstr_monster_header.

* Clear exporting parameters
    CLEAR: eo_message,
           et_failed_key.

    TRY.
* Get persistent (database) values
        io_read->retrieve(
        EXPORTING iv_node = zif_monster_c=>sc_node-monster_header
                  it_key  = it_key
        IMPORTING et_data = bopf_monster_header_records ).

LOOP AT bopf_monster_header_records INTO DATA(bopf_monster_header_record).

CLEAR external_monster_header_record.
MOVE-CORRESPONDING bopf_monster_header_record TO external_monster_header_record.

* Use the model to derive the transient values
DATA(monster_model) = zcl_monster_model=>get_instance( external_monster_header_record-monster_number ).

monster_model->fill_header_derived_fields(
CHANGING cs_monster_header = external_monster_header_record ).

MOVE-CORRESPONDING external_monster_header_record TO bopf_monster_header_record.

* The combined structure is full, pass it back to the BOPF
DATA: header_record_reference TYPE REF TO data.

GET REFERENCE OF bopf_monster_header_record
INTO header_record_reference.

io_modify->update(
  EXPORTING iv_node = is_ctx-node_key
             iv_key  = bopf_monster_header_record-key
             is_data = header_record_reference ).

ENDLOOP."Monsters being Queried

* This exception started life as a BRFplus exception,
* was adapted to a generic monster exception,
* and now we’ll adapt it once again to BOPF
CATCH zcx_monster_exceptions INTO DATA(monster_exception).
READ TABLE it_key INTO DATA(monster_key) INDEX 1."Only one line

"This key (node) has failed in its mission. Shame upon it
INSERT monster_key INTO TABLE et_failed_key.

* Now send an error message in the format BOPF
* desires
DATA: origin_location_information TYPE /bobf/s_frw_location, "Location Location Location
       message_in_a_bottle         TYPE REF TO /bobf/cm_frw_core.

 origin_location_information-node_key = is_ctx-node_key.
 origin_location_information-key     =
monster_key-key."I heard you the first time

CREATE OBJECT message_in_a_bottle
EXPORTING
textid             = monster_exception->if_t100_message~t100key
severity           = /bobf/cm_frw=>co_severity_error
symptom         = /bobf/if_frw_message_symptoms=>co_bo_inconsistency
lifetime           = /bobf/if_frw_c=>sc_lifetime_set_by_bopf
ms_origin_location = origin_location_information.

zcl_bc_bopf_pl_helper=>put_message_in_bottle(
   EXPORTING i_hope_that_someone_gets_my = message_in_a_bottle
   CHANGING  co_bottle                   = eo_message ).

ENDTRY.

ENDMETHOD."EXECUTE of ZCL_D_FILL_MONSTERHEADER_TEXT
ENDCLASS.
