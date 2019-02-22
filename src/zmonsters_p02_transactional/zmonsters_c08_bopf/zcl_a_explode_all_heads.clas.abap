class ZCL_A_EXPLODE_ALL_HEADS definition
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



CLASS ZCL_A_EXPLODE_ALL_HEADS IMPLEMENTATION.


  METHOD /bobf/if_frw_action~execute.
* Local variables
    DATA: monster_header_records TYPE ztt_monster_header.

* Clear exporting parameters
    CLEAR: eo_message,
           et_failed_key.

* Get the current header values
    io_read->retrieve(
    EXPORTING iv_node = zif_monster_c=>sc_node-monster_header
              it_key  = it_key
    IMPORTING et_data = monster_header_records ).

    DATA(monster_header_record) = monster_header_records[ 1 ].

    "Explode All Heads
    monster_header_record-no_of_heads = 0.

    DATA: header_record_reference TYPE REF TO data.

    GET REFERENCE OF monster_header_record
    INTO header_record_reference.

    io_modify->update(
      EXPORTING iv_node = is_ctx-node_key
                 iv_key  = monster_header_record-key
                 is_data = header_record_reference ).

  ENDMETHOD.
ENDCLASS.
