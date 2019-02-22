*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*--------------------------------------------------------------------*
* Listing 03.06 : Short Lived Variables
*--------------------------------------------------------------------*
CLASS lcl_weapon_iterator DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      get_next_weapon RETURNING VALUE(rd_weapon) TYPE string.

  PRIVATE SECTION.
    DATA: weapon_table    TYPE TABLE OF string,
          last_used_index TYPE sy-tabix.

ENDCLASS.

CLASS lcl_weapon_iterator IMPLEMENTATION.

  METHOD constructor.

    weapon_table = VALUE string_table(
    ( |'FEATHER DUSTER'| )
    ( |'PEASHOOTER'| )
    ( |'THE BIG KNIFE'| )
    ( |'GUN'| )
    ( |'MACHINE GUN'| )
    ( |'LASER PISTOL'| )
    ( |'NUCLEAR MISSILE'| ) ).

  ENDMETHOD.

  METHOD get_next_weapon.

    CASE last_used_index.
      WHEN 0.
        DATA(row_to_read) = 1.
      WHEN lines( weapon_table ).
        row_to_read = 1.
      WHEN OTHERS.
        row_to_read = last_used_index + 1.
    ENDCASE.

    READ TABLE weapon_table INTO rd_weapon INDEX row_to_read.

    ASSERT sy-subrc EQ 0.

    last_used_index = row_to_read.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
* Listing 03.19 : Incorrect BOOLEAN Check
*--------------------------------------------------------------------*
CLASS lcl_atom_bomb DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: get_details IMPORTING id_bomb_number         TYPE vbeln
                               EXPORTING ed_something_spurious  TYPE string
                               CHANGING  cd_something_unrelated TYPE string
                               RETURNING VALUE(rd_bomb_name)    TYPE string.
    METHODS: explode.

ENDCLASS.

CLASS lcl_atom_bomb IMPLEMENTATION.

  METHOD get_details.

    rd_bomb_name = 'FAT BOY'.

  ENDMETHOD.

  METHOD explode.

    MESSAGE 'Bang!' TYPE 'I'.

  ENDMETHOD.

ENDCLASS.
*--------------------------------------------------------------------*
* Listing 03.60 : Defining Interface with Optional Methods
*--------------------------------------------------------------------*
INTERFACE lif_scary_behavior.
  METHODS: scare_small_children,
           sells_mortgages   DEFAULT FAIL,
           hide_under_bed    DEFAULT IGNORE,
           is_fire_breather  DEFAULT IGNORE
           RETURNING VALUE(rf_yes_it_is) TYPE abap_bool.
ENDINTERFACE. "Scary Behavior
