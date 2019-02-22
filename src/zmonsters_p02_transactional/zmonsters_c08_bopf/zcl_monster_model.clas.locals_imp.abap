*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
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
