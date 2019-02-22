class ZCL_EXCEPTIONAL_MONSTER definition
  public
  create public .

public section.

  data NO_OF_HEADS type SY-TABIX read-only .
  data NO_OF_EYES type SY-TABIX read-only .
  data EYES_ARE_OPEN type ABAP_BOOL read-only .
  data SCARINESS type STRING read-only .
  data BOLTS_IN_NECK type SY-TABIX read-only .
  data FLUFFINESS type INT4 read-only .
  data COLOR type STRING read-only .

  methods HOWL_AT_MOON .
  methods TERRORIZE_VILLAGE .
  methods SELL_MORTGAGES .
  methods LOCK .
  methods UNLOCK .
  methods REMOVE_CURRENT_HEAD .
  methods ADD_NEW_HEAD .
  methods REATTACH_OLD_HEAD .
  methods CONSTRUCTOR
    importing
      !NAME type ZDE_MONSTER_NAME optional .
  methods HEAD_SWAP_OPERATION .
  methods LEG_SWAP_OPERATION .
  methods ARM_SWAP_OPERATION .
  methods ATTACK_VILLAGERS .
  methods OPEN_MONSTERS_EYES .
  methods GET_MONSTERS
    importing
      !IT_MONSTER_TYPES type ANY TABLE
    returning
      value(RT_MONSTERS) type ZTT_MONSTER_HEADER .
  methods GET_AHEAD_GET_A_HAT
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    exporting
      !ED_NUMBER_OF_HEADS type I
      !ED_NUMBER_OF_HATS type I .
  methods INVITE_TO_PARTY
    importing
      !ID_NAME type CHAR20 .
  methods LOG
    importing
      !ID_VALUE type ref to DATA .
  class-methods FACTORY
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RO_MONSTER) type ref to ZCL_EXCEPTIONAL_MONSTER.
  methods IS_IT_MAD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RF_IT_IS_MAD) type ABAP_BOOL .
protected section.
private section.

  data MD_NAME type ZDE_MONSTER_NAME .
ENDCLASS.



CLASS ZCL_EXCEPTIONAL_MONSTER IMPLEMENTATION.


METHOD ADD_NEW_HEAD.

  MESSAGE 'Add New Head to Monster' TYPE 'I'.

  ADD 1 TO no_of_heads.

ENDMETHOD.


METHOD ARM_SWAP_OPERATION.

  MESSAGE 'Swapping Monsters Arms' TYPE 'I'.

ENDMETHOD.


METHOD ATTACK_VILLAGERS.

  MESSAGE 'Monster is Attacking the Villagers!' TYPE 'I'.

ENDMETHOD.


METHOD CONSTRUCTOR.

  no_of_heads = 1.

  md_name = name.

ENDMETHOD.


method FACTORY.
  endmethod.


method GET_AHEAD_GET_A_HAT.
  endmethod.


method GET_MONSTERS.
  endmethod.


METHOD HEAD_SWAP_OPERATION.
* Local Variables
  DATA: lo_candle TYPE REF TO zcl_candle.

  CREATE OBJECT lo_candle.

  TRY.
      MESSAGE 'Swapping Monsters Head' TYPE 'I'.
      TRY.
          remove_current_head( ).
        CATCH zcx_power_failure.
          lo_candle->light( ).
      ENDTRY.
      MESSAGE 'Oh No! Here Comes the Villagers!' TYPE 'I'.
      RAISE EXCEPTION TYPE zcx_castle_stormed_byvillagers.
    CLEANUP.
      MESSAGE 'CLEANUP block being excuted with HEAD_SWAP_OPERATION' TYPE 'I'.
      reattach_old_head( ).
  ENDTRY.

ENDMETHOD.


method HOWL_AT_MOON.
endmethod.


method INVITE_TO_PARTY.
  endmethod.


METHOD IS_IT_MAD.
  rf_it_is_mad = abap_true.
ENDMETHOD.


METHOD LEG_SWAP_OPERATION.

  MESSAGE 'Swapping Monsters Legs' TYPE 'I'.

ENDMETHOD.


METHOD LOCK.

  MESSAGE 'Monster Locked' TYPE 'I'.

ENDMETHOD.


method LOG.
  endmethod.


METHOD OPEN_MONSTERS_EYES.
*--------------------------------------------------------------------*
* Listing 04.13 - Design by Contract in ABAP
*--------------------------------------------------------------------*
* Preconditions
  zcl_dbc=>require( that             = 'The Monster has at least one eye'
                    which_is_true_if = xsdbool( no_of_eyes GE 1 ) ).

* Code to Open the Monsters Eyes
  eyes_are_open = abap_true.

* Postconditions
  zcl_dbc=>ensure( that = 'The Monsters Eyes are Open'
                   which_is_true_if = xsdbool( eyes_are_open = abap_true ) ).

ENDMETHOD.


METHOD REATTACH_OLD_HEAD.

  MESSAGE 'Re-Attaching Old Head to Monster' TYPE 'I'.

  ADD 1 TO no_of_heads.

ENDMETHOD.


METHOD REMOVE_CURRENT_HEAD.

  MESSAGE 'Removing Monsters Head' TYPE 'I'.

  IF no_of_heads = 0.
    MESSAGE 'Monster has no head!' TYPE 'I'.
    RETURN.
  ENDIF.

  SUBTRACT 1 FROM no_of_heads.

  IF sy-uzeit > '200000'.
    "Once it goes dark, the villagers storm the castle
    "waving flaming brands
    MESSAGE 'Here come the Villagers!' TYPE 'I'.
    RAISE EXCEPTION TYPE zcx_castle_stormed_byvillagers.
  ELSEIF sy-uzeit > '120000'.
    "In the afternoon, the power goes out
    MESSAGE 'Power has gone out!' TYPE 'I'.
    RAISE EXCEPTION TYPE zcx_power_failure.
  ENDIF.

ENDMETHOD.


METHOD SELL_MORTGAGES.

  MESSAGE 'Monster is about to sell a Mortgage' TYPE 'I'.
  "Even the Baron can't go through with this, evil as he is
  "So he presses the CANCEL button
  MESSAGE 'Baron has pressed the CANCEL button' TYPE 'I'.
  RAISE EXCEPTION TYPE zcx_bc_user_cancelled.

ENDMETHOD.


method TERRORIZE_VILLAGE.
endmethod.


METHOD UNLOCK.

  MESSAGE 'Monster Unlocked' TYPE 'I'.

ENDMETHOD.
ENDCLASS.
