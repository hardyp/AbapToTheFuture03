*&---------------------------------------------------------------------*
*& Report  ZCH04_EXCEPTION_HANDLING
*&
*&---------------------------------------------------------------------*
* This program will not do anything when run, it is intended to be read and will
* concentrate on the various ways you can use exception classes to
* handle unexpected situations
* Each method will have a reference to the listing in the book where the
* code is explained in detail
*&---------------------------------------------------------------------*
REPORT zch04_exception_handling.

CLASS lcl_golf_scores DEFINITION INHERITING FROM zcl_function_module_wrapper.
  PUBLIC SECTION.
    METHODS golf_handicap_of_monster IMPORTING id_monster_number       TYPE zde_monster_number
                                     RETURNING VALUE(rd_golf_handicap) TYPE i.
ENDCLASS."LCL_GOLF_SCORES Defintion

*--------------------------------------------------------------------*
* Listing 04.06 : Wrapping a Function Module in a Method
*--------------------------------------------------------------------*
CLASS lcl_golf_scores IMPLEMENTATION.

  METHOD golf_handicap_of_monster.

    remove_existing_messages( ).

    CALL FUNCTION 'ZMONSTER_GOLF_SCORES'
      EXPORTING
        id_monster_number             = id_monster_number
      IMPORTING
        ed_golf_handicap              = rd_golf_handicap
      EXCEPTIONS
        monster_only_one_inch_tall    = 1
        monster_has_no_silly_trousers = 2
        OTHERS                        = 3.

    IF sy-subrc <> 0.
      throw_exception_on_error_from( 'ZMONSTER_GOLF_SCORES' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS."LCL_GLF_SCORES
*--------------------------------------------------------------------*
* Listing 04.15:  Pink and Fluffy Is Not Scary!
*--------------------------------------------------------------------*
CLASS lcl_monster_constraint DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_constraint.

ENDCLASS. "Monster Constraint Definition

CLASS lcl_monster_constraint IMPLEMENTATION.

  METHOD if_constraint~is_valid.
*-------------------------------------------------------*
* IMPORTING data_object TYPE data
* RETURNING result      TYPE abap_bool
*-------------------------------------------------------*
* Local Variables
    DATA: monster TYPE REF TO zcl_exceptional_monster.

    monster ?= data_object.

    result = abap_false.

    CHECK monster->scariness     CS 'SCARY'.
    CHECK monster->bolts_in_neck EQ 2.
    CHECK monster->fluffiness    EQ 0.
    CHECK monster->color         NE 'PINK'.

    result = abap_true.

  ENDMETHOD.                      "IF_CONSTRAINT~is_valid

  METHOD if_constraint~get_description.
*-------------------------------------------------------*
* RETURNING result TYPE string_table
*-------------------------------------------------------*
* Local Variables
    DATA: message TYPE string.

    message = 'Monster is no longer a monster!'.

    APPEND message TO result.

  ENDMETHOD. "IF_CONSTRAINT~get_description

ENDCLASS. "Monster Constraint Implementation
*----------------------------------------------------------------------*
*       CLASS lcl_monstrous_exceptions DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_monstrous_exceptions DEFINITION.
  PUBLIC SECTION.
    METHODS: something_static RAISING zcx_static_exception,
      user_command     IMPORTING id_ucomm      TYPE sy-ucomm
                                 id_chosen_row TYPE i,
      try_catch_block    RAISING zcx_monster_exception_lop,
      classic_conversion RAISING zcx_monster_exception_mc,
      wrapping_a_function IMPORTING id_monster_number       TYPE zde_monster_number
                          RETURNING VALUE(rd_golf_handicap) TYPE i,
      head_swap_operation_wrong,
      head_swap_operation_right,
      replace_everything,
      retry,
      resumable,
      open_monsters_eyes,
      raise_short_dump.


  PRIVATE SECTION.
    DATA : mo_monster         TYPE REF TO zcl_exceptional_monster,
           mo_candle          TYPE REF TO zcl_candle,
           mf_data_is_rubbish TYPE abap_bool.

    METHODS: get_chosen_monster IMPORTING id_chosen_row     TYPE i
                                RETURNING VALUE(ro_monster) TYPE REF TO zcl_exceptional_monster,
      do_something RAISING zcx_monster_exceptions
                           zcx_monster_exception_mc
                           zcx_monster_exception_lop,
      handle_this_exception IMPORTING io_exception TYPE REF TO zcx_monster_exceptions,
      handle_that_exception IMPORTING io_exception TYPE REF TO zcx_monster_exception_mc,
      cleanup_main_method,
      do_not_know RAISING RESUMABLE(zcx_static_exception).

ENDCLASS.                    "lcl_monstrous_exceptions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_monstrous_exceptions IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_monstrous_exceptions IMPLEMENTATION.

*--------------------------------------------------------------------*
* Listing 04.01 - Handling an Exception Locally or Propogating it
*                 up the call stack
*--------------------------------------------------------------------*
  METHOD something_static."RAISING zcx_static_exception.

    MESSAGE 'Executing Method SOMETHING_STATIC' TYPE 'I'.

    IF sy-uzeit > '120000'.
      "In the afternoons we do not want to handle the error
      "locally, so we propogate it up the stack
      MESSAGE 'Exception raised in method SOMETHING_STATIC' TYPE 'I'.
      RAISE EXCEPTION TYPE zcx_static_exception.
    ENDIF.

    TRY .
        "In the mornings we want to handle this exception within this
        "method
        MESSAGE 'Exception raised in method SOMETHING_STATIC' TYPE 'I'.
        RAISE EXCEPTION TYPE zcx_static_exception.
      CATCH zcx_static_exception.
        MESSAGE 'Static Exception caught inside of method SOMETHING_STATIC' TYPE 'I'.
    ENDTRY.
  ENDMETHOD.                    "lcl_monstrous_exceptions

*--------------------------------------------------------------------*
* Listing 04.02 - Using a NO_CHECK exception during user command
*                 processing
*--------------------------------------------------------------------*
  METHOD user_command.
*--------------------------------------------------------------------*
* IMPORTING id_ucomm      TYPE sy-ucomm
*           id_chosen_row TYPE i.
*--------------------------------------------------------------------*
* Local Variables
    DATA: lo_monster TYPE REF TO zcl_exceptional_monster,
          ld_message TYPE string.

    MESSAGE 'Executing Method USER_COMMAND' TYPE 'I'.

    lo_monster = get_chosen_monster( id_chosen_row ).

    CHECK lo_monster IS BOUND."as opposed to Frankenstien Unbound

    ld_message = |Monster is Responding to { id_ucomm } user command|.
    MESSAGE ld_message TYPE 'I'.

    TRY.
        lo_monster->lock( ).

        CASE id_ucomm.
          WHEN 'HOWL'.
            lo_monster->howl_at_moon( ).
          WHEN 'TERROR'.
            lo_monster->terrorize_village( ).
          WHEN 'SUBPRIME'.
            "Most Monstrous activity possible
            lo_monster->sell_mortgages( )."etc...
          WHEN OTHERS.
            MESSAGE 'Function is not available' TYPE 'I'.
            RAISE EXCEPTION TYPE zcx_bc_user_cancelled.
        ENDCASE.
      CATCH zcx_bc_user_cancelled."NO_CHECK
        MESSAGE 'User Cancelled Exception has been caught' TYPE 'I'.
        lo_monster->unlock( ).
        RETURN.
    ENDTRY.

* Do common tasks that you need to do after a successful user
* command like update the monsters bank balance based on what
* they just did

    lo_monster->unlock( ).

  ENDMETHOD.                    "user_command

*--------------------------------------------------------------------*
* Listing 04.03 - TRY / CATCH / CLEANUP
* A demonstration of the separtion of concerns, which each method
* looking after one task only i.e. a method does ONE THING only
*--------------------------------------------------------------------*
  METHOD try_catch_block.
* Local Variables
    DATA: lcx_monster_exception    TYPE REF TO zcx_monster_exceptions,
          lcx_monster_exception_mc TYPE REF TO zcx_monster_exception_mc.

    MESSAGE 'Executing Method TRY_CATCH_BLOCK' TYPE 'I'.

    TRY.
        "ONE THING is the executing the actual business logic - the task at hand
        do_something( ).
      CATCH zcx_monster_exceptions INTO lcx_monster_exception.
        "ONE THING is handling one sort of error
        handle_this_exception( lcx_monster_exception ).
      CATCH zcx_monster_exception_mc INTO lcx_monster_exception_mc.
        "ONE THING is handling another sort of error
        handle_that_exception( lcx_monster_exception_mc ).
      CLEANUP.
        "ONE THING is restoring system state to the way it was at the start of
        "method TRY_CATCH_BLOCK in the event another sort of
        "exception is called, one that cannot be handled locally
        "and so has to be propogated from this method up the call stack
        cleanup_main_method( ).
    ENDTRY.

  ENDMETHOD.                    "try_catch_block

  METHOD classic_conversion.
*--------------------------------------------------------------------*
* Listing 4.05 Converting Classical Exception to Class-Based
*--------------------------------------------------------------------*
    DATA : current_monster_number TYPE zde_monster_number,
           monster_handicap       TYPE i,
           current_www_count      TYPE i VALUE 3.

    CALL FUNCTION 'ZMONSTER_GOLF_SCORES'
      EXPORTING
        id_monster_number             = current_monster_number
      IMPORTING
        ed_golf_handicap              = monster_handicap
      EXCEPTIONS
        monster_only_one_inch_tall    = 1
        monster_has_no_silly_trousers = 2
        error_message                 = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_monster_exception_mc
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          wibbly_wobbly_woos = current_www_count.
    ENDIF.

  ENDMETHOD."Classic Conversion

  METHOD wrapping_a_function.
*--------------------------------------------------------------------*
* IMPORTING id_monster_number       TYPE zde_monster_number
* RETURNING VALUE(rd_golf_handicap) TYPE i.
*--------------------------------------------------------------------*
    DATA(golf_scores) = NEW lcl_golf_scores( ).

    rd_golf_handicap = golf_scores->golf_handicap_of_monster( id_monster_number ).

  ENDMETHOD.

  METHOD head_swap_operation_wrong.
*--------------------------------------------------------------------*
* Listing 04.07 - Head Swap Operation
* In this example the CLEANUP block does not get called, leading to
* disaster
*--------------------------------------------------------------------*
    CREATE OBJECT mo_monster.
    CREATE OBJECT mo_candle.

    MESSAGE 'Executing Method HEAD_SWAP_OPERATION_WRONG' TYPE 'I'.

    TRY.
        mo_monster->remove_current_head( ).
      CATCH zcx_power_failure.
        mo_candle->light( ).
      CLEANUP.
        "You would expect that the CLEANUP block would be executed
        "after the ZCX_POWER_FAILURE exception has been processed.
        "How wrong you would be.
        MESSAGE 'Cleanup Block being Executed' TYPE 'I'.
        mo_monster->reattach_old_head( ).
    ENDTRY.

    IF mo_monster->no_of_heads = 0.
      MESSAGE 'The Monster has No Head!' TYPE 'I'.
    ENDIF.

  ENDMETHOD.                    "head_swap_operation_wrong

  METHOD head_swap_operation_right.
*--------------------------------------------------------------------*
* Listing 04.08 - Making 100% sure a method cleans up after an exception
*--------------------------------------------------------------------*
    CREATE OBJECT mo_monster.
    CREATE OBJECT mo_candle.

    MESSAGE 'Executing Method HEAD_SWAP_OPERATION_RIGHT' TYPE 'I'.

    TRY.
        mo_monster->remove_current_head( ).
      CATCH zcx_power_failure.
        mo_candle->light( ).
      CLEANUP.
        "In case an exception is raised which is not caught
        "inside this routine e.g. the Castle is Stormed by the Villagers
        MESSAGE 'Cleanup Block being Executed' TYPE 'I'.
        mo_monster->reattach_old_head( ).
    ENDTRY.

    "In case the exception is raised within this routine e.g. Candle Power Failure
    "and so the new head has not been attached
    IF mo_monster->no_of_heads = 0.
      MESSAGE 'The Monster has No Head! Better do something about that' TYPE 'I'.
      mo_monster->reattach_old_head( ).
    ENDIF.

  ENDMETHOD.                    "head_swap_operation_right

  METHOD replace_everything.
*--------------------------------------------------------------------*
* Listing 04.09 - Catching an Exception in a Different Routine
*--------------------------------------------------------------------*
    CREATE OBJECT mo_monster.

    MESSAGE 'Executing Method REPLACE_EVERYTHING' TYPE 'I'.

    TRY.
        mo_monster->head_swap_operation( ).
        mo_monster->leg_swap_operation( ).
        mo_monster->arm_swap_operation( ).
      CATCH zcx_castle_stormed_byvillagers.
        MESSAGE 'Exception caught within method REPLACE_EVERYTHING' TYPE 'I'.
        mo_monster->attack_villagers( ).
    ENDTRY.

  ENDMETHOD.                    "replace_everything

  METHOD retry.
*--------------------------------------------------------------------*
* Listing 04.10 RETRY after Handling an Exception
*--------------------------------------------------------------------*
* Local Variables
    DATA: has_repair_been_attempted TYPE abap_bool.

    CREATE OBJECT mo_monster.
    CREATE OBJECT mo_candle.

    MESSAGE 'Executing Method RETRY' TYPE 'I'.

    TRY.
        TRY.
            mo_monster->remove_current_head( ).
          CATCH zcx_power_failure.
            mo_candle->light( ).
          CLEANUP.
            "You would expect that the CLEANUP block would be executed
            "after the ZCX_POWER_FAILURE exception has been processed.
            "How wrong you would be.
            MESSAGE 'Cleanup Block being Executed' TYPE 'I'.
            mo_monster->reattach_old_head( ).
        ENDTRY."Small TRY Block

        IF mo_monster->no_of_heads = 0.

          MESSAGE 'The Monster has No Head!' TYPE 'I'.
          MESSAGE 'Raise an Exception!' TYPE 'I'.
          RAISE EXCEPTION TYPE zcx_monster_exception_lop
            EXPORTING
              textid = zcx_monster_exception_lop=>head_error
              heads  = mo_monster->no_of_heads.

        ENDIF.

      CATCH zcx_monster_exception_lop.
        IF has_repair_been_attempted = abap_false.
          "Set parameters such that the user can call the MONSTER MONITOR
          "and add the extra head themselves via a link from the information
          "message
          MESSAGE i001(zvc_monster_errors) WITH mo_monster->no_of_heads.
          "Hopefully the user has fixed the problem, so let us try
          "again, from the top of the Big TRY Block
          has_repair_been_attempted = abap_true.
          MESSAGE 'RETRY' TYPE 'I'.
          RETRY.
        ELSE.
          MESSAGE 'Monster STILL has no heads, I am giving up!' TYPE 'I'.
        ENDIF.
    ENDTRY."Big Try Block

  ENDMETHOD.                    "retry

  METHOD resumable.
*--------------------------------------------------------------------*
* Part of Listing 04.11 - Raising a Resumable Exception
*--------------------------------------------------------------------*

    MESSAGE 'Executing Method RESUMABLE' TYPE 'I'.

    "Start off by faking system state such that we have a problem,
    "namely that the data is rubbish
    mf_data_is_rubbish = abap_true.

    TRY.
        "Then call a method which does not know how to deal with
        "the unexpected situation of the data being rubbish
        do_not_know( ).
      CATCH BEFORE UNWIND zcx_static_exception.
        "Repair the data using special knowledge that only this
        "level of the call stack knows
        IF mf_data_is_rubbish = abap_true.
          "Data is now repaired so it is no longer rubbish
          mf_data_is_rubbish = abap_false."Now everything is fine!
        ENDIF.
        RESUME.
    ENDTRY.

  ENDMETHOD.                    "resumable

  METHOD open_monsters_eyes.

    DATA(sleepy_monster) = NEW zcl_exceptional_monster( ).

    sleepy_monster->open_monsters_eyes( ).

* Listing 04.16 : Calling Class Invariant at End of Each Method Call
    DATA(monster_constraint) = NEW lcl_monster_constraint( ).
    zcl_dbc=>ensure(
    that             = 'The Monster is still a Monster'
    which_is_true_if = monster_constraint->if_constraint~is_valid( me ) ).

  ENDMETHOD.

  METHOD raise_short_dump.
*--------------------------------------------------------------------*
* Listing 04.17: Passsing Exception Classes to Short Dumps
* Only works from 7.53 upwards
*--------------------------------------------------------------------*
*    TRY.
*     open_monsters_eyes( ).
*    CATCH zcx_violated_precondition INTO lo_precondition.
*      "A bug was detected at the start of a subroutine - the caller of the subroutine is at fault
*      lo_precondition->mo_error_log->show_error_log( ).
*      RAISE SHORTDUMP lo_precondition.
*    CATCH zcx_violated_postcondition INTO lo_postcondition.
*      "A bug was detected at the end of a subroutine - the subroutine is at fault
*      lo_postcondition->mo_error_log->show_error_log( ).
*      RAISE SHORTDUMP lo_postcondition.
*  ENDTRY.

  ENDMETHOD.

  METHOD get_chosen_monster.
*--------------------------------------------------------------------*
* IMPORTING id_chosen_row TYPE i
* RETURNING value(ro_monster) TYPE REF TO zcl_monster.
*--------------------------------------------------------------------*
    " You would read an internal table being displayed on a SALV grid
    " and then return the monster object instance related to that row
    " For this example we just create a generic monster
    CREATE OBJECT ro_monster.
  ENDMETHOD.                    "get_chosen_monster

  METHOD do_something.
*--------------------------------------------------------------------*
* Listing 04.04 - Exporting Vital Information when raising an
* exception
*--------------------------------------------------------------------*
    DATA: wibbly_wobbly_woos TYPE i,
          scariness          TYPE string,
          heads              TYPE int4,
          arms               TYPE int4,
          legs               TYPE int4,
          bolts              TYPE int4.

    wibbly_wobbly_woos = 94.
    scariness          = 'REALLY_SCARY'.
    heads              = 2.
    arms               = 8.
    legs               = 3.
    bolts              = 2.

    MESSAGE 'Performing generic TRY / CATCH / CLEANUP method business logic' TYPE 'I'.

    "In the afternoon we raise one type of exception
    IF sy-uzeit GT '120000'.

      MESSAGE 'Raising an exception due to excessive number of wibbly wobbly woos' TYPE 'I'.

      RAISE EXCEPTION TYPE zcx_monster_exceptions
        EXPORTING
          wibbly_wobbly_woos = wibbly_wobbly_woos.

    ELSEIF sy-uzeit LT '120000'.
      "In the morning we raise another type of exception

      MESSAGE 'Raising an exception due to unusual number of monster components' TYPE 'I'.

      RAISE EXCEPTION TYPE zcx_monster_exception_mc
        EXPORTING
          scariness = scariness
          heads     = heads
          arms      = arms
          legs      = legs.

    ELSE.
      "At noon we raise a different sort of exception yet again
      MESSAGE 'Raising an exception five five whole parameters!' TYPE 'I'.

      RAISE EXCEPTION TYPE zcx_monster_exception_lop
        EXPORTING
          scariness = scariness
          heads     = heads
          arms      = arms
          legs      = legs
          bolts     = bolts.

    ENDIF.

  ENDMETHOD.                    "do_something

  METHOD handle_this_exception.
*--------------------------------------------------------------------*
* IMPORTING io_exception TYPE REF TO zcx_monster_exceptions
*--------------------------------------------------------------------*
* Local Variables
    DATA: ld_error_message TYPE string.

    ld_error_message = |There are { io_exception->wibbly_wobbly_woos } wibbly wobbly woos which is terrible!|.

    MESSAGE ld_error_message TYPE 'I'.

  ENDMETHOD.                    "handle_this_exception

  METHOD handle_that_exception.
*--------------------------------------------------------------------*
* IMPORTING io_exception TYPE REF TO zcx_monster_exception_mc
*--------------------------------------------------------------------*
  ENDMETHOD.                    "handle_that_exception

  METHOD cleanup_main_method.

    MESSAGE 'Cleaning up TRY_CATCH_CLEANUP method prior to exception propogation' TYPE 'I'.

  ENDMETHOD.                    "cleanup_main_method

  METHOD do_not_know.
*--------------------------------------------------------------------*
* This routine does not know how to repair incorrect data
*--------------------------------------------------------------------*
* Part of Listing 04.11 - Raising a Resumable Exception
*--------------------------------------------------------------------*

    TRY.
        MESSAGE 'Starting Method DO_NOT_KNOW' TYPE 'I'.

        IF mf_data_is_rubbish = abap_true.
          MESSAGE 'The data is rubbish - raise resumable exception' TYPE 'I'.
          RAISE RESUMABLE EXCEPTION TYPE zcx_static_exception.
        ELSE.
          MESSAGE 'The data is fine' TYPE 'I'.
        ENDIF.

        MESSAGE 'The Data has now been repaired - Completing Method DO_NOT_KNOW' TYPE 'I'.
      CATCH zcx_monster_exceptions.
        "Do Nothing
      CLEANUP.
        "This never gets called due to BEFORE_UNWIND in calling method
        MESSAGE 'Cleanup' TYPE 'I'.
    ENDTRY.

  ENDMETHOD.                    "do_not_know

ENDCLASS.                    "lcl_monstrous_exceptions IMPLEMENTATION

*--------------------------------------------------------------------*
* Evil Global Variables
*--------------------------------------------------------------------*
DATA: go_application   TYPE REF TO lcl_monstrous_exceptions,
      go_exception_lop TYPE REF TO zcx_monster_exception_lop,
      gd_error_message TYPE string.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  LEAVE PROGRAM."Not supposed to actually run this
  CREATE OBJECT go_application.
*--------------------------------------------------------------------*
* Listing 04.01 : CX_STATIC_CHECK
*--------------------------------------------------------------------*
  "Testing an exception class derived from CX_STATIC_CHECK
  TRY .
      go_application->something_static( ).
    CATCH zcx_static_exception.
      MESSAGE 'Static Exception caught outside of method SOMETHING_STATIC' TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.02 : CX_NO_CHECK
*--------------------------------------------------------------------*
  "Testing an exception class derived from CX_NO_CHECK
  go_application->user_command( id_ucomm      = 'SUBPRIME'
                                id_chosen_row = 1 ).

*--------------------------------------------------------------------*
* Listing 04.03 : TRY / CATCH / CLEANUP
* Listing 04.04 : Exporting Vital Information While Raising Exception
*--------------------------------------------------------------------*
* Generic example of TRY / CATCH / CLEANUP
  TRY.
      go_application->try_catch_block( ).
    CATCH zcx_monster_exception_lop INTO go_exception_lop.
      gd_error_message = go_exception_lop->get_text( ).
      MESSAGE gd_error_message TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.05 : Converting Classical Exception to Class-Based
*--------------------------------------------------------------------*
  go_application->classic_conversion( ).

*--------------------------------------------------------------------*
* Listing 04.06 : Wrapping a Function Module in a Method
*--------------------------------------------------------------------*
  DATA(monster_gold_handicap) =
  go_application->wrapping_a_function( id_monster_number = '1'  ).

*--------------------------------------------------------------------*
* Listing 04.07 : Head Swap Operations
*--------------------------------------------------------------------*
* Wrong usage of CLEANUP
  go_application->head_swap_operation_wrong( ).

*--------------------------------------------------------------------*
* Listing 04.08 : Making 100% Sure Method Cleans Up after Exception
*--------------------------------------------------------------------*
* Correct usage of CLEANUP
  TRY .
      go_application->head_swap_operation_right( ).
    CATCH zcx_castle_stormed_byvillagers.
      MESSAGE 'Castle Storming Exception has been Caught outside of main method' TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.09 : Catching Exception in Different Routine
*--------------------------------------------------------------------*
* Another example of CLEANUP done correctly
  go_application->replace_everything( ).

*--------------------------------------------------------------------*
* Listing 04.10 : RETRY after Handling Exception
*--------------------------------------------------------------------*
* Example of RETRY
  go_application->retry( ).

*--------------------------------------------------------------------*
* Listing 04.11 :  Raising Resumable Exception
*--------------------------------------------------------------------*
* Example of RESUME
  go_application->resumable( ).

*--------------------------------------------------------------------*
* Listing 04.13 : Design by Contract in ABAP
* Listing 04.16 : Class Invariants
*--------------------------------------------------------------------*
  go_application->open_monsters_eyes( ).

*--------------------------------------------------------------------*
* Listing 04.17: Passsing Exception Classes to Short Dumps
* Only works from 7.53 upwards
*--------------------------------------------------------------------*
  go_application->raise_short_dump( ).
