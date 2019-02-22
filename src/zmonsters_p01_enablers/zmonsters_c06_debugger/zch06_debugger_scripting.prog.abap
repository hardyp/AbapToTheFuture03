*&---------------------------------------------------------------------*
*& Report ZCH06_DEBUGGER_SCRIPTING
*&---------------------------------------------------------------------*
*& To use a "Toastmaster" term this program is the "Target Speaker"
*& That is it has no purpose of its own, it exists only as an example
*& to be shot down (corrected) by Debugger Scripting
*&---------------------------------------------------------------------*
REPORT zch06_debugger_scripting.

START-OF-SELECTION.
  PERFORM laugh_like_a_monster.
*&---------------------------------------------------------------------*
*& Form LAUGH_LIKE_A_MONSTER
*&---------------------------------------------------------------------*
* Listing 6.06 - Laughing Program with Error
*----------------------------------------------------------------------*
FORM laugh_like_a_monster .
* Local Variables
  DATA: current_number  TYPE sy-tabix,
        laugh_count     TYPE sy-tabix,
        lightning_count TYPE sy-tabix.

  DO 10 TIMES.
    ADD 1 TO current_number.
    WRITE:/ 'Monster has counted to',current_number.
    IF current_number DIV 3 = 0."Error Here!
      WRITE:/ 'Ha ha ha!'.
      ADD 1 TO laugh_count.
    ENDIF.
    IF current_number MOD 5 = 0.
      WRITE:/ 'Lightning Flashes!'.
      ADD 1 TO lightning_count.
    ENDIF.
  ENDDO.

ENDFORM.
