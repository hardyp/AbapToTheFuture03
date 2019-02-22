FUNCTION ZCH05_01_02_TEST_SEAMS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"--------------------------------------------------------------------
  PERFORM fire_nuclear_missile.

ENDFUNCTION.
*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
FORM fire_nuclear_missile.

  TEST-SEAM read_database.
* Here the dependency is needing an actual database with real data
    CALL FUNCTION 'ZREAD_MONSTER_CUSTOMIZING'.
  END-TEST-SEAM.

  TEST-SEAM missile_sensor.
* Here the dependency is needing contact with an actual missile system
    CALL FUNCTION 'ZGET_NUCLEAR_MISSILE_STATUS'.
  END-TEST-SEAM.

* Actual Business Logic (that you want to test)
* You would want to test that the missile gets sent to the right place
* i.e. gets dropped on your enemy, not on you
* IF something.
*   "We fire the missile here
* ELSEIF something_else.
*   "We fire the missile here
* ENDIF.

* Ask the user if they want to fire the missile
  TEST-SEAM user_input.
    DATA: user_answer TYPE char01.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Missile Confirmation'
        text_question  = 'Do you want to launch the missile?'
        text_button_1  = 'Yes'
        text_button_2  = 'No'
        default_button = '2'
      IMPORTING
        answer         = user_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  END-TEST-SEAM.

* Some more business logic
* You would want to test that saying "no" prevented the
* missile from firing
  CASE user_answer.
    WHEN '1'.
      "Off We Go! Bombs Away!
    WHEN '2'.
      RETURN.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  TEST-SEAM missile_interface.
* Here the dependency is needing an actual missile to fire
    CALL FUNCTION 'ZTELL_PI_PROXY_TO_FIRE_MISSILE'.
  END-TEST-SEAM.

  TEST-SEAM printer.
* Here the dependency is needing an actual printer
    CALL FUNCTION 'ZPRINT_NUCLEAR_SMARTFORM'.
  END-TEST-SEAM.

ENDFORM.                    "fire_nuclear_missile
