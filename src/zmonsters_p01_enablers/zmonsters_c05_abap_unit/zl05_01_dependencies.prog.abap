*&---------------------------------------------------------------------*
*& Report  ZL05_01_DEPENDENCIES
*&
*&---------------------------------------------------------------------*
* Listing 05.01 - Common ABAP Application
*--------------------------------------------------------------------*
* This shows a typical procedural program which is chock full of
* dependencies on various external parties, thus rendering the
* program impossible to run unit tests upon.
*--------------------------------------------------------------------*
REPORT  zl05_01_dependencies.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fire_nuclear_missile.

*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
FORM fire_nuclear_missile.

* Read Database
* Here the dependency is needing an actual database with real data
  CALL FUNCTION 'ZREAD_MONSTER_CUSTOMIZING'.

* Query Missile Sensor
* Here the dependency is needing contact with an actual missile system
  CALL FUNCTION 'ZGET_NUCLEAR_MISSILE_STATUS'.

* Actual Business Logic (that you want to test)
* You would want to test that the missile gets sent to the right place
* i.e. gets dropped on your enemy, not on you
* IF something.
*   "We fire the missile here
* ELSEIF something_else.
*   "We fire the missile here
* ENDIF.

* Ask the user if they want to fire the missile
* Here the depenendcy is on having an actual user
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

* Fire Missile
* Here the dependency is needing an actual missile to fire
  CALL FUNCTION 'ZTELL_PI_PROXY_TO_FIRE_MISSILE'.

* Print Results
* Here the dependency is needing an actual printer
  CALL FUNCTION 'ZPRINT_NUCLEAR_SMARTFORM'.

*--------------------------------------------------------------------*
* As can be seen, this program is impossible to test automatically
* You would need an actual user in front of the screen, and a printer
* hooked up, and valid data in the database - often data in DEV is rubbish
* Then you would have to have dummy missile systems.
* So testing could be done - manually - with a large amount of effort
*--------------------------------------------------------------------*
ENDFORM.                    "fire_nuclear_missile
