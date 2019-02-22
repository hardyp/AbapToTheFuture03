*&---------------------------------------------------------------------*
*& Report  ZL05_03_DEPENDENCIES
*&
*&---------------------------------------------------------------------*
* Listing 05.02 - Calling Methods of Classes
*--------------------------------------------------------------------*
* In this program all calls to external systems are made by way of
* call to methods of classes
* You cannot subclass a function module, but you can subclass
* classes to simulate the actions of a user for example
* With all the dependencies broken, automated unit tests can be run
* upon the program
*--------------------------------------------------------------------*
REPORT  zl05_03_broken_dependencies.

*--------------------------------------------------------------------*
* Class Definitions / Implementations
*--------------------------------------------------------------------*
CLASS lcl_database_access DEFINITION.
  PUBLIC SECTION.
    METHODS : read_customising.
ENDCLASS.                    "lcl_database_access DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_database_access IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_database_access IMPLEMENTATION.
  METHOD read_customising.
    CALL FUNCTION 'ZREAD_MONSTER_CUSTOMIZING'.
  ENDMETHOD.                    "read_customising
ENDCLASS.                    "lcl_database_access IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_missile_interface DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_missile_interface DEFINITION.
  PUBLIC SECTION.
    METHODS : get_nuclear_missile_status,
              tell_pi_proxy_to_fire_missile.
ENDCLASS.                    "lcl_missile_interface DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_missile_interface IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_missile_interface IMPLEMENTATION.
  METHOD get_nuclear_missile_status.
    CALL FUNCTION 'ZGET_NUCLEAR_MISSILE_STATUS'.
  ENDMETHOD.                    "get_nuclear_missile_status

  METHOD tell_pi_proxy_to_fire_missile.
    CALL FUNCTION 'ZTELL_PI_PROXY_TO_FIRE_MISSILE'.
  ENDMETHOD.                    "tell_pi_proxy_to_fire_missile
ENDCLASS.                    "lcl_missile_interface IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_user_interface DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_user_interface DEFINITION.
  PUBLIC SECTION.
    METHODS popup_to_confirm RETURNING value(user_answer) TYPE char01.
ENDCLASS.                    "lcl_user_interface DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user_interface IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_user_interface IMPLEMENTATION.
  METHOD popup_to_confirm.
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

  ENDMETHOD.                    "popup_to_confirm
ENDCLASS.                    "lcl_user_interface IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_printer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_printer DEFINITION.
  PUBLIC SECTION.
    METHODS : print_nuclear_smartform.
ENDCLASS.                    "lcl_printer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_printer IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_printer IMPLEMENTATION.
  METHOD print_nuclear_smartform.
    CALL FUNCTION 'ZPRINT_NUCLEAR_SMARTFORM'.
  ENDMETHOD.                    "print_nuclear_smartform
ENDCLASS.                    "lcl_printer IMPLEMENTATION

*--------------------------------------------------------------------*
* Evil Global Variables
*--------------------------------------------------------------------*
DATA: go_database_access   TYPE REF TO lcl_database_access,
      go_missile_interface TYPE REF TO lcl_missile_interface,
      go_user_interface    TYPE REF TO lcl_user_interface,
      go_printer           TYPE REF TO lcl_printer.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  CREATE OBJECT go_database_access.
  CREATE OBJECT go_missile_interface.
  CREATE OBJECT go_user_interface.
  CREATE OBJECT go_printer.
  PERFORM fire_nuclear_missile.

*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
FORM fire_nuclear_missile.

* Read Database
* Here the dependency is needing an actual database with real data
  go_database_access->read_customising( ).

* Query Missile Sensor
* Here the dependency is needing contact with an actual missile system
  go_missile_interface->get_nuclear_missile_status( ).

* Actual Business Logic (that you want to test)
* You would want to test that the missile gets sent to the right place
* i.e. gets dropped on your enemy, not on you
* IF something.
*   "We fire the missile here
* ELSEIF something_else.
*   "We fire the missile here
* ENDIF.

* Ask the user if they want to fire the missile
* Here the dependency is on having an actual user
  DATA(user_answer) = go_user_interface->popup_to_confirm( ).

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
  go_missile_interface->tell_pi_proxy_to_fire_missile( ).

* Print Results
* Here the dependency is needing an actual printer
  go_printer->print_nuclear_smartform( ).

*--------------------------------------------------------------------*
* As it stands the program is functionally totally unchanged
* The difference is, that in the main FORM there is only business
* logic. All external dependencies are handled by classes which can
* be subclassed
* Thus automated unit tests are now possible on this program
* I have deliberately left this as a procedural program, to show how
* you can change parts of your monolithic ten thousand line existing
* procedural program, to bring islands of code (individual routines)
* under test, a bit at a time
*--------------------------------------------------------------------*
ENDFORM.                    "fire_nuclear_missile
