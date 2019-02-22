class ZCL_WDA_MONSTER_TEST_CLASS definition
  public
  create private
  for testing
  duration short
  risk level harmless .

public section.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tester TYPE REF TO cl_wd_web_dynpro_tester .

    METHODS: setup,
      allow_box_to_be_checked FOR TESTING,
      given_user_ticks_box,
      when_round_trip_occurs,
      then_box_is_ticked.

ENDCLASS.



CLASS ZCL_WDA_MONSTER_TEST_CLASS IMPLEMENTATION.


  METHOD allow_box_to_be_checked."for TESTING
*--------------------------------------------------------------------*
* Listing 11.09 :  Setting Up WDA Unit Test
*--------------------------------------------------------------------*
    given_user_ticks_box( ).

    when_round_trip_occurs( ).

    then_box_is_ticked( ).

  ENDMETHOD.


  METHOD given_user_ticks_box.
*--------------------------------------------------------------------*
* Listing 11.10 : Executing WDA Unit Test (1/3)
*--------------------------------------------------------------------*
    DATA: checkbox TYPE REF TO if_wdt_checkbox.

    checkbox ?= mo_tester->get_ui_element_tester( EXPORTING view_id        = 'V_SELECT_OPTIONS'
                                                            uielement_id   = 'CBOX_BED_HIDER' ).

    checkbox->raise_change( EXPORTING checked = abap_true ).

  ENDMETHOD.


  METHOD setup.
*--------------------------------------------------------------------*
* Listing 11.09 :  Setting Up WDA Unit Test
*--------------------------------------------------------------------*
    mo_tester   = cl_wd_web_dynpro_tester=>create(
    application = 'ZWDA_MONSTER_MONITOR' ).

  ENDMETHOD.


  METHOD then_box_is_ticked.
*--------------------------------------------------------------------*
* Listing 11.10 : Executing WDA Unit Test (3/3)
*--------------------------------------------------------------------*
    DATA: checked TYPE abap_bool.

    DATA(root_node) = mo_tester->get_context_root_node( controller_id = 'V_SELECT_OPTIONS' ).

    DATA(node) = root_node->get_child_node( 'BED_HIDER_FLAG' ).

    node->get_attribute( EXPORTING name  = 'BED_HIDER_FLAG'
                         IMPORTING value = checked ).

    cl_abap_unit_assert=>assert_equals(
        exp                  = abap_true
        act                  = checked ).

  ENDMETHOD.


  METHOD when_round_trip_occurs.
*--------------------------------------------------------------------*
* Listing 11.10 : Executing WDA Unit Test (2/3)
*--------------------------------------------------------------------*
    mo_tester->execute_request( ).

  ENDMETHOD.
ENDCLASS.
