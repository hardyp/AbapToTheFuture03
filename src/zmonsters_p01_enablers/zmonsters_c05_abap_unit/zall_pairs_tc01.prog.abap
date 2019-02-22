*&---------------------------------------------------------------------*
*& Include          ZALL_PAIRS_TC01
*&---------------------------------------------------------------------*
* This contains the definitions/implementations of the test classes, and
* any test doubles needed to get them working. The test doubles come
* first naturally
*----------------------------------------------------------------------*
CLASS lcl_mock_persistency_layer DEFINITION INHERITING FROM lcl_persistency_layer.
  PUBLIC SECTION.
    METHODS: get_data REDEFINITION.
ENDCLASS.

CLASS lcl_mock_persistency_layer IMPLEMENTATION.

  METHOD get_data.
*--------------------------------------------------------------------*
* 06.10 - Inserting Variables into a Test Double
*--------------------------------------------------------------------*
    et_configuration[] = VALUE g_tt_configuration(
    ( variable_name = 'Monster Model'(007) count = 1 possible_value = 'BTNK' ) "Bolts Through Neck
    ( variable_name = 'Monster Model'(007) count = 2 possible_value = 'KLKL')  "Killer Klown
    ( variable_name = 'Monster Model'(007) count = 3 possible_value = 'ISDD' ) "Ice Skating Dead
    ( variable_name = 'Evilness'(008)      count = 1 possible_value = 'EVIL' )
    ( variable_name = 'Evilness'(008)      count = 2 possible_value = 'VERY' )
    ( variable_name = 'Brain Size'(009)    count = 1 possible_value = 'SMALL' )
    ( variable_name = 'Brain Size'(009)    count = 2 possible_value = 'MICRO' )
    ( variable_name = 'Scariness'(010)     count = 1 possible_value = 'NORM' )
    ( variable_name = 'Scariness'(010)     count = 2 possible_value = 'BANK' )
    ( variable_name = 'Usage'(011)         count = 1 possible_value = 'NORM' ) "Scares Peasants
    ( variable_name = 'Usage'(011)         count = 2 possible_value = 'PLUM' ) "It's the PLUMBER!
    ( variable_name = 'Color'(012)         count = 1 possible_value = 'BLUE' )
    ( variable_name = 'Color'(012)         count = 2 possible_value = 'GREEN' ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_test_class DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA: mo_class_under_test TYPE REF TO lcl_all_pairs,
          mo_mock_pers_layer  TYPE REF TO lcl_mock_persistency_layer.

    METHODS: setup,
*--------------------------------------------------------------------*
* IT SHOULD.....
* Calculate the mapping correctly
* Process the first two columns correctly
* Get All Pairs when you add Brain Size as an extra variable
* Get All Pairs when you add Scariness as an extra variable
* Get All Pairs when you add Usage as an extra variable
* Get All Pairs when you add Color as an extra variable
*--------------------------------------------------------------------*
      calculate_mapping         FOR TESTING,
      process_first_two_columns FOR TESTING,
      get_ap_for_brain_size     FOR TESTING,
      get_ap_for_scariness      FOR TESTING,
      get_ap_for_usage          FOR TESTING,
      get_ap_for_color          FOR TESTING,
* Helper Methods
      given_monster_options,
      when_mapping_is_calculated,
      then_mapping_is_correct,
      given_mapping_calculated,
      when_first_2_columns_filled,
      then_first_2_columns_are_ok,
      given_first_2_columns_filled,
      when_brain_size_added,
      then_aps_for_brain_size_ok,
      given_brain_size_added,
      when_scariness_added,
      then_aps_for_scariness_ok,
      given_scariness_added,
      when_usage_added,
      then_aps_for_usage_ok,
      given_usage_added,
      when_color_added,
      then_aps_for_color_ok.

ENDCLASS.

CLASS lcl_test_class IMPLEMENTATION.

  DEFINE pair_is_in_a_test_case.

    cl_abap_unit_assert=>assert_equals(
    exp = abap_true
    act = mo_class_under_test->pair_is_in_test_case(
          variable_1   = &1
          first_value  = &2
          variable_2   = &3
          second_value = &4 )
    msg = | Pair { &1 } / { &2 } + { &3 } / { &4 } not in any test case| ).

  END-OF-DEFINITION.

  METHOD setup.

    CREATE OBJECT mo_mock_pers_layer.
    CREATE OBJECT mo_class_under_test.

    TRY.
        mo_class_under_test->mt_configuration = mo_mock_pers_layer->get_data( ).
      CATCH zcx_excel.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD calculate_mapping.

    given_monster_options( ).

    when_mapping_is_calculated( ).

    then_mapping_is_correct( ).

  ENDMETHOD.

  METHOD process_first_two_columns.

    given_monster_options( ).

    given_mapping_calculated( ).

    when_first_2_columns_filled( ).

    then_first_2_columns_are_ok( ).

  ENDMETHOD.

  METHOD get_ap_for_brain_size.

    given_monster_options( ).

    given_mapping_calculated( ).

    given_first_2_columns_filled( ).

    when_brain_size_added(  ).

    then_aps_for_brain_size_ok( ).

  ENDMETHOD.

  METHOD get_ap_for_scariness.

    given_monster_options( ).

    given_mapping_calculated( ).

    given_first_2_columns_filled( ).

    given_brain_size_added( ).

    when_scariness_added( ).

    then_aps_for_scariness_ok( ).

  ENDMETHOD.

  METHOD get_ap_for_usage.

    given_monster_options( ).

    given_mapping_calculated( ).

    given_first_2_columns_filled( ).

    given_brain_size_added( ).

    given_scariness_added( ).

    when_usage_added( ).

    then_aps_for_usage_ok( ).

  ENDMETHOD.

  METHOD get_ap_for_color.

    given_monster_options( ).

    given_mapping_calculated( ).

    given_first_2_columns_filled( ).

    given_brain_size_added( ).

    given_scariness_added( ).

    given_usage_added( ).

    when_color_added( ).

    then_aps_for_color_ok( ).

  ENDMETHOD.


  METHOD given_monster_options ##needed.

* This does nothing, it just makes the unit tests look like English
* The actual monster options are created during SETUP before each test

  ENDMETHOD.


  METHOD when_mapping_is_calculated.

    mo_class_under_test->calculate_mapping( ).

  ENDMETHOD.


  METHOD then_mapping_is_correct.
* The mapping table should now have one line for each variables saying how many distinct values
* there are.
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_mapping )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in Mapping Table' ).

* The variables with the most distinct values should be first in the table
    DATA(first_entry) = mo_class_under_test->mt_mapping[ 1 ].

    cl_abap_unit_assert=>assert_equals( act = first_entry-variable_name
                                        exp = 'Monster Model'
                                        msg = 'Incorrect 1st Entry in Mapping Table' ).

    cl_abap_unit_assert=>assert_equals( act = first_entry-value_count
                                        exp = 3
                                        msg = 'Incorrect Value Count in Mapping Table' ).

  ENDMETHOD.


  METHOD given_mapping_calculated.

    mo_class_under_test->calculate_mapping( ).

  ENDMETHOD.


  METHOD when_first_2_columns_filled.

    mo_class_under_test->process_first_two_columns( ).

  ENDMETHOD.


  METHOD then_first_2_columns_are_ok.
*--------------------------------------------------------------------*
* At this point we have all possible combinations of the first and second variables in the
* test case table, and also in the "all pairs" table
* We check if this is the case using a Unit Test
*--------------------------------------------------------------------*
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_test_cases )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in Test Case Table' ).

    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_pairs )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in All Pairs Table' ).

  ENDMETHOD.


  METHOD given_first_2_columns_filled.

    mo_class_under_test->process_first_two_columns( ).

  ENDMETHOD.


  METHOD when_brain_size_added.

    mo_class_under_test->insert_new_column( 3 ).

  ENDMETHOD.


  METHOD then_aps_for_brain_size_ok.
*--------------------------------------------------------------------*
* The new variable BRAIN SIZE has be added to the equation with
* two possible values - SMALL & MICRO
* We are expecting at least one test case in te list that pairs each possible
* value of BRAIN SIZE with each possible value of MODEL
* We are expecting at least one test case in te list that pairs each possible
* value of BRAIN SIZE with each possible value of EVILNESS
* A single test case can cater for more than one set of pairs
*--------------------------------------------------------------------*
* Pairs for BRAIN SIZE / MODEL
    pair_is_in_a_test_case :
    'Brain Size' 'SMALL' 'Monster Model' 'BTNK',
    'Brain Size' 'SMALL' 'Monster Model' 'KLKL',
    'Brain Size' 'SMALL' 'Monster Model' 'ISDD',
    'Brain Size' 'MICRO' 'Monster Model' 'BTNK',
    'Brain Size' 'MICRO' 'Monster Model' 'KLKL',
    'Brain Size' 'MICRO' 'Monster Model' 'ISDD',
* Pairs for BRAIN SIZE / EVILNESS
    'Brain Size' 'SMALL' 'Evilness' 'EVIL',
    'Brain Size' 'SMALL' 'Evilness' 'VERY',
    'Brain Size' 'MICRO' 'Evilness' 'EVIL',
    'Brain Size' 'MICRO' 'Evilness' 'VERY'.

* The new pairs can be slotted into the existing 6 test cases
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_test_cases )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in Test Case Table' ).

  ENDMETHOD.


  METHOD given_brain_size_added.

    mo_class_under_test->insert_new_column( 3 ).

  ENDMETHOD.


  METHOD when_scariness_added.

    mo_class_under_test->insert_new_column( 4 ).

  ENDMETHOD.


  METHOD then_aps_for_scariness_ok.
*--------------------------------------------------------------------*
* The new variable SCARINESS has be added to the equation with
* two possible values - NORM & BANK
* Note that the amount of pairs being tested for increases in a
* linear manner as opposed to a geometric manner
*--------------------------------------------------------------------*
* Pairs for SCARINESS / MODEL
    pair_is_in_a_test_case :
    'Scariness' 'NORM' 'Monster Model' 'BTNK',
    'Scariness' 'NORM' 'Monster Model' 'KLKL',
    'Scariness' 'NORM' 'Monster Model' 'ISDD',
    'Scariness' 'BANK' 'Monster Model' 'BTNK',
    'Scariness' 'BANK' 'Monster Model' 'KLKL',
    'Scariness' 'BANK' 'Monster Model' 'ISDD',
* Pairs for SCARINESS / EVIL
    'Scariness' 'NORM' 'Evilness' 'EVIL',
    'Scariness' 'NORM' 'Evilness' 'VERY',
    'Scariness' 'BANK' 'Evilness' 'EVIL',
    'Scariness' 'BANK' 'Evilness' 'VERY',
* Pairs for SCARINESS / BRAIN SIZE
    'Scariness' 'NORM' 'Brain Size' 'SMALL',
    'Scariness' 'NORM' 'Brain Size' 'MICRO',
    'Scariness' 'BANK' 'Brain Size' 'SMALL',
    'Scariness' 'BANK' 'Brain Size' 'MICRO'.

* The new pairs can be slotted into the existing 6 test cases
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_test_cases )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in Test Case Table' ).

  ENDMETHOD.


  METHOD given_scariness_added.

    mo_class_under_test->insert_new_column( 4 ).

  ENDMETHOD.


  METHOD when_usage_added.

    mo_class_under_test->insert_new_column( 5 ).

  ENDMETHOD.


  METHOD then_aps_for_usage_ok.
*--------------------------------------------------------------------*
* The new variable USAGE has be added to the equation with
* two possible values - NORM & PLUM i.e. Normal & IT'S THE PLUMBER!
*--------------------------------------------------------------------*
* Pairs for USAGE / MODEL
    pair_is_in_a_test_case :
    'Usage' 'NORM' 'Monster Model' 'BTNK',
    'Usage' 'NORM' 'Monster Model' 'KLKL',
    'Usage' 'NORM' 'Monster Model' 'ISDD',
    'Usage' 'PLUM' 'Monster Model' 'BTNK',
    'Usage' 'PLUM' 'Monster Model' 'KLKL',
    'Usage' 'PLUM' 'Monster Model' 'ISDD',
* Pairs for USAGE / EVIL
    'Usage' 'NORM' 'Evilness' 'EVIL',
    'Usage' 'NORM' 'Evilness' 'VERY',
    'Usage' 'PLUM' 'Evilness' 'EVIL',
    'Usage' 'PLUM' 'Evilness' 'VERY',
* Pairs for USAGE / BRAIN SIZE
    'Usage' 'NORM' 'Brain Size' 'SMALL',
    'Usage' 'NORM' 'Brain Size' 'MICRO',
    'Usage' 'PLUM' 'Brain Size' 'SMALL',
    'Usage' 'PLUM' 'Brain Size' 'MICRO',
* Pairs for USAGE / SCARINESS
    'Usage' 'NORM' 'Scariness' 'NORM',
    'Usage' 'NORM' 'Scariness' 'BANK',
    'Usage' 'PLUM' 'Scariness' 'NORM',
    'Usage' 'PLUM' 'Scariness' 'BANK'.

* The new pairs can be slotted into the existing 6 test cases
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_test_cases )
                                        exp = 6
                                        msg = 'Incorrect Number of Lines in Test Case Table' ).

  ENDMETHOD.


  METHOD given_usage_added.

    mo_class_under_test->insert_new_column( 5 ).

  ENDMETHOD.


  METHOD when_color_added.

    mo_class_under_test->insert_new_column( 6 ).

  ENDMETHOD.


  METHOD then_aps_for_color_ok.
*--------------------------------------------------------------------*
* The new variable COLOR has be added to the equation with
* two possible values - BLUE & GREEN
*--------------------------------------------------------------------*
* Pairs for COLOR / MODEL
    pair_is_in_a_test_case :
    'Color' 'BLUE'  'Monster Model' 'BTNK',
    'Color' 'BLUE'  'Monster Model' 'KLKL',
    'Color' 'BLUE'  'Monster Model' 'ISDD',
    'Color' 'GREEN' 'Monster Model' 'BTNK',
    'Color' 'GREEN' 'Monster Model' 'KLKL',
    'Color' 'GREEN' 'Monster Model' 'ISDD',
* Pairs for COLOR / EVIL
    'Color' 'BLUE'  'Evilness' 'EVIL',
    'Color' 'BLUE'  'Evilness' 'VERY',
    'Color' 'GREEN' 'Evilness' 'EVIL',
    'Color' 'GREEN' 'Evilness' 'VERY',
* Pairs for COLOR / BRAIN SIZE
    'Color' 'BLUE'  'Brain Size' 'SMALL',
    'Color' 'BLUE'  'Brain Size' 'MICRO',
    'Color' 'GREEN' 'Brain Size' 'SMALL',
    'Color' 'GREEN' 'Brain Size' 'MICRO',
* Pairs for COLOR / SCARINESS
    'Color' 'BLUE'  'Scariness' 'NORM',
    'Color' 'BLUE'  'Scariness' 'BANK',
    'Color' 'GREEN' 'Scariness' 'NORM',
    'Color' 'GREEN' 'Scariness' 'BANK',
* Pairs for COLOR / USAGE
    'Color' 'BLUE'  'Usage' 'NORM',
    'Color' 'BLUE'  'Usage' 'PLUM',
    'Color' 'GREEN' 'Usage' 'NORM',
    'Color' 'GREEN' 'Usage' 'PLUM'.

* All good things come to an end. We have reached te stage where we have so
* many variables we need two new test cases. If all the new variables had more
* than two values we would have got here sooner
    cl_abap_unit_assert=>assert_equals( act = lines(  mo_class_under_test->mt_test_cases )
                                        exp = 8
                                        msg = 'Incorrect Number of Lines in Test Case Table' ).

  ENDMETHOD.

ENDCLASS."LCL_TEST_CLASS
