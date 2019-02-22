*"* use this source file for your ABAP unit test classes
CLASS ltc_monster_transactions DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.
    CLASS-DATA: mo_environment TYPE REF TO if_osql_test_environment.

    DATA: mo_cut       TYPE REF TO zcl_monster_transactions_perl,
          mt_scariness TYPE zcl_monster_transactions_perl=>mtt_scariness.

    CLASS-METHODS: class_setup,
      class_teardown.

    METHODS:
      setup,
*--------------------------------------------------------------------*
* Specifications : IT is the Monster Transactions Persistency Layer
*    "IT SHOULD.....................
*--------------------------------------------------------------------*
      derive_monster_scariness FOR TESTING,
      given_fake_database_values,
      when_sql_statement_executes,
      then_result_is_correct.

ENDCLASS.

CLASS ltc_monster_transactions IMPLEMENTATION.

  METHOD derive_monster_scariness.

    given_fake_database_values(  ).

    when_sql_statement_executes(  ).

    then_result_is_correct(  ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 07.20 : Setting Up and Tearing Down OSQL Tests
*--------------------------------------------------------------------*
  METHOD class_setup.
    mo_environment = cl_osql_test_environment=>create(
    i_dependency_list = VALUE #( ( 'ztmonster_header' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
    CREATE OBJECT mo_cut.
  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 07.21 : Unit Test for SQL Statement
*--------------------------------------------------------------------*
  METHOD given_fake_database_values.

    DATA: lt_fake_monsters TYPE STANDARD TABLE OF ztmonster_header.

    lt_fake_monsters = VALUE #( ( monster_number    = '0000000001'
                                  name              = 'HUBERT'
                                  sanity_percentage = 1
                                  strength          = 99 ) ).
    mo_environment->insert_test_data( lt_fake_monsters ).

  ENDMETHOD.


  METHOD when_sql_statement_executes.
    mt_scariness[] = mo_cut->derive_monster_scariness( id_monster_number = '0000000001' ).
  ENDMETHOD.


  METHOD then_result_is_correct.

    DATA: lt_expected_result TYPE zcl_monster_transactions_perl=>mtt_scariness.

    lt_expected_result = VALUE #( ( monster_number   = '0000000001'
                                    name             = 'HUBERT'
                                    scariness_string = 'REALLY SCARY' ) ).

    cl_abap_unit_assert=>assert_equals(
    exp = lt_expected_result[]
    act = mt_scariness
    msg = 'SQL Query Returned Incorrect Results' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_monster_cds_views DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.
    CLASS-DATA: mo_environment TYPE REF TO if_cds_test_environment.

    DATA: mo_cut            TYPE REF TO zcl_monster_transactions_perl,
          mt_green_monsters TYPE STANDARD TABLE OF zv_monsters.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
*--------------------------------------------------------------------*
* Specifications : IT is the Monster Transactions Persistent Layer
*    "IT SHOULD.....................
*--------------------------------------------------------------------*
      derive_green_monsters FOR TESTING,
      given_fake_database_values,
      when_cds_view_is_read,
      then_result_is_correct.

ENDCLASS.

CLASS ltc_monster_cds_views IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 07.37 : Setting Up and Tearing Down CDS Unit Test
*--------------------------------------------------------------------*
  METHOD class_setup.
    mo_environment = cl_cds_test_environment=>create(
    i_for_entity = 'ZCDS_MONSTERS_JOIN' ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD derive_green_monsters.

    given_fake_database_values( ).

    when_cds_view_is_read( ).

    then_result_is_correct( ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 07.38 : Unit Test for CDS View
*--------------------------------------------------------------------*
  METHOD given_fake_database_values.

    DATA: lt_fake_header TYPE STANDARD TABLE OF ztmonster_header,
          lt_fake_items  TYPE STANDARD TABLE OF ztmonster_items.

     lt_fake_header = VALUE #( ( monster_number    = '0000000001'
                                 name              = 'HUBERT'
                                 sanity_percentage = 1
                                 strength          = 100
                                 evilness          = 'BANK'
                                 color             = 'GREN' ) ).

     lt_fake_items = VALUE #(  ( monster_number = '0000000001'
                                 monster_item   = '000010'
                                 part_category  = 'HD "Head'
                                 part_quantity  = 6 ) ).

    mo_environment->insert_test_data( lt_fake_header ).
    mo_environment->insert_test_data( lt_fake_items  ).

  ENDMETHOD.

  METHOD when_cds_view_is_read.
    mt_green_monsters = mo_cut->derive_green_monsters( ).
* The code in the CUT does the following...
*    SELECT * FROM zcds_monsters_join
*    INTO CORRESPONDING FIELDS OF TABLE @mt_green_monsters.
  ENDMETHOD.

  METHOD then_result_is_correct.

    DATA: lt_expected_result TYPE STANDARD TABLE OF zv_monsters.

    lt_expected_result = VALUE #( ( monster_number        = '0000000001'
                                    monster_name          = 'HUBERT'
                                    first_initial         = 'H'
                                    scariness_description = 'REALLY SCARY' ) ).

    cl_abap_unit_assert=>assert_equals(
    exp = lt_expected_result[]
    act = mt_green_monsters
    msg = 'CDS View Query Returned Incorrect Results' ).
  ENDMETHOD.

ENDCLASS.
