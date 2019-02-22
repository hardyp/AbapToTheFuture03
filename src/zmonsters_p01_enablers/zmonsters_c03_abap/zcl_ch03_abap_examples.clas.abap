class ZCL_CH03_ABAP_EXAMPLES definition
  public
  create public .

public section.
protected section.
private section.

  methods L03_01_QUERY_WITHOUT_VALUE .
  methods L03_02_QUERY_WITH_VALUE .
  methods L03_03_VALUE_TO_FILL_ITAB .
  methods L03_04_FILLING_ITAB_HARDCODE .
  methods L03_05_FILLING_ITAB_VIA_FOR .
  methods L03_06_SHORT_LIVED_VARIABLES .
  methods L03_07_BASIC_ENUMERATION .
  methods L03_08_NON_INTEGER_ENUMERATION .
  methods L03_09_STRUCTURED_ENUMERATION .
  methods L03_10_ALPHA_INPUT_OUTPUT .
  methods L03_11_ALPHA_FOMATTING_OPTION .
  methods L03_12_METHOD_GUESS_DATA_TYPE .
  methods L03_13_METHOD_INLINE_DECS .
  methods L03_14_METHOD_CALL_W_HELPER .
  methods L03_15_METHOD_CALL_W_CONV .
  methods L03_16_TYPE_REF_TO_OLD
    importing
      !ID_VALUE type ANY .
  methods L03_17_ABAP_TRUE .
  methods L03_18_NO_ABAP_TRUE .
  methods L03_19_IF_WRONG_CHECK .
  methods L03_20_BOOLC
    returning
      value(RT_RESULT) type ZTT_BC_BDCMSGCOLL .
  methods L03_21_NEGATIVE_TESTING .
  methods L03_22_XSDBOOL .
  methods L03_23_CASE_STATEMENT
    importing
      !ID_EDIT_MODE type LRM_CRUD_MODE .
  methods L03_24_SWTCH_STATEMENT
    importing
      !ID_EDIT_MODE type LRM_CRUD_MODE .
  methods L03_25_CASE_STATEMENT
    changing
      !CS_MONSTER_HEADER type ZSTR_MONSTER_HEADER .
  methods L03_26_COND_STATEMENT
    changing
      !CS_MONSTER_HEADER type ZSTR_MONSTER_HEADER .
  methods L03_27_COND_STATEMENT_UPDATED
    changing
      !CS_MONSTER_HEADER type ZSTR_MONSTER_HEADER .
  methods L03_28_INTERNAL_TABLES_WA .
  methods L03_29_INTERNAL_TABLES_FS .
  methods L03_30_READING_TABLE_LINE_OLD .
  methods L03_31_READING_TABLE_LINE_NEW .
  methods L03_32_READING_TABLE_OPTIONAL .
  methods L03_33_COPYING_ITABS_OLD .
  methods L03_34_COPYING_ITABS_NEW .
  methods L03_35_DEEP_STRUCTURE .
  methods L03_36_CORRESPONDING_OLD .
  methods L03_37_CORRESPONDING_OLD_V2 .
  methods L03_38_CORRESPONDING_NEW .
  methods L03_39_CORRESPONDING_DYNAMIC
    returning
      value(RS_BEST_BRAINS) type ZST_MONSTER_BEST_BRAINS .
  methods L03_40_GETTING_ROW_NO_OLD .
  methods L03_41_GETTING_ROW_NO_NEW .
  methods L03_42_GETTING_ROW_NO_NEWER .
  methods L03_43_ITAB_ROW_EXISTS_OLD .
  methods L03_44_ITAB_ROW_EXISTS_NEW .
  methods L03_45_ITAB_ROW_EXISTS_NEWER .
  methods L03_46_REDUCE .
  methods L03_47_GROUP_BY .
  methods L03_48_TABLE_EXTRACTION_OLD .
  methods L03_49_TABLE_EXTRACTION_NEW .
  methods L03_50_FAE_DATABASE .
  methods L03_51_FAE_ITAB .
  methods L03_52_VIRTUAL_SORTING .
  methods L03_53_VIRTUAL_SORTING_PART_2 .
  methods L03_54_QUERY_DDIC_NO_CAST .
  methods L03_55_QUERY_DDIC_WITH_CAST .
  methods L03_56_FIND_A_SUBCLASS_OLD
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_57_FIND_A_SUBCLASS_NEW
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_58_FIND_A_SUBCLASS_NEW_V2
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_59_FUNCTIONAL_METHOD_PLUS .
ENDCLASS.



CLASS ZCL_CH03_ABAP_EXAMPLES IMPLEMENTATION.


  METHOD L03_01_QUERY_WITHOUT_VALUE.

    DATA:
      monster_type_range            TYPE ztt_bc_coseltab,
      monster_type_selection_option LIKE LINE OF  monster_type_range.

    monster_type_selection_option-field  = 'EVILNESS'.
    monster_type_selection_option-option = 'EQ'.
    monster_type_selection_option-sign   = 'I'.
    monster_type_selection_option-low    = 'EVIL'."Evil Monster
    APPEND monster_type_selection_option TO monster_type_range.

    monster_type_selection_option-field  = 'EVILNESS'.
    monster_type_selection_option-option = 'EQ'.
    monster_type_selection_option-sign   = 'I'.
    monster_type_selection_option-low    = 'VERY'."Very Evil Monster
    APPEND monster_type_selection_option TO monster_type_range.

    DATA(monster) = NEW zcl_monster_model( ).
    monster->retrieve_headers_by_attribute(
      EXPORTING it_selections      = monster_type_range
      IMPORTING et_monster_headers = DATA(monster_headers) ).

  ENDMETHOD.


  METHOD L03_02_QUERY_WITH_VALUE.

    NEW zcl_monster_model( )->retrieve_headers_by_attribute(
      EXPORTING it_selections =
      VALUE ztt_bc_coseltab(
            ( field  = 'EVILNESS' )
            ( option = 'EQ' )
            ( sign   = 'I' )
            ( low    = 'EVIL' )    "Evil Monster
            ( low    = 'VERY' ) )  "Very Evil Monster
      IMPORTING et_monster_headers = DATA(monster_headers) ).

  ENDMETHOD.


  METHOD l03_03_value_to_fill_itab.

    TYPES: BEGIN OF l_typ_configuration,
             variable_name  TYPE string,
             count          TYPE i,
             possible_value TYPE string,
           END OF   l_typ_configuration.

    TYPES: l_tt_configuration TYPE STANDARD TABLE OF l_typ_configuration
                              WITH KEY variable_name count.

    DATA(lt_configuration) = VALUE l_tt_configuration(
    ( variable_name = 'Monster Model' count = 1 possible_value = 'BTNK' ) "Bolts Through Neck
    ( variable_name = 'Monster Model' count = 2 possible_value = 'KLKL')  "Killer Klown
    ( variable_name = 'Monster Model' count = 3 possible_value = 'ISDD' ) "Ice Skating Dead
    ( variable_name = 'Evilness'      count = 1 possible_value = 'EVIL' )
    ( variable_name = 'Evilness'      count = 2 possible_value = 'VERY' )
    ( variable_name = 'Brain Size'    count = 1 possible_value = 'SMALL' )
    ( variable_name = 'Brain Size'    count = 2 possible_value = 'MICRO' )
    ( variable_name = 'Scariness'     count = 1 possible_value = 'NORM' )
    ( variable_name = 'Scariness'     count = 2 possible_value = 'BANK' )
    ( variable_name = 'Usage'         count = 1 possible_value = 'NORM' )
    ( variable_name = 'Usage'         count = 2 possible_value = 'PLUM' )
    ( variable_name = 'Color'         count = 1 possible_value = 'BLUE' )
    ( variable_name = 'Color'         count = 2 possible_value = 'GREEN' ) ).

  ENDMETHOD.


  METHOD l03_04_filling_itab_hardcode.

    DATA(table_of_monsters) = VALUE ztt_monster_header(
   ( name = 'FRED'   monster_number = 1 )
   ( name = 'HUBERT' monster_number = 2 ) ).

  ENDMETHOD.


  METHOD L03_05_FILLING_ITAB_VIA_FOR.

    SELECT *
    FROM ztmonster_header
    INTO TABLE @DATA(all_monsters).

    DATA(neurotic_monsters) = VALUE ztt_monster_header(
     FOR monster_details IN all_monsters WHERE ( sanity_percentage < 20 )
         ( name           = monster_details-name
           monster_number = monster_details-monster_number ) ).

  ENDMETHOD.


  METHOD l03_06_short_lived_variables.

    SELECT *
          FROM ztmonster_header
          INTO TABLE @DATA(all_monsters).

    DATA(iterator) = NEW lcl_weapon_iterator( ).

    DO lines( all_monsters[] ) TIMES.
      DATA(arming_description) = CONV string(
        LET weapon_name  = iterator->get_next_weapon( )
            monster_name = all_monsters[ sy-index ]-name
            date_string  =
           |{ sy-datum+6(2) } / { sy-datum+4(2) } / { sy-datum(4) }|
        IN |Monster { monster_name } was issued a { weapon_name } on { date_string }| ).
      MESSAGE arming_description TYPE 'I'.
    ENDDO.

  ENDMETHOD.


  METHOD l03_07_basic_enumeration.

    TYPES: BEGIN OF ENUM monster_brain_size,
             normal,
             small,
             micro,
           END OF ENUM monster_brain_size.

    DATA: brain_size TYPE monster_brain_size.

  ENDMETHOD.


  METHOD l03_08_non_integer_enumeration.

    TYPES:
      brain_size TYPE c LENGTH 8,
      BEGIN OF ENUM monster_brain_size BASE TYPE brain_size,
        normal VALUE IS INITIAL,
        small  VALUE 'SMALL',
        micro  VALUE 'MICRO',
      END OF ENUM monster_brain_size.

    DATA(brain_size) = NEW monster_brain_size( small ).

  ENDMETHOD.


  METHOD l03_09_structured_enumeration.

    TYPES: BEGIN OF ENUM monster_brain_size
           STRUCTURE brain_sizes,
             normal,
             small,
             micro,
           END OF ENUM monster_brain_size
           STRUCTURE brain_sizes.

    DATA(brain_size) = brain_sizes-small.

  ENDMETHOD.


  METHOD l03_10_alpha_input_output.
* Local Variables
    DATA: monster_number TYPE zde_monster_number VALUE '0000000001',
          monster_header TYPE ztmonster_header.

    "Remove leading zeroes before output to user
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        in  = monster_number
      IMPORTING
        out = monster_number.

    DATA(message) = |Problem with monster number { monster_number }|.
    MESSAGE message TYPE 'I'.

    "Now add the leading zeroes back before database read
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        in  = monster_number
      IMPORTING
        out = monster_number.

    SELECT SINGLE *
      FROM ztmonster_header
      INTO CORRESPONDING FIELDS OF monster_header
      WHERE monster_number = monster_number.

  ENDMETHOD.


  METHOD l03_11_alpha_fomatting_option.
* Local Variables
    DATA: monster_number TYPE zde_monster_number VALUE '0000000001',
          monster_header TYPE ztmonster_header.

    DATA(message) = | Problem with Monster Number { monster_number ALPHA = OUT }|.
    MESSAGE message TYPE 'I'.

    SELECT SINGLE *
      FROM ztmonster_header
      INTO CORRESPONDING FIELDS OF monster_header
      WHERE monster_number = monster_number.

  ENDMETHOD.


  METHOD l03_12_method_guess_data_type.

    DATA: number_of_heads TYPE i,
          number_of_hats  TYPE i,
          monster_number  TYPE zde_monster_number VALUE '0000000001'.

    DATA(monster) = NEW zcl_monster_model( ).

    monster->get_ahead_get_a_hat(
    EXPORTING id_monster_number  = monster_number
    IMPORTING ed_number_of_heads = number_of_heads
              ed_number_of_hats  = number_of_hats ).

  ENDMETHOD.


  METHOD l03_13_method_inline_decs.

    DATA: monster_number  TYPE zde_monster_number VALUE '0000000001'.

    DATA(monster) = NEW zcl_monster_model( ).

    monster->get_ahead_get_a_hat(
    EXPORTING id_monster_number  = monster_number
    IMPORTING ed_number_of_heads = DATA(number_of_heads)
              ed_number_of_hats  = DATA(number_of_hats) ).

  ENDMETHOD.


  METHOD L03_14_METHOD_CALL_W_HELPER.
*--------------------------------------------------------------------*
* As an example the incoming Monster Name came from an external system
* in the form of a string, but our method wants a CHAR30
*--------------------------------------------------------------------*
    DATA: helper                TYPE char30,
          incoming_monster_name TYPE string VALUE 'HUBERT'.

    helper = incoming_monster_name.

    DATA(monster) = NEW zcl_monster_model( ).

    monster->invite_to_party(
      id_monster_name = helper
      id_party_name   = 'WILD PARTY' ).

  ENDMETHOD.


  METHOD L03_15_METHOD_CALL_W_CONV.
*--------------------------------------------------------------------*
* As an example the incoming Monster Name came from an external system
* in the form of a string, but our method wants a CHAR30
*--------------------------------------------------------------------*
    DATA: incoming_monster_name TYPE string VALUE 'HUBERT'.

    DATA(monster) = NEW zcl_monster_model( ).

    monster->invite_to_party(
      id_monster_name = CONV #( incoming_monster_name )
      id_party_name   = 'BIRTHDAY PARTY' ).

  ENDMETHOD.


  METHOD l03_16_type_ref_to_old.
*--------------------------------------------------------------------*
* ID_VALUE is an IMPORTING parameter TYPE ANY
*--------------------------------------------------------------------*
    DATA: generic_data_object TYPE REF TO data.

    DATA(monster) = NEW zcl_monster_model( ).

* Before

    CREATE DATA generic_data_object LIKE id_value.
    GET REFERENCE OF id_value INTO generic_data_object.
    monster->log( generic_data_object ).

* After

    monster->log( REF #( id_value ) ).

  ENDMETHOD.


  METHOD l03_17_abap_true.

    DATA(monster) = zcl_monster_model=>get_instance( '0000000001' ).

    IF monster->is_scary( ) = abap_true.
      MESSAGE 'Oh No! Send for the Fire Brigade!' TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD l03_18_no_abap_true.

    DATA(monster) =
    zcl_monster_model=>get_instance( '0000000001' ).

    IF monster->is_scary( ).
      MESSAGE 'Oh No! Send for the Fire Brigade!' TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD l03_19_if_wrong_check.

    DATA(monster) = NEW zcl_monster_model( ).

    IF monster->wants_to_blow_up_world( ).
      DATA(massive_atom_bomb) = NEW lcl_atom_bomb( ).
      massive_atom_bomb->explode( ).
    ENDIF.

  ENDMETHOD.


  METHOD l03_20_boolc.
* Do some groovy things
* Get some results

* Postconditions
    zcl_dbc=>ensure( that = 'A result table is returned'
    which_is_true_if = boolc( rt_result[] IS NOT INITIAL ) ).

  ENDMETHOD.


  METHOD l03_21_negative_testing.

    DATA: empty_table TYPE STANDARD TABLE OF ztmonster_header.

    IF boolc( empty_table[] IS NOT INITIAL ) = abap_false.
      WRITE:/ 'This table is empty'.
    ELSE.
      WRITE:/ 'This table is as full as full can be'.
    ENDIF.
    IF boolc( 1 = 2 ) = abap_false.
      WRITE:/ '1 does not equal 2'.
    ELSE.
      WRITE:/ '1 equals 2, and the world is made of snow'.
    ENDIF.

  ENDMETHOD.


  METHOD l03_22_xsdbool.

    DATA: empty_table TYPE STANDARD TABLE OF ztmonster_header.

* Then do the same using XSDBOOL.
    IF xsdbool( empty_table[] IS NOT INITIAL ) = abap_false.
      WRITE:/ 'This table is empty'.
    ELSE.
      WRITE:/ 'This table is as full as full can be'.
    ENDIF.
    IF xsdbool( 1 = 2 ) = abap_false.
      WRITE:/ '1 does not equal 2'.
    ELSE.
      WRITE:/ '1 equals 2, and the world is made of snow'.
    ENDIF.

  ENDMETHOD.


  method L03_23_CASE_STATEMENT.

* Use adapter pattern to translate human readable CRUD standard to
* the BOPF equivalent
    DATA: bopf_edit_mode TYPE /bobf/conf_edit_mode.

    CASE id_edit_mode.
      WHEN 'R'."Read
        bopf_edit_mode = /bobf/if_conf_c=>sc_edit_read_only.
      WHEN 'U'."Update
        bopf_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive.
      WHEN OTHERS.
        "Unexpected Situation
        RAISE EXCEPTION TYPE zcx_monster_exceptions.
    ENDCASE.

*    DATA(bopf_edit_mode) = SWITCH /bobf/conf_edit_mode( id_edit_mode
*      WHEN 'R' THEN /bobf/if_conf_c=>sc_edit_read_only  "Read
*      WHEN 'U' THEN /bobf/if_conf_c=>sc_edit_exclusive  "Update
*      ELSE THROW zcx_monster_exceptions( ) ).           "Unexpected Situation
  endmethod.


  METHOD l03_24_swtch_statement.
* Use adapter pattern to translate human readable CRUD
* standard values to the BOPF equivalent
    DATA(bopf_edit_mode) = SWITCH /bobf/conf_edit_mode( id_edit_mode
      WHEN 'R' THEN /bobf/if_conf_c=>sc_edit_read_only  "Read
      WHEN 'U' THEN /bobf/if_conf_c=>sc_edit_exclusive  "Update
      ELSE THROW zcx_monster_exceptions( ) ).           "Unexpected Situation

  ENDMETHOD.


  METHOD l03_25_case_statement.
* Fill the Sanity Description
    CASE cs_monster_header-sanity_percentage.
      WHEN 5.
        cs_monster_header-sanity_description = 'VERY SANE'.
      WHEN 4.
        cs_monster_header-sanity_description = 'SANE'.
      WHEN 3.
        cs_monster_header-sanity_description = 'SLIGHTLY MAD'.
      WHEN 2.
        cs_monster_header-sanity_description = 'VERY MAD'.
      WHEN 1.
        cs_monster_header-sanity_description = 'BONKERS'.
      WHEN OTHERS.
        cs_monster_header-sanity_description = 'RENAMES SAP PRODUCTS'.
    ENDCASE.

  ENDMETHOD.


  METHOD l03_26_cond_statement.
* Fill the Sanity Description
    cs_monster_header-sanity_description =
    COND text30(
    WHEN cs_monster_header-sanity_percentage = 5 THEN 'VERY SANE'
    WHEN cs_monster_header-sanity_percentage = 4 THEN 'SANE'
    WHEN cs_monster_header-sanity_percentage = 3 THEN 'SLIGHTLY MAD'
    WHEN cs_monster_header-sanity_percentage = 2 THEN 'VERY MAD'
    WHEN cs_monster_header-sanity_percentage = 1 THEN 'BONKERS'
    ELSE 'RENAMES SAP PRODUCTS' ).

  ENDMETHOD.


  METHOD l03_27_cond_statement_updated.

    DATA: day TYPE char10 VALUE 'Tuesday'."Lenny Henry!
* Fill the Sanity Description
    cs_monster_header-sanity_description =
    COND text30(
    WHEN cs_monster_header-sanity_percentage = 5 THEN 'VERY SANE'
    WHEN cs_monster_header-sanity_percentage = 4 THEN 'SANE'
    WHEN cs_monster_header-sanity_percentage = 3 THEN 'SLIGHTLY MAD'
    WHEN cs_monster_header-sanity_percentage = 2 THEN 'VERY MAD'
    WHEN cs_monster_header-sanity_percentage = 1 AND
         day = 'Tuesday'                         THEN 'HAVING AN OFF DAY'
    WHEN cs_monster_header-sanity_percentage = 1 THEN 'BONKERS'
    ELSE 'RENAMES SAP PRODUCTS' ).

  ENDMETHOD.


  METHOD L03_28_INTERNAL_TABLES_WA.

    DATA: table_of_monsters TYPE STANDARD TABLE OF ztmonster_header.

    READ TABLE table_of_monsters
    WITH KEY color = 'RED'
    INTO DATA(red_monster_details).

    LOOP AT table_of_monsters INTO DATA(loopy_monster_details).
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_29_internal_tables_fs.

    DATA: table_of_monsters TYPE STANDARD TABLE OF ztmonster_header.

    READ TABLE table_of_monsters
    WITH KEY color = 'RED'
    ASSIGNING FIELD-SYMBOL(<red_monster_details>).

    LOOP AT table_of_monsters ASSIGNING FIELD-SYMBOL(<loopy_monster_details>).
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_30_reading_table_line_old.
* Local Variables
    DATA: monster_name      TYPE zde_monster_name VALUE 'HUBERT',
          table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
          monster_details   LIKE LINE OF table_of_monsters,
          monster           TYPE REF TO zcl_monster_model.

    READ TABLE table_of_monsters INTO monster_details
    WITH KEY name = monster_name.

    monster = zcl_monster_model=>get_instance( monster_details-monster_number ).

  ENDMETHOD.


  METHOD l03_31_reading_table_line_new.
* Local Variables
    DATA: monster_name      TYPE zde_monster_name VALUE 'HUBERT',
          table_of_monsters TYPE STANDARD TABLE OF ztmonster_header.

    DATA(monster) = zcl_monster_model=>get_instance( table_of_monsters[ monster_name ]-monster_number  ).

  ENDMETHOD.


  METHOD l03_32_reading_table_optional.
* Local Variables
    DATA: monster_name      TYPE zde_monster_name VALUE 'HUBERT',
          table_of_monsters TYPE STANDARD TABLE OF ztmonster_header.

    DATA(message1) = |{ monster_name }'s Monster Number is { VALUE #( table_of_monsters[ monster_name ]-monster_number OPTIONAL ) }|.

    DATA(message2) = |{ monster_name }’s Monster Number is { VALUE #( table_of_monsters[ monster_name ]-monster_number DEFAULT '9999999999' ) }|.

  ENDMETHOD.


  METHOD l03_33_copying_itabs_old.
* Local Variables
    DATA: green_monsters TYPE STANDARD TABLE OF ztmonster_header,
          blue_monsters  TYPE STANDARD TABLE OF ztmonster_header.

    FIELD-SYMBOLS:
      <green_monsters> LIKE LINE OF green_monsters,
      <blue_monsters>  LIKE LINE OF blue_monsters.

    LOOP AT green_monsters ASSIGNING <green_monsters>.
      APPEND INITIAL LINE TO blue_monsters
      ASSIGNING <blue_monsters>.
      MOVE-CORRESPONDING <green_monsters> TO <blue_monsters>.
      CLEAR <blue_monsters>-evilness.
      <blue_monsters>-early_age_strength =
      <green_monsters>-strength.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_34_copying_itabs_new.
* Local Variables
    DATA: green_monsters TYPE STANDARD TABLE OF ztmonster_header,
          blue_monsters  TYPE STANDARD TABLE OF ztmonster_header.

    green_monsters = CORRESPONDING #(
    blue_monsters
    MAPPING early_age_strength = strength
    EXCEPT evilness ).

  ENDMETHOD.


  METHOD l03_35_deep_structure.

    TYPES: BEGIN OF l_typ_monsters,
             monster_number TYPE zde_monster_number,
             monster_name   TYPE zde_monster_name,
             t_items        TYPE ztt_monster_items,
           END OF l_typ_monsters.
    DATA: monster_table TYPE STANDARD TABLE OF l_typ_monsters.

  ENDMETHOD.


  METHOD l03_36_corresponding_old.

    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.

*--------------------------------------------------------------------*
* Important Bit - This DOES NOT work
*--------------------------------------------------------------------*
    MOVE-CORRESPONDING european_result TO us_result.

  ENDMETHOD.


  METHOD l03_37_corresponding_old_v2.

    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.
    INSERT european_result  INTO TABLE european_results.

*--------------------------------------------------------------------*
* Important Bit - This Works - Takes 4 Lines
*--------------------------------------------------------------------*
    LOOP AT european_results ASSIGNING FIELD-SYMBOL(<european_result>).
      APPEND INITIAL LINE TO us_results ASSIGNING FIELD-SYMBOL(<us_result>).
      MOVE-CORRESPONDING <european_result> TO <us_result>.
    ENDLOOP.

  ENDMETHOD.


  METHOD L03_38_CORRESPONDING_NEW.
* Local Variables
    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.
    INSERT european_result  INTO TABLE european_results.

*--------------------------------------------------------------------*
* Important Bit - This Works - Takes Only 1 Line
*--------------------------------------------------------------------*
    MOVE-CORRESPONDING european_results TO us_results.

  ENDMETHOD.


  METHOD l03_39_corresponding_dynamic.
*--------------------------------------------------------------------*
* This example demonstrates building up the mapping rules dynamically
* at runtime to aid in the make (monster) to order business process
*--------------------------------------------------------------------*
* Incoming Parameters are
* - a record describing the customer requirements (ZMN_ORDER_ITEMS)
*   this comes from sales (taking customer orders)
* - a record describing possible brains available
*   this comes from procurement (digging up graves)
* - the returning parameter is a ranked list of preferred brains
*   this is sent to production (Baron Frankenstein)
*--------------------------------------------------------------------*
* Local Variables
    DATA: mapping_record TYPE cl_abap_corresponding=>mapping_info,
          mapping_table  TYPE cl_abap_corresponding=>mapping_table.

* Simulate incoming customer requirements
    DATA(is_customer_requirements) = VALUE zmn_order_items(
      usage_desired      = 'MORI'    "Morris Dancer
      brain_size_desired = 'MICRO' )."Microscopic

* Simulate incoming possible brains
    DATA(is_possible_brains) = VALUE zst_monster_possible_brains(
      biggest_brain  = 'EINSTIEN'
      smallest_brain = 'RENAMER'
      weirdest_brain = 'YANKOVICH'
      evilest_brain  = 'BANKER' ).

    mapping_record-level = 0.
    mapping_record-kind  = cl_abap_corresponding=>mapping_component.

    mapping_record-dstname = 'BEST_BRAIN_01'.

    IF is_customer_requirements-brain_size_desired = 'NORM'."Normal Brain
      mapping_record-srcname = 'BIGGEST_BRAIN'.
    ELSE.
      mapping_record-srcname = 'SMALLEST_BRAIN'.
    ENDIF.

    APPEND mapping_record TO mapping_table.

    mapping_record-dstname = 'BEST_BRAIN_02'.

    IF is_customer_requirements-usage_desired = 'MORT'."Mortgage Salesman
      mapping_record-srcname = 'EVILEST_BRAIN'.
    ELSEIF is_customer_requirements-usage_desired = 'MORI'."Morris Dancer
      mapping_record-srcname = 'WEIRDEST_BRAIN'.
    ENDIF.

    APPEND mapping_record TO mapping_table.

    TRY.
        DATA(dynamic_mapper) =
        cl_abap_corresponding=>create(
            source            = is_possible_brains
            destination       = rs_best_brains
            mapping           = mapping_table ).

        dynamic_mapper->execute(
          EXPORTING source      = is_possible_brains
          CHANGING  destination = rs_best_brains    ).

      CATCH cx_corr_dyn_error.
        "Fatal Error
    ENDTRY.

* If the RETURNING parameter is a structure, you can say you only want
* one field of thet structure, as follows:-
    DATA(best_brain) = l03_39_corresponding_dynamic( )-best_brain_01.

  ENDMETHOD.


  METHOD l03_40_getting_row_no_old.
*--------------------------------------------------------------------*
* 7.02 Syntax
*--------------------------------------------------------------------*
    DATA:
      start_row         TYPE sy-tabix,
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    READ TABLE table_of_monsters
    WITH KEY monster_number = monster_number
    TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      start_row = sy-tabix.
    ENDIF.

  ENDMETHOD.


  METHOD L03_41_GETTING_ROW_NO_NEW.
*--------------------------------------------------------------------*
* Post 7.40 Syntax
*--------------------------------------------------------------------*
* Local Variables
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    DATA(start_row) = line_index( table_of_monsters[ monster_number = monster_number ] ).

    LOOP AT table_of_monsters FROM start_row ASSIGNING FIELD-SYMBOL(<monster_details>).
      "Do Something
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_42_getting_row_no_newer.
*--------------------------------------------------------------------*
* Post 7.40 Syntax - V2
*--------------------------------------------------------------------*
* Local Variables
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    LOOP AT table_of_monsters FROM line_index( table_of_monsters[ monster_number = monster_number ] )
      ASSIGNING FIELD-SYMBOL(<monster_details>).
      "Do Something
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_43_itab_row_exists_old.
*--------------------------------------------------------------------*
* Pre 7.4 Syntax
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    FIELD-SYMBOLS: <monster_details> LIKE LINE OF table_of_monsters.

    READ TABLE table_of_monsters ASSIGNING <monster_details>
    WITH KEY monster_number = monster_number.

    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO table_of_monsters
      ASSIGNING <monster_details>.
    ENDIF.
    ADD 1 TO <monster_details>-sanity_percentage.

  ENDMETHOD.


  METHOD l03_44_itab_row_exists_new.
*--------------------------------------------------------------------*
* Post 7.4 Syntax
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    IF line_exists( table_of_monsters[ monster_number = monster_number ] ).
      READ TABLE table_of_monsters ASSIGNING FIELD-SYMBOL(<monster_details>)
      WITH KEY monster_number = monster_number.
    ELSE.
      APPEND INITIAL LINE TO table_of_monsters
      ASSIGNING <monster_details>.
    ENDIF.

    ADD 1 TO <monster_details>-sanity_percentage.

  ENDMETHOD.


  METHOD L03_45_ITAB_ROW_EXISTS_NEWER.
*--------------------------------------------------------------------*
* Post 7.4 Syntax V2
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF ztmonster_header,
      monster_number    TYPE zde_monster_number VALUE '0000000001'.

    IF line_exists( table_of_monsters[ monster_number = monster_number ] ).
      "Do Something
    ENDIF.

  ENDMETHOD.


  METHOD l03_46_reduce.
* Local Variables
    DATA: neurotic_monsters TYPE STANDARD TABLE OF ztmonster_header.
*--------------------------------------------------------------------*
* Fill up NEUROTIC_MONSTERS table
*--------------------------------------------------------------------*
    DATA(mad_monsters_count) = REDUCE sy-tabix(
    INIT result = 0
    FOR  monster_details IN neurotic_monsters
    NEXT result = result +
    zcl_bc_utilities=>add_1_if_true(
    zcl_monster_model=>is_mad( monster_details-monster_number ) ) ).

  ENDMETHOD.


  METHOD l03_47_group_by.
*--------------------------------------------------------------------*
* In this example we are trying to prove that bonkers monsters with
* bolts througth their neck have more heads per monster on average
* compared to bonkers monsters who like to ice skate.
* This is most likely a 100% correlation with the business problem
* you are trying to solve at work right at this moment.
*--------------------------------------------------------------------*
    TYPES: tt_monsters TYPE STANDARD TABLE OF ztmonster_header
                       WITH DEFAULT KEY.

    DATA: monster_sub_set TYPE tt_monsters,
          total_heads     TYPE i.

    DATA(table_of_monsters) = VALUE tt_monsters(
    ( monster_number = '1' model = 'BTNK' no_of_heads = 3 )
    ( monster_number = '2' model = 'BTNK' no_of_heads = 4 )
    ( monster_number = '3' model = 'BTNK' no_of_heads = 2 )
    ( monster_number = '4' model = 'ISDD' no_of_heads = 1 )
    ( monster_number = '5' model = 'ISDD' no_of_heads = 1 )
     ).

    LOOP AT table_of_monsters INTO DATA(monster_details)
    GROUP BY ( model          = monster_details-model
               is_it_crackers = zcl_monster_model=>is_mad( monster_details-monster_number ) )
    ASSIGNING FIELD-SYMBOL(<monster_group_record>).

      CHECK <monster_group_record>-is_it_crackers = abap_true.

      CLEAR monster_sub_set.

      LOOP AT GROUP <monster_group_record> ASSIGNING FIELD-SYMBOL(<bonkers_monsters>).
        monster_sub_set = VALUE #( BASE monster_sub_set ( <bonkers_monsters> ) ).
      ENDLOOP.

      CLEAR total_heads.

      LOOP AT monster_sub_set INTO DATA(sub_set_record).
        ADD sub_set_record-no_of_heads TO total_heads.
      ENDLOOP.

      WRITE:/ 'Bonkers Monsters of Type',<monster_group_record>-model,' have ',total_heads,' heads'.

    ENDLOOP.

  ENDMETHOD.


  METHOD l03_48_table_extraction_old.

    DATA: all_monsters
          TYPE SORTED TABLE OF ztmonster_header
          WITH NON-UNIQUE KEY monster_number
          WITH NON-UNIQUE SORTED KEY bonkers_ness
          COMPONENTS sanity_percentage,
          averagely_mad_monsters   TYPE STANDARD TABLE OF ztmonster_header,
          an_averagely_mad_monster LIKE LINE OF averagely_mad_monsters.

    LOOP AT all_monsters INTO DATA(monster_record)
      WHERE sanity_percentage < 75.
      CLEAR an_averagely_mad_monster.
      MOVE-CORRESPONDING monster_record TO an_averagely_mad_monster.
      APPEND an_averagely_mad_monster   TO averagely_mad_monsters.
    ENDLOOP."All Monsters

  ENDMETHOD.


  METHOD L03_49_TABLE_EXTRACTION_NEW.

    DATA: all_monsters
             TYPE SORTED TABLE OF ztmonster_header
             WITH NON-UNIQUE KEY monster_number
             WITH NON-UNIQUE SORTED KEY bonkers_ness
             COMPONENTS sanity_percentage.

    DATA(averagely_mad_monsters) =
    FILTER #( all_monsters USING KEY bonkers_ness
    WHERE sanity_percentage < 75 ).

  ENDMETHOD.


  METHOD l03_50_fae_database.

    DATA: monster_pets TYPE SORTED TABLE OF ztmonster_pets
                       WITH NON-UNIQUE KEY owner,
          all_monsters TYPE STANDARD TABLE OF ztmonster_header.

* Fill up table ALL_MONSTERS

    SELECT *
    FROM ztmonster_pets
    INTO CORRESPONDING FIELDS OF TABLE monster_pets
    FOR ALL ENTRIES IN all_monsters
    WHERE owner = all_monsters-monster_number.

  ENDMETHOD.


  METHOD l03_51_fae_itab.
*--------------------------------------------------------------------*
* In my humble opinion the requirement that both tables need to be
* SORTED or HASHED dramatically reduces the usefulness of this
*--------------------------------------------------------------------*
    DATA: monster_pets TYPE SORTED TABLE OF ztmonster_pets
                       WITH NON-UNIQUE KEY owner,
          all_monsters TYPE SORTED TABLE OF ztmonster_header
                       WITH UNIQUE KEY monster_number.

*--------------------------------------------------------------------*
* Fill up table ALL_MONSTERS
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Do a FAE on the interal table....
*--------------------------------------------------------------------*
    DATA(pets_of_our_monsters) =
    FILTER #( monster_pets IN all_monsters
    WHERE owner = monster_number ).

  ENDMETHOD.


  METHOD l03_52_virtual_sorting.
*--------------------------------------------------------------------*
* This is a problem that actually happened...
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_disputes,
             dispute_number TYPE c LENGTH 10,
           END OF l_typ_disputes.

    TYPES: BEGIN OF l_typ_credit_notes,
             dispute_number TYPE c LENGTH 10,
             credit_value   TYPE alv_curr,
             rejected       TYPE abap_bool,
           END OF   l_typ_credit_notes.

    DATA: dispute_table     TYPE STANDARD TABLE OF l_typ_disputes,
          credit_note_table TYPE STANDARD TABLE OF l_typ_credit_notes.

*--------------------------------------------------------------------*
* Fill up the two tables
*--------------------------------------------------------------------*

* Whatever....

*--------------------------------------------------------------------*
* Virtual Sorting...
*--------------------------------------------------------------------*
    DATA(dispute_indexes) = cl_abap_itab_utilities=>virtual_sort(
    im_virtual_source =
    VALUE #(
    ( source = REF #( credit_note_table )
    components =
    VALUE #( ( name = 'credit_value'
    descending = abap_true ) ) )
    ( source = REF #( dispute_table )
    components =
    VALUE #( ( name = 'dispute_number' ) ) ) ) ).

  ENDMETHOD.


  METHOD l03_53_virtual_sorting_part_2.
*--------------------------------------------------------------------*
* This is a problem that actually happened...
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_disputes,
             dispute_number TYPE c LENGTH 10,
           END OF l_typ_disputes.

    TYPES: BEGIN OF l_typ_credit_notes,
             dispute_number TYPE c LENGTH 10,
             credit_value   TYPE alv_curr,
             rejected       TYPE abap_bool,
           END OF   l_typ_credit_notes.

    DATA: dispute_table     TYPE STANDARD TABLE OF l_typ_disputes,
          credit_note_table TYPE STANDARD TABLE OF l_typ_credit_notes.

*--------------------------------------------------------------------*
* Fill up the two tables
*--------------------------------------------------------------------*

* Whatever....

*--------------------------------------------------------------------*
* Virtual Sorting...
*--------------------------------------------------------------------*
    DATA(dispute_indexes) = cl_abap_itab_utilities=>virtual_sort(
    im_virtual_source =
    VALUE #(
    ( source = REF #( credit_note_table )
    components =
    VALUE #( ( name = 'credit_value'
    descending = abap_true ) ) )
    ( source = REF #( dispute_table )
    components =
    VALUE #( ( name = 'dispute_number' ) ) ) ) ).

*--------------------------------------------------------------------*
* Evaluate Results
*--------------------------------------------------------------------*
    DATA top_disputes TYPE STANDARD TABLE OF l_typ_disputes.

    LOOP AT dispute_indexes ASSIGNING FIELD-SYMBOL(<index>).
      APPEND dispute_table[ <index> ] TO top_disputes.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_54_query_ddic_no_cast.

    DATA structure_description TYPE REF TO cl_abap_structdescr.
    structure_description
    ?= cl_abap_typedescr=>describe_by_name( 'ZSP_MONSTER_HEADER_D' ).
    DATA structure_components TYPE abap_compdescr_tab.
    structure_components = structure_description->components.

  ENDMETHOD.


  METHOD L03_55_QUERY_DDIC_WITH_CAST.

    DATA(structure_components) = CAST cl_abap_structdescr(
    cl_abap_typedescr=>describe_by_name( 'ZSP_MONSTER_HEADER_D' ) )->components.

  ENDMETHOD.


  METHOD l03_56_find_a_subclass_old.
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*
    DATA: full_screen_adapter TYPE REF TO cl_salv_fullscreen_adapter,
          container_adapter   TYPE REF TO cl_salv_grid_adapter.
    TRY.
        "Presume full screen mode (No Container)
        "Target FULL_SCREEN_ADAPTER = CL_SALV_FULLSCREEN_ADAPTER
        "CL_SALV_FULLSCREEN_ADAPTER is a subclass of CL_SALV_ADAPTER
        full_screen_adapter ?= io_salv_adapter.
        "Get the Grid
        ro_alv_grid = full_screen_adapter->get_grid( ).
      CATCH cx_sy_move_cast_error.
        "We must be in container mode
        "Target CONTAINER_ADAPTER = CL_SALV_GRID_ADAPTER
        "CL_SALV_GRID_ADAPTER is a subclass of CL_SALV_ADAPTER
        container_adapter ?= io_salv_adapter.
        ro_alv_grid = container_adapter->get_grid( ).
    ENDTRY.

  ENDMETHOD.


  METHOD l03_57_find_a_subclass_new.
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*
    DATA: full_screen_adapter TYPE REF TO cl_salv_fullscreen_adapter,
          container_adapter   TYPE REF TO cl_salv_grid_adapter.

    IF io_salv_adapter IS INSTANCE OF cl_salv_fullscreen_adapter.
      full_screen_adapter ?= io_salv_adapter.
      ro_alv_grid = full_screen_adapter->get_grid( ).
    ELSEIF io_salv_adapter IS INSTANCE OF cl_salv_grid_adapter.
      container_adapter ?= io_salv_adapter.
      ro_alv_grid = container_adapter->get_grid( ).
    ENDIF.

  ENDMETHOD.


  METHOD l03_58_find_a_subclass_new_v2.
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*

    CASE TYPE OF io_salv_adapter.
      WHEN TYPE cl_salv_fullscreen_adapter
      INTO DATA(full_screen_adapter).
        ro_alv_grid = full_screen_adapter->get_grid( ).
      WHEN TYPE cl_salv_grid_adapter
      INTO DATA(container_adapter).
        ro_alv_grid = container_adapter->get_grid( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  method L03_59_FUNCTIONAL_METHOD_PLUS.

    DATA: something_spurious  TYPE string,
          something_unrelated TYPE string.

    DATA(bomb_name) = lcl_atom_bomb=>get_details(
    EXPORTING id_bomb_number         = '0000000001'
    IMPORTING ed_something_spurious  = something_spurious
    CHANGING  cd_something_unrelated = something_unrelated ).

  endmethod.
ENDCLASS.
