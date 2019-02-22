class ZCL_MONSTER_TRANSACTIONS_PERL definition
  public
  create public .

public section.

  types:
    mtt_green_monsters TYPE STANDARD TABLE OF zv_monsters WITH DEFAULT KEY .
  types:
    BEGIN OF m_typ_scariness,
        monster_number    TYPE zde_monster_number,
        name              TYPE zde_monster_name,
        scariness_string  TYPE string,
        strength          TYPE zde_monster_strength,
        sanity_percentage TYPE zde_monster_sanity,
        scariness_ratio   TYPE DECFLOAT34,
      END OF m_typ_scariness .
  types:
    mtt_scariness TYPE STANDARD TABLE OF m_typ_scariness WITH DEFAULT KEY .
  types:
    mtt_deliveries TYPE STANDARD TABLE OF zmn_deliveries WITH DEFAULT KEY .
  types:
    mtt_villages TYPE STANDARD TABLE OF zmn_villages WITH DEFAULT KEY .

  methods DERIVE_MONSTER_SCARINESS
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RT_SCARINESS) type MTT_SCARINESS .
  methods DERIVE_SCARY_RATIO_OLD
    returning
      value(RT_SCARINESS) type MTT_SCARINESS .
  methods DERIVE_SCARY_RATIO_NEW .
  methods DERIVE_VIA_COOL_WHERE_CLAUSES .
  methods DERIVE_STRINGY_MONSTERS .
  methods DERIVE_MONSTER_INITIALS .
  methods DERIVE_VILLAGES_BY_DESCRIPTION
    importing
      !ID_DESCRIPTION type STRING
    returning
      value(RT_VILLAGES) type MTT_VILLAGES .
  methods DERIVE_MANUAL_MONSTERS .
  methods DERIVE_AUTOMATIC_MONSTERS .
  methods DERIVE_MONSTER_EXISTENCE
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RF_MONSTER_EXISTS) type ABAP_BOOL .
  methods DERIVE_PETS_THE_HARD_WAY .
  methods DERIVE_PETS_THE_EASY_WAY .
  methods DERIVE_VIA_COOL_ON_CLAUSE .
  methods DERIVE_UNION_MEMBERSHIP .
  methods DERIVE_MONSTER_VILLAGE_GRID .
  methods DERIVE_BY_AGGREGATION .
  methods DERIVE_DELS_BY_NO_OF_HEADS
    importing
      !ID_NO_OF_HEADS type ZDE_MONSTER_HEADS
      !IT_DATE_RANGE type /GC1/TAB_RNG_DATE
    returning
      value(RT_ORDERS) type MTT_DELIVERIES .
  methods DERIVE_INSANE_MONSTERS .
  methods DERIVE_GREEN_MONSTERS
    returning
      value(RT_GREEN_MONSTERS) type MTT_GREEN_MONSTERS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MONSTER_TRANSACTIONS_PERL IMPLEMENTATION.


  METHOD derive_automatic_monsters.
*--------------------------------------------------------------------*
* Listing 07.10 Defining Internal Table Based on SQL Query
*--------------------------------------------------------------------*

    SELECT name, monster_number, sanity_percentage
    FROM ztmonster_header
    WHERE name = 'FRED'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD derive_by_aggregation.
*--------------------------------------------------------------------*
* Listing 07.17 : Populating Aggregated Database Table
*--------------------------------------------------------------------*
* This is a silly example as it does not actualy aggregate anything
* In real life you want want to sum up people scared by reading the
* delivery table, as there can be multiple rows per monster
*--------------------------------------------------------------------*
    INSERT ztscared_totals FROM
    ( SELECT
    FROM ztmonster_header
    FIELDS monster_number,
    SUM( people_scared ) AS people_scared
    GROUP BY mandt, monster_number ).

  ENDMETHOD.


  METHOD derive_dels_by_no_of_heads.
*--------------------------------------------------------------------*
* Demonstration of Common Table Expressions
*--------------------------------------------------------------------*
    IF 1 = 2.
*--------------------------------------------------------------------*
* Listing 07.18 Traditonal Way of Doing Subqueries in Open SQL
*--------------------------------------------------------------------*
      SELECT *
      FROM zmn_deliveries
      INTO TABLE @DATA(lt_deliveries_old)
      WHERE due_date IN @it_date_range
      AND   monster_number IN ( SELECT monster_number FROM  ztmonster_header
                                                      WHERE no_of_heads = @id_no_of_heads ).
    ELSE.
*--------------------------------------------------------------------*
* Listing 07.19 Subqueries from 7.51 Onward
*--------------------------------------------------------------------*
     WITH +monsters_with_heads AS
        ( SELECT monster_number FROM ztmonster_header
          WHERE  no_of_heads = @id_no_of_heads )

      SELECT *
      FROM zmn_deliveries
      WHERE due_date       IN @it_date_range
      AND   monster_number IN ( SELECT monster_number FROM +monsters_with_heads )
      INTO TABLE @DATA(lt_deliveries_new).

    ENDIF.

  ENDMETHOD.


  METHOD DERIVE_GREEN_MONSTERS.
*--------------------------------------------------------------------*
* Listing 07.36 Reading Data from CDS View Entity in ABAP
*--------------------------------------------------------------------*
* Read the view into an internal table
    SELECT * FROM zcds_monsters_join
    INTO CORRESPONDING FIELDS OF TABLE @rt_green_monsters.

* Or just display the contents directly
    cl_salv_gui_table_ida=>create_for_cds_view( 'ZCDS_MONSTERS_JOIN' )->fullscreen( )->display( ).

  ENDMETHOD.


  METHOD derive_insane_monsters.
*--------------------------------------------------------------------*
* Listing 07.33 : Calling CDS View with Parameters
*--------------------------------------------------------------------*
    DATA: p_color  TYPE zde_monster_color,
          r_sanity TYPE RANGE OF zde_monster_sanity,
          s_sanity LIKE LINE OF r_sanity.

    SELECT *
    FROM zcds_monsters_parameters(
    p_color       = @p_color,
    p_sanity_low  = @s_sanity-low,
    p_sanity_high = @s_sanity-high )
    INTO TABLE @DATA(colorful_mad_monster_table)
    WHERE strength = 'REALLY_STRONG'.

  ENDMETHOD.


  METHOD derive_manual_monsters.
*--------------------------------------------------------------------*
* Listing 07.09 : Defining an Internal Table Manually
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_monsters,
             name              TYPE ztmonster_header-name,
             monster_number    TYPE ztmonster_header-monster_number,
             sanity_percentage TYPE ztmonster_header-sanity_percentage,
           END OF l_typ_monsters.

    DATA: table_of_monsters TYPE STANDARD TABLE OF l_typ_monsters.

    SELECT name monster_number sanity_percentage
    FROM ztmonster_header
    INTO CORRESPONDING FIELDS OF TABLE table_of_monsters
    WHERE name = 'FRED'.

  ENDMETHOD.


  METHOD derive_monster_existence.
*--------------------------------------------------------------------*
* Listing 07.11 Existence Check with No Database Rows Transported
*--------------------------------------------------------------------*
    SELECT SINGLE @abap_true
    FROM ztmonster_header
    WHERE monster_number = @id_monster_number
    INTO @rf_monster_exists.

  ENDMETHOD.


  METHOD derive_monster_initials.
*--------------------------------------------------------------------*
* Listing 07.07 Using IS INITIAL in WHERE Clause
*--------------------------------------------------------------------*
* Only works from ABAP 1809 (7.53) Upwards
*--------------------------------------------------------------------*
*    SELECT monster_number
*    FROM ztmonster_header
*    WHERE hat_size IS INITIAL
*    INTO TABLE @DATA(monsters_without_hats)."They don’t dance

  ENDMETHOD.


  METHOD DERIVE_MONSTER_SCARINESS.
*--------------------------------------------------------------------*
* Listing 07.02 - CASE Statement within SQL Statement
*--------------------------------------------------------------------*
    CONSTANTS: lc_scary1 TYPE char20 VALUE 'SLIGHTLY SCARY',
               lc_scary2 TYPE char20 VALUE 'REALLY SCARY',
               lc_scary3 TYPE char20 VALUE 'NORMAL'.

    SELECT name, monster_number,
      CASE
        WHEN sanity_percentage <= 10 AND strength >= 75 THEN @lc_scary2
        WHEN sanity_percentage <= 25 AND strength >= 50 THEN @lc_scary1
        ELSE @lc_scary3
      END AS scariness_string
      FROM ztmonster_header
      WHERE monster_number = @id_monster_number
      INTO CORRESPONDING FIELDS OF TABLE @rt_scariness.

  ENDMETHOD.


  METHOD derive_monster_village_grid.
*--------------------------------------------------------------------*
* Listing 07.16 Cross Join in ABAP
*--------------------------------------------------------------------*
    SELECT zmn_deliveries~monster_number,zmn_deliveries~monster_name,
           zmn_villages~village_number,zmn_villages~village_address,
           CASE
            WHEN zmn_deliveries~monster_number = '0000000001' AND
                 zmn_villages~village_number   = '0000100000' THEN 'YES'
            WHEN zmn_deliveries~monster_number = '0000000002' AND
                 zmn_villages~village_number   = '0000500000' THEN 'YES'
            ELSE 'NO'
           END AS good_idea
      FROM zmn_deliveries
      CROSS JOIN zmn_villages
      GROUP BY monster_number,monster_name,zmn_villages~village_number,zmn_villages~village_address
      ORDER BY monster_number,zmn_villages~village_number
      INTO TABLE @DATA(monster_village_grid).

  ENDMETHOD.


  METHOD derive_pets_the_easy_way.
*--------------------------------------------------------------------*
* Listing 07.13 Reading Everything from One Table during Inner Join
*--------------------------------------------------------------------*
    SELECT ztmonster_header~*
    FROM ztmonster_header
    INNER JOIN ztmonster_pets
    ON ztmonster_header~monster_number = ztmonster_pets~owner
    WHERE name = 'FRED'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD derive_pets_the_hard_way.
*--------------------------------------------------------------------*
* Listing 07.12 : Lots of Fields from Main Table
*--------------------------------------------------------------------*
* Sometimes this list can on for pages for really wide tables
    TYPES: BEGIN OF l_typ_monsters,
             name              TYPE ztmonster_header-name,
             monster_number    TYPE ztmonster_header-monster_number,
             sanity_percentage TYPE ztmonster_header-sanity_percentage,
             strength          TYPE ztmonster_header-strength,
             hat_size          TYPE ztmonster_header-hat_size,
             no_of_heads       TYPE ztmonster_header-no_of_heads,
             pet_species       TYPE ztmonster_pets-pet_species,
           END OF l_typ_monsters.

    DATA: table_of_monsters TYPE STANDARD TABLE OF l_typ_monsters.

    SELECT ztmonster_header~name       ztmonster_header~monster_number
    ztmonster_header~sanity_percentage ztmonster_header~strength
    ztmonster_header~hat_size          ztmonster_header~no_of_heads
    ztmonster_pets~pet_species
    FROM ztmonster_header
    INNER JOIN ztmonster_pets
    ON ztmonster_header~monster_number = ztmonster_pets~owner
    INTO CORRESPONDING FIELDS OF TABLE table_of_monsters
    WHERE name = 'FRED'.

  ENDMETHOD.


  METHOD DERIVE_SCARY_RATIO_NEW.
*--------------------------------------------------------------------*
* Listing 07.04 : Operations inside SQL Statements: After
*--------------------------------------------------------------------*

    SELECT name, monster_number,
    CAST( strength AS FLTP ) / CAST( sanity_percentage AS FLTP )
    AS scariness_ratio
    FROM ztmonster_header
    INTO TABLE @DATA(scariness_table).

  ENDMETHOD.


  METHOD DERIVE_SCARY_RATIO_OLD.
*--------------------------------------------------------------------*
* Listing 07.03 : Operations inside SQL Statements: Before 7.4
*--------------------------------------------------------------------*
* Local Variables
    DATA: converted_strength TYPE decfloat34,
          converted_sanity   TYPE decfloat34.

    FIELD-SYMBOLS: <monster_details>
    LIKE LINE OF rt_scariness.

    SELECT name monster_number strength sanity_percentage
    ##too_many_itab_fields "in the world
    FROM ztmonster_header
    INTO CORRESPONDING FIELDS OF TABLE rt_scariness.

    LOOP AT rt_scariness ASSIGNING <monster_details>.
      converted_strength = <monster_details>-strength.
      converted_sanity   = <monster_details>-sanity_percentage.
      <monster_details>-scariness_ratio =
      converted_strength / converted_sanity.
    ENDLOOP.

  ENDMETHOD.


  METHOD derive_stringy_monsters.
*--------------------------------------------------------------------*
* Listing 07.06 String Functions in SELECT Statements
*--------------------------------------------------------------------*
    DATA(helper_function) = NEW lcl_monster_functions( ).

    SELECT
    concat( monster_number, replace( 'EVIL', 'VERY_EVIL' , name ) )
    AS monster_description,
    length( weapon ) AS weapon_length
    FROM ztmonster_header
    WHERE hat_size = @( helper_function->hat_size_of_the_day( ) )
    INTO TABLE @DATA(evilized_monster_weapons).

  ENDMETHOD.


  METHOD derive_union_membership.
*--------------------------------------------------------------------*
* Listing 07.15 : SELECT from Different Tables Using UNION
*--------------------------------------------------------------------*
* Oh my love, you've been so much on my mind
* I've been waiting for this day to arrive
* It feels so long, I watch the hours go by
* It really doesn't matter when you're here by my side
* UNION - together we're strong
* UNION - forever be mine
* (c) Erasure (1986)
*--------------------------------------------------------------------*
    SELECT monster_number AS monster_number, tax_number AS tax_number
    FROM ztmonster_header
    UNION DISTINCT
    SELECT monster_number AS monster_number, ' ' AS tax_number
    FROM ztmonster_op_itm
    UNION DISTINCT
    SELECT monster_number AS monster_number, ' ' AS tax_number
    FROM ztmonster_cl_itm
    INTO TABLE @DATA(monster_tax_no_list).

  ENDMETHOD.


  METHOD derive_via_cool_on_clause.
*--------------------------------------------------------------------*
* Listing 07.14 : Joining Two Tables in Newly Possible Ways
*--------------------------------------------------------------------*

    SELECT ztmonster_header~*
    FROM ztmonster_header
    INNER JOIN ztmonster_pets
    ON  ztmonster_pets~owner       EQ ztmonster_header~monster_number
    AND ztmonster_pets~pet_type    LIKE 'GIGANTIC%'
    AND ztmonster_pets~pet_species IN ('SHARK','CROCODILE','DINOSAUR')
    WHERE name = 'FRED'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD DERIVE_VIA_COOL_WHERE_CLAUSES.
*--------------------------------------------------------------------*
* Listing 07.05 : New Possibilities in WHERE Clauses
*--------------------------------------------------------------------*
    SELECT monster_number, name
    FROM ztmonster_header
    WHERE eas_days + sanity_percentage > 100
    INTO TABLE @DATA(old_sane_monster_list).

    DATA(helper_function) = NEW lcl_monster_functions( ).

    SELECT monster_number, name
    FROM ztmonster_header
    WHERE hat_size = @( helper_function->hat_size_of_the_day( ) )
    INTO TABLE @DATA(fashionable_monster_list).

  ENDMETHOD.


  METHOD DERIVE_VILLAGES_BY_DESCRIPTION.
*--------------------------------------------------------------------*
* Listing 07.08 Case-Insensitive Database Query
*--------------------------------------------------------------------*
* Preconditions
    CHECK id_description IS NOT INITIAL.

    DATA(upper_case_query) = `%` && to_upper( id_description ) && `%`.

    SELECT *
    FROM zmn_villages
    WHERE upper( village_address ) LIKE @upper_case_query
    ORDER BY village_number
    INTO TABLE @rt_villages.

  ENDMETHOD.
ENDCLASS.
