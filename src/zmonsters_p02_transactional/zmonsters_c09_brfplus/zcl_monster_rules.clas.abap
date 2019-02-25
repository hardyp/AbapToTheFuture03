class ZCL_MONSTER_RULES definition
  public
  final
  create public .

public section.

  constants MC_SANITY_FUNCTION type FDT_UUID value '005056B074C91ED689AA2B1061B060F9' ##NO_TEXT.
  constants MC_HAT_SIZE_FUNCTION type FDT_UUID value '005056B074C91ED689AA010171BC00F9' ##NO_TEXT.

  methods DERIVE_SANITY_DESCRIPTION
    importing
      !ID_SANITY_PERCENTAGE type ZDE_MONSTER_SANITY
    returning
      value(RD_SANITY_DESCRIPTION) type TEXT30
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods DERIVE_HAT_SIZE_DESCRIPTION
    importing
      !ID_HAT_SIZE type ZDE_MONSTER_HAT_SIZE
    returning
      value(RD_HAT_SIZE_DESCRIPTION) type TEXT30
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods DERIVE_INGREDIENT_TYPE
    importing
      !ID_SANITY_DESIRED type ZDE_MONSTER_SANITY
      !ID_USAGE_DESIRED type ZDE_MONSTER_USAGE
      !ID_EVILNESS_DESIRED type ZDE_MONSTER_EVILNESS
      !ID_SCARINESS_DESIRED type ZDE_MONSTER_SCARINESS
      !ID_RAGES_PER_DAY_DESIRED type ZDE_MONSTER_RAGES_PER_DAY
    returning
      value(RD_INGREDIENT_TYPE) type ZDE_MONSTER_INGREDIENTS
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods DERIVE_INGREDIENT_SPLIT
    importing
      !IS_INPUT_DATA type ZST_MONSTER_INPUT_DATA
    returning
      value(RS_INGREDIENT_SPLIT) type ZST_MONSTER_INGREDIENT_SPLIT
    raising
      ZCX_MONSTER_EXCEPTIONS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MONSTER_RULES IMPLEMENTATION.


METHOD derive_hat_size_description.
** Local Variables
  CONSTANTS: hat_size_function TYPE if_fdt_types=>id VALUE '005056B074C91ED98BF6D0384C59A112'.

  DATA:timestamp            TYPE timestamp,
       name_value_table     TYPE abap_parmbind_tab,
       name_value_pair      TYPE abap_parmbind,
       data_reference       TYPE REF TO data,
       monster_hat_size     TYPE if_fdt_types=>element_number,
       hat_size_description TYPE zde_hat_size_description.

  FIELD-SYMBOLS <result> TYPE any.

  GET TIME STAMP FIELD timestamp.

  name_value_pair-name = 'ZDE_MONSTER_HAT_SIZE'.
  monster_hat_size     = id_hat_size.
  GET REFERENCE OF monster_hat_size INTO data_reference.
  name_value_pair-value = data_reference.
  INSERT name_value_pair INTO TABLE name_value_table.

  GET REFERENCE OF hat_size_description INTO data_reference.
  ASSIGN data_reference->* TO <result>.

  TRY.
      cl_fdt_function_process=>process( EXPORTING iv_function_id = hat_size_function
                                                  iv_timestamp   = timestamp
                                        IMPORTING ea_result      = <result>
                                        CHANGING  ct_name_value  = name_value_table ).

      rd_hat_size_description = hat_size_description.

    CATCH cx_fdt INTO DATA(function_error).
      READ TABLE function_error->mt_message INDEX 1 ASSIGNING FIELD-SYMBOL(<function_error_info>).

      DATA(monster_error_info) = VALUE scx_t100key(
        msgid = 'ZMONSTERS'
        msgno = '003' "&1 &2 &3 &4
        attr1 = <function_error_info>-text ).

      RAISE EXCEPTION TYPE zcx_monster_exceptions
        EXPORTING
          textid = monster_error_info.

  ENDTRY.

ENDMETHOD.


  METHOD derive_ingredient_split.

    CONSTANTS: ingredient_split_function TYPE if_fdt_types=>id VALUE '005056B074C91ED98BF5B7F7860CE112'.

    DATA:timestamp                 TYPE timestamp,
         parameter_list            TYPE abap_parmbind_tab,
         parameter_values          TYPE abap_parmbind,
         data_reference            TYPE REF TO data,
         village_region            TYPE if_fdt_types=>element_text,
         monster_brain_size        TYPE if_fdt_types=>element_text,
         monster_color             TYPE if_fdt_types=>element_text,
         monster_model             TYPE if_fdt_types=>element_text,
         monster_osoup_percentage  TYPE if_fdt_types=>element_number,
         monster_growth_percentage TYPE if_fdt_types=>element_number,
         early_age_strength        TYPE if_fdt_types=>element_number,
         eas_days                  TYPE if_fdt_types=>element_number,
         monster_ingredient_split  TYPE zst_monster_ingredient_split.

    FIELD-SYMBOLS <result> TYPE any.

    GET TIME STAMP FIELD timestamp.

*--------------------------------------------------------------------*
* Input Parameters
*--------------------------------------------------------------------*
    parameter_values-name = 'ZDE_VILLAGE_REGION'.
    village_region = is_input_data-region.
    GET REFERENCE OF village_region INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_BRAIN_SIZE'.
    monster_brain_size = is_input_data-brain_size.
    GET REFERENCE OF monster_brain_size INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_COLOR'.
    monster_color = is_input_data-color.
    GET REFERENCE OF monster_color INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_MODEL'.
    monster_model = is_input_data-model.
    GET REFERENCE OF monster_model INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_OSOUP_PERCENTAGE'.
    monster_osoup_percentage = is_input_data-osoup_percentage.
    GET REFERENCE OF monster_osoup_percentage INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_GROWTH_PERCENTAGE'.
    monster_growth_percentage = is_input_data-growth_percentage.
    GET REFERENCE OF monster_growth_percentage INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_EARLY_AGE_STRENGTH'.
    early_age_strength = is_input_data-early_age_strength.
    GET REFERENCE OF early_age_strength INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_EAS_DAYS'.
    eas_days = is_input_data-eas_days.
    GET REFERENCE OF eas_days INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
*--------------------------------------------------------------------*
* Result
*--------------------------------------------------------------------*
    GET REFERENCE OF monster_ingredient_split INTO data_reference.
    ASSIGN data_reference->* TO <result>.

*--------------------------------------------------------------------*
* Here we go!
*--------------------------------------------------------------------*
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = ingredient_split_function
                                                    iv_timestamp   = timestamp
                                          IMPORTING ea_result      = <result>
                                          CHANGING  ct_name_value  = parameter_list ).

*--------------------------------------------------------------------*
* Error Handling
*--------------------------------------------------------------------*
      CATCH cx_fdt INTO DATA(function_error).

        READ TABLE function_error->mt_message INDEX 1 ASSIGNING FIELD-SYMBOL(<function_error_info>).

        DATA(monster_error_info) = VALUE scx_t100key(
        msgid = 'ZMONSTERS'
        msgno = '003' "&1 &2 &3 &4
        attr1 = <function_error_info>-text ).

        RAISE EXCEPTION TYPE zcx_monster_exceptions
          EXPORTING
            textid = monster_error_info.
    ENDTRY.

  ENDMETHOD.


  METHOD derive_ingredient_type.
*--------------------------------------------------------------------*
* Listing 09.02 : Calling BRFplus Function from ABAP
*--------------------------------------------------------------------*
    CONSTANTS: ingredient_function TYPE if_fdt_types=>id VALUE '005056B074C91ED8A2BA59B5E05F810F'.

    DATA:timestamp                 TYPE timestamp,
         parameter_list            TYPE abap_parmbind_tab,
         parameter_values          TYPE abap_parmbind,
         data_reference            TYPE REF TO data,
         brf_monster_sanity        TYPE if_fdt_types=>element_number,
         brf_monster_usage         TYPE if_fdt_types=>element_text,
         brf_monster_scariness     TYPE if_fdt_types=>element_text,
         brf_monster_evilness      TYPE if_fdt_types=>element_text,
         brf_monster_rages_per_day TYPE if_fdt_types=>element_number.

    FIELD-SYMBOLS <result> TYPE any.

    GET TIME STAMP FIELD timestamp.

    parameter_values-name = 'ZDE_MONSTER_SANITY'.
    brf_monster_sanity = id_sanity_desired.
    GET REFERENCE OF brf_monster_sanity INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_USAGE'.
    brf_monster_usage = id_usage_desired.
    GET REFERENCE OF brf_monster_usage INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_SCARINESS'.
    brf_monster_scariness = id_scariness_desired.
    GET REFERENCE OF brf_monster_scariness INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_EVILNESS'.
    brf_monster_evilness = id_evilness_desired.
    GET REFERENCE OF brf_monster_evilness INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    parameter_values-name = 'ZDE_MONSTER_RAGES_PER_DAY'.
    brf_monster_rages_per_day = id_rages_per_day_desired.
    GET REFERENCE OF brf_monster_rages_per_day INTO data_reference.
    parameter_values-value = data_reference.
    INSERT parameter_values INTO TABLE parameter_list.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = ingredient_function
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = data_reference ).
    ASSIGN data_reference->* TO <result>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = ingredient_function
                                                    iv_timestamp   = timestamp
                                          IMPORTING ea_result      = <result>
                                          CHANGING  ct_name_value  = parameter_list ).

        rd_ingredient_type = <result>.

      CATCH cx_fdt INTO DATA(function_error).

        READ TABLE function_error->mt_message INDEX 1 ASSIGNING FIELD-SYMBOL(<function_error_info>).

        DATA(monster_error_info) = VALUE scx_t100key(
        msgid = 'ZMONSTERS'
        msgno = '003' "&1 &2 &3 &4
        attr1 = <function_error_info>-text ).

        RAISE EXCEPTION TYPE zcx_monster_exceptions
          EXPORTING
            textid = monster_error_info.
    ENDTRY.

  ENDMETHOD.


METHOD derive_sanity_description.
* Local Variables
  CONSTANTS: sanity_deriving_function TYPE if_fdt_types=>id VALUE '005056B074C91ED98BF6F0A17957E112'.

  DATA:timestamp          TYPE timestamp,
       name_value_table   TYPE abap_parmbind_tab,
       name_value_pair    TYPE abap_parmbind,
       data_reference     TYPE REF TO data,
       monster_sanity     TYPE if_fdt_types=>element_number,
       sanity_description TYPE zde_sanity_description.

  FIELD-SYMBOLS <result> TYPE any.

  GET TIME STAMP FIELD timestamp.

  name_value_pair-name = 'ZDE_MONSTER_SANITY'.
  monster_sanity = id_sanity_percentage.
  GET REFERENCE OF monster_sanity INTO data_reference.
  name_value_pair-value = data_reference.
  INSERT name_value_pair INTO TABLE name_value_table.

  GET REFERENCE OF sanity_description INTO data_reference.
  ASSIGN data_reference->* TO <result>.

  TRY.
      cl_fdt_function_process=>process( EXPORTING iv_function_id = sanity_deriving_function
                                                  iv_timestamp   = timestamp
                                        IMPORTING ea_result      = <result>
                                        CHANGING  ct_name_value  = name_value_table ).

      rd_sanity_description = sanity_description.

    CATCH cx_fdt INTO DATA(function_error).
      READ TABLE function_error->mt_message INDEX 1 ASSIGNING FIELD-SYMBOL(<function_error_info>).

      DATA(monster_error_info) = VALUE scx_t100key(
        msgid = 'ZMONSTERS'
        msgno = '003' "&1 &2 &3 &4
        attr1 = <function_error_info>-text ).

      RAISE EXCEPTION TYPE zcx_monster_exceptions
        EXPORTING
          textid = monster_error_info.

  ENDTRY.

ENDMETHOD.
ENDCLASS.
