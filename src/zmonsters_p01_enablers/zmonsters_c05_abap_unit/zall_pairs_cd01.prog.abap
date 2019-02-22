*&---------------------------------------------------------------------*
*& Include          ZALL_PAIRS_CD01
*&---------------------------------------------------------------------*
************************************************************************
* Class Definitions
************************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION.

  PUBLIC SECTION.
    METHODS:
      get_data RETURNING VALUE(et_configuration) TYPE g_tt_configuration
               RAISING   zcx_excel.

ENDCLASS.                    "lcl_persistency_layer DEFINITION

CLASS lcl_all_pairs DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF m_typ_mapping,
             variable_name TYPE string,
             value_count   TYPE i,
             column_number TYPE i,
           END OF   m_typ_mapping.

    TYPES: BEGIN OF m_typ_pairs,
             variable_1 TYPE string,
             value_1    TYPE string,
             variable_2 TYPE string,
             value_2    TYPE string,
             test_case  TYPE i,
           END OF   m_typ_pairs.

    DATA: mt_configuration TYPE g_tt_configuration,
          mt_test_cases    TYPE g_tt_output_data,
          mt_mapping       TYPE STANDARD TABLE OF m_typ_mapping,
          mt_pairs         TYPE HASHED TABLE OF m_typ_pairs
                           WITH UNIQUE KEY variable_1 value_1 variable_2 value_2.

    METHODS : main IMPORTING it_configuration     TYPE g_tt_configuration
                   RETURNING VALUE(rt_test_cases) TYPE g_tt_output_data,
      calculate_mapping,
      process_first_two_columns,
      insert_new_column IMPORTING new_column TYPE i,
      pair_is_in_test_case IMPORTING variable_1         TYPE string
                                     first_value        TYPE string
                                     variable_2         TYPE string
                                     second_value       TYPE string
                           RETURNING VALUE(yes_it_is)   TYPE abap_bool.

  PRIVATE SECTION.

    METHODS: populate        IMPORTING left_populated_column TYPE i
                                       right_blank_column    TYPE i,
      compare_columns IMPORTING left_column  TYPE i
                                right_column TYPE i.

    METHODS: get_variable_for_column      IMPORTING column          TYPE i
                                          RETURNING VALUE(variable) TYPE string,
             get_column_for_variable      IMPORTING variable        TYPE string
                                          RETURNING VALUE(column)   TYPE i,
      get_number_of_new_values_for IMPORTING variable_name       TYPE string
                                   RETURNING VALUE(no_of_values) TYPE i,
      get_proposed_value           IMPORTING variable_name TYPE string
                                             index         TYPE i
                                   RETURNING VALUE(value)  TYPE string,
      get_current_value            IMPORTING test_case TYPE g_typ_alv_output_data
                                             column    TYPE i
                                   EXPORTING value     TYPE string
                                             arbitrary TYPE abap_bool,
      set_test_case_value          IMPORTING column    TYPE i
                                             value     TYPE string
                                             arbitrary TYPE abap_bool
                                   CHANGING  test_case TYPE g_typ_alv_output_data,
      combination_exists            IMPORTING variable_1         TYPE string
                                              first_value        TYPE string
                                              variable_2         TYPE string
                                              second_value       TYPE string
                                    RETURNING VALUE(yes_it_does) TYPE abap_bool.

ENDCLASS."LCL_ALL_PAIRS

*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION INHERITING FROM zcl_bc_model.

  PUBLIC SECTION.
    DATA: mo_persistency_layer TYPE REF TO lcl_persistency_layer,
          mo_all_pairs         TYPE REF TO lcl_all_pairs,
          mt_configuration     TYPE g_tt_configuration,
          mt_output_data       TYPE g_tt_output_data.

    METHODS: constructor IMPORTING io_access_class TYPE REF TO lcl_persistency_layer OPTIONAL,
      data_retrieval,
      prepare_data_for_ouput,
      fill_user_commands    REDEFINITION,
      fill_layout_data      REDEFINITION,
      fill_editable_fields  REDEFINITION,
      fill_hidden_fields    REDEFINITION,
      fill_technical_fields REDEFINITION,
      fill_hotspot_fields   REDEFINITION,
      fill_subtotal_fields  REDEFINITION,
      fill_field_texts      REDEFINITION,
      fill_checkbox_fields  REDEFINITION,
      user_command          REDEFINITION.

ENDCLASS.                    "lcl_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
* We want a subclass whereby we can add extra features specific to
* this application only
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION INHERITING FROM zcl_bc_view_salv_table ##needed.

ENDCLASS.                    "lcl_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_bc_controller.

    ALIASES: on_user_command FOR zif_bc_controller~on_user_command.

    DATA: mo_model TYPE REF TO lcl_model,
          mo_view  TYPE REF TO zif_bc_alv_report_view.

    METHODS : constructor  IMPORTING io_model TYPE REF TO lcl_model
                                     io_view  TYPE REF TO zif_bc_alv_report_view,
      on_data_changed FOR EVENT data_changed OF lcl_model.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_controller DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA: mo_model      TYPE REF TO lcl_model,
                mo_controller TYPE REF TO lcl_controller,
                mo_view       TYPE REF TO zif_bc_alv_report_view.

    CLASS-METHODS: main.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_selections DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections DEFINITION.
  PUBLIC SECTION.
    DATA: p_file TYPE string.

    METHODS : constructor IMPORTING
                            ip_file LIKE p_file.

ENDCLASS.                    "lcl_selections DEFINITION
