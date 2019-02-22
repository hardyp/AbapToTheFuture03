*&---------------------------------------------------------------------*
*& Include          Z_MONSTER_MAP_CD01
*&---------------------------------------------------------------------*
************************************************************************
* Class Definitions
************************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION.

  PUBLIC SECTION.
    METHODS: constructor,
             get_data EXPORTING et_output_data TYPE g_tt_output_data.

ENDCLASS.                    "lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION INHERITING FROM zcl_bc_model.

  PUBLIC SECTION.
    DATA: mo_persistency_layer TYPE REF TO lcl_persistency_layer,
          mt_output_data       TYPE g_tt_output_data.

    METHODS: constructor IMPORTING io_access_class TYPE REF TO lcl_persistency_layer OPTIONAL,
             data_retrieval,
             prepare_data_for_ouput,
             fill_user_commands    REDEFINITION,
             fill_editable_fields  REDEFINITION,
             fill_hidden_fields    REDEFINITION,
             fill_technical_fields REDEFINITION,
             fill_hotspot_fields   REDEFINITION,
             fill_subtotal_fields  REDEFINITION,
             fill_field_texts      REDEFINITION,
             fill_checkbox_fields  REDEFINITION,
             fill_layout_data      REDEFINITION,
             user_command          REDEFINITION.

ENDCLASS.                    "lcl_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
* We want a subclass whereby we can add extra features specific to
* this application only
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION INHERITING FROM zcl_bc_view_salv_table.

  PUBLIC SECTION.
    METHODS make_column_editable REDEFINITION.

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
    DATA: s_chap TYPE RANGE OF sy-tabix,
          p_vari  TYPE disvariant-variant.

    METHODS : constructor IMPORTING
       is_chap  LIKE s_chap
       ip_vari  LIKE p_vari.

ENDCLASS.                    "lcl_selections DEFINITION
