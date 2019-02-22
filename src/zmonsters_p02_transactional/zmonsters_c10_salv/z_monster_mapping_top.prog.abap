*&---------------------------------------------------------------------*
*& Include          Z_MONSTER_MAPPING_TOP
*&---------------------------------------------------------------------*
REPORT z_monster_listings_mapping.
**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF g_typ_alv_output_data,
         chapter     TYPE sy-tabix,
         listing     TYPE sy-tabix,
         name        TYPE string,
         object_type TYPE trobjtype, "e.g. class/program
         object_name TYPE string, "e.g. class name
         subobject   TYPE string, "e.g. method name
       END OF g_typ_alv_output_data.

TYPES: g_tt_output_data TYPE STANDARD TABLE OF g_typ_alv_output_data.

**********************************************************************
* Constants
**********************************************************************

*--------------------------------------------------------------------*
* TABLES
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Global Variables
*--------------------------------------------------------------------*
DATA: go_no_handler    TYPE REF TO cx_sy_no_handler,
      go_precondition  TYPE REF TO zcx_violated_precondition,
      go_postcondition TYPE REF TO zcx_violated_postcondition,
      gd_error_class   TYPE string.

**********************************************************************
* Local Classes
**********************************************************************
INCLUDE z_monster_map_cd01.

DATA:   go_selections    TYPE REF TO lcl_selections.
