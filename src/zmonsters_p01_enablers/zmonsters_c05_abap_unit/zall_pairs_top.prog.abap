*&---------------------------------------------------------------------*
*& Include          ZALL_PAIRS_TOP
*&---------------------------------------------------------------------*
REPORT zall_pairs.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF g_typ_alv_output_data,
         case_number  TYPE i,
         column_01    TYPE string,
         arbitrary_01 TYPE abap_bool,
         column_02    TYPE string,
         arbitrary_02 TYPE abap_bool,
         column_03    TYPE string,
         arbitrary_03 TYPE abap_bool,
         column_04    TYPE string,
         arbitrary_04 TYPE abap_bool,
         column_05    TYPE string,
         arbitrary_05 TYPE abap_bool,
         column_06    TYPE string,
         arbitrary_06 TYPE abap_bool,
       END OF g_typ_alv_output_data.

TYPES: g_tt_output_data TYPE STANDARD TABLE OF g_typ_alv_output_data
                        WITH DEFAULT KEY.

TYPES: BEGIN OF g_typ_configuration,
             variable_name  TYPE string,
             possible_value TYPE string,
             count          TYPE i,
           END OF   g_typ_configuration.

    TYPES: g_tt_configuration TYPE STANDARD TABLE OF g_typ_configuration
                              WITH KEY variable_name.

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
INCLUDE zall_pairs_cd01.

DATA:   go_selections    TYPE REF TO lcl_selections.
