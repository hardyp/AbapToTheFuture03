*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_MONITOR_TOP
*&---------------------------------------------------------------------*
REPORT  z_monster_monitor.
**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF g_typ_alv_output_data.
        INCLUDE STRUCTURE zsc_monster_header.
       "Need this to make individual cells/columns editable
TYPES: celltab           TYPE lvc_t_styl,
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
INCLUDE z_monster_monitor_cd01.

DATA:   go_selections    TYPE REF TO lcl_selections.
