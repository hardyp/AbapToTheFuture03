*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_AM_TOP
*&---------------------------------------------------------------------*
REPORT  z_monster_atrocity_monitor.
**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF g_typ_alv_output_data,
         check TYPE char01.
        INCLUDE STRUCTURE ztmonster_am.
"Need this to make individual cells/columns editable
TYPES: celltab TYPE lvc_t_styl,
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

* Global Variables for Push Channels
* Not 100% sure they need to be global as yet, but they are in the
* example programs
DATA : gd_appl_id  TYPE amc_application_id VALUE 'ZAMC_FOR_MONSTERS',
       gd_ch_id    TYPE amc_channel_id     VALUE '/monsters',
       gd_chext_id TYPE amc_channel_extension_id.

DATA: go_consumer         TYPE REF TO if_amc_message_consumer.
DATA: gf_message_received TYPE abap_bool,
      gv_message          TYPE string.
DATA: gt_message_list     TYPE TABLE OF string.
DATA: gt_pcp_list         TYPE TABLE OF REF TO if_ac_message_type_pcp.
DATA: go_pcp              TYPE REF TO if_ac_message_type_pcp.
DATA: gt_pcp_fields       TYPE pcp_fields.
**********************************************************************
* Local Classes
**********************************************************************
INCLUDE z_monster_am_cd01.

*--------------------------------------------------------------------*
* Global Variables that Refer to Local Classes
*--------------------------------------------------------------------*
DATA: go_selections TYPE REF TO lcl_selections.

DATA: go_receiver TYPE REF TO lcl_amc_receiver.
