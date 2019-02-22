*&---------------------------------------------------------------------*
*& Report  Z_MONSTER_MONITOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* Data Declarations
*--------------------------------------------------------------------*
INCLUDE z_monster_monitor_top.

TABLES: zsc_monster_header.
**********************************************************************
* Selection Screen
**********************************************************************
* Monster Header Data
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_numbr FOR zsc_monster_header-monster_number,
                s_name  FOR zsc_monster_header-name.

PARAMETERS: p_bhf TYPE ZDE_MONSTER_BED_HIDER_FLAG.

SELECTION-SCREEN END OF BLOCK blk1.

* Display Options
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS: p_vari  LIKE disvariant-variant,
            p_edit  TYPE abap_bool AS CHECKBOX,
            p_macro TYPE abap_bool AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK blk2.

* Background Execution - send to email address
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-003.
PARAMETERS: p_send  AS CHECKBOX.
PARAMETERS: p_email TYPE ad_smtpadr.
SELECTION-SCREEN END OF BLOCK blk3.

**********************************************************************
* Initialisation
**********************************************************************
INITIALIZATION.
  PERFORM initalisation.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
* There are some exceptions that you cannot predict when and where
* they will occur e.g. system exceptions inheriting from CX_NO_CHECK
* Such exceptions cannot be manually raised, the system raises them
* itself. In such cases we want to avoid a dump and output a message
* instead
*--------------------------------------------------------------------*
START-OF-SELECTION.
  "This nonsense is the only way I can avoid getting bogus syntax
  "errors when doing a syntax check on the local class implementations
  CREATE OBJECT go_selections
    EXPORTING
      is_numbr = s_numbr[]
      is_name  = s_name[]
      ip_bhf   = p_bhf
      ip_vari  = p_vari
      ip_edit  = p_edit
      ip_macro = p_macro
      ip_send  = p_send
      ip_email = p_email.

  IF zcl_bc_system_environment=>is_production( ) = abap_true.
    "In production we never want a short dump, but the "design by contract"
    "things would just confuse the user
    TRY.
        lcl_application=>main( ).
      CATCH cx_sy_no_handler INTO go_no_handler.
        "An exception was raised that was not caught at any point in the call stack
        gd_error_class = |Fatal Error concerning Class { go_no_handler->classname } - Please Call the Helpdesk|.
        MESSAGE gd_error_class TYPE 'I'.
      CATCH cx_root ##catch_all.
        "We do not know what was happened, output a message instead of dumping
        MESSAGE 'Report in Trouble - please call helpdesk' TYPE 'I'.
    ENDTRY.
  ELSE.
    "Development / Test / Quality Assurance
    "Here we DO want short dumps so we can analyse them, and we want the design by
    "contract messages to make it obvious there is a bug and the code should not
    "go to production
    TRY.
        lcl_application=>main( ).
      CATCH zcx_violated_precondition INTO go_precondition.
        "A bug was detected at the start of a subroutine - the caller of the
        "subroutine is at fault
        go_precondition->mo_error_log->show_error_log( ).
      CATCH zcx_violated_postcondition INTO go_postcondition.
        "A bug was detected at the end of a subroutine - the subroutine is
        "at fault
        go_postcondition->mo_error_log->show_error_log( ).
    ENDTRY.
  ENDIF.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_layouts USING cl_salv_layout=>restrict_none CHANGING p_vari.

*&---------------------------------------------------------------------*
*&      Form  F4_LAYOUTS
*&---------------------------------------------------------------------*
FORM f4_layouts USING    pud_restrict TYPE salv_de_layout_restriction
                CHANGING pcd_layout   TYPE disvariant-variant.

  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

  ls_key-report = sy-repid.

  ls_layout = cl_salv_layout_service=>f4_layouts( s_key    = ls_key
                                                  layout   = pcd_layout
                                                  restrict = pud_restrict ).

  pcd_layout = ls_layout-layout.

ENDFORM.                    " F4_LAYOUTS

**********************************************************************
* Class Implementations
**********************************************************************
INCLUDE z_monster_monitor_io1.

*&---------------------------------------------------------------------*
*&      Form  INITALISATION
*&---------------------------------------------------------------------*
FORM initalisation .
* Do I want to put my custom ABAP2XLSX classes in the A2TF repository?
* Yes I do. In fact I want to submit them to the real ABAP2XLSX project
*  "Get email address of current user
*  CALL METHOD zcl_excel_emailer=>get_email_address
*    RECEIVING
*      rd_email_address = p_email.

ENDFORM.                    " INITALISATION
