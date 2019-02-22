*&---------------------------------------------------------------------*
*& Report ZALL_PAIRS
*&---------------------------------------------------------------------*
* This is an attempt to recreate the "all pairs" algorithim in ABAP
* I have the source code of a program in PEARL to do this, but it is
* much more fun to re-invent the wheel
* The idea is as follows:-
* You populate a speadsheet with two columns
* In the first are the variable names
* For each variable there are multiple rows with the different possible
* values in the second column
* You upload this, and the minimum possible number of test cases to
* get full coverage are calculated, and displayed via the ALV
* You can then download the test cases to another spreadsheet and use
* that to populate the ZMOCKUP_LOADER (after adding in expected results)
* Hey Presto! The program with all the different variables wll use the
* ZMOCKUP_LOADER to loop through the test cases and Uncle Bob is your Uncle
*&---------------------------------------------------------------------*
INCLUDE zall_pairs_top.

**********************************************************************
* Selection Screen
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_file TYPE string LOWER CASE MEMORY ID gr8.

SELECTION-SCREEN END OF BLOCK blk1.

**********************************************************************
* Initialisation
**********************************************************************
INITIALIZATION.
  PERFORM initalisation.

*--------------------------------------------------------------------*
* At Selection-Screen
*--------------------------------------------------------------------*
* Call up Windows open file dialog
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM select_file_path.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  "This nonsense is the only way I can avoid getting bogus syntax
  "errors when doing a syntax check on the local class implementations
  CREATE OBJECT go_selections
    EXPORTING
      ip_file = p_file.

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
        MESSAGE 'Report in Trouble - please call helpdesk'(001) TYPE 'I'.
    ENDTRY.
  ELSE.
    "Development / Test / Quality Assurance
    "Here we DO want short dumps so we can analyse them, and we want the design by
    "contract messages to make it obvious there is a bug and the code should not
    "go to production
    "Put another the way the two DBC exceptions are impossible errors I am actively
    "looking to cause a dump. If any other sort of dump occurs then it is something
    "I am not expecting and I want to know all about it
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

*--------------------------------------------------------------------*
* Class Implementations
*--------------------------------------------------------------------*
  INCLUDE zall_pairs_cio1.

*--------------------------------------------------------------------*
* Test Classes & Test Doubles
*--------------------------------------------------------------------*
  INCLUDE zall_pairs_tc01.

*&---------------------------------------------------------------------*
*&      Form  INITALISATION
*&---------------------------------------------------------------------*
FORM initalisation ##needed.

ENDFORM.                    " INITALISATION
*&---------------------------------------------------------------------*
*& Form SELECT_FILE_PATH
*&---------------------------------------------------------------------*
*& User Dialog to Choose a File from their local PC
*& This comes from ZDEMO_EXCEL37
*&---------------------------------------------------------------------*
FORM select_file_path .

  DATA: repid       TYPE syrepid,
        fields      TYPE dynpread_tabtype,
        field       LIKE LINE OF fields,
        files       TYPE filetable,
        file_filter TYPE string.

  repid = sy-repid.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = repid
      dynumb               = '1000'
      request              = 'A'
    TABLES
      dynpfields           = fields
    EXCEPTIONS
      invalid_abapworkarea = 01
      invalid_dynprofield  = 02
      invalid_dynproname   = 03
      invalid_dynpronummer = 04
      invalid_request      = 05
      no_fielddescription  = 06
      undefind_error       = 07.

  CHECK sy-subrc EQ 0.

  READ TABLE fields INTO field WITH KEY fieldname = 'P_PFILE'.
  p_file = field-fieldvalue.

  file_filter = 'Excel Files (*.XLSX;*.XLSM)|*.XLSX;*.XLSM' ##NO_TEXT.
  cl_gui_frontend_services=>file_open_dialog( EXPORTING
                                                default_filename        = p_file
                                                file_filter             = file_filter
                                              CHANGING
                                                file_table              = files
                                                rc                      = sy-tabix
                                              EXCEPTIONS
                                                OTHERS                  = 1 ).
  CHECK sy-subrc EQ 0.

  READ TABLE files INDEX 1 INTO p_file.

ENDFORM.
