*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_ADL_IO1
*&---------------------------------------------------------------------*
* Local Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD main."Of Monster Atrocity Due List
* Local Variables
    DATA: ld_report_name TYPE string,
          ld_repid       TYPE sy-repid.

    CONCATENATE sy-tcode sy-title INTO ld_report_name
    SEPARATED BY ' : '.

    "Hard Coded here - would use configuration or similar in
    "real life
    DATA(context_data_list) = VALUE wdr_simple_name_value_list( (
    name  = 'UI_TECHNOLOGY'
    value = 'CL_SALV_TABLE' ) ).

    CREATE OBJECT mo_model.

    zcl_ocp_factory=>return_object_given(
      EXPORTING it_context_data = context_data_list
      CHANGING  co_object       = mo_view ).

    CREATE OBJECT mo_controller
      EXPORTING
        io_model = mo_model
        io_view  = mo_view.

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_ouput( ).

    "It is bad news to pass system variables as parameters
    ld_repid = sy-repid.

    IF sy-batch IS INITIAL.
*--------------------------------------------------------------------*
* Listing 10.31 - Calling a SALV report whilst creating a container
*                 automatically
*--------------------------------------------------------------------*
* Program flow is as follows:-
* ZCL_BC_VIEW_SALV_TABLE->CREATE_CONTAINER_PREPARE_DATA
* Function ZSALV_CSQT_CREATE_CONTAINER
* ZSALV_CSQT_CREATE_CONTAINER->FILL_CONTAINER_CONTENT
* ZCL_BC_VIEW_SALV_TABLE->PREPARE_DISPLAY_DATA
* --> INITIALISE (Generic)
* --> Application Specific Changes (Generic)
* --> Display (Generic)
      mo_view->create_container_prep_display(
    EXPORTING
      id_report_name        = ld_repid                                                                                                                                             " Calling program
      if_start_in_edit_mode = abap_false
      is_layout             = mo_model->ms_layout
      id_edit_control_field = mo_model->md_edit_control_field
      it_editable_fields    = mo_model->mt_editable_fields
      it_technicals         = mo_model->mt_technicals
      it_hidden             = mo_model->mt_hidden
      it_hotspots           = mo_model->mt_hotspots
      it_checkboxes         = mo_model->mt_checkboxes
      it_subtotal_fields    = mo_model->mt_subtotal_fields
      it_field_texts        = mo_model->mt_field_texts                                                                                                                                    " Display Variant as specified by user
      it_user_commands      = mo_model->mt_user_commands
    CHANGING
      ct_data_table         = mo_model->mt_output_data ).

    ELSE.
* If this is running in the background there is no way
* in the world we want/need a container, as there is no
* chance for the user to press any user command buttons or
* edit the data, as there is no user, and no screen for the
* container to live on for that matter
      mo_view->prepare_display_data(
        EXPORTING
          id_report_name     = ld_repid
          it_technicals      = mo_model->mt_technicals
          it_hidden          = mo_model->mt_hidden
          it_hotspots        = mo_model->mt_hotspots
          it_checkboxes      = mo_model->mt_checkboxes
          it_subtotal_fields = mo_model->mt_subtotal_fields
          it_field_texts     = mo_model->mt_field_texts
          it_user_commands   = mo_model->mt_user_commands
        CHANGING
          ct_data_table      = mo_model->mt_output_data ).
    ENDIF."Are we running in the background?

  ENDMETHOD.                                               "main

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_chap  = is_chap.
    p_vari  = ip_vari.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*
* This is going to be hard coding, through and through
*--------------------------------------------------------------------*
    DEFINE add_listing.
      APPEND VALUE #(
      chapter     = &1
      listing     = &2
      name        = &3
      object_type = &4
      object_name = &5
      subobject   = &6
      )
      TO et_output_data.
    END-OF-DEFINITION.

    add_listing :
*--------------------------------------------------------------------*
* Chapter 03 : ABAP
*--------------------------------------------------------------------*
     3  1 'Listings 3.01 to 3.60'            'CLAS' 'ZCL_CH03_ABAP_EXAMPLES'      'VARIOUS METHODS',
*--------------------------------------------------------------------*
* Chapter 04 : Exception Handling
*--------------------------------------------------------------------*
     4  1 'Handling an Exception Locally'    'PROG' 'ZCH04_EXCEPTION_HANDLING'    'SOMETHING_STATIC',
     4  2 'NO_CHECK Exception'               'PROG' 'ZCH04_EXCEPTION_HANDLING'    'USER_COMMAND',
     4  3 'TRY / CATCH / CLEANUP'            'PROG' 'ZCH04_EXCEPTION_HANDLING'    'TRY_CATCH_BLOCK',
     4  4 'Exporting Vital Information'      'PROG' 'ZCH04_EXCEPTION_HANDLING'    'DO_SOMETHING',
     4  5 'Converting Classical Exceptions'  'PROG' 'ZCH04_EXCEPTION_HANDLING'    'CLASSIC_CONVERSION',
     4  6 'Wrapping a Function Module'       'PROG' 'ZCH04_EXCEPTION_HANDLING'    'WRAPPING_A_FUNCTION',
     4  7 'CLEANUP (Wrong)'                  'PROG' 'ZCH04_EXCEPTION_HANDLING'    'HESD_SWAP_OPERATION_WRONG',
     4  8 'CLEANUP (Right)'                  'PROG' 'ZCH04_EXCEPTION_HANDLING'    'HEAD_SWAP_OPERATION_RIGHT',
     4  9 'CLEANUP (Right#2)'                'PROG' 'ZCH04_EXCEPTION_HANDLING'    'REPLACE_EVERYTHING',
     4 10 'RETRY'                            'PROG' 'ZCH04_EXCEPTION_HANDLING'    'RETRY',
     4 11 'RESUME'                           'PROG' 'ZCH04_EXCEPTION_HANDLING'    'RESUMABLE',
* 4/12 is not in the ABAP language but rather EIFFEL
     4 13 'Design by Contract'               'PROG' 'ZCH04_EXCEPTION_HANDLING'    'OPEN_MONSTERS_EYES',
     4 14 'Building Error Message'           'CLAS' 'ZCL_DBC'                     'ENSURE',
     4 15 'Custom Constraint'                'PROG' 'ZCH04_EXCEPTION_HANDLING'    'LOCAL CLASS DEFINITIONS',
     4 16 'Class Invariant'                  'PROG' 'ZCH04_EXCEPTION_HANDLING'    'OPEN_MONSTERS_EYES',
     4 17 'Exception Class > Short Dump'     'PROG' 'ZCH04_EXCEPTION_HANDLING'    'RAISE_SHORT_DUMP',
*--------------------------------------------------------------------*
* Chapter 05 : TDD / ABAP Unit
*--------------------------------------------------------------------*
     5  1 'Dependencies'                     'PROG' 'ZL05_01_DEPENDENCIES'        'FIRE_NUCLEAR_MISSILE',
     5  2 'Test Seams'                       'PROG' 'ZL05_02_TEST_SEAMS'          'N/A',
     5  3 'Broken Dependencies'              'PROG' 'ZL05_03_BROKEN_DEPENDENCIES' 'FIRE_NUCLEAR_MISSILE',
     5  4 'Test Seams#2'                     'PROG' 'LZCH05_01_02_TEST_SEAMST99'  'FIRE_NUCLEAR_MISSILE',
     5  6 'Helper Classes'                   'CLAS' 'ZCL_MONSTER_SIMULATOR'       'DEFINITION',
     5  7 'Bad Way to Create Objects'        'CLAS' 'ZCL_MONSTER_SIMULATOR'       'CONSTRUCTOR',
     5  8 'Better Way to Create Objects'     'CLAS' 'ZCL_MONSTER_SIMULATOR'       'CONSTRUCTOR',
     5  9 'Best Way to Create Objects'       'CLAS' 'ZCL_MONSTER_SIMULATOR'       'CONSTRUCTOR',
     5 10 'Factory Definition'               'CLAS' 'ZCL_MONSTER_OBJECT_FACTORY'  'DEFINITION',
     5 11 'Injector Example'                 'CLAS' 'ZCL_MONSTER_OBJECT_INJECTOR' 'INJECT_MONSTER_MAKING_MACHINE',
     5 12 'Getting an Instance'              'CLAS' 'ZCL_MONSTER_OBJECT_FACTORY'  'GET_MONSTER_MAKING_MACHINE',
     5 13 'Dependency Lookup Injecction'     'CLAS' 'ZCL_MONSTER_SIMULATOR'       'SETUP',
     5 14 'Defining Test Class'              'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASSES',
     5 15 'Test Class General Settings'      'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 16 'Defining Test Doubles'            'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 17 'Test Class Variables'             'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 18 'Unit Test Methods'                'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 19 'IT SHOULD Methods'                'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 20 'GIVEN / WHEN / THEN'              'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 21 'Implementation of Test Class'     'CLAS' 'ZCL_MONSTER_SIMULATOR'       'RETURN_A_BOM_FOR_A_MONSTER',
     5 22 'SETUP Method Implenatation'       'CLAS' 'ZCL_MONSTER_SIMULATOR'       'SETUP',
     5 23 'Simulating External Input'        'CLAS' 'ZCL_MONSTER_SIMULATOR'       'GIVEN_MONSTER_DETAILS_ENTERED',
     5 24 'Calling Production Code'          'CLAS' 'ZCL_MONSTER_SIMULATOR'       'WHEN_BOM_IS_CALCULATED',
     5 25 'Check for Basic Errors'           'CLAS' 'ZCL_MONSTER_SIMULATOR'       'RETURN_A_BOM_FOR_A_MONSTER',
     5 26 'Using Assertions in Tests'        'CLAS' 'ZCL_MONSTER_SIMULATOR'       'THEN_RESULTING_BOM_IS_CORRECT',
     5 27 'ASSERT_THAT Class Definiton'      'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_DEFINITION',
     5 28 'ASSERT_THAT Class Implementation' 'CLAS' 'ZCL_MONSTER_SIMULATOR'       'LOCAL_TEST_CLASS_IMPLEMENTATION',
     5 29 'ASSERT_THAT In Action'            'CLAS' 'ZCL_MONSTER_SIMULATOR'       'MOCKING_FRAMEWORK_TEST',
     5 36 'ABAP Test Double Framework'       'CLAS' 'ZCL_MONSTER_SIMULATOR'       'MOCKING_FRAMEWORK_TEST',
     5 37 'ATDF Exception Test'              'CLAS' 'ZCL_MONSTER_SIMULATOR'       'MOCKING_EXECEPTION_TEST',
     5 38 'Multiple Test Cases'              'CLAS' 'ZCL_MONSTER_SIMULATOR'       'MOCKUP_LOADER',
*--------------------------------------------------------------------*
* Chapter 06 : Debugger scripting
*--------------------------------------------------------------------*
     6  6 'Laughing Program with Error'      'PROG' 'ZCH06_DEBUGGER_SCRIPTING'    'LAUGH_LIKE_A_MONSTER',
     6  7 'INIT Method'                      'PROG' 'ZDS_LAUGHING_MONSTER'        'INIT',
     6  8 'SCRIPT Method'                    'PROG' 'ZDS_LAUGHING_MONSTER'        'SCRIPT',
     6  9 'END Method'                       'PROG' 'ZDS_LAUGHING_MONSTER'        'END',
     6 10 'Inserting into Test Doubles'      'PROG' 'ZALL_PAIRS'                  'GET_DATA',
*--------------------------------------------------------------------*
* Chapter 07 : Database Programming - ABAP SQL / CDS Views / AMDPs
*--------------------------------------------------------------------*
     7  2 'CASE Statements in SQL'           'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_MONSTER_SCARINESS',
     7  3 'Calculations outside SQL'         'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_SCARY_RATIO_OLD',
     7  4 'Calculations insidee SQL'         'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_SCARY_RATIO_NEW',
     7  5 'Cool WHERE clauses'               'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_VIA_COOL_WHERE_CLAUSES',
     7  6 'String Functions in SQL'          'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_STRINGY_MONSTERS',
     7  7 'IS INITIAL in WHERE Clause'       'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_MONSTER_INITIALS',
     7  8 'Case Insensitive Search'          'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_VILLAGES_BY_DESCRIPTION',
     7  9 'Manual Internal Table'            'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_MANUAL_MONSTERS',
     7 10 'Automatic Internal Table'         'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_AUTOMATIC_MONSTERS',
     7 11 'Existence Check'                  'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_MONSTER_EXISTENCE',
     7 12 'INNER JOIN with Field List'       'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_PETS_THE_HARD_WAY',
     7 13 'INNER JOIN with Wild Card'        'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_PETS_THE_EASY_WAY',
     7 14 'ON CLAUSE Improvements'           'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_VIA_COOL_ON_CLAUSE',
     7 15 'UNION in ABAP'                    'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_UNION_MEMBERSHIP',
     7 16 'CROSS JOIN in ABAP'               'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_MONSTER_VILLAGE_GRID',
     7 17 'INSERT Aggregated Data'           'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_BY_AGGREGATION',
     7 18 'Common Table Expressions 1'       'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_DELS_BY_NO_OF_HEADS',
     7 19 'Common Table Expressions 2'       'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_DELS_BY_NO_OF_HEADS',
     7 20 'OSQL Setup/Teardown'              'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'LOCAL_TEST_METHODS',
     7 21 'OSQL Unit Test'                   'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'LOCAL_TEST_METHODS',
     7 28 'CDS View with Join'               'DDLS' 'ZCDS_MONSTERS_JOIN'            'N/A',
     7 30 'CDS View with Association'        'DDLS' 'ZCDS_MONSTERS_ASSOCIATION'     'N/A',
     7 32 'CDS View with Parameters'         'DDLS' 'ZCDS_MONSTERS_PARAMETERS'      'N/A',
     7 33 'CDS View with Parameters'         'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_INSANE_MONSTERS',
     7 35 'CDS Access Control'               'DCLS' 'ZDCL_MONSTERS_PARAMETERS'      'N/A',
     7 36 'Reading CDS View from ABAP'       'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'DERIVE_GREEN_MONSTERS',
     7 37 'CDS Test Double Setup/Teardown'   'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'LOCAL_TEST_METHODS',
     7 38 'CDS Test Double Unit Test'        'CLAS' 'ZCL_MONSTER_TRANSACTIONS_PERL' 'LOCAL_TEST_METHODS',
*--------------------------------------------------------------------*
* Chapter 08 : BOPF
*--------------------------------------------------------------------*
     8  2 'BOPF P.Layer Constructor'         'CLAS' 'ZCL_MONSTER_MODEL_PERS_BOPF'   'CONSTRUCTOR',
     8  3 'Retrieve Monster Record'          'CLAS' 'ZCL_MONSTER_MODEL_PERS_BOPF'   'RETRIEVE_MONSTER_RECORD',
     8  4 'Getting a BOPF Key'               'CLAS' 'ZCL_MONSTER_MODEL_PERS_BOPF'   'GET_BOPF_KEY_4_MONSTER_NUMBER',
     8  5 'Get Node Row'                     'CLAS' 'ZCL_BC_BOPF_PL_HELPER'         'GET_NODE_ROW',
     8  6 'Get Node Table'                   'CLAS' 'ZCL_BC_BOPF_PL_HELPER'         'GET_NODE_TABLE',
     8  7 'Get Item Table'                   'CLAS' 'ZCL_BC_BOPF_PL_HELPER'         'GET_CHILD_NODE_TABLE',
     8  9 'Determination of Header Texts'    'CLAS' 'ZCL_MONSTER_MODEL'             'FILL_HEADER_DERIVED_FIELDS',
     8 10 'Checking Changed Fields'          'CLAS' 'ZCL_D_FILL_MONSTERHEADER_TEXT' 'CHECK DELTA',
     8 11 'Check Method'                     'CLAS' 'ZCL_D_FILL_MONSTERHEADER_TEXT' 'CHECK',
     8 12 'Execute Determination'            'CLAS' 'ZCL_D_FILL_MONSTERHEADER_TEXT' 'EXECUTE',
     8 14 'Validation - Monster Model'       'CLAS' 'ZCL_MONSTER_MODEL'             'VALIDATE_MONSTER_HEADER',
     8 15 'Validation - BOPF'                'CLAS' 'ZCL_V_MONSTER_HEAD_CONS_CHECK' 'EXECUTE',
     8 16 'Howling Method in Model Class'    'CLAS' 'ZCL_MONSTER_MODEL'             'HOWL_AT_THE_MOON',
     8 17 'Howling Methdod in BOPF'          'CLAS' 'ZCL_A_HOWL_AT_THE_MOON'        'EXECUTE',
     8 18 'Action Validation in Model Class' 'CLAS' 'ZCL_MONSTER_MODEL'             'VALIDATE_HOWL_ACTION',
     8 19 'Action Validation in BOPF'        'CLAS' 'ZCL_V_CHECK_HOWLING_STATUS'    'EXECUTE',
     8 20 'Creating Record in BOPF'          'CLAS' 'ZCL_MONSTER_MODEL_PERS_BOPF'   'CREATE_MONSTER_RECORD',
     8 21 'Change Data in Memory'            'CLAS' 'ZCL_BC_BOPF_PL_HELPER'         'CHANGE_DATA_IN_MEMORY',
     8 22 'Change Data in Database'          'CLAS' 'ZCL_BC_BOPF_PL_HELPER'         'CHANGE_DATA_IN_DATABASE',
     8 23 'Changing Record in BOPF'          'CLAS' 'ZCL_MONSTER_MODEL_PERS_BOPF'   'UPDATE_MONSTER_RECORD',
     8 25 'BUnit Test Defintion'             'CLAS' 'ZCL_A_EXPLODE_ALL_HEADS'       'LOCAL_TEST_CLASSES',
     8 26 'BUnit Test Implementation'        'CLAS' 'ZCL_A_EXPLODE_ALL_HEADS'       'LOCAL_TEST_CLASSES',
*--------------------------------------------------------------------*
* Chapter 09 : BRFplus
*--------------------------------------------------------------------*
     9  2 'Calling BRFplus from ABAP'        'CLAS' 'ZCL_MONSTER_RULES'             'DERIVE_INGREDIENT_TYPE',
*--------------------------------------------------------------------*
* Chapter 10 : SALV
*--------------------------------------------------------------------*
    10  1 'MAIN Method in SALV Reports'      'PROG' 'Z_MONSTER_ADL_IO1'             'MAIN',
    10  2 'Preparing and Displaying Data'    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'PREPARE_DISPLAY_DATA',
    10  3 'Creating SALV Object'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10  4 'Display Basic Toolbar'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'DISPLAY_BASIC_TOOLBAR',
    10  5 'Setting up the Layout'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_LAYOUT',
    10  6 'Setting up Event Handlers'        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_HANDLERS',
    10  7 'Full Initlization Method '        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10  8 'Application Specific Changes'     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'APPLICATION_SPECIFIC_CHANGES',
    10  9 'Set Column Attributes'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_COLUMN_ATTRIBUTES',
    10 10 'Set Checkbox'                     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_CHECKBOX',
    10 11 'Set Hotspot'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_HOTSPOT',
    10 12 'Set Visible'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_VISIBLE',
    10 13 'Set Technical'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_TECHNICAL',
    10 14 'Set Column as Push Button'        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_COLUMN_AS_BUTTON',
    10 15 'Set Sub-Total'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_SUBTOTAL',
    10 16 'Set Long Text'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_LONG_TEXT',
    10 17 'Set Tooltip'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_TOOLTIP',
    10 18 'Adding Sort Criteria'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'ADD_SORT_CRITERIA',
    10 19 'Displaying the SALV Grid'         'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'DISPLAY',
    10 20 'Responding to Double Click'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'HANDLE_LINK_CLICK',
    10 21 'Responding to User Command'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'HANDLE_USER_COMMAND',
    10 22 'Refresh Display'                  'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'REFRESH_DISPLAY',
    10 23 'Create Container Automatically'   'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'CREATE_CONTAINER_PREP_DISPLAY',
    10 24 'Fill Container Content'           'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'FILL_CONTAINER_CONTENT',
    10 25 'Creating SALV with Container'     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10 26 'Adding Custom User Commands'      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'ADD_COMMANDS_TO_TOOLBAR',
    10 27 'Model adding User Commands'       'REPS' 'Z_MONSTER_MONITOR_IO1'         'FILL_USER_COMMANDS',
    10 28 'Enhancing SAP UI Class'           'CLAS' 'CL_SALV_GRID_ADAPTER'          'ZEI_C10_SALV',
    10 29 'Subclassing SALV Model'           'CLAS' 'ZCL_SALV_MODEL'                'CONSTRUCTOR',
    10 30 'Change Initalisation Method'      'CLAS' 'ZCL_BC_VIEW_ALV_TABLE'         'INITIALISE',
    10 31 'Prepare for Grid Refresh Event'   'CLAS' 'ZCL_SALV_MODEL'                'SET_EDITABLE',
    10 32 'Make Fields Editable'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'MAKE_COLUMN_EDITABLE',
    10 33 'Get Underlying Grid Object'       'CLAS' 'ZCL_SALV_MODEL'                'GET_ALV_GRID',
    10 34 'Handle AFTER REFRESH event'       'CLAS' 'ZCL_BC_SALV_EVENT_HANDLER'     'ON_AFTER_REFRESH',
    10 35 'Report Calling SALV + Container'  'REPS' 'Z_MONSTER_MONITOR_IO1'         'MAIN',
    10 36 'User Command ZEDIT (Controller)'  'REPS' 'Z_MONSTER_MONITOR_IO1'         'ON_USER_COMMAND',
    10 37 'Get Underlying Grid Object'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'GET_ALV_GRID_OBJECT',
    10 38 'Making Column Editable (Cont)'    'REPS' 'Z_MONSTER_MONITOR_IO1'         'MAKE_COLUMN_EDITABLE',
    10 39 'SALV IDA Demo - with Criteria'    'REPS' 'Z_MONSTER_MONITOR_IO1'         'IDA_DEMO',
    10 40 'SALV IDA Demo - Basic'            'REPS' 'Z_MONSTER_MONITOR_IO1'         'IDA_DEMO',
    10 41 'SALV IDA Demo - CDS View'         'REPS' 'Z_MONSTER_MONITOR_IO1'         'IDA_DEMO',
*--------------------------------------------------------------------*
* Chapter 11 : Web Dynpro ABAP
*--------------------------------------------------------------------*
    11 01 'Reacting to FIND MONSTERS'        'WDYN' 'ZWDC_MONSTER_MONITOR'          'ONACTIONFIND_MONSTERS',
    11 02 'Controller gets Monster Data'     'WDYN' 'ZWDC_MONSTER_MONITOR'          'RETRIEVE_HEADERS_BY_ATTRIBUTE',
    11 03 'Action to Display Monster'        'WDYN' 'ZWDC_MONSTER_MONITOR'          'ONACTIONSHOW_MONSTER',
    11 04 'Handling Inbound Plug'            'WDYN' 'ZWDC_MONSTER_MONITOR'          'HANDLEIP_FROM_HEADER_TABLE',
    11 05 'Controller Reads Monster Data'    'WDYN' 'ZWDC_MONSTER_MONITOR'          'RETRIEVE_MONSTER',
    11 06 'FPM Feeder Class Defintion'       'CLAS' 'ZCL_FPM_MONSTER_SEARCH_FEEDER' 'GET_DEFINITION',
    11 07 'FPM Feeder Where Clause'          'CLAS' 'ZCL_FPM_MONSTER_SEARCH_FEEDER' 'PROCESS_EVENT',
    11 08 'FPM Feeder Return Result'         'CLAS' 'ZCL_FPM_MONSTER_SEARCH_FEEDER' 'GET_DATA',
    11 09 'WDA Unit Test Setup'              'CLAS' 'ZCL_WDA_MONSTER_TEST_CLASS'    'SETUP',
    11 10 'WDA Unit Test Execution'          'CLAS' 'ZCL_WDA_MONSTER_TEST_CLASS'    'GIVEN_USER_TICKS_BOX'.
*--------------------------------------------------------------------*
* Chapter 12 : UI5
*--------------------------------------------------------------------*
* 30 Listings
*--------------------------------------------------------------------*
* Chapter 13 : Push Channels
*--------------------------------------------------------------------*
* 11 Listings
*--------------------------------------------------------------------*
* Chapter 14 : (RAP) is in a version of ABAP not yet readily available...
*--------------------------------------------------------------------*
  ENDMETHOD.                                               "get_data

ENDCLASS.                    "lcl_persistency_layer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
* When creating the model for real we do not fill the import parameter
* and thus the data is read for real
* When creating the model within a unit test, we pass in a reference to
* the fake database access class
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    IF io_access_class IS SUPPLIED.
      mo_persistency_layer = io_access_class.
    ELSE.
      CREATE OBJECT mo_persistency_layer.
    ENDIF.

    fill_user_commands( ).

    fill_layout_data( ).

    fill_technical_fields( ).

    fill_hidden_fields( ).

    fill_hotspot_fields( ).

    fill_subtotal_fields( ).

    fill_field_texts( ).

    fill_editable_fields( ).

    fill_checkbox_fields( ).

    set_edit_control_field( ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    mo_persistency_layer->get_data( IMPORTING et_output_data = mt_output_data ).

  ENDMETHOD.                                               "data_retrieval

**********************************************************************
* METHOD prepare_data_for_output
**********************************************************************
* Get text names of objects, mapping, etc etc
*----------------------------------------------------------------------*
  METHOD prepare_data_for_ouput.

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.
*--------------------------------------------------------------------*
* Listing 10.27 - Method in the Model Class to Define User Commands
*--------------------------------------------------------------------*
* Local Variables
    DATA: ls_user_commands LIKE LINE OF mt_user_commands.

    REFRESH mt_user_commands.

  ENDMETHOD.                                               "fill_user_commands

  METHOD fill_editable_fields.

  ENDMETHOD.                    "fill_editable_fields

  METHOD fill_hidden_fields.
    "No Hidden Fields
  ENDMETHOD.                    "fill_hidden_fields

  METHOD fill_technical_fields.

  ENDMETHOD.                    "fill_technical_fields

  METHOD fill_hotspot_fields.
    APPEND 'LISTING' TO mt_hotspots.
  ENDMETHOD.                    "fill_hotspot_fields

  METHOD fill_subtotal_fields.
    "No Subtotals
  ENDMETHOD.                    "fill_subtotal_fields

  METHOD fill_field_texts.
    DEFINE add_text.
      APPEND VALUE #(
      field_name = &1
      long_text  = &2
      tooltip    = &3 )
      TO mt_field_texts.
    END-OF-DEFINITION.

    add_text:
    'CHAPTER'     'Chapter'             'Chapter Number',
    'LISTING'     'Listing'             'Listing Number',
    'NAME'        'Listing Description' 'Listing Description',
    'OBJECT_NAME' 'Object Name'         'Program/Class Name',
    'SUBOBJECT'   'Routine Name'        'Routine/Method Name'.

  ENDMETHOD.                    "fill_field_texts

  METHOD fill_checkbox_fields.

  ENDMETHOD.                    "fill_checkbox_fields

  METHOD fill_layout_data.

    ms_layout = VALUE #( list_header       = 'Monster Listing Mapping'
                         variant           = go_selections->p_vari
                         colwidth_optimize = abap_true
                         striped_pattern   = abap_true
                         no_cell_merging   = abap_true ).

  ENDMETHOD.

  METHOD user_command.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF mt_output_data.

    CASE id_user_command.
      WHEN '&IC1'.
        READ TABLE mt_output_data ASSIGNING <ls_output> INDEX id_row.
        CHECK sy-subrc = 0.
        CASE id_column.
          WHEN 'LISTING'.
            "Drill Down
          WHEN OTHERS.
            RETURN.
        ENDCASE."What column was selected for drill down?

      WHEN OTHERS.
        RETURN.
    ENDCASE."What user command was chosen?

  ENDMETHOD."User Command / Model

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
* During the INITIALISATION method this method is called so that
* every row in the output table will be
* changed such that nominated columns have been made editable.
* Now we want to extend this logic to restrict the ability to change
* the task description. If a monster has always been assigned to the task,
* the nature of the task can no longer be changed.
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* ID_EDIT_CONTROL_FIELD Importing Type  LVC_FNAME
* IT_EDITABLE_FIELDS    Importing Type  LVC_T_FNAM
* CT_DATA_TABLE         Changing  Type  ANY TABLE
*--------------------------------------------------------------------*
* Local Variables
    DATA : ls_celltab     TYPE lvc_s_styl,
           lt_celltab     TYPE lvc_t_styl,
           ld_index       TYPE sy-tabix,
           ldo_table_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_table> TYPE any,
                   <ls_celltab>    TYPE lvc_s_styl,
                   <lt_celltab>    TYPE lvc_t_styl,
                   <ld_status>     TYPE zde_monster_order_status.

    super->make_column_editable(
      EXPORTING id_edit_control_field = id_edit_control_field
                it_editable_fields    = it_editable_fields
      CHANGING  ct_data_table         = ct_data_table ).

*--------------------------------------------------------------------*
* Now, when the status is "in progress" gray out the task
* description fields
*--------------------------------------------------------------------*
* Dynamically create work area for looping through the table
* that was passed in
*--------------------------------------------------------------------*
    CREATE DATA ldo_table_line LIKE LINE OF ct_data_table.

    ASSIGN ldo_table_line->*  TO <ls_data_table>.

    LOOP AT ct_data_table ASSIGNING <ls_data_table>.
* Determine the Order Status
      ASSIGN COMPONENT 'ORDER_STATUS' OF STRUCTURE <ls_data_table> TO <ld_status>.
      CHECK sy-subrc = 0.
* Based upon this, alter the CELLTAB nested table, to make the
* cell read only if need be
      CHECK <ld_status> = 'C'."Foul Deed has been Requested
* Orders in this status cannot have the task description changed
      ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <ls_data_table> TO <lt_celltab>.
      CHECK sy-subrc = 0.

      READ TABLE <lt_celltab> ASSIGNING <ls_celltab> WITH KEY fieldname = 'TASK_DESCRIPTION'.

      IF sy-subrc <> 0.
        ld_index             = sy-tabix.
        ls_celltab-fieldname = 'TASK_DESCRIPTION'.
        INSERT ls_celltab INTO <lt_celltab> INDEX ld_index.
        READ TABLE <lt_celltab> ASSIGNING <ls_celltab> WITH KEY fieldname = 'TASK_DESCRIPTION'.
      ENDIF.

      <ls_celltab>-style = cl_gui_alv_grid=>mc_style_disabled."Read Only

    ENDLOOP."Data Table

  ENDMETHOD.                    "application_specific_changes

ENDCLASS.                    "lcl_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_model = io_model.
    mo_view  = io_view.

    "Make the controller react to the views events
    SET HANDLER on_user_command FOR mo_view.
    "If the model changes some data, then it needs to
    "tell the controller, so it can tell the view
    "to refresh the data
    SET HANDLER on_data_changed FOR mo_model.

  ENDMETHOD.                    "constructor



  METHOD on_user_command.
*--------------------------------------------------------------------*
* Listing 10.32 - User Command to Make a SALV Grid Editable
*--------------------------------------------------------------------*
* FOR EVENT added_function OF cl_salv_events
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*
* Local Variables
    DATA: lo_alv    TYPE REF TO cl_gui_alv_grid,
          ls_layout TYPE lvc_s_layo,
          lf_valid  TYPE abap_bool ##needed,
          lt_fcat   TYPE lvc_t_fcat,
          ld_answer TYPE char01,
          ls_stable TYPE lvc_s_stbl.

    mo_model->user_command(
      EXPORTING
        id_user_command = ed_user_command                                              " Function code that PAI triggered
        id_column       = ed_column                                                    " Selected Column
        id_row          = ed_row ).                                                    " Selected Row

    mo_view->refresh_display( ).

  ENDMETHOD."User Command / Controller

  METHOD on_data_changed.

    mo_view->refresh_display( ).

  ENDMETHOD.                                               "on_data_changed

ENDCLASS.                    "lcl_controller IMPLEMENTATION
