FUNCTION ZSALV_CSQT_CREATE_CONTAINER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(R_CONTENT_MANAGER) TYPE REF TO
*"        IF_SALV_CSQT_CONTENT_MANAGER
*"     REFERENCE(TITLE) TYPE  LVC_TITLE
*"----------------------------------------------------------------------

  PERFORM clear_global_variables.

  gr_content_manager = r_content_manager.
  g_title            = title.

  CALL SCREEN 100.

ENDFUNCTION.
