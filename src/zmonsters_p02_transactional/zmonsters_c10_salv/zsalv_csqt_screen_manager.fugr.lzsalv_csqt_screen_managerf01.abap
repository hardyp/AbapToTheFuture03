*----------------------------------------------------------------------*
***INCLUDE LZSALV_CSQT_SCREEN_MANAGERF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CLEAR_GLOBAL_VARIABLES
*&---------------------------------------------------------------------*
FORM clear_global_variables .

   IF gr_container IS BOUND.
    CALL METHOD gr_container->free.
    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.

  CLEAR : okcode,
          gr_content_manager,
          g_title,
          gr_container.

ENDFORM.
