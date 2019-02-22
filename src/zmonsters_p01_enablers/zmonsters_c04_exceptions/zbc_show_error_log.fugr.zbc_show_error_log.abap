FUNCTION ZBC_SHOW_ERROR_LOG.
*"--------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     REFERENCE(ID_TITLE) TYPE  STRING OPTIONAL
*"  TABLES
*"      IT_ERROR_LOG STRUCTURE  ZBC_S_ERROR_LOG
*"--------------------------------------------------------------------
zcl_bc_screen_message=>output( EXPORTING id_text = 'Show Error Log'(001) ).

* Make the box a bit taller when the table has lots of rows
  LOOP AT it_error_log.
    IF sy-tabix > 6 AND gd_end_line < 35.
      gd_end_line = gd_end_line + 1.
    ENDIF.
  ENDLOOP.

  PERFORM start_list_viewer.





ENDFUNCTION.
