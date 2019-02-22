*----------------------------------------------------------------------*
***INCLUDE LZSALV_CSQT_SCREEN_MANAGERO01 .
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  D_0100_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  PERFORM pbo.
ENDMODULE.                    "pbo OUTPUT

*&---------------------------------------------------------------------*
*&      Form  pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo.

  SET PF-STATUS 'D0100'.

  IF gr_container IS INITIAL.
    IF cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false.
      CREATE OBJECT gr_container
        EXPORTING
          container_name = 'CONTAINER'.
    ENDIF.

    SET TITLEBAR 'STANDARD' WITH g_title.

    gr_content_manager->fill_container_content(
        r_container = gr_container ).
  ENDIF.

ENDFORM.                    "pbo


*----------------------------------------------------------------------*
*  MODULE pai INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai INPUT.
  PERFORM pai.
ENDMODULE.                    "pai INPUT


*&---------------------------------------------------------------------*
*&      Form  pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pai.

  DATA: l_okcode LIKE sy-ucomm.

  l_okcode = okcode.
  CLEAR okcode.

  CASE l_okcode.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE PROGRAM.
      CALL METHOD gr_container->free.
      CALL METHOD cl_gui_cfw=>flush.

      CLEAR gr_container.

      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    "pai
