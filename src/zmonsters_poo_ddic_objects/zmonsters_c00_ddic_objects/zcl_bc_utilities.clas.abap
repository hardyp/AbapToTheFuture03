class ZCL_BC_UTILITIES definition
  public
  create public .

public section.

  class-methods ADD_1_IF_TRUE
    importing
      !IF_BOOLEAN type ABAP_BOOL
    returning
      value(RD_RESULT) type SY-TABIX .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_UTILITIES IMPLEMENTATION.


  METHOD add_1_if_true.

    IF if_boolean EQ abap_true.
      rd_result = 1.
    ELSE.
      rd_result = 0.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
