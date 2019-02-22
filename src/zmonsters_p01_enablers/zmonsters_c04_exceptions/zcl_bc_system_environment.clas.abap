class ZCL_BC_SYSTEM_ENVIRONMENT definition
  public
  create public .

public section.

  constants MC_PRODUCTION type T000-CCCATEGORY value 'P' ##NO_TEXT.

  class-methods IS_PRODUCTION
    returning
      value(RF_IS_PRODUCTION) type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_SYSTEM_ENVIRONMENT IMPLEMENTATION.


METHOD is_production.
* Local Variables
  DATA: ld_client_function TYPE t000-cccategory.

* T000 - Fully Buffered
  SELECT SINGLE cccategory
    FROM  t000
    INTO  ld_client_function
    WHERE mandt EQ sy-mandt.

  IF ld_client_function = mc_production.
    rf_is_production = abap_true.
  ELSE.
    rf_is_production = abap_false.
  ENDIF.

ENDMETHOD.
ENDCLASS.
