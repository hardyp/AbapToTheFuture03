interface ZIF_CREATED_VIA_OCP_FACTORY
  public .


  class-data DEFAULT_CLASS type SEOCLSNAME .

  class-methods IS_THE_RIGHT_CLASS_TYPE_GIVEN
    importing
      !IT_CONTEXT_DATA type WDR_SIMPLE_NAME_VALUE_LIST
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
endinterface.
