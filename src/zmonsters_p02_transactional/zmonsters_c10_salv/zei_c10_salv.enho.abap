CLASS lcl_zei_c10_salv DEFINITION DEFERRED.
CLASS cl_salv_grid_adapter DEFINITION LOCAL FRIENDS lcl_zei_c10_salv.
CLASS lcl_zei_c10_salv DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zei_c10_salv.            "#EC NEEDED
    DATA core_object TYPE REF TO cl_salv_grid_adapter .     "#EC NEEDED
 INTERFACES  IPO_ZEI_C10_SALV.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_salv_grid_adapter OPTIONAL.
ENDCLASS.
CLASS lcl_zei_c10_salv IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD ipo_zei_c10_salv~build_uifunction.
*"------------------------------------------------------------------------*
*" Declaration of POST-method, do not insert any comments here please!
*"
*"methods BUILD_UIFUNCTION
*"  changing
*"    !T_TOOLBAR_EXCLUDING type UI_FUNCTIONS
*"    !T_TOOLBAR type TTB_BUTTON .
*"------------------------------------------------------------------------*
* Listing 10.28 : Post Exit on Standard SAP Class
*-------------------------------------------------------------------------*
    LOOP AT t_toolbar ASSIGNING FIELD-SYMBOL(<cs_toolbar_item>)
      WHERE function CS cl_gui_alv_grid=>mc_fc_separator .

      <cs_toolbar_item>-butn_type = '3'."It's a seperator!

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
