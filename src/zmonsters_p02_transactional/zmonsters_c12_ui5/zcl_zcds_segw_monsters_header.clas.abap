class ZCL_ZCDS_SEGW_MONSTERS_HEADER definition
  public
  inheriting from CL_SADL_GTK_EXPOSURE_MPC
  final
  create public .

public section.
protected section.

  methods GET_PATHS
    redefinition .
  methods GET_TIMESTAMP
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZCDS_SEGW_MONSTERS_HEADER IMPLEMENTATION.


  method GET_PATHS.
et_paths = VALUE #(
( |CDS~ZCDS_SEGW_MONSTERS_HEADER| )
).
  endmethod.


  method GET_TIMESTAMP.
RV_TIMESTAMP = 20180813155450.
  endmethod.
ENDCLASS.
