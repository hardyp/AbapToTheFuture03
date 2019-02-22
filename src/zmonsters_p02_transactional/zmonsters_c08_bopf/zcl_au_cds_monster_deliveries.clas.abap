class ZCL_AU_CDS_MONSTER_DELIVERIES definition
  public
  inheriting from /BOBF/CL_LIB_AUTH_DRAFT_ACTIVE
  final
  create public .

public section.

  methods /BOBF/IF_LIB_AUTH_DRAFT_ACTIVE~CHECK_INSTANCE_AUTHORITY
    redefinition .
  methods /BOBF/IF_LIB_AUTH_DRAFT_ACTIVE~CHECK_STATIC_AUTHORITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AU_CDS_MONSTER_DELIVERIES IMPLEMENTATION.


  method /BOBF/IF_LIB_AUTH_DRAFT_ACTIVE~CHECK_INSTANCE_AUTHORITY.
  endmethod.


  method /BOBF/IF_LIB_AUTH_DRAFT_ACTIVE~CHECK_STATIC_AUTHORITY.
  endmethod.
ENDCLASS.
