interface ZIF_MONSTER_SIMULATOR
  public .


  methods CALCULATE_SCARINESS
    importing
      !IS_BOM_INPUT_DATA type ZST_MONSTER_INPUT_DATA
    returning
      value(RD_SCARINESS) type ZDE_MONSTER_SCARINESS
    raising
      ZCX_VIOLATED_PRECONDITION_STAT .
endinterface.
