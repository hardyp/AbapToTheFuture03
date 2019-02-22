interface ZIF_BC_CONTROLLER
  public .


  methods ON_USER_COMMAND
    for event USER_COMMAND_RECEIVED of ZIF_BC_ALV_REPORT_VIEW
    importing
      !ED_USER_COMMAND
      !ED_ROW
      !ED_COLUMN .
endinterface.
