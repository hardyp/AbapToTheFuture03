interface ZIF_BC_MODEL
  public .


  data MD_SUCCESS_MESSAGE type STRING .
  data MT_USER_COMMANDS type TTB_BUTTON .
  data MD_SELECTED_COLUMN type SALV_DE_COLUMN .
  data MD_SELECTED_ROW type SALV_DE_ROW .
  data MD_EDIT_CONTROL_FIELD type LVC_FNAME .
  data MS_LAYOUT type ZSBC_ALV_LAYOUT .
  data MT_EDITABLE_FIELDS type LVC_T_FNAM .
  data MT_TECHNICALS type LVC_T_FNAM .
  data MT_HIDDEN type LVC_T_FNAM .
  data MT_HOTSPOTS type LVC_T_FNAM .
  data MT_SUBTOTAL_FIELDS type LVC_T_FNAM .
  data MT_CHECKBOXES type LVC_T_FNAM .
  data MT_FIELD_TEXTS type ZTY_BC_ALV_TEXTS .
  data MT_SORT_CRITERIA type ZTY_BC_SORT_CRITERIA .

  events DATA_CHANGED .

  methods FILL_USER_COMMANDS .
  methods USER_COMMAND
    importing
      !ID_USER_COMMAND type SY-UCOMM
      !ID_COLUMN type SALV_DE_COLUMN optional
      !ID_ROW type SALV_DE_ROW optional .
  methods FILL_EDITABLE_FIELDS .
  methods FILL_TECHNICAL_FIELDS .
  methods FILL_HIDDEN_FIELDS .
  methods FILL_HOTSPOT_FIELDS .
  methods FILL_SUBTOTAL_FIELDS .
  methods FILL_FIELD_TEXTS .
  methods FILL_CHECKBOX_FIELDS .
  methods FILL_SORT_CRITERIA .
  methods SET_EDIT_CONTROL_FIELD
    importing
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME default 'CELLTAB' .
  methods FILL_LAYOUT_DATA
    importing
      !ID_LIST_HEADER type LVC_TITLE optional
      !ID_VARIANT type SLIS_VARI optional
      !IF_COLWIDTH_OPTIMIZE type SAP_BOOL default ABAP_TRUE
      !IF_STRIPED_PATTERN type SAP_BOOL default ABAP_TRUE
      !IF_NO_CELL_MERGING type SAP_BOOL default ABAP_TRUE
      !ID_COLOR_FIELDNAME type LVC_FNAME optional .
endinterface.
