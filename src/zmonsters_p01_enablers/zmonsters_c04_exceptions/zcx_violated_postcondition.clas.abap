class ZCX_VIOLATED_POSTCONDITION definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  constants ZCX_VIOLATED_POSTCONDITION type SOTR_CONC value '005056B074C91ED981A35452159F2110' ##NO_TEXT.
  data MD_CONDITION type ITEX132 .
  data MO_ERROR_LOG type ref to ZCL_BC_MESSAGE_HANDLER .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MD_CONDITION type ITEX132 optional
      !MO_ERROR_LOG type ref to ZCL_BC_MESSAGE_HANDLER optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_VIOLATED_POSTCONDITION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_VIOLATED_POSTCONDITION .
 ENDIF.
me->MD_CONDITION = MD_CONDITION .
me->MO_ERROR_LOG = MO_ERROR_LOG .
  endmethod.
ENDCLASS.
