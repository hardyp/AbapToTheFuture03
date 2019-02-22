class ZCX_MONSTER_EXCEPTION_MC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_MONSTER_EXCEPTION_MC,
      msgid type symsgid value 'ZVC_MONSTER_ERRORS',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'SCARINESS',
      attr2 type scx_attrname value 'HEADS',
      attr3 type scx_attrname value 'ARMS',
      attr4 type scx_attrname value 'LEGS',
    end of ZCX_MONSTER_EXCEPTION_MC .
  constants:
    begin of HEAD_ERROR,
      msgid type symsgid value 'ZVC_MONSTER_ERRORS',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'HEADS',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HEAD_ERROR .
  data SCARINESS type STRING .
  data HEADS type INT4 .
  data ARMS type INT4 .
  data LEGS type INT4 .
  data BOLTS type INT4 .
  data WIBBLY_WOBBLY_WOOS type I .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !SCARINESS type STRING optional
      !HEADS type INT4 optional
      !ARMS type INT4 optional
      !LEGS type INT4 optional
      !BOLTS type INT4 optional
      !WIBBLY_WOBBLY_WOOS type I optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MONSTER_EXCEPTION_MC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->SCARINESS = SCARINESS .
me->HEADS = HEADS .
me->ARMS = ARMS .
me->LEGS = LEGS .
me->BOLTS = BOLTS .
me->WIBBLY_WOBBLY_WOOS = WIBBLY_WOBBLY_WOOS .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MONSTER_EXCEPTION_MC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
