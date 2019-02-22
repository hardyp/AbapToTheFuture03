class ZCX_MONSTER_EXCEPTIONS definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_MONSTER_EXCEPTIONS,
      msgid type symsgid value 'ZMONSTERS',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MONSTER_EXCEPTIONS .
  constants:
    begin of HEAD_HAT_DISPARITY,
      msgid type symsgid value 'ZMONSTERS',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HEAD_HAT_DISPARITY .
  constants:
    begin of NO_HEAD_HOWLING_PROBLEM,
      msgid type symsgid value 'ZMONSTERS',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_HEAD_HOWLING_PROBLEM .
  data WIBBLY_WOBBLY_WOOS type I read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !WIBBLY_WOBBLY_WOOS type I optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MONSTER_EXCEPTIONS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->WIBBLY_WOBBLY_WOOS = WIBBLY_WOBBLY_WOOS .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MONSTER_EXCEPTIONS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
