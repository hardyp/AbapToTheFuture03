class ZCX_MONSTER_EXCEPTION_LOP definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ZCX_MONSTER_EXCEPTION_LOP type SOTR_CONC value '005056B074C91ED89FC184F2C45A810F' ##NO_TEXT.
  constants HEAD_ERROR type SOTR_CONC value '005056B074C91ED89FC184F2C45A410F' ##NO_TEXT.
  constants LEGS_ERROR type SOTR_CONC value '005056B074C91ED89FC184F2C45A610F' ##NO_TEXT.
  data SCARINESS type STRING .
  data HEADS type INT4 .
  data ARMS type INT4 .
  data LEGS type INT4 .
  data BOLTS type INT4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !SCARINESS type STRING optional
      !HEADS type INT4 optional
      !ARMS type INT4 optional
      !LEGS type INT4 optional
      !BOLTS type INT4 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MONSTER_EXCEPTION_LOP IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_MONSTER_EXCEPTION_LOP .
 ENDIF.
me->SCARINESS = SCARINESS .
me->HEADS = HEADS .
me->ARMS = ARMS .
me->LEGS = LEGS .
me->BOLTS = BOLTS .
  endmethod.
ENDCLASS.
