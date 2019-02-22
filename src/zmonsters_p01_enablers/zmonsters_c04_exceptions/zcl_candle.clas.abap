class ZCL_CANDLE definition
  public
  create public .

public section.

  methods LIGHT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CANDLE IMPLEMENTATION.


METHOD light.

  MESSAGE 'Lighting Candle' TYPE 'I'.

ENDMETHOD.
ENDCLASS.
