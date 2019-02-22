class ZCL_MONSTER_MAKING_MACHINE definition
  public
  create public

  global friends ZCL_MONSTER_OBJECT_FACTORY .

public section.

  interfaces ZIF_MONSTER_MAKING_MACHINE .

  aliases MAKE_MONSTER
    for ZIF_MONSTER_MAKING_MACHINE~MAKE_MONSTER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MONSTER_MAKING_MACHINE IMPLEMENTATION.


  METHOD zif_monster_making_machine~make_monster.

    MESSAGE 'I am Making a Monster' TYPE 'I'.

  ENDMETHOD.
ENDCLASS.
