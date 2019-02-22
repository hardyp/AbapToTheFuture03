class ZCL_MONSTER_OBJECT_INJECTOR definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  methods INJECT_PERS_LAYER
    importing
      !IO_PERS_LAYER type ref to ZIF_MONSTER_SIM_PERS_LAYER .
  methods INJECT_LOGGER
    importing
      !IO_LOGGER type ref to ZIF_MONSTER_LOGGER .
  methods INJECT_MONSTER_MAKING_MACHINE
    importing
      !IO_MONSTER_MAKING_MACHINE type ref to ZIF_MONSTER_MAKING_MACHINE .
protected section.
private section.

  data MO_FACTORY type ref to ZCL_MONSTER_OBJECT_FACTORY .
ENDCLASS.



CLASS ZCL_MONSTER_OBJECT_INJECTOR IMPLEMENTATION.


  METHOD constructor.

    CLEAR zcl_monster_object_factory=>mo_factory.

    mo_factory = zcl_monster_object_factory=>get_instance( ).

  ENDMETHOD.


  METHOD inject_logger.

    mo_factory->mo_logger = io_logger.

  ENDMETHOD.


  METHOD inject_monster_making_machine.
*--------------------------------------------------------------------*
* Listing 05.11: Monster Object Injector
*--------------------------------------------------------------------*
    mo_factory->mo_monster_making_machine = io_monster_making_machine.

  ENDMETHOD.


  METHOD inject_pers_layer.

    mo_factory->mo_pers_layer = io_pers_layer.

  ENDMETHOD.
ENDCLASS.
