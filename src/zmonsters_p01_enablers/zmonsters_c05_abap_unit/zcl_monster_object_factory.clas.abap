class ZCL_MONSTER_OBJECT_FACTORY definition
  public
  create public

  global friends ZCL_MONSTER_OBJECT_INJECTOR .

public section.

*--------------------------------------------------------------------*
* Listing 05.10: Monster Object Factory Class Definiton
*--------------------------------------------------------------------*
  methods GET_MONSTER_MAKING_MACHINE
    returning
      value(RO_MONSTER_MAKING_MACHINE) type ref to ZIF_MONSTER_MAKING_MACHINE .
  methods GET_PERSISTENCY_LAYER
    returning
      value(RO_PERSISTENCY_LAYER) type ref to ZIF_MONSTER_SIM_PERS_LAYER .
  methods GET_LOGGER
    returning
      value(RO_LOGGER) type ref to ZIF_MONSTER_LOGGER .
  class-methods GET_INSTANCE
    returning
      value(RO_FACTORY) type ref to ZCL_MONSTER_OBJECT_FACTORY .
  PROTECTED SECTION.
private section.

  data MO_MONSTER_MAKING_MACHINE type ref to ZIF_MONSTER_MAKING_MACHINE .
  data MO_PERS_LAYER type ref to ZIF_MONSTER_SIM_PERS_LAYER .
  data MO_LOGGER type ref to ZIF_MONSTER_LOGGER .
  class-data MO_FACTORY type ref to ZCL_MONSTER_OBJECT_FACTORY .
ENDCLASS.



CLASS ZCL_MONSTER_OBJECT_FACTORY IMPLEMENTATION.


  METHOD get_instance.

    IF mo_factory IS NOT BOUND.
      CREATE OBJECT mo_factory.
    ENDIF.

    ro_factory = mo_factory.

  ENDMETHOD.


  METHOD get_logger.

    IF mo_logger IS NOT BOUND.
      "Logic could be really complicated here
      CREATE OBJECT mo_logger TYPE zcl_monster_logger.
    ENDIF.

    ro_logger = mo_logger.

  ENDMETHOD.


  METHOD get_monster_making_machine.
*--------------------------------------------------------------------*
* Listing 05.12: Monster Object Factory Returning an Instance
*--------------------------------------------------------------------*
    IF mo_monster_making_machine IS NOT BOUND.
      "Logic could be really complicated here - all data needed to determine the
      "exact class type can be passed into the GET method an then the so called
      "OPENC-CLOSED factory determines the correct concrete class
      CREATE OBJECT mo_monster_making_machine TYPE zcl_monster_making_machine.
    ENDIF.

    ro_monster_making_machine = mo_monster_making_machine.

  ENDMETHOD.


  METHOD get_persistency_layer.

    IF mo_pers_layer IS NOT BOUND.
      CREATE OBJECT mo_pers_layer TYPE zcl_monster_sim_pers_layer
        EXPORTING
          io_logger   = get_logger( )
          id_valid_on = sy-datum.    " Validaty Date
    ENDIF.

    ro_persistency_layer = mo_pers_layer.

  ENDMETHOD.
ENDCLASS.
