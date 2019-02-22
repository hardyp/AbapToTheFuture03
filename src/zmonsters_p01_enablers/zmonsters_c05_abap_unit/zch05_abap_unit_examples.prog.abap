*&---------------------------------------------------------------------*
*& Report ZCH03_ABAP_UNIT_EXAMPLES
*&---------------------------------------------------------------------*
* This program serves as a central access point to the programs and
* classes discussed in Chapter 5 of the book, in regard to
* unit testing in general, and ABAP Unit in particular
* This progra will do nothing at all if you run it, this is just a
* repository for double clicking on the objects to see the sample
* code inside
*&---------------------------------------------------------------------*
REPORT zch05_abap_unit_examples.

START-OF-SELECTION.
*--------------------------------------------------------------------*
* Section 5.1.1 Identifying Dependencies
*--------------------------------------------------------------------*
  SUBMIT zl05_01_dependencies.

*--------------------------------------------------------------------*
* Section 5.1.2 Breaking up Dependencies in an Insane Manner
*--------------------------------------------------------------------*
  SUBMIT zl05_02_test_seams.

*--------------------------------------------------------------------*
* Section 5.1.3 Breaking up Dependencies Properly
*--------------------------------------------------------------------*
  SUBMIT zl05_03_broken_dependencies.

*--------------------------------------------------------------------*
* Section 5.2.3 (Proper) Dependency Injection
*--------------------------------------------------------------------*
* Drill into class ZCL_MONSTER_SIMULATOR to see most of the code
* samples in this chapter
*--------------------------------------------------------------------*
DATA: go_monster_simulator TYPE REF TO zcl_monster_simulator,
      go_pers_layer        TYPE REF TO zcl_monster_sim_pers_layer,
      go_logger            TYPE REF TO zcl_monster_logger.

*--------------------------------------------------------------------*
* Here is an example of creating lots of small objects and then
* injecting them into the main object (Monster Simulator)
* these small objects are the "dependencies" which get injected
* during construction of the main object
*--------------------------------------------------------------------*
CREATE OBJECT go_logger.
CREATE OBJECT go_pers_layer
  EXPORTING
    id_valid_on = sy-datum
    io_logger   = go_logger.
CREATE OBJECT go_monster_simulator
  EXPORTING
    id_creator    = 'BARON_FRANKENSTIEN'   " Mad Scientist
    io_pers_layer = go_pers_layer
    io_logger     = go_logger.
