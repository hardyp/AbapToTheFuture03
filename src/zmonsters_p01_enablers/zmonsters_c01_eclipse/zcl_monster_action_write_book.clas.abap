CLASS zcl_monster_action_write_book DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">WRITE DESCRIPTION HERE</p>
    "!
    "! @parameter subject | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter pages | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter x_rated | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter book | <p class="shorttext synchronized" lang="en"></p>
    METHODS write_book
      IMPORTING subject TYPE char30
                pages   TYPE i
                x_rated TYPE abap_bool
      RETURNING VALUE(book) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MONSTER_ACTION_WRITE_BOOK IMPLEMENTATION.


  METHOD write_book.
*--------------------------------------------------------------------*
* Figure 01.29 & Figure 01.30 - Word Wrap in Eclipse
*--------------------------------------------------------------------*
* I am by birth a Genevese, and my family is one of the most distinguished of that republic. My ancestors had been for
* many years counsellors and syndics, and my father had filled several public situations with honour and reputation.
* He was respected by all who knew him for his integrity and indefatigable attention to public business.
* He passed his younger days perpetually occupied by the affairs of his country; a variety of circumstances had
* prevented his marrying early, nor was it until the decline of life that he became a husband and the father of a family.
*--------------------------------------------------------------------*
     DATA: monster TYPE i,
           bonster TYPE i,
           lobster TYPE i.

      WRITE:/ 'Hello'.
      monster = 1.
      bonster = 1.
      lobster = monster + bonster.

      WRITE:/ 'Goodbye'(001).
      monster = 1.
      bonster = 1.
      lobster = monster + bonster.

  ENDMETHOD.
ENDCLASS.
