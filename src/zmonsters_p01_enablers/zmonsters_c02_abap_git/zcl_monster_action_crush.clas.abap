class ZCL_MONSTER_ACTION_CRUSH definition
  public
  create public .

public section.

  methods DO_ACTION
    importing
      !IF_EVERYTHING type BOOLEAN
      !IF_SING type BOOLEAN
      !IF_DANCE type BOOLEAN
      !IF_ARRRIVE_ON_POGO_STICK type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MONSTER_ACTION_CRUSH IMPLEMENTATION.


  METHOD do_action.
*--------------------------------------------------------------------*
* Monster! Animal! Monster! Animal!
*--------------------------------------------------------------------*
    IF if_arrrive_on_pogo_stick EQ abap_true.
      MESSAGE 'Boing! Boing! Here comes the Monster!' TYPE 'I'.
    ENDIF.

    IF if_sing EQ abap_true.
      MESSAGE 'Monster is going to sing a Song' TYPE 'I'.
      DATA(prior_action) = abap_true.
    ENDIF.

    IF if_dance EQ abap_true.
      MESSAGE 'Monster is going to Dance like you Dance when there is no-one around' TYPE 'I'.
      prior_action = abap_true.
    ENDIF.

    IF if_everything EQ abap_true AND
       prior_action  EQ abap_true.
      MESSAGE 'Monster is then going to crush EVERYTHING in its path' TYPE 'I'.
    ELSEIF if_everything EQ abap_true.
      MESSAGE 'Monster is going to crush EVERYTHING in its path' TYPE 'I'.
    ELSEIF prior_action EQ abap_true.
      MESSAGE 'Monster is then just going to play it cool' TYPE 'I'.
    ELSE.
      MESSAGE 'Monster is just going to play it cool' TYPE 'I'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
