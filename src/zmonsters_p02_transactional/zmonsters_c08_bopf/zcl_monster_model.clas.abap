class ZCL_MONSTER_MODEL definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_PL type ref to ZIF_MONSTER_MODEL_PERS_LAYER optional
      !IO_RULES type ref to ZCL_MONSTER_RULES optional .
  methods CREATE_MONSTER_RECORD
    importing
      !IT_MONSTER_ITEMS type ZTTYP_MONSTER_ITEMS
      !IS_MONSTER_HEADER type ZSTR_MONSTER_HEADER
    exporting
      !EF_CREATION_SUCCESSFUL type ABAP_BOOL
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods RETRIEVE_MONSTER_RECORD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
      !ID_EDIT_MODE type LRM_CRUD_MODE default 'R'
    exporting
      !ES_MONSTER_HEADER type ZSTR_MONSTER_HEADER
      !ET_MONSTER_ITEMS type ZTTYP_MONSTER_ITEMS
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods UPDATE_MONSTER_RECORD
    importing
      !IT_MONSTER_ITEMS type ZTTYP_MONSTER_ITEMS
      !IS_MONSTER_HEADER type ZSTR_MONSTER_HEADER
    exporting
      !EF_UPDATE_SUCCESSFUL type ABAP_BOOL
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods DELETE_MONSTER_RECORD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods FILL_HEADER_DERIVED_FIELDS
    changing
      !CS_MONSTER_HEADER type ZSTR_MONSTER_HEADER
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods FILL_ITEM_DERIVED_FIELDS
    changing
      !CS_MONSTER_ITEMS type ZSTR_MONSTER_ITEMS .
  class-methods GET_INSTANCE
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RO_MONSTER_MODEL) type ref to ZCL_MONSTER_MODEL
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods IS_DERIVATION_RELEVANT
    importing
      !IT_CHANGED_FIELDS type BAL_T_FLD
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods ARE_VALUES_DERIVATION_RELEVANT
    importing
      !IS_HEADER_VALUES type ZSTR_MONSTER_HEADER
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods VALIDATE_MONSTER_HEADER
    importing
      !IS_HEADER_VALUES type ZSTR_MONSTER_HEADER
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods HOWL_AT_THE_MOON
    importing
      !IS_HOWL_REQUEST type ZSA_HOWL_AT_THE_MOON
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods VALIDATE_HOWL_ACTION
    importing
      !IS_HEADER_VALUES type ZSTR_MONSTER_HEADER
    raising
      ZCX_MONSTER_EXCEPTIONS .
  methods RETRIEVE_HEADERS_BY_ATTRIBUTE
    importing
      !IT_SELECTIONS type ZTT_BC_COSELTAB
    exporting
      !ET_MONSTER_HEADERS type ZTTYP_MONSTER_HEADER .
  class-methods ARM_ALL_MONSTERS .
  methods GET_AHEAD_GET_A_HAT
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    exporting
      !ED_NUMBER_OF_HEADS type SY-TABIX
      !ED_NUMBER_OF_HATS type SY-TABIX .
  methods INVITE_TO_PARTY
    importing
      !ID_MONSTER_NAME type ZDE_MONSTER_NAME
      !ID_PARTY_NAME type STRING .
  methods IS_SCARY
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
  class-methods IS_MAD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
  methods LOG
    importing
      !ID_DATA_OBJECT type ref to DATA .
  methods WANTS_TO_BLOW_UP_WORLD
    returning
      value(RF_YES_IT_DOES) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF m_typ_monster_model,
           monster_number TYPE zde_monster_number,
           monster_model  TYPE REF TO zcl_monster_model,
         END OF m_typ_monster_model .
  types:
    mtt_monster_models TYPE HASHED TABLE OF m_typ_monster_model
        WITH UNIQUE KEY monster_number .

  data MO_PL type ref to ZIF_MONSTER_MODEL_PERS_LAYER .
  data MO_RULES type ref to ZCL_MONSTER_RULES .
  class-data MT_MONSTER_MODELS type MTT_MONSTER_MODELS .
ENDCLASS.



CLASS ZCL_MONSTER_MODEL IMPLEMENTATION.


METHOD are_values_derivation_relevant.

  IF is_header_values-no_of_heads > 0.
    rf_relevant = abap_true.
  ELSE.
    rf_relevant = abap_false.
  ENDIF.

ENDMETHOD.


  METHOD arm_all_monsters.
*--------------------------------------------------------------------*
* Listing 03.06 : Creating Short-Lived Variables
*--------------------------------------------------------------------*
    SELECT *
      FROM ztmonster_header
      INTO TABLE @DATA(all_monsters).

    DATA(iterator) = NEW lcl_weapon_iterator( ).

    DO lines( all_monsters[] ) TIMES.
      DATA(arming_description) = CONV string(
        LET weapon_name  = iterator->get_next_weapon( )
            monster_name = all_monsters[ sy-index ]-name
            date_string  =
           |{ sy-datum+6(2) } / { sy-datum+4(2) } / { sy-datum(4) }|
        IN |Monster { monster_name } was issued a { weapon_name } on { date_string }| ).
      MESSAGE arming_description TYPE 'I'.
    ENDDO.

  ENDMETHOD.


METHOD constructor.

  TRY.
      IF io_pl IS SUPPLIED.
        mo_pl = io_pl.
      ELSE.
        CREATE OBJECT mo_pl TYPE zcl_monster_model_pers_bopf.
      ENDIF.

      IF io_rules IS SUPPLIED.
        mo_rules = io_rules.
      ELSE.
        CREATE OBJECT mo_rules.
      ENDIF.

    CATCH cx_root.
* Raise Fatal Exception
  ENDTRY.

ENDMETHOD.


METHOD create_monster_record.

  mo_pl->create_monster_record(
    EXPORTING
      it_monster_items       = it_monster_items    " Monster Items
      is_monster_header      = is_monster_header    " Monster Header Persistent Structure
    IMPORTING
      ef_creation_successful = ef_creation_successful ).    " Object was Created

ENDMETHOD."Create Monster Record


  METHOD delete_monster_record.
* Monsters do not like being deleted
  ENDMETHOD.


METHOD fill_header_derived_fields.
*--------------------------------------------------------------------*
* Listing 08.09 - Filling Derived Header Fields
*--------------------------------------------------------------------*
* NB - in the book this method starts off using IF statements and
* then changes to using BRF+ rules instead
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
* Fill the Hat Size Description using business rules (BRF+)
*--------------------------------------------------------------------*
  TRY.
      cs_monster_header-hat_size_description =
      mo_rules->derive_hat_size_description( cs_monster_header-hat_size ).

    CATCH zcx_monster_exceptions.
* Fill the Hat Size Description using ABAP Code
      IF cs_monster_header-hat_size > 10.
        cs_monster_header-hat_size_description = 'REALLY BIG HAT'.
      ELSEIF cs_monster_header-hat_size > 5.
        cs_monster_header-hat_size_description = 'BIG HAT'.
      ELSE.
        cs_monster_header-hat_size_description = 'NORMAL HAT'.
      ENDIF.
  ENDTRY.

*--------------------------------------------------------------------*
* Fill the Sanity Description using business rules (BRF+)
*--------------------------------------------------------------------*
  TRY.
  cs_monster_header-sanity_description =
  mo_rules->derive_sanity_description( cs_monster_header-sanity_percentage ).
  CATCH zcx_monster_exceptions.
* Fill the Sanity Description using ABAP Code
  IF cs_monster_header-sanity_percentage > 75.
    cs_monster_header-sanity_description = 'VERY SANE'.
  ELSEIF cs_monster_header-sanity_percentage > 50.
    cs_monster_header-sanity_description = 'SANE'.
  ELSEIF cs_monster_header-sanity_percentage > 25.
    cs_monster_header-sanity_description = 'SLIGHTLY MAD'.
  ELSEIF cs_monster_header-sanity_percentage > 12.
    cs_monster_header-sanity_description = 'VERY MAD'.
  ELSEIF cs_monster_header-sanity_percentage > 1.
    cs_monster_header-sanity_description = 'BONKERS'.
  ELSE.
    cs_monster_header-sanity_description = 'RENAMES SAP PRODUCTS'.
  ENDIF.
  ENDTRY.

* Fill the Ingredient Type using business rules
  cs_monster_header-ingredient_type =
  mo_rules->derive_ingredient_type(
      id_sanity_desired        = cs_monster_header-sanity_percentage
      id_usage_desired         = cs_monster_header-monster_usage
      id_evilness_desired      = cs_monster_header-evilness
      id_scariness_desired     = cs_monster_header-scariness
      id_rages_per_day_desired = cs_monster_header-rages_per_day ).

ENDMETHOD."Fill Header Derived Fields


METHOD fill_item_derived_fields.

  CASE cs_monster_items-part_category.
    WHEN 'HD'.
      cs_monster_items-part_description = 'Head'.
    WHEN 'LG'.
      cs_monster_items-part_description = 'Leg'.
    WHEN 'AR'.
      cs_monster_items-part_description = 'Arm'.
    WHEN 'TA'.
      cs_monster_items-part_description = 'Tail'.
    WHEN 'WI'.
      cs_monster_items-part_description = 'Wing'.
    WHEN 'TN'.
      cs_monster_items-part_description = 'Tentacle'.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDMETHOD.


  method GET_AHEAD_GET_A_HAT.
* Used as ABAP example
  endmethod.


METHOD get_instance.
* Local Variables
  DATA: monster_model_info TYPE m_typ_monster_model.

* Firstly, check buffer, the vampire slayer
  READ TABLE mt_monster_models INTO monster_model_info
  WITH TABLE KEY monster_number = id_monster_number.

  IF sy-subrc = 0.
    ro_monster_model = monster_model_info-monster_model.
    RETURN.
  ENDIF.

* If not in buffer, create new model, and add to buffer
  CREATE OBJECT ro_monster_model.

  monster_model_info-monster_number = id_monster_number.
  monster_model_info-monster_model  = ro_monster_model.
  INSERT monster_model_info INTO TABLE mt_monster_models.

ENDMETHOD.


METHOD howl_at_the_moon.
*--------------------------------------------------------------------*
* Listing 08.16 - Coding the Howl at the Moon Method in the Model Class
*--------------------------------------------------------------------*
  IF is_howl_request-no_of_howls = 0.
    RAISE EXCEPTION TYPE zcx_monster_exceptions.
  ENDIF.

  DO is_howl_request-no_of_howls TIMES.
    MESSAGE 'Ooooooooooooooooooooooooo'(001) TYPE 'I'.
  ENDDO.

ENDMETHOD."Howl at the Moon


  METHOD invite_to_party.
* Another ABAP Example
  ENDMETHOD.


METHOD is_derivation_relevant.

  LOOP AT it_changed_fields INTO DATA(changed_field).
    CHECK changed_field = 'SANITY' OR
          changed_field = 'HAT_SIZE'.
    rf_relevant = abap_true.
    RETURN.
  ENDLOOP.

ENDMETHOD."Is Derivation Relevant


  METHOD is_mad.

    rf_yes_it_is = abap_true.

  ENDMETHOD.


  METHOD is_scary.
* Used for ABAP Example
  ENDMETHOD.


  METHOD log.
* ABAP Example
  ENDMETHOD.


METHOD retrieve_headers_by_attribute.

  mo_pl->retrieve_headers_by_attribute(
    EXPORTING
      it_selections      = it_selections
    IMPORTING
      et_monster_headers = et_monster_headers ).

ENDMETHOD.


METHOD retrieve_monster_record.

  mo_pl->retrieve_monster_record(
    EXPORTING
      id_monster_number = id_monster_number    " Monster  Number
      id_edit_mode      = id_edit_mode
    IMPORTING
      es_monster_header = es_monster_header    " Monster Header Persistent Structure
      et_monster_items  = et_monster_items ).  " Monster Items

ENDMETHOD.                    "Retrieve Monster Record


METHOD update_monster_record.

  mo_pl->update_monster_record(
    EXPORTING
      it_monster_items     = it_monster_items    " Monster Items
      is_monster_header    = is_monster_header    " Monster Header Persistent Structure
    IMPORTING
      ef_update_successful = ef_update_successful ).    " Update was Successful

ENDMETHOD."Update Monster Record


METHOD validate_howl_action.
*--------------------------------------------------------------------*
* Listing 08.18 - Validating the Howl Action in the Monster Model Class
*--------------------------------------------------------------------*
  IF is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_monster_exceptions
      EXPORTING
        textid = zcx_monster_exceptions=>no_head_howling_problem.
  ENDIF.

ENDMETHOD."Validate Howl Action


METHOD validate_monster_header.
*--------------------------------------------------------------------*
* Listing 08.14 - Validation Method in Main Monster Model
*--------------------------------------------------------------------*
  IF is_header_values-hat_size    GT 0 AND
     is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_monster_exceptions
      EXPORTING
        textid = zcx_monster_exceptions=>head_hat_disparity.
  ENDIF.

ENDMETHOD."Validate Monster Header


  METHOD wants_to_blow_up_world.

    "Monsters are not environmentally friendly
    rf_yes_it_does = abap_true.

  ENDMETHOD.
ENDCLASS.
