class ZCL_APC_WSP_EXT_ZAPC_FOR_MONST definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.

  data MO_MONSTER_MODEL type ref to ZCL_MONSTER_MODEL .
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZAPC_FOR_MONST IMPLEMENTATION.


METHOD if_apc_wsp_extension~on_message.
    TRY.
* (1) Decode Incoming Message
        DATA(incoming_text_message) = i_message->get_text( ).

        DATA: order_number TYPE ztmonster_adl-order_number,
              monster_name TYPE ztmonster_am-monster_name.

        SPLIT incoming_text_message AT ';'
        INTO order_number monster_name.

* (2) Get existing order details
        SELECT SINGLE *
          FROM ztmonster_adl
          INTO @DATA(monster_due_list_record)
          WHERE order_number = @order_number.

        CHECK sy-subrc = 0.

* (3) Prepare new "delivery" details
        DATA: monitor_list_record TYPE ztmonster_am.

        MOVE-CORRESPONDING monster_due_list_record TO monitor_list_record.

        monitor_list_record-monster_name    = monster_name.
        monitor_list_record-current_status  = 'A'."Atrocity Ready to be Committed
        monitor_list_record-delivery_number = sy-datum+2(2) &&
                                     sy-datum+4(2) &&
                                     sy-datum+6(2) &&
                                     sy-uzeit(2)   &&
                                     sy-uzeit+2(2).

* (4) Update Delivery and Order Tables
        "Create the Delivery
        MODIFY ztmonster_am FROM monitor_list_record.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          RETURN.
        ENDIF.

        "Now update the order
        UPDATE ztmonster_adl SET   order_status = 'C' "Foul Deed has been Requested
                             WHERE order_number = order_number.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          RETURN.
        ELSE.
* (5) Send confirmation message back to web application via APC
          DATA(message) = i_message_manager->create_message( ).
          message->set_text( 'Horrible, Horrible, Deed has Been Scheduled' ).
          i_message_manager->send( message ).
          COMMIT WORK.
        ENDIF.

* (6) Send message to SAP GUI application via AMC
* Determine message type of the AMC channel
        TRY.
            DATA(amc_dt_manager) = cl_amc_dt_manager=>create( i_application_id = 'ZAMC_FOR_MONSTERS'
                                                              i_channel_id     = '/monsters' ).
            DATA(amc_message_type) = amc_dt_manager->get_message_type( ).
          CATCH cx_amc_dt_error INTO DATA(amc_dt_error).
            MESSAGE amc_dt_error->get_text( ) TYPE 'E'.
        ENDTRY.

        CHECK amc_message_type = 'PCP'.

* Fill Payload
        TRY.
* Create Bottle to Send
* Set extension ID to be the Castle Number.
            DATA(message_bottle) = CAST if_amc_message_producer_pcp(
            cl_amc_channel_manager=>create_message_producer(
              i_application_id       = 'ZAMC_FOR_MONSTERS'
              i_channel_id           = '/monsters'
              i_channel_extension_id = CONV #( monitor_list_record-castle_number ) ) ).
* Create Message for Bottle
            DATA(pcp_message) = cl_ac_message_type_pcp=>create( ).
            pcp_message->set_text( 'A New Atrocity needs to be Committed' ).
            pcp_message->set_field( i_name  = 'Delivery Number'
                                    i_value = CONV #( monitor_list_record-delivery_number ) ).
* Put message in bottle, and throw bottle into the sea
            message_bottle->send( pcp_message ).
          CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
            MESSAGE pcp_error->get_text( ) TYPE 'E'.
          CATCH cx_amc_error INTO DATA(amc_error).
            MESSAGE amc_error->get_text( ) TYPE 'E'.
        ENDTRY.

      CATCH cx_apc_error INTO DATA(apc_error).
        MESSAGE apc_error->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD."On Message


METHOD if_apc_wsp_extension~on_start.

    CREATE OBJECT mo_monster_model.

    TRY.
        DATA(message) = i_message_manager->create_message( ).
        message->set_text( 'Monster Object has been Created!' ).
        i_message_manager->send( message ).

      CATCH cx_apc_error INTO DATA(apc_error).
        MESSAGE apc_error->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD."On Start
ENDCLASS.
