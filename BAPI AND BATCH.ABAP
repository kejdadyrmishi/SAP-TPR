REPORT zkd_bapi_batch.

TYPE-POOLS: esp1.

TYPES: BEGIN OF ty_data,
         ebeln        TYPE ebeln,
         ebelp        TYPE ebelp,
         zzxx_comment TYPE char10,
       END OF ty_data.

DATA: gt_data TYPE STANDARD TABLE OF ty_data,
      gt_raw  TYPE truxs_t_text_data.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_file  TYPE localfile OBLIGATORY,
            p_bapi  RADIOBUTTON GROUP rb DEFAULT 'X',
            p_batch RADIOBUTTON GROUP rb.

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_purchase_order_update DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS: execute ABSTRACT.
    CLASS-METHODS get_filename.

  PROTECTED SECTION.

    METHODS: read_excel_data,
      po_nr_check.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_purchase_order_update=>get_filename( ).

CLASS lcl_purchase_order_update IMPLEMENTATION.

  METHOD read_excel_data.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_tab_raw_data       = gt_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = gt_data
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF gt_data IS INITIAL.
      MESSAGE 'File is empty' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD get_filename.

    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'P_FILE'
      IMPORTING
        file_name     = p_file.

  ENDMETHOD.
  METHOD po_nr_check.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      DATA lv_exist TYPE abap_bool.

      SELECT SINGLE @abap_true
        FROM ekko
        INTO @lv_exist
        WHERE ebeln = @<ls_data>-ebeln.

      IF lv_exist = abap_false.
        MESSAGE 'Purchasing Document Number does not exist' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_bapi DEFINITION INHERITING FROM lcl_purchase_order_update.

  PUBLIC SECTION.
    METHODS execute REDEFINITION.

  PRIVATE SECTION.
    METHODS :bapi_change,
      bapi_popup_message,
      function_bapi_po_change.

    DATA: mt_return        TYPE STANDARD TABLE OF bapiret2,
          mt_message       TYPE esp1_message_tab_type,
          ms_message       TYPE esp1_message_wa_type,
          ms_purchaseorder TYPE bapimepoheader-po_number,
          mt_poitem        TYPE STANDARD TABLE OF bapimepoitem,
          mt_poitemx       TYPE STANDARD TABLE OF bapimepoitemx,
          mt_extensionin   TYPE STANDARD TABLE OF bapiparex.

ENDCLASS.

CLASS lcl_bapi IMPLEMENTATION.

  METHOD bapi_change.

    DATA:ls_poitem      TYPE bapimepoitem,
         ls_poitemx     TYPE bapimepoitemx,
         ls_extensionin TYPE bapiparex,
         ls_bapi_te_po  TYPE bapi_te_mepoitem,
         ls_bapi_te_pox TYPE bapi_te_mepoitemx.

    FIELD-SYMBOLS :  <fs_bapi_te_po> TYPE any.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      AT NEW ebeln.

        IF sy-tabix <> 1.

          function_bapi_po_change( ).
          bapi_popup_message( ).

        ENDIF.
        CLEAR : mt_poitem, mt_poitemx , mt_extensionin.
      ENDAT.

      ms_purchaseorder = <ls_data>-ebeln.

      ls_poitem-po_item = <ls_data>-ebelp.

      APPEND ls_poitem TO mt_poitem.

      ls_poitemx-po_item = <ls_data>-ebelp. "TO UPDATE THE INFO
      ls_poitemx-po_itemx = 'X'.

      APPEND ls_poitemx TO mt_poitemx.

      CLEAR: ls_bapi_te_pox.

      ls_bapi_te_pox-po_item = <ls_data>-ebelp.
      ls_bapi_te_pox-zzxx_comment = 'X'.

      CLEAR: ls_bapi_te_po.

      ls_bapi_te_po-po_item = <ls_data>-ebelp.
      ls_bapi_te_po-zzxx_comment = <ls_data>-zzxx_comment.

      ls_extensionin-structure = 'BAPI_TE_MEPOITEM'.
      ASSIGN ls_extensionin-valuepart1 TO <fs_bapi_te_po> CASTING TYPE bapi_te_mepoitem.

      <fs_bapi_te_po> = ls_bapi_te_po.

      APPEND ls_extensionin TO mt_extensionin.
      CLEAR ls_extensionin.

      ls_extensionin-structure = 'BAPI_TE_MEPOITEMX'.
      ASSIGN ls_extensionin-valuepart1 TO <fs_bapi_te_po> CASTING TYPE bapi_te_mepoitemx.

      <fs_bapi_te_po> = ls_bapi_te_pox.

      APPEND ls_extensionin TO mt_extensionin.
      CLEAR ls_extensionin.

      CLEAR : ls_poitem, ls_poitemx , ls_extensionin , mt_return.

      AT LAST.

        function_bapi_po_change( ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDAT.

      bapi_popup_message( ).

    ENDLOOP.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = mt_message.

  ENDMETHOD.
  METHOD bapi_popup_message.

    LOOP AT mt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      IF <ls_return>-type CA 'EAX'.
        DATA(lv_error)  = abap_true.
      ENDIF.

      ms_message-msgty = <ls_return>-type.
      ms_message-msgid = <ls_return>-id.
      ms_message-msgno = <ls_return>-number.
      ms_message-lineno = lines( mt_message ) + 1 .
      ms_message-msgv1 = <ls_return>-message_v1 .
      ms_message-msgv2 = <ls_return>-message_v2 .
      ms_message-msgv3 = <ls_return>-message_v3 .
      ms_message-msgv4 = <ls_return>-message_v4 .
      APPEND ms_message TO mt_message.

    ENDLOOP.
    CLEAR mt_return.

    IF lv_error = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR lv_error.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

  ENDMETHOD.

  METHOD function_bapi_po_change.

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = ms_purchaseorder
      TABLES
        return        = mt_return "WHAT IT IS RETURNING IF IT IS SUCCESS OR ERROR
        poitem        = mt_poitem
        poitemx       = mt_poitemx
        extensionin   = mt_extensionin.

  ENDMETHOD.

  METHOD execute.
    read_excel_data( ).
    bapi_change( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_batch DEFINITION INHERITING FROM lcl_purchase_order_update.

  PUBLIC SECTION.
    METHODS execute REDEFINITION.

  PRIVATE SECTION.
    METHODS: batch_change,
      call_transaction.

    DATA:mt_bdcdata TYPE STANDARD TABLE OF bdcdata, "INSTRUCTIONS FROM THE PROGRAM
         mt_message TYPE esp1_message_tab_type,
         mt_msgbdc  TYPE STANDARD TABLE OF bdcmsgcoll.
ENDCLASS.

CLASS lcl_batch IMPLEMENTATION.

  METHOD batch_change.

    DATA: lt_ebelp   TYPE  STANDARD TABLE OF ebelp.

    mt_bdcdata = VALUE #( ( fnam = 'ME22N' dynbegin = 'T') ).
    SORT gt_data BY ebeln .

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      AT NEW ebeln.
        IF sy-tabix <> 1.

          call_transaction( ).

          CLEAR mt_bdcdata.
        ENDIF.

        CLEAR lt_ebelp.

        SELECT ebelp
          FROM ekpo
          INTO TABLE lt_ebelp
          WHERE ebeln = <ls_data>-ebeln
          ORDER BY ebelp .

        APPEND LINES OF VALUE bdcdata_tab( (  program = 'SAPLMEGUI' dynpro  = '0014' dynbegin = 'X' )
                                           ( fnam = 'BDC_OKCODE' fval = '=MECHOB' )
                                           ( program = 'SAPLMEGUI' dynpro  = '0002' dynbegin = 'X' )
                                           ( fnam = 'BDC_OKCODE'  fval = '=MEOK' )
                                           ( fnam = 'MEPO_SELECT-EBELN'  fval = <ls_data>-ebeln )
                                           ( program = 'SAPLMEGUI' dynpro  = '0014' dynbegin = 'X' )
                                           ( fnam = 'BDC_OKCODE' fval = '=TABIDT22' ) ) TO mt_bdcdata.

      ENDAT.

      READ TABLE lt_ebelp WITH KEY table_line = <ls_data>-ebelp TRANSPORTING NO FIELDS.

      DATA(lv_index) = CONV char4( sy-tabix ).
      lv_index = COND #( WHEN strlen( lv_index ) = 1 THEN |   { lv_index }|
                        WHEN strlen( lv_index ) = 2 THEN |  { lv_index }|
                        WHEN strlen( lv_index ) = 3 THEN | { lv_index }| ).

      APPEND LINES OF VALUE bdcdata_tab( (  program = 'SAPLMEGUI' dynpro  = '0014' dynbegin = 'X' )
                                         ( fnam = 'BDC_OKCODE' fval = '=DDOWN3200' )
                                         ( fnam = 'DYN_6000-LIST' fval = lv_index )
                                         ( program = 'SAPLMEGUI' dynpro  = '0014' dynbegin = 'X' )
                                         ( fnam = 'BDC_OKCODE' fval = '/00' )
                                         ( fnam = 'CI_EKPODB-ZZXX_COMMENT' fval = <ls_data>-zzxx_comment ) ) TO mt_bdcdata.

      AT LAST.

        call_transaction( ).

      ENDAT.
    ENDLOOP.

    LOOP AT mt_msgbdc ASSIGNING FIELD-SYMBOL(<ls_msg>).

      APPEND VALUE #( msgid = <ls_msg>-msgid
                      msgno = <ls_msg>-msgnr
                      msgty = <ls_msg>-msgtyp
                      msgv1 = <ls_msg>-msgv1
                      msgv2 = <ls_msg>-msgv2
                      msgv3 = <ls_msg>-msgv3
                      msgv4 = <ls_msg>-msgv4
                      lineno = lines( mt_message ) + 1  ) TO mt_message.
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = mt_message.

  ENDMETHOD.

  METHOD call_transaction.

    APPEND LINES OF VALUE bdcdata_tab( ( program = 'SAPLMEGUI'   dynpro  = '0014'   dynbegin = 'X')
                                        ( fnam    = 'BDC_OKCODE'  fval    = '=MESAVE')
                                        ( program = 'SAPLSPO2'  dynpro  = '0101'   dynbegin = 'X')
                                        ( fnam    = 'BDC_OKCODE'  fval = '=OPT1')
                                        ( program = 'SAPMSSY0' dynpro = '0120' dynbegin = 'X' )
                                        ( fnam = 'BDC_OKCODE' fval = '=&ONT' )  ) TO mt_bdcdata.

    CALL TRANSACTION 'ME22N' USING mt_bdcdata MODE 'N' UPDATE 'S' MESSAGES INTO mt_msgbdc.
  ENDMETHOD.


  METHOD execute.
    read_excel_data( ).
    batch_change( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CASE abap_true.

    WHEN p_bapi.
      DATA(go_bapi) = NEW lcl_bapi( ).
      go_bapi->execute( ).

    WHEN p_batch.
      DATA(go_batch) = NEW lcl_batch( ).
      go_batch->execute( ).

  ENDCASE.
