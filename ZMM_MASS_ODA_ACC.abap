*&---------------------------------------------------------------------*
*& Report ZMM_MASS_ODA_ACC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_mass_oda_acc.

PARAMETERS: p_file TYPE string OBLIGATORY.

CLASS lcl_mass_oda DEFINITION.
  PUBLIC SECTION.
    METHODS: exec,
      constructor.

    CLASS-METHODS: get_path.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_excel,
             ebeln      TYPE ekkn-ebeln,
             ebelp      TYPE ekkn-ebelp,
             kostl      TYPE ekkn-kostl,
             ps_psp_pnr TYPE ekkn-ps_psp_pnr,
             aufnr      TYPE ekkn-aufnr,
           END OF ty_excel.

    TYPES: BEGIN OF ty_output.
*             Excel fields
             INCLUDE TYPE ty_excel.

*             Technical fields
    TYPES:   status TYPE icon_d,
           END OF ty_output,
           tt_output TYPE STANDARD TABLE OF ty_output.

    TYPES: BEGIN OF ty_msg,
             time     TYPE sy-uzeit,
             test     TYPE icon_d,
             ebeln    TYPE ekkn-ebeln,
             ebelp    TYPE ekkn-ebelp,
             status   TYPE icon_d,
             msg(220) TYPE c,
           END OF ty_msg.

    TYPES: BEGIN OF ty_ekkn,
             ebeln      TYPE ekkn-ebeln,
             ebelp      TYPE ekkn-ebelp,
             zekkn      TYPE ekkn-zekkn,
             packno     TYPE ekpo-packno,
             delete_ind TYPE abap_bool,
           END OF ty_ekkn.

    DATA: mt_output    TYPE tt_output,
          mt_ekkn      TYPE STANDARD TABLE OF ty_ekkn,
          mt_msg       TYPE STANDARD TABLE OF ty_msg,
          mo_salv      TYPE REF TO cl_salv_table,
          mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mo_cust_cont TYPE REF TO cl_gui_custom_container.

    METHODS:
      upload_file_to_table,
      mass_update IMPORTING iv_test TYPE bapiflag-bapiflag,
      display,
      adjust_columns,
      sort_cols,
      get_updatable_records,
      set_tooltips,
      set_default_tooltips CHANGING co_tooltips TYPE REF TO cl_salv_tooltips,
      show_messages,
      set_tooltips_msg,
      adjust_cols_msg,
      sort_cols_msg,
      add_functionalities,
      bapi_change  IMPORTING iv_from TYPE i
                             iv_test TYPE bapiflag-bapiflag
                             iv_time TYPE sy-uzeit
                   CHANGING  cs_out  TYPE ty_output,
      set_filter_msg IMPORTING is_out TYPE ty_output,
      get_status_icon IMPORTING iv_type TYPE sy-msgty RETURNING VALUE(rv_icon) TYPE icon_d.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

    METHODS:
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column  .
ENDCLASS.

DATA: go_mass_oda TYPE REF TO lcl_mass_oda,
      gv_ok_code  TYPE sy-ucomm.

CLASS lcl_mass_oda IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_cust_cont
      EXPORTING
        container_name              = 'CUSTOM_CONT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_dock_msg
      EXPORTING
        repid = sy-repid
        dynnr = sy-dynnr
        ratio = 50
        side  = cl_gui_docking_container=>dock_at_right.

    mo_dock_msg->set_visible(
      EXPORTING
        visible           = abap_false                 " Visible
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'FC_REAL'.
        mass_update( iv_test = abap_false ).
      WHEN 'FC_TEST'.
        mass_update( iv_test = abap_true ).
    ENDCASE.

  ENDMETHOD.
  METHOD on_link_click.
    READ TABLE mt_output ASSIGNING FIELD-SYMBOL(<ls_out>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'EBELN'.
        SET PARAMETER ID 'BES' FIELD <ls_out>-ebeln.
        CALL TRANSACTION 'ME23N' WITH AUTHORITY-CHECK.
        get_updatable_records( ).
      WHEN 'STATUS'.
        IF <ls_out>-status IS NOT INITIAL.
          set_filter_msg( <ls_out> ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.
  METHOD set_filter_msg.
    DATA(lo_filters) = mo_salv_msg->get_filters( ).

    DATA(lt_filt) = lo_filters->get( ).

    LOOP AT lt_filt ASSIGNING FIELD-SYMBOL(<ls_filt>).
      DATA(lt_selopt_ref) = <ls_filt>-r_filter->get( ).
      DATA(lo_selopt)  = lt_selopt_ref[ 1 ].

      CASE <ls_filt>-columnname.
        WHEN 'EBELN'.
          DATA(lv_ebeln) = lo_selopt->get_low( ).

        WHEN 'EBELP'.
          DATA(lv_ebelp) = lo_selopt->get_low( ).

      ENDCASE.
    ENDLOOP.

    lo_filters->clear( ).

    IF lv_ebeln <> is_out-ebeln OR lv_ebelp <> is_out-ebelp.

      TRY.
          lo_filters->add_filter(
            EXPORTING
              columnname = 'EBELN'
              low        = CONV #( is_out-ebeln )
          ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_data_error.
        CATCH cx_salv_existing.
      ENDTRY.

      TRY.
          lo_filters->add_filter(
            EXPORTING
              columnname = 'EBELP'
              low        = CONV #( is_out-ebelp )
          ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_data_error.
        CATCH cx_salv_existing.
      ENDTRY.
    ENDIF.

    mo_salv_msg->refresh( ).

  ENDMETHOD.
  METHOD show_messages.
    IF mt_msg IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_salv_msg IS NOT INITIAL.
      sort_cols_msg( ).
      mo_salv_msg->refresh( ).
      RETURN.
    ENDIF.

    mo_dock_msg->set_visible(
      EXPORTING
        visible           =  abap_true                " Visible
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
            EXPORTING
            r_container = mo_dock_msg
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_msg ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

*    mo_salv->get_functions( )->set_all( ).
    adjust_cols_msg( ).
    sort_cols_msg( ).
    set_tooltips_msg( ).

    mo_salv_msg->display( ).

  ENDMETHOD.
  METHOD add_functionalities.

    DATA(lo_funct) = mo_salv->get_functions( ).
    lo_funct->set_all( ).

    TRY.
        lo_funct->add_function(
          name     = 'FC_REAL'
          icon     = CONV #( icon_execute_object )
          text     = 'Modalità Update'
          tooltip  = 'Modalità Update'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_existing cx_salv_wrong_call.
    ENDTRY.

    TRY.
        lo_funct->add_function(
          name     = 'FC_TEST'
          icon     = CONV #( icon_test )
          text     = 'Modalità Test'
          tooltip  = 'Modalità Test'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_existing cx_salv_wrong_call.
    ENDTRY.

  ENDMETHOD.
  METHOD get_status_icon.
    rv_icon = SWITCH #( iv_type
                        WHEN 'E' OR 'X' OR 'A' THEN icon_red_light
                        WHEN 'W' THEN icon_yellow_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'I' THEN icon_information ).
  ENDMETHOD.
  METHOD adjust_cols_msg.
    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'TIME' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'TEST' ).
        DATA(lv_text) = CONV scrtext_s( 'Test').
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'EBELN' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'EBELP' ).
        lo_column->set_key( ).

        lv_text = CONV scrtext_s( 'Stato'(005) ).
        lo_column ?= lo_cols->get_column( 'STATUS' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Messaggi'(004).
        lo_column ?= lo_cols->get_column( 'MSG' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.
  METHOD sort_cols_msg.
    DATA: lo_sort TYPE REF TO cl_salv_sorts.
*
*   get Sort object
    lo_sort = mo_salv_msg->get_sorts( ).
    lo_sort->clear( ).
*
*   Set the SORT on the AUART with Subtotal
    TRY.
        lo_sort->add_sort( columnname = 'TIME'  sequence = if_salv_c_sort=>sort_down ).
        lo_sort->add_sort( columnname = 'EBELN' ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

    mo_salv_msg->get_columns( )->set_optimize( ).
    mo_salv_msg->get_filters( )->clear( ).

  ENDMETHOD.
  METHOD adjust_columns.
    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'EBELN' ).
        lo_column->set_key( ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        lo_column ?= lo_cols->get_column( 'EBELP' ).
        lo_column->set_key( ).

        DATA(lv_text) = CONV scrtext_s( 'Stato'(005) ).
        lo_column ?= lo_cols->get_column( 'STATUS' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).
        lo_column->set_key( ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).


      CATCH cx_salv_not_found.
    ENDTRY.

    lo_cols->set_column_position( columnname = 'STATUS'
                                  position   = 1 ).

  ENDMETHOD.
  METHOD display.
    DATA: lx_msg TYPE REF TO cx_salv_msg.

    TRY.
        cl_salv_table=>factory(
            EXPORTING
            r_container = mo_cust_cont
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_output ).
      CATCH cx_salv_msg INTO lx_msg.
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    add_functionalities( ).
    adjust_columns( ).
    sort_cols( ).
    set_tooltips( ).

    mo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    SET HANDLER on_link_click
                on_user_command
                FOR mo_salv->get_event( ).

    mo_salv->display( ).

  ENDMETHOD.
  METHOD sort_cols.
    DATA: lo_sort TYPE REF TO cl_salv_sorts.
*
*   get Sort object
    lo_sort = mo_salv->get_sorts( ).
*
*   Set the SORT on the AUART with Subtotal
    TRY.
        lo_sort->add_sort( columnname = 'EBELN' ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.

  METHOD exec.
    upload_file_to_table( ).
    get_updatable_records( ).
    display( ).
  ENDMETHOD.
  METHOD upload_file_to_table.

    DATA: lt_data      TYPE solix_tab.

    FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_file    " Name of file
        filetype                = 'BIN'    " File Type (ASCII, Binary)
      CHANGING
        data_tab                = lt_data   " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring(
                        it_solix   = lt_data
                    ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name     =  p_file
        xdocument         = lv_bin_data
    ).

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheet)
    ).

    IF lt_worksheet IS INITIAL.
      MESSAGE 'Il programma non è riuscito a trovare i fogli in Excel'(003) TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_table) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).

    ASSIGN lo_worksheet_table->* TO <lt_excel>.

    DELETE <lt_excel> INDEX 1.

    LOOP AT <lt_excel> ASSIGNING FIELD-SYMBOL(<ls_excel>).
      APPEND INITIAL LINE TO mt_output ASSIGNING FIELD-SYMBOL(<ls_out>).
      DO 5 TIMES.
        DATA(lv_index) = sy-index.
        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel> TO FIELD-SYMBOL(<lv_from>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_out> TO FIELD-SYMBOL(<lv_to>).

        CASE lv_index.
          WHEN 4. "WBS
            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
              EXPORTING
                input     = <lv_from>
              IMPORTING
                output    = <lv_to>
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

          WHEN OTHERS.
            <lv_to> = <lv_from>.
        ENDCASE.

      ENDDO.

    ENDLOOP.

    IF mt_output IS INITIAL.
      MESSAGE 'Il file Excel non contiene alcun dato!'(002) TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT mt_output BY ebeln ebelp.

  ENDMETHOD.
  METHOD get_updatable_records.

    SELECT DISTINCT ek~ebeln, ek~ebelp, ek~zekkn, ep~packno,
      CASE WHEN es~packno IS NULL THEN @abap_false
           WHEN es~loekz = @abap_false THEN @abap_false
           ELSE @abap_true
      END AS delete_ind
      FROM @mt_output AS out

      JOIN ekkn AS ek
        ON ek~ebeln = out~ebeln
       AND ek~ebelp = out~ebelp
       AND ek~loekz = @abap_false

      JOIN ekpo AS ep
        ON ep~ebeln = out~ebeln
       AND ep~ebelp = out~ebelp
       AND ep~loekz = @abap_false

       LEFT JOIN eskl AS es
        ON es~packno = ep~packno
       AND es~zekkn  = ek~zekkn

   WHERE NOT EXISTS ( SELECT ebeln
                        FROM ekbe AS eb
                       WHERE eb~ebeln = ek~ebeln
                         AND eb~ebelp = ek~ebelp
                         AND eb~zekkn = ek~zekkn )
        ORDER BY ek~ebeln, ek~ebelp, ek~zekkn
        INTO TABLE @mt_ekkn.

  ENDMETHOD.
  METHOD set_tooltips_msg.
    DATA(lo_tooltips) = mo_salv_msg->get_functional_settings( )->get_tooltips( ).

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_execute_object )
          tooltip = 'Modalità Update' ).                    "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_test )
          tooltip = 'Modalità Test' ).                      "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.


    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_check )
          tooltip = 'Controllo del sistema' ).              "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    set_default_tooltips( CHANGING co_tooltips = lo_tooltips ).

  ENDMETHOD.
  METHOD set_tooltips.

    DATA(lo_tooltips) = mo_salv->get_functional_settings( )->get_tooltips( ).

    set_default_tooltips( CHANGING co_tooltips = lo_tooltips ).
  ENDMETHOD.
  METHOD set_default_tooltips.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_green_light )
          tooltip = 'Successo' ).                           "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_yellow_light )
          tooltip = 'Avvertimento' ).                       "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_red_light )
          tooltip = 'Errore' ).                             "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        co_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_information )
          tooltip = 'Informazione' ).                       "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

  METHOD mass_update.

    DATA(lt_selected) = mo_salv->get_selections( )->get_selected_rows( ).
    IF lt_selected IS INITIAL.
      MESSAGE 'Perfavore, selezionate almeno 1 righa' TYPE 'I'.
      RETURN.
    ENDIF.

    GET TIME FIELD DATA(lv_time).
    DATA(lv_lines) = lines( lt_selected ).
    LOOP AT lt_selected ASSIGNING FIELD-SYMBOL(<lv_sel>).
      DATA(lv_perc) = CONV dec15_4( sy-tabix / lv_lines ) * 100.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = |{ sy-tabix }/{ lv_lines } PO aggiornati|.

      READ TABLE mt_output ASSIGNING FIELD-SYMBOL(<ls_out>) INDEX <lv_sel>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE mt_ekkn TRANSPORTING NO FIELDS
        WITH KEY ebeln = <ls_out>-ebeln
                 ebelp = <ls_out>-ebelp BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_out>-status = get_status_icon( 'E' ).
        APPEND VALUE #( time    = lv_time
                        test    = icon_check
                        ebeln   = <ls_out>-ebeln
                        ebelp   = <ls_out>-ebelp
                        status  = <ls_out>-status
                        msg     = 'Dati oda non modificabili, sviluppi presenti' ) TO mt_msg.

        CONTINUE.
      ENDIF.
      DATA(lv_tabix) = sy-tabix.

      bapi_change( EXPORTING
                      iv_test = iv_test
                      iv_time = lv_time
                      iv_from = lv_tabix
                    CHANGING
                      cs_out  = <ls_out>
                      ).

    ENDLOOP.

    show_messages( ).

    mo_salv->get_columns( )->set_optimize( ).
    mo_salv->refresh( ).

  ENDMETHOD.
  METHOD get_path.
    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             =  cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              =  lt_filetable  " Table Holding Selected Files
        rc                      =  lv_rc   " Return Code, Number of Files or -1 If Error Occurred
        user_action             =  lv_user   " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_filetable IS NOT INITIAL.
      p_file = lt_filetable[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.
  METHOD bapi_change.
*
    DATA: lt_acc     TYPE STANDARD TABLE OF bapimepoaccount,
          lt_accx    TYPE STANDARD TABLE OF bapimepoaccountx,
          lt_return  TYPE STANDARD TABLE OF bapiret2,
          lt_poitem  TYPE STANDARD TABLE OF bapimepoitem,
          lt_poitemx TYPE STANDARD TABLE OF bapimepoitemx,
          lv_zekkn   TYPE numc2.

    LOOP AT mt_ekkn ASSIGNING FIELD-SYMBOL(<ls_ekkn>) FROM iv_from.
      IF <ls_ekkn>-ebeln <> cs_out-ebeln
        OR <ls_ekkn>-ebelp <> cs_out-ebelp.
        EXIT.
      ENDIF.

      lv_zekkn = lv_zekkn + 1.

      IF <ls_ekkn>-delete_ind IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF lt_poitem IS INITIAL AND <ls_ekkn>-packno IS NOT INITIAL.
        lt_poitem = VALUE #( ( po_item = cs_out-ebelp  pckg_no = <ls_ekkn>-packno ) ).
        lt_poitemx = VALUE #( ( po_item = cs_out-ebelp po_itemx = abap_true pckg_no = abap_true  ) ).
      ENDIF.

      APPEND INITIAL LINE TO lt_acc ASSIGNING FIELD-SYMBOL(<ls_acc>).
      <ls_acc> =  VALUE #( po_item     = <ls_ekkn>-ebelp
                           serial_no   = lv_zekkn
                           costcenter  = cs_out-kostl
                           orderid     = cs_out-aufnr
                           ).

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = cs_out-ps_psp_pnr
        IMPORTING
          output = <ls_acc>-wbs_element.

      APPEND VALUE #( po_item     = <ls_ekkn>-ebelp
                      serial_no   = lv_zekkn
                      po_itemx    = abap_true
                      serial_nox  = abap_true
                      costcenter  = COND #( WHEN <ls_acc>-costcenter IS NOT INITIAL THEN abap_true )
                      wbs_element = COND #( WHEN <ls_acc>-wbs_element IS NOT INITIAL THEN abap_true )
                      orderid     = COND #( WHEN <ls_acc>-orderid IS NOT INITIAL THEN abap_true )
                        ) TO lt_accx.
    ENDLOOP.

    CLEAR lt_return.

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = cs_out-ebeln
        testrun       = iv_test
      TABLES
        return        = lt_return
        poitem        = lt_poitem
        poitemx       = lt_poitemx
        poaccount     = lt_acc
        poaccountx    = lt_accx.

    CLEAR: lt_acc, lt_accx.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      APPEND INITIAL LINE TO mt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      <ls_msg> = VALUE #( time   = iv_time
                          test   = SWITCH #( iv_test WHEN abap_true THEN icon_test ELSE icon_execute_object )
                          ebeln  = cs_out-ebeln
                          ebelp  = cs_out-ebelp
                          status = get_status_icon( <ls_return>-type )
                        ).

      MESSAGE ID <ls_return>-id
         TYPE <ls_return>-type
       NUMBER <ls_return>-number
         WITH <ls_return>-message_v1
              <ls_return>-message_v2
              <ls_return>-message_v3
              <ls_return>-message_v4
              INTO <ls_msg>-msg.

      IF <ls_return>-type CA 'EAX'.
        DATA(lv_error) = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_error IS NOT INITIAL.
      CLEAR lv_error.
      cs_out-status = icon_red_light.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RETURN.
    ENDIF.

    IF line_exists( lt_return[ type = 'W' ] ).
      cs_out-status = icon_yellow_light.
    ELSE.
      cs_out-status = icon_green_light.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_mass_oda=>get_path( ).

START-OF-SELECTION.
  CALL SCREEN 100.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'MAIN'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT OUTPUT
*&---------------------------------------------------------------------*
MODULE init OUTPUT.
  IF go_mass_oda IS NOT INITIAL.
    RETURN.
  ENDIF.

  go_mass_oda = NEW lcl_mass_oda( ).
  go_mass_oda->exec( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
