*&---------------------------------------------------------------------*
*& REPORT ZSD_TRANS_BANK_ACC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_cont_tech_ivass.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string OBLIGATORY,
              p_doct TYPE blart OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cont_tech_ivass DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :execute,
      constructor.
    CLASS-METHODS value_request.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_items,
             awkey         TYPE awkey,        " PROGRESSIVO SCRITTURA CONTABILE
             gl_account    TYPE hkont,        " COGE
             item_text     TYPE sgtxt,        " TESTO POSIZIONE
             pargb         TYPE pargb,        " TIPO LAVORO/TIPO PORTAFOGLIO/RAMO
             bus_area      TYPE gsber,        " LINEA DI BUSINESS (DANN - VITA)
             segment       TYPE fb_segment,   " LOB
             attribuzione  TYPE dzuonr,       " ATTRIBUZIONE
             par_comp      TYPE rassc,        " SOCIETÀ PARTNER
             int_bank      TYPE hbkid,        " BANCA INTERNA
             account_id    TYPE hktid,        " ID CONTO
             dare_interno  TYPE fins_vhcur12, " DARE IMPORTO CURRENCY INTERNA
             avere_interno TYPE fins_vhcur12, " AVERE IMPORTO CURRENCY INTERNA
             dare_esterno  TYPE fins_vhcur12, " DARE IMPORTO CURRENCY ESTERNA
             avere_esterno TYPE fins_vhcur12, " AVERE IMPORTO CURRENCY ESTERNA
             currency      TYPE waers,        " CURRENCY
           END OF ty_items.

    TYPES: BEGIN OF ty_header,
             expand(1)     TYPE c,
             awkey         TYPE awkey,       " PROGRESSIVO SCRITTURA CONTABILE
             comp_code     TYPE bukrs,       " SOCIETÀ
             ledger        TYPE fins_ledger, " LEDGER
             doc_type      TYPE blart,       " TIPO DOC
             header_txt    TYPE bktxt,       " TESTO TESTATA POSIZIONE
             fisc_year     TYPE gjahr,       " ESERCIZIO
             pstng_date    TYPE budat,       " DATA REGISTRAZIONE
             doc_date      TYPE bldat,       " DATA OPERAZIONE
             hdr_txt_trans TYPE bktxt,
             t_items       TYPE STANDARD TABLE OF ty_items WITH DEFAULT KEY,
           END OF ty_header.

    TYPES: BEGIN OF ty_messages,
             time    TYPE sy-uzeit,
             awkey   TYPE awkey,
             prog_nr TYPE i,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
           END OF ty_messages.

    DATA: mt_items     TYPE STANDARD TABLE OF ty_items,
          mo_hier      TYPE REF TO cl_salv_hierseq_table,
          mt_header    TYPE STANDARD TABLE OF ty_header,
          mt_messages  TYPE STANDARD TABLE OF ty_messages,
          mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mo_cust_cont TYPE REF TO cl_gui_custom_container.

    METHODS separate_data.
    METHODS post_doc IMPORTING iv_test TYPE abap_bool.
    METHODS call_bapi IMPORTING iv_test    TYPE abap_bool
                                is_head    TYPE bapiache09
                                it_glacc   TYPE bapiacgl09_tab
                                it_curre   TYPE bapiaccr09_tab
                                iv_doc     TYPE string
                                iv_time    TYPE sy-uzeit
                      EXPORTING ev_err     TYPE abap_bool
                                ev_obj_key TYPE bapiache09-obj_key.
    METHODS display_alv.
    METHODS on_user_comm FOR EVENT added_function OF cl_salv_events_hierseq IMPORTING e_salv_function.

    METHODS handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS : display_msg,
      adjust_cols_msg.
ENDCLASS.                    "

DATA go_cont_tech_ivass TYPE REF TO lcl_cont_tech_ivass.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cont_tech_ivass IMPLEMENTATION.

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
        repid = 'SAPMSSY0'
        dynnr = '0120'
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

  METHOD execute.
    separate_data( ).
    display_alv( ).

  ENDMETHOD.                    "EXECUTE

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_events    TYPE REF TO cl_salv_events_table.

    IF mt_messages IS INITIAL.
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

    IF mo_salv_msg IS NOT INITIAL.
*      sort_cols_msg( ).
      mo_salv_msg->refresh( ).
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
            EXPORTING
            r_container = mo_dock_msg
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_messages ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    lo_functions = mo_salv_msg->get_functions( ).
    lo_functions->set_all( 'X' ).

    TRY.
        DATA(lv_icon) = icon_close.
        lo_functions->add_function(
          EXPORTING
            name     = 'FC_CLOSE'
            icon     = CONV #( lv_icon )
            tooltip  = 'Close'
            position = if_salv_c_function_position=>right_of_salv_functions
        ).
      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.

    lo_events = mo_salv_msg->get_event( ).
    SET HANDLER handle_logs_ucomm FOR lo_events.

    adjust_cols_msg( ).
*    sort_cols_msg( ).
*    set_tooltips_msg( ).

    mo_salv_msg->display( ).

  ENDMETHOD.

  METHOD adjust_cols_msg.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'TIME' ).
        lo_column->set_key( ).

        DATA(lv_text) = 'Progressivo Scrittura contabile'.
        lo_column ?= lo_cols->get_column( 'AWKEY' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).


        lv_text = 'Progressivo numero'.
        lo_column ?= lo_cols->get_column( 'PROG_NR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = CONV scrtext_s( 'Status' ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'.
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_logs_ucomm.
    CASE e_salv_function.
      WHEN 'FC_CLOSE'.

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

    ENDCASE.
  ENDMETHOD.                    "handle_logs_ucomm

  METHOD separate_data.

    TYPES: BEGIN OF ty_split,
             awkey         TYPE string,
             comp_code     TYPE string,
             ledger        TYPE string,
             doc_type      TYPE string,
             header_txt    TYPE string,
             fisc_year     TYPE string,
             pstng_date    TYPE string,
             doc_date      TYPE string,
             gl_account    TYPE string,
             dare_interno  TYPE string,
             avere_interno TYPE string,
             dare_esterno  TYPE string,
             avere_esterno TYPE string,
             item_text     TYPE string,
             currency      TYPE string,
             pargb         TYPE string,
             bus_area      TYPE string,
             segment       TYPE string,
             attribuzione  TYPE string,
             par_comp      TYPE string,
             int_bank      TYPE string,
             account_id    TYPE string,
           END OF ty_split.

    DATA: lt_strings TYPE STANDARD TABLE OF string,
          ls_split   TYPE ty_split.

    cl_gui_frontend_services=>gui_upload( EXPORTING
                                        filename = p_file
                                        filetype = 'ASC'
                                      CHANGING
                                        data_tab = lt_strings ).

    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 2.


      SPLIT <ls_strings> AT ';' INTO
            ls_split-awkey
            ls_split-comp_code
            ls_split-ledger
            ls_split-doc_type
            ls_split-header_txt
            ls_split-fisc_year
            ls_split-pstng_date
            ls_split-doc_date
            ls_split-gl_account
            ls_split-dare_interno
            ls_split-avere_interno
            ls_split-dare_esterno
            ls_split-avere_esterno
            ls_split-item_text
            ls_split-currency
            ls_split-pargb
            ls_split-bus_area
            ls_split-segment
            ls_split-attribuzione
            ls_split-par_comp
            ls_split-int_bank
            ls_split-account_id.

      ls_split-pstng_date = |{ ls_split-pstng_date+6(4) }{ ls_split-pstng_date+3(2) }{ ls_split-pstng_date(2) }|.
      ls_split-doc_date = |{ ls_split-doc_date+6(4) }{ ls_split-doc_date+3(2) }{ ls_split-doc_date(2) }|.

      TRANSLATE ls_split-dare_interno   USING ',.'.
      TRANSLATE ls_split-avere_interno  USING ',.'.
      TRANSLATE ls_split-avere_esterno  USING ',.'.
      TRANSLATE ls_split-dare_esterno   USING ',.'.

      READ TABLE mt_header ASSIGNING FIELD-SYMBOL(<ls_hdr>) WITH KEY awkey = ls_split-awkey.
      IF sy-subrc <> 0.
        APPEND VALUE #(
        awkey      = ls_split-awkey
        comp_code  = ls_split-comp_code
        ledger     = ls_split-ledger
        doc_type   = ls_split-doc_type
        header_txt = ls_split-header_txt
        fisc_year  = ls_split-fisc_year
        pstng_date = ls_split-pstng_date
        doc_date   = ls_split-doc_date
         ) TO mt_header ASSIGNING <ls_hdr>.
      ENDIF.

      IF ls_split-account_id IS NOT INITIAL AND <ls_hdr>-hdr_txt_trans IS INITIAL.

        SELECT SINGLE concat_with_space( t012t~text1, t012k~bankn ,1 ) AS hdr_txt_trans
          FROM t012k
         LEFT JOIN t012t
           ON t012k~bukrs = t012t~bukrs
          AND t012k~hbkid = t012t~hbkid
          AND t012k~hktid = t012t~hktid
          AND t012t~spras = @sy-langu

          WHERE t012k~bukrs = @<ls_hdr>-comp_code
            AND t012k~hbkid = @ls_split-int_bank
          INTO @<ls_hdr>-hdr_txt_trans.

      ENDIF.

      APPEND CORRESPONDING #( ls_split ) TO mt_items.
      APPEND CORRESPONDING #( ls_split ) TO <ls_hdr>-t_items.

    ENDLOOP.

  ENDMETHOD.                    "EXTRACT_DATA

  METHOD post_doc.

    DATA: lt_selected       TYPE salv_t_row,
          ls_bapi_hdr_cotec TYPE bapiache09,
          ls_bapi_hdr_trans TYPE bapiache09,
          lt_bapi_itm_cotec TYPE TABLE OF bapiacgl09,
          lt_bapi_itm_trans TYPE TABLE OF bapiacgl09,
          lt_bapi_cur_cotec TYPE TABLE OF bapiaccr09,
          lt_bapi_cur_trans TYPE TABLE OF bapiaccr09,
          lv_buzei          TYPE posnr_acc,
          lt_db_item_all    TYPE STANDARD TABLE OF zsd_cont_tech_it,
          lt_db_item        TYPE STANDARD TABLE OF zsd_cont_tech_it,
          lt_db_header      TYPE STANDARD TABLE OF zsd_cont_tech_hd,
          ls_db_header      TYPE zsd_cont_tech_hd,
          lv_indice         TYPE buzei,
          lv_error          TYPE abap_bool,
          lv_obj_key        TYPE  bapiache09-obj_key.

    GET TIME FIELD DATA(lv_time).

    TRY.
        lt_selected = mo_hier->get_selections( level = 1 )->get_selected_rows( ).
      CATCH cx_salv_not_found.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)
    ENDTRY.

    IF lt_selected IS INITIAL.
      MESSAGE 'Please, select at least 1 line to continue!'(002)  TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_selected ASSIGNING FIELD-SYMBOL(<lv_index>).
      READ TABLE mt_header ASSIGNING FIELD-SYMBOL(<ls_hdr>) INDEX <lv_index>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*      SELECT SINGLE @abap_true
*        FROM zsd_cont_tech_hd
*        INTO @DATA(lv_exists)
*        WHERE awkey = @<ls_hdr>-awkey
*          AND bukrs = @<ls_hdr>-comp_code.
      DATA(lv_xblnr) = EXACT bkpf-xblnr( <ls_hdr>-awkey(8) && <ls_hdr>-awkey+10(8) ).
*      SELECT SINGLE @abap_true
*        FROM bkpf
*        INTO @DATA(lv_exists)
*        WHERE xblnr = @lv_xblnr
*          AND bukrs = @<ls_hdr>-comp_code.

*      IF lv_exists =  abap_true.
*
*        APPEND VALUE #( icon    = icon_red_light
*                        message = 'Document already exists!'(005)
*                        awkey   = <ls_hdr>-awkey ) TO mt_messages.
*
*        CLEAR lv_exists.
*        CONTINUE.
*      ENDIF.

      ls_db_header = VALUE #(
          awkey        = <ls_hdr>-awkey
          bukrs        = <ls_hdr>-comp_code
          gjahr        = <ls_hdr>-fisc_year
          fins_ledger  = <ls_hdr>-ledger
          blart_cotec  = <ls_hdr>-doc_type
          blart_gtran  = p_doct
          bktxt        = <ls_hdr>-header_txt
          budat        = <ls_hdr>-pstng_date
          bldat        = <ls_hdr>-doc_date
       ).

      ls_bapi_hdr_cotec = VALUE #(
                    username     = sy-uname
                    comp_code    = <ls_hdr>-comp_code
                    ledger_group = <ls_hdr>-ledger
                    doc_date     = <ls_hdr>-doc_date
                    pstng_date   = <ls_hdr>-pstng_date
                    doc_type     = <ls_hdr>-doc_type
                    fisc_year    = <ls_hdr>-fisc_year
                    header_txt   = <ls_hdr>-header_txt
                    ref_doc_no   = <ls_hdr>-awkey(8) && <ls_hdr>-awkey+10(8)
                              ).

      ls_bapi_hdr_trans = VALUE #(
                    username     = sy-uname
                    comp_code    = <ls_hdr>-comp_code
                    doc_date     = <ls_hdr>-doc_date
                    pstng_date   = <ls_hdr>-pstng_date
                    doc_type     = p_doct
                    fisc_year    = <ls_hdr>-fisc_year
                    header_txt   = <ls_hdr>-header_txt
                    ref_doc_no   = <ls_hdr>-awkey(8) && <ls_hdr>-awkey+10(8)
                              ).

*      SELECT saknr
*        FROM ska1
*        FOR ALL ENTRIES IN @<ls_hdr>-t_items
*        WHERE ktopl             = 'BPMV'
*          AND saknr             = @<ls_hdr>-t_items-gl_account
*          AND glaccount_subtype = 'S'
*        INTO TABLE @DATA(lt_ska1).
*      SORT lt_ska1 BY saknr.

      CLEAR: lv_buzei, lt_db_item.
      LOOP AT <ls_hdr>-t_items ASSIGNING FIELD-SYMBOL(<ls_items>).

        lv_buzei += 1.
        APPEND VALUE bapiacgl09(  itemno_acc      = lv_buzei
                                  gl_account      = <ls_items>-gl_account
                                  comp_code       = <ls_hdr>-comp_code
                                  item_text       = <ls_items>-item_text
                                  tr_part_ba      = <ls_items>-pargb
                                  bus_area        = <ls_items>-bus_area
                                  segment         = <ls_items>-segment
                                  trade_id        = <ls_items>-par_comp
                                  housebankid     = <ls_items>-int_bank
                                  housebankacctid = <ls_items>-account_id
                                  alloc_nmbr = <ls_items>-attribuzione
                                  value_date = sy-datum
                                  ) TO lt_bapi_itm_cotec.

        APPEND INITIAL LINE TO lt_bapi_itm_trans ASSIGNING FIELD-SYMBOL(<ls_bapi_itm_trans>).

        <ls_bapi_itm_trans> = VALUE bapiacgl09(
                itemno_acc      = lv_buzei
                comp_code       = <ls_hdr>-comp_code
                item_text       = <ls_items>-item_text
                tr_part_ba      = <ls_items>-pargb
                bus_area        = <ls_items>-bus_area
                segment         = <ls_items>-segment
                trade_id        = <ls_items>-par_comp
                housebankid     = <ls_items>-int_bank
                housebankacctid = <ls_items>-account_id
                alloc_nmbr      = <ls_items>-attribuzione
                value_date      = sy-datum
                ).

        IF <ls_items>-int_bank IS NOT INITIAL.
          SELECT SINGLE ska1~saknr
            FROM t012k
            JOIN ska1
              ON ska1~ktopl             = 'BPMV'
             AND ska1~main_saknr        = t012k~hkont
             AND ska1~glaccount_subtype = 'S'

            WHERE t012k~bukrs = @<ls_hdr>-comp_code
              AND t012k~hbkid = @<ls_items>-int_bank
              AND t012k~hktid = @<ls_items>-account_id

            INTO @<ls_bapi_itm_trans>-gl_account.

        ELSE.

*          READ TABLE lt_ska1 TRANSPORTING NO FIELDS WITH KEY saknr = <ls_items>-gl_account BINARY SEARCH.
*          IF sy-subrc = 0.
            <ls_bapi_itm_trans>-gl_account      = <ls_items>-gl_account.
*          ENDIF.
        ENDIF.

        APPEND VALUE bapiaccr09(
                         itemno_acc = lv_buzei
                         curr_type  = '00'
                         currency   = <ls_items>-currency
                         amt_doccur = COND #( WHEN <ls_items>-dare_interno IS INITIAL
                                              THEN <ls_items>-avere_interno * -1
                                              ELSE <ls_items>-dare_interno )
                         exch_rate  = COND #( WHEN <ls_items>-currency <> 'EUR' AND  <ls_items>-dare_interno IS NOT INITIAL
                                              THEN <ls_items>-dare_interno / <ls_items>-dare_esterno
                                              WHEN <ls_items>-currency <> 'EUR' AND  <ls_items>-avere_interno IS NOT INITIAL
                                              THEN <ls_items>-avere_interno / <ls_items>-avere_esterno )
                          ) TO lt_bapi_cur_cotec ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).

        APPEND VALUE bapiaccr09(
                         itemno_acc = lv_buzei
                         curr_type  = '00'
                         currency   = <ls_items>-currency
                         amt_doccur = <ls_bapi_accr>-amt_doccur * -1
                         exch_rate  = <ls_bapi_accr>-exch_rate
                          ) TO lt_bapi_cur_trans.

        APPEND VALUE #(
                       mandt          =  sy-mandt
                       awkey          =  <ls_items>-awkey
                       bukrs          =  <ls_hdr>-comp_code
                       buzei          =  lv_buzei
                       hkont          =  <ls_items>-gl_account
                       sgtxt          =  <ls_items>-item_text
                       pargb          =  <ls_items>-pargb
                       gsber          =  <ls_items>-bus_area
                       fb_segment     =  <ls_items>-segment
                       dzuonr         =  <ls_items>-attribuzione
                       rassc          =  <ls_items>-par_comp
                       hbkid          =  <ls_items>-int_bank
                       hktid          =  <ls_items>-account_id
                       dare_interno   =  <ls_items>-dare_interno
                       avere_interno  =  <ls_items>-avere_interno
                       dare_esterno   =  <ls_items>-dare_esterno
                       avere_esterno  =  <ls_items>-avere_esterno
                       waers          =  <ls_items>-currency
                       ) TO lt_db_item.

      ENDLOOP.

      CLEAR: lv_error,
             lv_obj_key.
*             lt_ska1.


      call_bapi(
        EXPORTING
          iv_test  = iv_test
          is_head  = ls_bapi_hdr_cotec
          it_glacc = lt_bapi_itm_cotec
          it_curre = lt_bapi_cur_cotec
          iv_doc   = CONV #( <ls_hdr>-awkey )
          iv_time = lv_time
        IMPORTING
          ev_err     = lv_error
          ev_obj_key = lv_obj_key
      ).

      IF lv_error = abap_true.
        CONTINUE.
      ENDIF.

      ls_db_header-belnr_cotec   = lv_obj_key(10).

      CLEAR lv_obj_key.

      call_bapi(
        EXPORTING
          iv_test     = iv_test
          is_head     = ls_bapi_hdr_trans
          it_glacc    = lt_bapi_itm_trans
          it_curre    = lt_bapi_cur_trans
          iv_doc      = CONV #( <ls_hdr>-awkey )
          iv_time     = lv_time
       IMPORTING
          ev_err      = lv_error
          ev_obj_key  = lv_obj_key
        ).

      IF lv_error = abap_true.
        CONTINUE.
      ENDIF.

      IF iv_test IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      ls_db_header-belnr_gtran = lv_obj_key(10).
      APPEND ls_db_header TO lt_db_header .
      APPEND LINES OF lt_db_item TO lt_db_item_all.

    ENDLOOP.

    IF iv_test IS INITIAL.
*      INSERT zsd_cont_tech_hd FROM TABLE lt_db_header.
*      INSERT zsd_cont_tech_it FROM TABLE lt_db_item_all.
      MESSAGE 'Data saved successfully!'(004) TYPE 'S'.
    ELSE.
      MESSAGE 'Simulated concluded. Please, check the messages!'(006) TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lt_binding TYPE salv_t_hierseq_binding,
          ls_binding TYPE salv_s_hierseq_binding,
          lr_columns TYPE REF TO cl_salv_columns_hierseq,
          lr_level   TYPE REF TO cl_salv_hierseq_level.

    lt_binding = VALUE #( ( master = 'AWKEY'  slave = 'AWKEY') ).

    TRY.
        cl_salv_hierseq_table=>factory(
          EXPORTING
            t_binding_level1_level2  = lt_binding
          IMPORTING
            r_hierseq                = mo_hier
          CHANGING
            t_table_level1           = mt_header
            t_table_level2           = mt_items ).
      CATCH cx_salv_data_error cx_salv_not_found.
    ENDTRY.

    mo_hier->set_screen_status(
      EXPORTING
        report        = sy-repid     " ABAP PROGRAM: CURRENT MAIN PROGRAM
        pfstatus      = 'ZSTANDARD_FULLSCREEN'    " SCREENS, CURRENT GUI STATUS
        set_functions = mo_hier->c_functions_all    " ALV: DATA ELEMENT FOR CONSTANTS
    ).

    TRY.
        mo_hier->get_selections( level = 1 )->set_selection_mode( cl_salv_selections=>if_salv_c_selection_mode~multiple ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        lr_columns = mo_hier->get_columns( 1 ).
        lr_columns->set_optimize( ).

      CATCH cx_salv_not_found.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)
      CATCH cx_salv_data_error.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)

    ENDTRY.

    TRY.
        lr_columns->set_expand_column( 'EXPAND' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_columns = mo_hier->get_columns( 2 ).
        lr_columns->set_optimize( ).

        lr_columns->get_column( columnname = 'AWKEY'  )->set_technical( ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_level = mo_hier->get_level( 1 ).
        lr_level->set_items_expanded( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        DATA(lo_sorts) = mo_hier->get_sorts( level = 1 ).
        DATA(lo_sort) =
            lo_sorts->add_sort(
              EXPORTING
                columnname = 'AWKEY'                           " ALV CONTROL: FIELD NAME OF INTERNAL TABLE FIELD
                position   = 1
                subtotal   = if_salv_c_bool_sap=>true
            ).

      CATCH cx_salv_not_found.  " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_existing.   " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_data_error. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
    ENDTRY.

    TRY.
        DATA(lo_aggr) = mo_hier->get_aggregations( level = 2 ).
        DATA(lo_aggr_col) = lo_aggr->add_aggregation( columnname  = 'DARE_INTERNO' ).
        lo_aggr->add_aggregation( columnname  = 'DARE_ESTERNO' ).
        lo_aggr->add_aggregation( columnname  = 'AVERE_INTERNO' ).
        lo_aggr->add_aggregation( columnname  = 'AVERE_ESTERNO' ).


      CATCH cx_salv_data_error. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_existing.   " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_not_found. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
    ENDTRY.

    SET HANDLER on_user_comm FOR mo_hier->get_event( ).

    mo_hier->display( ).

  ENDMETHOD.

  METHOD value_request.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename      = '*.CSV'
        file_filter           = '*.CSV'
        multiselection        = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

    IF sy-subrc = 0.
      READ TABLE lt_filetable INDEX 1 INTO p_file.
    ENDIF.

  ENDMETHOD.

  METHOD on_user_comm.

    CASE e_salv_function.
      WHEN 'FC_SIMU'.
        post_doc( iv_test  = abap_true ).
      WHEN 'FC_POST'.
        post_doc( iv_test  = abap_false ).

    ENDCASE.

    display_msg( ).
    mo_hier->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.                    "ON_USER_COMM
  METHOD call_bapi.
    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_glacc  TYPE STANDARD TABLE OF bapiacgl09,
          lt_curre  TYPE STANDARD TABLE OF bapiaccr09.

    lt_glacc = it_glacc.
    lt_curre = it_curre.

    IF iv_test = abap_true.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = is_head
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return.


    ELSE.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = is_head
        IMPORTING
          obj_key        = ev_obj_key
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      APPEND VALUE #( time   = iv_time
                      awkey = iv_doc
                      prog_nr = sy-tabix
                      icon = SWITCH #(
                      <ls_return>-type
                      WHEN 'E' THEN icon_red_light
                      WHEN 'S' THEN icon_green_light
                      WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                       ) TO mt_messages.

      IF  <ls_return>-type CA 'EAX'.
        ev_err = abap_true.
      ENDIF.
    ENDLOOP.

    IF ev_err = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      EXIT.
    ENDIF.


  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_cont_tech_ivass=>value_request( ).

START-OF-SELECTION.
  go_cont_tech_ivass = NEW lcl_cont_tech_ivass( ).
  go_cont_tech_ivass->execute( ).
