*&---------------------------------------------------------------------*
*& REPORT ZSD_TRANS_BANK_ACC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_cont_tech_ivass.

TABLES zsd_cont_tech_hd.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-015.
  PARAMETERS: r1 RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', "register directly
              r2 RADIOBUTTON GROUP rb, " view and register
              r3 RADIOBUTTON GROUP rb. "view history
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string DEFAULT 'C:\Users\Perdorues\OneDrive\Desktop\Input per Cruscotto.csv' MODIF ID bl1,
              p_doct TYPE blart DEFAULT 'B2' MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-016.
  SELECT-OPTIONS : s_awkey FOR zsd_cont_tech_hd-awkey MODIF ID bl2,
                   s_bukrs FOR zsd_cont_tech_hd-bukrs MODIF ID bl2,
                   s_gjahr FOR zsd_cont_tech_hd-gjahr MODIF ID bl2,
                   s_budat FOR zsd_cont_tech_hd-budat MODIF ID bl2,
                   s_bldat FOR zsd_cont_tech_hd-bldat MODIF ID bl2,
                   s_blart FOR zsd_cont_tech_hd-blart MODIF ID bl2.

SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_cont_tech_ivass DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :execute.
    CLASS-METHODS value_request.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_items,
             status       TYPE icon-id,
             belnr        TYPE belnr_d,
             awkey        TYPE awkey,        " PROGRESSIVO SCRITTURA CONTABILE
             buzei        TYPE buzei,
             gl_account   TYPE hkont,        " COGE
             item_text    TYPE sgtxt,        " TESTO POSIZIONE
             pargb        TYPE pargb,        " TIPO LAVORO/TIPO PORTAFOGLIO/RAMO
             bus_area     TYPE gsber,        " LINEA DI BUSINESS (DANN - VITA)
             segment      TYPE fb_segment,   " LOB
             attribuzione TYPE dzuonr,       " ATTRIBUZIONE
             par_comp     TYPE rassc,        " SOCIETÀ PARTNER
             int_bank     TYPE hbkid,        " BANCA INTERNA
             account_id   TYPE hktid,        " ID CONTO
             hsl          TYPE acdoca-hsl, " DARE IMPORTO CURRENCY INTERNA
             tsl          TYPE acdoca-tsl, " AVERE IMPORTO CURRENCY INTERNA
             currency     TYPE waers,        " CURRENCY
           END OF ty_items.

    TYPES: BEGIN OF ty_header,
             expand(1)  TYPE c,
             status     TYPE icon-id,
             belnr      TYPE belnr_d,
             awkey      TYPE awkey,       " PROGRESSIVO SCRITTURA CONTABILE
             comp_code  TYPE bukrs,       " SOCIETÀ
             ledger     TYPE fins_ledger, " LEDGER
             doc_type   TYPE blart,       " TIPO DOC
             header_txt TYPE bktxt,       " TESTO TESTATA POSIZIONE
             fisc_year  TYPE gjahr,       " ESERCIZIO
             pstng_date TYPE budat,       " DATA REGISTRAZIONE
             doc_date   TYPE bldat,       " DATA OPERAZIONE
             t_items    TYPE STANDARD TABLE OF ty_items WITH DEFAULT KEY,
           END OF ty_header.

    TYPES: BEGIN OF ty_messages,
             time    TYPE sy-uzeit,
             awkey   TYPE awkey,
             buzei   TYPE buzei,
             prog_nr TYPE i,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
             belnr   TYPE belnr_d,
           END OF ty_messages.

    DATA: mt_items    TYPE STANDARD TABLE OF ty_items,
          mo_hier     TYPE REF TO cl_salv_hierseq_table,
          mt_header   TYPE STANDARD TABLE OF ty_header,
          mt_messages TYPE STANDARD TABLE OF ty_messages,
          mo_salv_msg TYPE REF TO cl_salv_table,
          mo_dock_msg TYPE REF TO cl_gui_docking_container.

    METHODS csv_to_table.
    METHODS post_doc IMPORTING iv_test TYPE abap_bool OPTIONAL
                               iv_job  TYPE abap_bool OPTIONAL.

    METHODS call_bapi IMPORTING iv_test    TYPE abap_bool
                                is_head    TYPE bapiache09
                                it_glacc   TYPE bapiacgl09_tab
                                it_curre   TYPE bapiaccr09_tab
                                iv_buzei   TYPE buzei
                                iv_doc     TYPE string
                      EXPORTING ev_err     TYPE abap_bool
                                ev_obj_key TYPE bapiache09-obj_key.
    METHODS display_alv.
    METHODS get_historic_data.
    METHODS download_excel.
    METHODS on_user_comm FOR EVENT added_function OF cl_salv_events_hierseq IMPORTING e_salv_function.
    METHODS on_hier_link_click FOR EVENT link_click OF cl_salv_events_hierseq
      IMPORTING level
                row
                column.

    METHODS handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS : display_msg IMPORTING iv_no_dock TYPE abap_bool OPTIONAL,
      adjust_cols_msg.

    METHODS: on_msg_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column  .

    METHODS view_belnr IMPORTING iv_belnr TYPE belnr_d
                                 iv_awkey TYPE awkey.
ENDCLASS.                    "

DATA go_cont_tech_ivass TYPE REF TO lcl_cont_tech_ivass.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cont_tech_ivass IMPLEMENTATION.

  METHOD execute.

    CASE abap_true.
      WHEN r1 OR r2.

        IF p_file IS INITIAL OR p_doct IS INITIAL.
          MESSAGE 'Please, fill obligatory fields' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        csv_to_table( ).

        CASE abap_true.
          WHEN r1.
            post_doc( iv_job  = abap_true ).
            display_msg( iv_no_dock = abap_true ).
          WHEN r2.
            display_alv( ).
        ENDCASE.

      WHEN r3.
        get_historic_data( ).
        display_alv( ).
    ENDCASE.

  ENDMETHOD.                    "EXECUTE

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_events    TYPE REF TO cl_salv_events_table.

    IF mt_messages IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_no_dock IS INITIAL.

      IF mo_salv_msg IS NOT INITIAL.
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

        mo_salv_msg->refresh( ).
        RETURN.
      ENDIF.

      CREATE OBJECT mo_dock_msg
        EXPORTING
          repid = 'SAPMSSY0'
          dynnr = '0120'
          ratio = 50
          side  = cl_gui_docking_container=>dock_at_right.

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

    ELSE.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv_msg
            CHANGING
              t_table      = mt_messages ).
        CATCH cx_salv_msg INTO lx_msg.
          MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDTRY.

      mo_salv_msg->get_functions( )->set_all( 'X' ).

    ENDIF.

    adjust_cols_msg( ).

    lo_events = mo_salv_msg->get_event( ).
    SET HANDLER handle_logs_ucomm
                on_msg_link_click
           FOR lo_events.


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

        lo_column ?= lo_cols->get_column( 'BUZEI' ).
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

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).
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

  METHOD csv_to_table.

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

      APPEND INITIAL LINE TO <ls_hdr>-t_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item> = VALUE #(
                    awkey        = ls_split-awkey
                    buzei        = lines( <ls_hdr>-t_items )
                    gl_account   = ls_split-gl_account
                    item_text    = ls_split-item_text
                    pargb        = ls_split-pargb
                    bus_area     = ls_split-bus_area
                    segment      = ls_split-segment
                    attribuzione = ls_split-attribuzione
                    par_comp     = ls_split-par_comp
                    int_bank     = ls_split-int_bank
                    account_id   = ls_split-account_id
                    hsl          = COND #( WHEN ls_split-dare_interno <> 0 THEN -1 * ls_split-dare_interno ELSE ls_split-avere_interno )
                    tsl          = COND #( WHEN ls_split-dare_esterno <> 0 THEN -1 * ls_split-dare_interno ELSE ls_split-avere_esterno )
                    currency     = ls_split-currency
                    ).

      APPEND <ls_item> TO mt_items.

    ENDLOOP.

  ENDMETHOD.                    "EXTRACT_DATA

  METHOD post_doc.
    TYPES: BEGIN OF ty_post_bapi,
             buzei  TYPE buzei,
             header TYPE bapiache09,
             items  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
             currc  TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
           END OF ty_post_bapi.

    DATA: lt_selected    TYPE salv_t_row,
          lt_db_item_all TYPE STANDARD TABLE OF zsd_cont_tech_it,
          lt_db_item     TYPE STANDARD TABLE OF zsd_cont_tech_it,
          lt_db_header   TYPE STANDARD TABLE OF zsd_cont_tech_hd,
          ls_db_header   TYPE zsd_cont_tech_hd,
          ls_db_item     TYPE zsd_cont_tech_it,
          lv_error       TYPE abap_bool,
          lv_obj_key     TYPE bapiache09-obj_key,
          lt_post_bapi   TYPE STANDARD TABLE OF ty_post_bapi.

    IF iv_job IS INITIAL.
      TRY.
          lt_selected = mo_hier->get_selections( level = 1 )->get_selected_rows( ).
        CATCH cx_salv_not_found.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)
      ENDTRY.

      IF lt_selected IS INITIAL.
        MESSAGE 'Please, select at least 1 line to continue!'(002)  TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ELSE.
      DO lines( mt_header ) TIMES.
        APPEND sy-index TO lt_selected.
      ENDDO.
    ENDIF.

    LOOP AT lt_selected ASSIGNING FIELD-SYMBOL(<lv_index>).
      READ TABLE mt_header ASSIGNING FIELD-SYMBOL(<ls_hdr>) INDEX <lv_index>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*      SELECT SINGLE @abap_true
*        FROM zsd_cont_tech_hd
*        INTO @DATA(lv_exists)
*        WHERE awkey = @<ls_hdr>-awkey.

      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post_bapi_main>).
      <ls_post_bapi_main>-header = VALUE #(
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

      SELECT SINGLE ktopl
        INTO @DATA(lv_ktopl)
        FROM t001
        WHERE bukrs = @<ls_hdr>-comp_code.

      SELECT parent~saknr AS act_saknr,
             child~saknr  AS opp_saknr
        FROM ska1 AS parent
        JOIN ska1 AS child
          ON parent~main_saknr        = child~main_saknr
         AND parent~glaccount_subtype = child~glaccount_subtype
         AND parent~ktopl             = child~ktopl
         AND parent~xbilk             = child~xbilk
         AND parent~glaccount_type    = child~glaccount_type
        FOR ALL ENTRIES IN @<ls_hdr>-t_items
        WHERE parent~ktopl             = @lv_ktopl
          AND parent~saknr             = @<ls_hdr>-t_items-gl_account
          AND child~saknr             <> @<ls_hdr>-t_items-gl_account
          AND parent~glaccount_subtype = 'S'
          AND parent~xbilk             = 'X'
          AND parent~glaccount_type    = 'C'
        INTO TABLE @DATA(lt_ska1).
      SORT lt_ska1 BY act_saknr.

      CLEAR: lt_db_item.
      LOOP AT <ls_hdr>-t_items ASSIGNING FIELD-SYMBOL(<ls_items>).
        DATA(lv_tabix) = sy-tabix.

        APPEND VALUE bapiacgl09(  itemno_acc      = <ls_items>-buzei
                                  gl_account      = <ls_items>-gl_account
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
                                  ) TO <ls_post_bapi_main>-items.

        APPEND VALUE bapiaccr09(
                         itemno_acc = <ls_items>-buzei
                         curr_type  = '00'
                         currency   = <ls_items>-currency
                         amt_doccur = <ls_items>-hsl
                         exch_rate  = COND #( WHEN <ls_items>-currency <> 'EUR' THEN <ls_items>-hsl / <ls_items>-tsl
                                              ELSE 1 )
                          ) TO <ls_post_bapi_main>-currc ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).


        IF <ls_items>-int_bank IS INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE lt_ska1 ASSIGNING FIELD-SYMBOL(<ls_ska1>)
          WITH KEY act_saknr = <ls_items>-gl_account BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post>).
        <ls_post>-buzei  = <ls_items>-buzei.
        <ls_post>-header =  VALUE #(
                    username     = sy-uname
                    comp_code    = <ls_hdr>-comp_code
                    doc_date     = <ls_hdr>-doc_date
                    pstng_date   = <ls_hdr>-pstng_date
                    doc_type     = p_doct
                    fisc_year    = <ls_hdr>-fisc_year
                    header_txt   = <ls_hdr>-header_txt
                    ref_doc_no   = <ls_hdr>-awkey(8) && <ls_hdr>-awkey+10(8)
                              ).
        <ls_post>-items = VALUE #(

        (  itemno_acc      = 1
           gl_account      = <ls_items>-gl_account
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
         )
         ( itemno_acc      = 2
           gl_account      = <ls_ska1>-opp_saknr
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
         ) ).

        <ls_post>-currc = VALUE #(
        (
          itemno_acc = 1
          curr_type  = '00'
          currency   = <ls_items>-currency
          amt_doccur = <ls_bapi_accr>-amt_doccur * -1
          exch_rate  = <ls_bapi_accr>-exch_rate
        )
        (
          itemno_acc = 2
          curr_type  = '00'
          currency   = <ls_items>-currency
          amt_doccur = <ls_bapi_accr>-amt_doccur
          exch_rate  = <ls_bapi_accr>-exch_rate
        ) ).

      ENDLOOP.

      DATA(lv_no_commit) = abap_false.
      LOOP AT lt_post_bapi ASSIGNING <ls_post>.

        CLEAR: lv_error,
               lv_obj_key.

        call_bapi(
          EXPORTING
            iv_test  = iv_test
            is_head  = <ls_post>-header
            it_glacc = <ls_post>-items
            it_curre = <ls_post>-currc
            iv_doc   = CONV #( <ls_hdr>-awkey )
            iv_buzei = <ls_post>-buzei
          IMPORTING
            ev_err     = lv_error
            ev_obj_key = lv_obj_key
        ).

        DATA(lv_status) = COND icon_d( WHEN lv_error IS INITIAL THEN icon_green_light ELSE icon_red_light ).

        IF <ls_post>-buzei IS INITIAL.
          <ls_hdr>-status = lv_status.
          <ls_hdr>-belnr  = lv_obj_key(10).
        ELSE.
          READ TABLE mt_items ASSIGNING <ls_items>
            WITH KEY awkey = <ls_hdr>-awkey
                     buzei = <ls_post>-buzei.
          <ls_items>-status = lv_status.
          <ls_items>-belnr  = lv_obj_key(10).

          READ TABLE <ls_hdr>-t_items ASSIGNING <ls_items>
            WITH KEY awkey = <ls_hdr>-awkey
                     buzei = <ls_post>-buzei.
          <ls_items>-status = lv_status.
          <ls_items>-belnr  = lv_obj_key(10).
        ENDIF.

        IF lv_error IS NOT INITIAL.
          lv_no_commit = abap_true.
        ENDIF.

      ENDLOOP.

      IF iv_test IS INITIAL AND lv_no_commit IS INITIAL.
        APPEND VALUE #(
            mandt         = sy-mandt
            awkey         = <ls_hdr>-awkey
            bukrs         = <ls_hdr>-comp_code
            gjahr         = <ls_hdr>-fisc_year
            fins_ledger   = <ls_hdr>-ledger
            bktxt         = <ls_hdr>-header_txt
            budat         = <ls_hdr>-pstng_date
            bldat         = <ls_hdr>-doc_date
            belnr         = <ls_hdr>-belnr
            blart         = <ls_hdr>-doc_type
        ) TO lt_db_header .

        LOOP AT <ls_hdr>-t_items ASSIGNING <ls_items>.

          APPEND VALUE #(
                         mandt          = sy-mandt
                         awkey          = <ls_items>-awkey
                         buzei          = <ls_items>-buzei
                         hkont          = <ls_items>-gl_account
                         sgtxt          = <ls_items>-item_text
                         pargb          = <ls_items>-pargb
                         gsber          = <ls_items>-bus_area
                         fb_segment     = <ls_items>-segment
                         dzuonr         = <ls_items>-attribuzione
                         rassc          = <ls_items>-par_comp
                         hbkid          = <ls_items>-int_bank
                         hktid          = <ls_items>-account_id
                         hsl            = <ls_items>-hsl
                         tsl            = <ls_items>-tsl
                         waers          = <ls_items>-currency
                         belnr_gtran    = <ls_items>-belnr
                         blart_gtran    = COND #( WHEN <ls_items>-belnr IS NOT INITIAL THEN p_doct )
                         ) TO lt_db_item_all.

        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      CLEAR: lv_no_commit, lt_post_bapi.

    ENDLOOP.

    IF iv_test IS INITIAL.
      INSERT zsd_cont_tech_hd FROM TABLE lt_db_header.
      INSERT zsd_cont_tech_it FROM TABLE lt_db_item_all.
      COMMIT WORK.

      MESSAGE 'Data saved successfully!'(004) TYPE 'S'.
    ELSE.
      MESSAGE 'Simulated concluded. Please, check the messages!'(006) TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lt_binding TYPE salv_t_hierseq_binding,
          ls_binding TYPE salv_s_hierseq_binding,
          lo_cols    TYPE REF TO cl_salv_columns_hierseq,
          lo_level   TYPE REF TO cl_salv_hierseq_level,
          lo_column  TYPE REF TO cl_salv_column_hierseq.


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

    IF r3 IS INITIAL.
      DATA(lv_pf_status) = EXACT sypfkey( 'ZSTANDARD_FULLSCREEN' ).
    ELSE.
      lv_pf_status = 'STAND_DISP_ONLY'.
    ENDIF.

    mo_hier->set_screen_status(
      EXPORTING
        report        = sy-repid     " ABAP PROGRAM: CURRENT MAIN PROGRAM
        pfstatus      = lv_pf_status    " SCREENS, CURRENT GUI STATUS
        set_functions = mo_hier->c_functions_all    " ALV: DATA ELEMENT FOR CONSTANTS
    ).

    IF r3 IS INITIAL.
      TRY.
          mo_hier->get_selections( level = 1 )->set_selection_mode( cl_salv_selections=>if_salv_c_selection_mode~multiple ).
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDIF.

    TRY.
        lo_cols = mo_hier->get_columns( 1 ).
        lo_cols->set_optimize( ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

      CATCH cx_salv_not_found.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)
      CATCH cx_salv_data_error.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)

    ENDTRY.

    TRY.
        lo_cols->set_expand_column( 'EXPAND' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_cols = mo_hier->get_columns( 2 ).
        lo_cols->set_optimize( ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        lo_cols->get_column( columnname = 'AWKEY'  )->set_technical( ).
        lo_cols->get_column( columnname = 'HSL' )->set_currency( value = 'EUR' ).
        lo_cols->get_column( columnname = 'TSL' )->set_currency_column( value = 'CURRENCY' ).

      CATCH cx_salv_data_error. " ALV: General Error Class (Checked in Syntax Check)
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_level = mo_hier->get_level( 1 ).
        lo_level->set_items_expanded( ).
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
        DATA(lo_aggr_col) = lo_aggr->add_aggregation( columnname  = 'HSL' ).
        lo_aggr->add_aggregation( columnname  = 'TSL' ).


      CATCH cx_salv_data_error. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_existing.   " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_not_found. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
    ENDTRY.

    SET HANDLER on_user_comm
                on_hier_link_click
                FOR mo_hier->get_event( ).

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
      WHEN 'FC_DOWNL'.
        download_excel( ).
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
    DATA(lv_uzeit) = sy-uzeit.
    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      APPEND VALUE #( time    = lv_uzeit
                      awkey   = iv_doc
                      buzei   = iv_buzei
                      prog_nr = sy-tabix
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                      belnr   = ev_obj_key(10)
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

  METHOD on_hier_link_click.

    IF level = 1.
      READ TABLE mt_header ASSIGNING FIELD-SYMBOL(<ls_hdr>) INDEX row.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CASE column.
        WHEN 'BELNR'.

          view_belnr(
            EXPORTING
              iv_belnr = <ls_hdr>-belnr
              iv_awkey = <ls_hdr>-awkey
          ).
      ENDCASE.

      RETURN.
    ENDIF.

    READ TABLE mt_items ASSIGNING FIELD-SYMBOL(<ls_itm>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'BELNR'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_itm>-belnr
            iv_awkey = <ls_itm>-awkey
        ).
    ENDCASE.

  ENDMETHOD.
  METHOD on_msg_link_click.

    READ TABLE mt_messages ASSIGNING FIELD-SYMBOL(<ls_msg>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'BELNR'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_msg>-belnr
            iv_awkey = <ls_msg>-awkey
        ).
    ENDCASE.

  ENDMETHOD.
  METHOD view_belnr.

    IF iv_belnr IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_gjahr) = iv_awkey(4).
    DATA(lv_bukrs) = iv_awkey+4(4).

    SET PARAMETER ID 'GJR' FIELD lv_gjahr.
    SET PARAMETER ID 'BUK' FIELD lv_bukrs.
    SET PARAMETER ID 'BLN' FIELD iv_belnr.

    CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.

  ENDMETHOD.
  METHOD download_excel.
    TYPES : BEGIN OF ty_excel,
              awkey        TYPE ty_header-awkey,
              belnr_hdr    TYPE ty_header-belnr,
              comp_code    TYPE ty_header-comp_code,
              ledger       TYPE ty_header-ledger,
              doc_type     TYPE ty_header-doc_type,
              header_txt   TYPE ty_header-header_txt,
              fisc_year    TYPE ty_header-fisc_year,
              pstng_date   TYPE ty_header-pstng_date,
              doc_date     TYPE ty_header-doc_date,

              buzei        TYPE ty_items-buzei,
              belnr_itm    TYPE ty_items-belnr,
              gl_account   TYPE ty_items-gl_account,
              item_text    TYPE ty_items-item_text,
              pargb        TYPE ty_items-pargb,
              bus_area     TYPE ty_items-bus_area,
              segment      TYPE ty_items-segment,
              attribuzione TYPE ty_items-attribuzione,
              par_comp     TYPE ty_items-par_comp,
              int_bank     TYPE ty_items-int_bank,
              account_id   TYPE ty_items-account_id,
              hsl          TYPE ty_items-hsl,
              tsl          TYPE ty_items-tsl,
              currency     TYPE ty_items-currency,

            END OF ty_excel.

    DATA: lt_excel    TYPE STANDARD TABLE OF ty_excel,
          lv_filename TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.

    SORT mt_header BY awkey.
    SORT mt_items  BY awkey buzei.

    LOOP AT mt_items ASSIGNING FIELD-SYMBOL(<ls_items>).
      READ TABLE mt_header ASSIGNING FIELD-SYMBOL(<ls_hdr>) WITH KEY awkey = <ls_items>-awkey BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
      <ls_excel> = CORRESPONDING #( <ls_hdr> ).
      MOVE-CORRESPONDING <ls_items> TO <ls_excel>.
      <ls_excel>-belnr_hdr = <ls_hdr>-belnr.
      <ls_excel>-belnr_itm = <ls_items>-belnr.

    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = lt_excel ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    DATA(lt_xstring) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xml ).

    cl_gui_frontend_services=>file_save_dialog(
      CHANGING
        filename                  = lv_filename                 " File Name to Save
        path                      = lv_path                 " Path to File
        fullpath                  = lv_fullpath                " Path + File Name
      EXCEPTIONS
        cntl_error                = 1                " Control error
        error_no_gui              = 2                " No GUI available
        not_supported_by_gui      = 3                " GUI does not support this
        invalid_default_file_name = 4                " Invalid default file name
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0 OR lv_fullpath IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_fullpath NP '*.xlsx'.
      lv_fullpath = lv_fullpath && '.xlsx'.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( lv_xml )
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_xstring                      " Transfer table
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to file denied.
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.
  METHOD get_historic_data.

    SELECT @icon_green_light AS status,
            belnr,
            awkey,
            bukrs         AS comp_code,
            gjahr         AS fisc_year,
            fins_ledger   AS ledger,
            bktxt         AS header_txt,
            budat         AS pstng_date,
            bldat         AS doc_date,
            blart         AS doc_type
      FROM zsd_cont_tech_hd
      WHERE awkey IN @s_awkey
        AND bukrs IN @s_bukrs
        AND gjahr IN @s_gjahr
        AND budat IN @s_budat
        AND bldat IN @s_bldat
        AND blart IN @s_blart
      INTO CORRESPONDING FIELDS OF TABLE @mt_header.

    SELECT belnr_gtran AS belnr,
           awkey,
           buzei,
           hkont AS gl_account,
           sgtxt AS item_text,
           pargb,
           gsber AS bus_area,
           fb_segment AS segment,
           dzuonr AS attribuzione,
           rassc  AS par_comp,
           hbkid  AS int_bank,
           hktid  AS account_id,
           hsl,
           tsl,
           waers AS currency

      FROM zsd_cont_tech_it
      FOR ALL ENTRIES IN @mt_header
      WHERE awkey = @mt_header-awkey
      INTO CORRESPONDING FIELDS OF TABLE @mt_items.

    MODIFY mt_items FROM VALUE #( status = icon_green_light ) TRANSPORTING status WHERE belnr IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE abap_true.
      WHEN r1 OR r2.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 1.
          WHEN 'BL2'.
            screen-active = 0.
        ENDCASE.
      WHEN OTHERS.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 0.
          WHEN 'BL2'.
            screen-active = 1.
        ENDCASE.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_cont_tech_ivass=>value_request( ).

START-OF-SELECTION.
  go_cont_tech_ivass = NEW lcl_cont_tech_ivass( ).
  go_cont_tech_ivass->execute( ).
