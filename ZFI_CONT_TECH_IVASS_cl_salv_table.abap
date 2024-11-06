*&---------------------------------------------------------------------*
*& REPORT ZFI_CONT_TECH_IVASS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_cont_tech_ivass.

TABLES zfi_t_hcont_tech.

DATA: BEGIN OF gs_screen0001,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0001.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-007.
  PARAMETERS: r1 RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', "register directly
              r2 RADIOBUTTON GROUP rb , " view and register
              r3 RADIOBUTTON GROUP rb. "view history
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string MODIF ID bl1,
              p_doct TYPE blart DEFAULT 'B2' MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_awkey FOR zfi_t_hcont_tech-awkey MODIF ID bl2,
                   s_bukrs FOR zfi_t_hcont_tech-bukrs MODIF ID bl2,
                   s_gjahr FOR zfi_t_hcont_tech-gjahr MODIF ID bl2,
                   s_budat FOR zfi_t_hcont_tech-budat MODIF ID bl2,
                   s_bldat FOR zfi_t_hcont_tech-bldat MODIF ID bl2,
                   s_blart FOR zfi_t_hcont_tech-blart MODIF ID bl2.

SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_cont_tech_ivass DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_messages,
             time    TYPE sy-uzeit,
             awkey   TYPE awkey,
             buzei   TYPE buzei,
             prog_nr TYPE i,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
             belnr   TYPE belnr_d,
           END OF ty_messages.

    DATA: mt_messages TYPE STANDARD TABLE OF ty_messages,
          mo_salv     TYPE REF TO cl_salv_table.

    METHODS :execute.
    CLASS-METHODS value_request.

    METHODS post_doc IMPORTING iv_test TYPE abap_bool OPTIONAL
                               iv_job  TYPE abap_bool OPTIONAL.

    METHODS : display_msg IMPORTING iv_no_dock TYPE abap_bool OPTIONAL,
      adjust_cols_msg.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_alv_data,
              status       TYPE icon-id,
              awkey        TYPE awkey,
              belnr        TYPE belnr_d,
              doc_type     TYPE blart,

              belnr_gtran  TYPE zfi_t_icont_tech-belnr_gtran,
              blart_gtran  TYPE zfi_t_icont_tech-blart_gtran,
              hkont_gtran  TYPE zfi_t_icont_tech-hkont_gtran,

              comp_code    TYPE bukrs,
              ledger       TYPE fins_ledger,
              header_txt   TYPE bktxt,
              fisc_year    TYPE gjahr,
              pstng_date   TYPE budat,
              doc_date     TYPE bldat,
              buzei        TYPE buzei,
              gl_account   TYPE hkont,
              item_text    TYPE sgtxt,
              pargb        TYPE pargb,
              bus_area     TYPE gsber,
              segment      TYPE fb_segment,
              attribuzione TYPE dzuonr,
              par_comp     TYPE rassc,
              int_bank     TYPE hbkid,
              account_id   TYPE hktid,
              hsl          TYPE acdoca-hsl,
              tsl          TYPE acdoca-tsl,
              currency     TYPE waers,
            END OF ty_alv_data.

    DATA: mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mo_cust_cont TYPE REF TO cl_gui_custom_container,
          mt_alv_data  TYPE STANDARD TABLE OF ty_alv_data.

    METHODS csv_to_table.

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

    METHODS on_link_click FOR EVENT link_click  "Hotspot Handler
      OF cl_salv_events_table
      IMPORTING row column.

    METHODS handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_msg_link_click
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
          MESSAGE 'Please, fill obligatory fields!'(008) TYPE 'S' DISPLAY LIKE 'E'.
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

*    IF mt_messages IS INITIAL.
*      RETURN.
*    ENDIF.

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
*         parent = cl_gui_container=>screen0
          repid = sy-repid
          dynnr = '0001'
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

        DATA(lv_text) = CONV string( 'Number of Line Item Within Accounting Document'(009) ).
        lo_column ?= lo_cols->get_column( 'AWKEY' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'BUZEI' ).
        lo_column->set_key( ).

        lv_text = 'Progressive number'(010).
        lo_column ?= lo_cols->get_column( 'PROG_NR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = CONV scrtext_s( 'Status'(011) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(012).
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
             pstng_date    TYPE char10,
             doc_date      TYPE char10,
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

    TYPES: BEGIN OF ty_awkey_buzei,
             awkey TYPE awkey,
             buzei TYPE buzei,
           END OF ty_awkey_buzei.

    DATA: lt_strings     TYPE STANDARD TABLE OF string,
          ls_split       TYPE ty_split,
          lv_line        TYPE string,
          lt_filetable   TYPE TABLE OF string,
          lt_awkey_buzei TYPE SORTED TABLE OF ty_awkey_buzei WITH UNIQUE KEY awkey.

    cl_gui_frontend_services=>gui_upload( EXPORTING
                                        filename = p_file
                                        filetype = 'ASC'
                                      CHANGING
                                        data_tab = lt_strings ).

    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 2.

*    DATA(lv_file) = ''.
*    OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
*
*    IF sy-subrc <> 0.
*      MESSAGE 'Error opening the file' TYPE 'E'.
*      RETURN.
*    ENDIF.
*
*    DO  .
*
*      READ DATASET lv_file INTO lv_line.
*
*      IF sy-subrc <> 0 .
*        EXIT.
*      ENDIF.
*
*      APPEND lv_line TO lt_filetable.
*    ENDDO.
*
*    CLOSE DATASET lv_file.
*
*
*    LOOP AT lt_filetable INTO lv_line.

      SPLIT <ls_strings> AT ';' INTO
*      SPLIT lv_line AT ';' INTO
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

      READ TABLE lt_awkey_buzei ASSIGNING FIELD-SYMBOL(<ls_awkey>) WITH KEY awkey = ls_split-awkey BINARY SEARCH.
      IF sy-subrc <> 0.
        INSERT VALUE #( awkey = ls_split-awkey ) INTO TABLE lt_awkey_buzei ASSIGNING <ls_awkey>.
      ENDIF.
      <ls_awkey>-buzei += 1.

      APPEND VALUE #(
            awkey           = ls_split-awkey
            doc_type        = ls_split-doc_type
            blart_gtran     = p_doct
            comp_code       = ls_split-comp_code
            fisc_year       = ls_split-fisc_year
            ledger          = ls_split-ledger
            header_txt      = ls_split-header_txt
            pstng_date      = ls_split-pstng_date
            doc_date        = ls_split-doc_date

            buzei           = <ls_awkey>-buzei
            gl_account      = ls_split-gl_account
            item_text       = ls_split-item_text
            pargb           = ls_split-pargb
            bus_area        = ls_split-bus_area
            segment         = ls_split-segment
            attribuzione    = ls_split-attribuzione
            par_comp        = ls_split-par_comp
            int_bank        = ls_split-int_bank
            account_id      = ls_split-account_id
            hsl             = COND #( WHEN ls_split-dare_interno <> 0 THEN -1 * ls_split-dare_interno
                                      ELSE ls_split-avere_interno )
            tsl             = COND #( WHEN ls_split-dare_esterno <> 0 THEN -1 * ls_split-dare_interno
                                      ELSE ls_split-avere_esterno )
            currency        = ls_split-currency
      ) TO mt_alv_data.

    ENDLOOP.

    IF mt_alv_data IS INITIAL .
      RETURN.
    ENDIF.

    SELECT parent~saknr AS act_saknr,
           child~saknr  AS opp_saknr
      FROM ska1 AS parent
      JOIN ska1 AS child
        ON parent~main_saknr        = child~main_saknr
       AND parent~glaccount_subtype = child~glaccount_subtype
       AND parent~ktopl             = child~ktopl
       AND parent~xbilk             = child~xbilk
       AND parent~glaccount_type    = child~glaccount_type

      JOIN t001
        ON t001~ktopl = parent~ktopl

      FOR ALL ENTRIES IN @mt_alv_data

      WHERE parent~saknr             = @mt_alv_data-gl_account
        AND child~saknr             <> @mt_alv_data-gl_account
        AND t001~bukrs               = @mt_alv_data-comp_code
        AND parent~glaccount_subtype = 'S'
        AND parent~xbilk             = 'X'
        AND parent~glaccount_type    = 'C'
      INTO TABLE @DATA(lt_ska1).
    SORT lt_ska1 BY act_saknr.


    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_alv_data>) WHERE int_bank IS NOT INITIAL.

      READ TABLE lt_ska1 ASSIGNING FIELD-SYMBOL(<ls_ska1>)
         WITH KEY act_saknr = <ls_alv_data>-gl_account BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <ls_alv_data>-hkont_gtran = <ls_ska1>-opp_saknr.

    ENDLOOP.


    SORT mt_alv_data BY awkey.

  ENDMETHOD.                    "EXTRACT_DATA

  METHOD post_doc.
    TYPES: BEGIN OF ty_post_bapi,
             buzei  TYPE buzei,
             header TYPE bapiache09,
             items  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
             currc  TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
           END OF ty_post_bapi.

    TYPES: BEGIN OF ty_belnr,
             buzei TYPE buzei,
             belnr TYPE belnr_d,
           END OF ty_belnr.

    DATA: lt_selected    TYPE salv_t_row,
          lt_db_item_all TYPE STANDARD TABLE OF zfi_t_icont_tech,
          lt_db_item     TYPE STANDARD TABLE OF zfi_t_icont_tech,
          lt_db_header   TYPE STANDARD TABLE OF zfi_t_hcont_tech,
          ls_db_header   TYPE zfi_t_hcont_tech,
          lv_error       TYPE abap_bool,
          lv_obj_key     TYPE bapiache09-obj_key,
          lt_post_bapi   TYPE STANDARD TABLE OF ty_post_bapi,
          lr_awkey       TYPE RANGE OF awkey,
          lt_belnr       TYPE STANDARD TABLE OF ty_belnr.

    GET TIME FIELD DATA(lv_uzeit).

    IF iv_job IS INITIAL.
      TRY.
          mo_salv->get_metadata( ).
          lt_selected = mo_salv->get_selections( )->get_selected_rows( ).
        CATCH cx_salv_not_found.    " ALV: GENERAL ERROR CLASS (CHECKED DURING SYNTAX CHECK)
      ENDTRY.

      IF lt_selected IS INITIAL.
        MESSAGE 'Please, select at least 1 line to continue!'(002)  TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT lt_selected ASSIGNING FIELD-SYMBOL(<lv_index>).
        READ TABLE mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <lv_index> .
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE lr_awkey TRANSPORTING NO FIELDS WITH KEY low = <ls_data>-awkey.
        IF sy-subrc <> 0.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_data>-awkey ) TO lr_awkey.
        ENDIF.

      ENDLOOP.

    ENDIF.

    LOOP AT mt_alv_data ASSIGNING <ls_data>
      WHERE awkey IN lr_awkey
      GROUP BY ( awkey = <ls_data>-awkey )
       ASSIGNING FIELD-SYMBOL(<lg_awkey>).

      CLEAR: lt_db_item, lt_belnr.

      SELECT SINGLE @abap_true
        FROM zfi_t_hcont_tech
        INTO @DATA(lv_exists)
        WHERE awkey = @<lg_awkey>-awkey.

      IF sy-subrc = 0.

        APPEND VALUE #( time    = lv_uzeit
                        awkey   = <lg_awkey>-awkey
                        prog_nr = 1
                        icon    = icon_red_light
                        message = 'Record already exists in the database!'(017)
                        belnr   = lv_obj_key(10)
                      ) TO mt_messages.


        LOOP AT GROUP <lg_awkey> ASSIGNING FIELD-SYMBOL(<ls_alv_data>).
          <ls_alv_data>-status = icon_red_light.
        ENDLOOP.

        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post_bapi_main>).

      LOOP AT GROUP <lg_awkey> ASSIGNING <ls_alv_data>.

        IF <ls_post_bapi_main>-header IS INITIAL.
          <ls_post_bapi_main>-header = VALUE #(
                        username     = sy-uname
                        comp_code    = <ls_alv_data>-comp_code
                        ledger_group = <ls_alv_data>-ledger
                        doc_date     = <ls_alv_data>-doc_date
                        pstng_date   = <ls_alv_data>-pstng_date
                        doc_type     = <ls_alv_data>-doc_type
                        fisc_year    = <ls_alv_data>-fisc_year
                        header_txt   = <ls_alv_data>-header_txt
                        ref_doc_no   = <ls_alv_data>-awkey(8) && <ls_alv_data>-awkey+10(8)
                                  ).
        ENDIF.

        APPEND VALUE bapiacgl09(  itemno_acc      = <ls_alv_data>-buzei
                                  gl_account      = <ls_alv_data>-gl_account
                                  comp_code       = <ls_alv_data>-comp_code
                                  item_text       = <ls_alv_data>-item_text
                                  tr_part_ba      = <ls_alv_data>-pargb
                                  bus_area        = <ls_alv_data>-bus_area
                                  segment         = <ls_alv_data>-segment
                                  trade_id        = <ls_alv_data>-par_comp
                                  housebankid     = <ls_alv_data>-int_bank
                                  housebankacctid = <ls_alv_data>-account_id
                                  alloc_nmbr      = <ls_alv_data>-attribuzione
                                  value_date      = sy-datum
                                  ) TO <ls_post_bapi_main>-items.

        APPEND VALUE bapiaccr09(
                         itemno_acc = <ls_alv_data>-buzei
                         curr_type  = '00'
                         currency   = <ls_alv_data>-currency
                         amt_doccur = <ls_alv_data>-hsl
                         exch_rate  = COND #( WHEN <ls_alv_data>-currency <> 'EUR' THEN <ls_alv_data>-hsl / <ls_alv_data>-tsl
                                              ELSE 1 )
                          ) TO <ls_post_bapi_main>-currc ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).


        IF <ls_alv_data>-hkont_gtran IS INITIAL .
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post>).
        <ls_post>-buzei  = <ls_alv_data>-buzei.
        <ls_post>-header =  VALUE #(
                    username     = sy-uname
                    comp_code    = <ls_alv_data>-comp_code
                    doc_date     = <ls_alv_data>-doc_date
                    pstng_date   = <ls_alv_data>-pstng_date
                    doc_type     = <ls_alv_data>-blart_gtran
                    fisc_year    = <ls_alv_data>-fisc_year
                    header_txt   = <ls_alv_data>-header_txt
                    ref_doc_no   = <ls_alv_data>-awkey(8) && <ls_alv_data>-awkey+10(8)
                              ).
        <ls_post>-items = VALUE #(

        (  itemno_acc      = 1
           gl_account      = <ls_alv_data>-gl_account
           comp_code       = <ls_alv_data>-comp_code
           item_text       = <ls_alv_data>-item_text
           tr_part_ba      = <ls_alv_data>-pargb
           bus_area        = <ls_alv_data>-bus_area
           segment         = <ls_alv_data>-segment
           trade_id        = <ls_alv_data>-par_comp
           housebankid     = <ls_alv_data>-int_bank
           housebankacctid = <ls_alv_data>-account_id
           alloc_nmbr      = <ls_alv_data>-attribuzione
           value_date      = sy-datum
         )
         ( itemno_acc      = 2
           gl_account      = <ls_alv_data>-hkont_gtran
           comp_code       = <ls_alv_data>-comp_code
           item_text       = <ls_alv_data>-item_text
           tr_part_ba      = <ls_alv_data>-pargb
           bus_area        = <ls_alv_data>-bus_area
           segment         = <ls_alv_data>-segment
           trade_id        = <ls_alv_data>-par_comp
           housebankid     = <ls_alv_data>-int_bank
           housebankacctid = <ls_alv_data>-account_id
           alloc_nmbr      = <ls_alv_data>-attribuzione
           value_date      = sy-datum
         ) ).

        <ls_post>-currc = VALUE #(
        (
          itemno_acc = 1
          curr_type  = '00'
          currency   = <ls_alv_data>-currency
          amt_doccur = <ls_bapi_accr>-amt_doccur * -1
          exch_rate  = <ls_bapi_accr>-exch_rate
        )
        (
          itemno_acc = 2
          curr_type  = '00'
          currency   = <ls_alv_data>-currency
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
            iv_doc   = CONV #( <ls_post>-header-ref_doc_no )
            iv_buzei = <ls_post>-buzei
          IMPORTING
            ev_err     = lv_error
            ev_obj_key = lv_obj_key
        ).

        IF <ls_post>-buzei IS NOT INITIAL.
          APPEND VALUE #( buzei = <ls_post>-buzei
                          belnr = lv_obj_key(10) ) TO lt_belnr.
        ELSE.
          DATA(lv_main_belnr) = lv_obj_key(10).
        ENDIF.

        IF lv_error IS NOT INITIAL.
          lv_no_commit = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      CLEAR ls_db_header.
      LOOP AT GROUP <lg_awkey> ASSIGNING <ls_alv_data>.
        IF lv_no_commit IS NOT INITIAL.
          <ls_alv_data>-status = icon_red_light.
          CONTINUE.
        ENDIF.

        <ls_alv_data>-status = icon_green_light.

        IF iv_test IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        <ls_alv_data>-belnr  = lv_main_belnr.

        IF ls_db_header IS INITIAL.
          ls_db_header = VALUE #(
              mandt         = sy-mandt
              awkey         = <ls_alv_data>-awkey
              bukrs         = <ls_alv_data>-comp_code
              gjahr         = <ls_alv_data>-fisc_year
              fins_ledger   = <ls_alv_data>-ledger
              bktxt         = <ls_alv_data>-header_txt
              budat         = <ls_alv_data>-pstng_date
              bldat         = <ls_alv_data>-doc_date
              belnr         = <ls_alv_data>-belnr
              blart         = <ls_alv_data>-doc_type
          ).

          APPEND ls_db_header TO lt_db_header .
        ENDIF.

        APPEND VALUE #(
            mandt          = sy-mandt
            awkey          = <ls_alv_data>-awkey
            buzei          = <ls_alv_data>-buzei
            hkont          = <ls_alv_data>-gl_account
            sgtxt          = <ls_alv_data>-item_text
            pargb          = <ls_alv_data>-pargb
            gsber          = <ls_alv_data>-bus_area
            fb_segment     = <ls_alv_data>-segment
            dzuonr         = <ls_alv_data>-attribuzione
            rassc          = <ls_alv_data>-par_comp
            hbkid          = <ls_alv_data>-int_bank
            hktid          = <ls_alv_data>-account_id
            hsl            = <ls_alv_data>-hsl
            tsl            = <ls_alv_data>-tsl
            waers          = <ls_alv_data>-currency
                       ) TO lt_db_item_all ASSIGNING FIELD-SYMBOL(<ls_db_itm>).

        IF <ls_alv_data>-hkont_gtran IS INITIAL .
          CONTINUE.
        ENDIF.

        READ TABLE lt_belnr ASSIGNING FIELD-SYMBOL(<ls_belnr>) WITH KEY buzei = <ls_alv_data>-buzei.
        IF sy-subrc = 0.
          <ls_alv_data>-belnr_gtran = <ls_belnr>-belnr.

          <ls_db_itm>-belnr_gtran   = <ls_alv_data>-belnr_gtran.
          <ls_db_itm>-blart_gtran   = <ls_alv_data>-blart_gtran.
          <ls_db_itm>-hkont_gtran   = <ls_alv_data>-hkont_gtran.
        ENDIF.

      ENDLOOP.


      IF iv_test IS INITIAL AND lv_no_commit IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      CLEAR: lv_no_commit, lt_post_bapi.

    ENDLOOP.

    IF iv_test IS INITIAL.
      INSERT zfi_t_hcont_tech FROM TABLE lt_db_header.
      INSERT zfi_t_icont_tech FROM TABLE lt_db_item_all.
      COMMIT WORK.

      MESSAGE 'Data saved successfully!'(004) TYPE 'S'.
    ELSE.
      MESSAGE 'Simulation concluded. Please, check the messages!'(006) TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_list.


    CREATE OBJECT mo_cust_cont
      EXPORTING
        container_name              = 'CUST_CONT'
      EXCEPTIONS
        cntl_error                  = 1                " CNTL_ERROR
        cntl_system_error           = 2                " CNTL_SYSTEM_ERROR
        create_error                = 3                " CREATE_ERROR
        lifetime_error              = 4                " LIFETIME_ERROR
        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
      EXPORTING
        r_container    = mo_cust_cont
          IMPORTING
            r_salv_table   = mo_salv  " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_alv_data
        ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        RETURN.
    ENDTRY.

    TRY.
        mo_salv->get_selections( )->set_selection_mode( cl_salv_selections=>if_salv_c_selection_mode~row_column ).
      CATCH cx_salv_not_found.
    ENDTRY.

    DATA(lo_funct) = mo_salv->get_functions( ).
    lo_funct->set_all( abap_true ).

    TRY.
        lo_columns = mo_salv->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column ?= lo_columns->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).


        lo_column ?= lo_columns->get_column( 'BELNR_GTRAN' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        IF r3 IS NOT INITIAL.
          lo_columns->get_column( 'STATUS' )->set_technical( ).
        ELSE.
          lo_column ?= lo_columns->get_column( 'STATUS' ).
          lo_column->set_icon( ).
          lo_column->set_short_text( value = 'Stato' ).
          lo_column->set_medium_text( value = 'Stato' ).
          lo_column->set_long_text( value = 'Stato' ).
        ENDIF.

        lo_columns->get_column( columnname = 'HSL' )->set_currency( value = 'EUR' ).
        lo_columns->get_column( columnname = 'TSL' )->set_currency_column( value = 'CURRENCY' ).

      CATCH cx_salv_data_error. " ALV: General Error Class (Checked in Syntax Check)
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        DATA(lo_sorts) = mo_salv->get_sorts( ).
        DATA(lo_sort) =
            lo_sorts->add_sort(
              EXPORTING
                columnname = 'AWKEY'                           " ALV CONTROL: FIELD NAME OF INTERNAL TABLE FIELD
                position   = 1
                subtotal   = if_salv_c_bool_sap=>true
            ).

        lo_sort =
        lo_sorts->add_sort(
      EXPORTING
        columnname = 'GL_ACCOUNT'                           " ALV CONTROL: FIELD NAME OF INTERNAL TABLE FIELD
        position   = 2
        subtotal   = if_salv_c_bool_sap=>true
    ).

        DATA(lo_aggr) = mo_salv->get_aggregations( ).
        DATA(lo_aggr_col) = lo_aggr->add_aggregation( columnname  = 'HSL' ).
        lo_aggr->add_aggregation( columnname  = 'TSL' ).

      CATCH cx_salv_data_error. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_existing.   " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
      CATCH cx_salv_not_found. " ALV: GENERAL ERROR CLASS (CHECKED IN SYNTAX CHECK)
    ENDTRY.

    SET HANDLER on_link_click
                FOR mo_salv->get_event( ).

    mo_salv->display( ).
    CALL SCREEN 0001.
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
    DATA(lv_progr) = 0.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>)
       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).

      IF <ls_return>-type = 'S'
        AND <ls_return>-id = 'RW'.

        CASE <ls_return>-number.
          WHEN 614.

            IF iv_buzei IS INITIAL.
              <ls_return>-message = 'Document Ready for Registration COTEC'(013) .  " Documento registrato COTEC
            ELSE.
              <ls_return>-message = 'Document Ready for Registration Girofondo Transito'(014) . "Documento registrato Girofondo Transitorio
            ENDIF.

          WHEN 605.

            IF iv_buzei IS INITIAL.
              <ls_return>-message = 'Registered Document COTEC'(015) .
            ELSE.
              <ls_return>-message = 'Registered Document Transitional Girofondo'(016).
            ENDIF.
        ENDCASE.

      ENDIF.

      lv_progr += 1.
      APPEND VALUE #( time    = lv_uzeit
                      awkey   = iv_doc
                      buzei   = iv_buzei
                      prog_nr = lv_progr
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

  METHOD on_link_click .

    READ TABLE mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'BELNR'.
        view_belnr(
          EXPORTING
            iv_belnr = <ls_data>-belnr
            iv_awkey = <ls_data>-awkey
        ).

      WHEN 'BELNR_GTRAN'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_data>-belnr_gtran
            iv_awkey = <ls_data>-awkey
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

  METHOD get_historic_data.

    SELECT hdr~awkey,
           hdr~belnr,
           hdr~blart          AS doc_type,

           itm~belnr_gtran,
           itm~blart_gtran,
           itm~hkont_gtran,

           hdr~bukrs          AS comp_code,
           hdr~gjahr          AS fisc_year,
           hdr~fins_ledger    AS ledger,
           hdr~bktxt          AS header_txt,
           hdr~budat          AS pstng_date,
           hdr~bldat          AS doc_date,

           itm~buzei,
           itm~hkont          AS gl_account,
           itm~sgtxt          AS item_text,
           itm~pargb,
           itm~gsber          AS bus_area,
           itm~fb_segment     AS segment,
           itm~dzuonr         AS attribuzione,
           itm~rassc          AS par_comp,
           itm~hbkid          AS int_bank,
           itm~hktid          AS account_id,
           itm~hsl,
           itm~tsl,
           itm~waers          AS currency

      FROM zfi_t_hcont_tech AS hdr
      JOIN zfi_t_icont_tech AS itm
        ON hdr~awkey = itm~awkey
      WHERE hdr~awkey IN @s_awkey
        AND hdr~bukrs IN @s_bukrs
        AND hdr~gjahr IN @s_gjahr
        AND hdr~budat IN @s_budat
        AND hdr~bldat IN @s_bldat
        AND hdr~blart IN @s_blart
        INTO CORRESPONDING FIELDS OF TABLE @mt_alv_data.

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

*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  DATA: lt_exclude TYPE TABLE OF syucomm.

  IF r3 IS NOT INITIAL.
    APPEND 'FC_SIMU' TO lt_exclude.
    APPEND 'FC_POST' TO lt_exclude.
  ENDIF.

  SET TITLEBAR 'TB_IVASS'.
  SET PF-STATUS 'PF-STATUS' EXCLUDING lt_exclude.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  CLEAR: go_cont_tech_ivass->mt_messages.

  CASE gs_screen0001-ok_code.
    WHEN 'FC_SIMU'.
      go_cont_tech_ivass->post_doc( iv_test  = abap_true ).
    WHEN 'FC_POST'.
      go_cont_tech_ivass->post_doc( iv_test  = abap_false ).
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  go_cont_tech_ivass->display_msg( ).
  go_cont_tech_ivass->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).

ENDMODULE.
