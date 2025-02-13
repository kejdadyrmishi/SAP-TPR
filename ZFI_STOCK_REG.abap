*&---------------------------------------------------------------------*
*& Report ZFI_STOCK_REG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_stock_reg.

DATA: BEGIN OF gs_screen0001,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0001.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-007.
  PARAMETERS: p_direct RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', "register directly
              p_disply RADIOBUTTON GROUP rb . " view and register
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_stock_reg DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_messages,
             prog_nr TYPE numc5,
             time    TYPE sy-uzeit,
             bukrs   TYPE bukrs,
             awkey   TYPE awkey,
             buzei   TYPE buzei,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
             belnr   TYPE belnr_d,
             gjahr   TYPE gjahr,
           END OF ty_messages.

    DATA: mt_messages TYPE STANDARD TABLE OF ty_messages,
          mo_salv     TYPE REF TO cl_salv_table.

    METHODS :execute.
    CLASS-METHODS value_request.

    METHODS post_doc IMPORTING iv_test      TYPE abap_bool OPTIONAL
                               iv_job       TYPE abap_bool OPTIONAL
                     CHANGING  iv_no_commit TYPE abap_bool OPTIONAL.

    METHODS : display_msg IMPORTING iv_no_dock TYPE abap_bool OPTIONAL,
      adjust_cols_msg IMPORTING iv_no_dock TYPE abap_bool .

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_alv_data,
             status      TYPE icon-id,
             buzei       TYPE buzei,

             belnr       TYPE belnr_d,
             belnr_gtran TYPE belnr_d,

             bukrs       TYPE bukrs,
             gjahr       TYPE gjahr,
             budat       TYPE budat,
             bldat       TYPE bldat,
             blart       TYPE blart,
             bktxt       TYPE bktxt,
             awkey       TYPE awkey,
             rwcur       TYPE rwcur,
             rhcur       TYPE fc_curr,       " local curr
             rldnr       TYPE rldnr,         " ledger
             docln       TYPE docln,
             sgtxt       TYPE sgtxt,
             zuonr       TYPE dzuonr,
             bschl       TYPE bschl,         " posting key

             racct       TYPE hkont, "racct,         " acc nr
             hkont_gtran TYPE hkont,

             kunnr       TYPE kunnr,
             lifnr       TYPE lifnr,
             valut       TYPE valut,
             wsl         TYPE acdoca-wsl,    " Amount in Transaction Currency
             hsl         TYPE acdoca-hsl,    " Amount in Company Code Currency
             mwskz       TYPE mwskz,
             wt_withcd   TYPE wt_withcd,
             wt_qsshh    TYPE wt_bs,
             augbl       TYPE augbl,
             zzramo      TYPE zzramo,
             zztipolav   TYPE zztipolav,
             zzptf       TYPE zzptf,
             segment     TYPE fb_segment,
             zzantpol    TYPE zzantpol,       " ztipo_polizza,
             zztipoliq   TYPE zztipoliq,
             zzrff       TYPE zzrff,
             zzcciv      TYPE zzcciv,
             zzlob       TYPE zzlob,          " BUSINESS AREA
             rbusa       TYPE gsber,
           END OF ty_alv_data.

    DATA: mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mo_cust_cont TYPE REF TO cl_gui_custom_container,
          mt_alv_data  TYPE STANDARD TABLE OF ty_alv_data,
          lv_error     TYPE abap_bool.

    METHODS csv_to_table.

    METHODS call_bapi IMPORTING iv_progr   TYPE numc5
                                iv_test    TYPE abap_bool
                                is_head    TYPE bapiache09
                                it_glacc   TYPE bapiacgl09_tab
                                it_exten   TYPE bapiparex_t
                                it_curre   TYPE bapiaccr09_tab
                                iv_buzei   TYPE posnr_acc
                                iv_bukrs   TYPE bukrs
                                iv_doc     TYPE string
                      EXPORTING ev_err     TYPE abap_bool
                                ev_obj_key TYPE bapiache09-obj_key.
    METHODS display_alv.

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
                                 iv_gjahr TYPE gjahr
                                 iv_bukrs TYPE bukrs.

ENDCLASS.                    "

DATA go_stock_reg TYPE REF TO lcl_stock_reg.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_stock_reg IMPLEMENTATION.

  METHOD execute.

    DATA lv_no_commit TYPE abap_bool.

    CASE abap_true.
      WHEN p_direct OR p_disply.

        IF p_file IS INITIAL .
          MESSAGE 'Please, fill the field first!'(002) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        csv_to_table( ).

        CASE abap_true.
          WHEN p_direct.

            IF lv_error EQ abap_true.
              display_msg( iv_no_dock = abap_true ).
              RETURN.
            ENDIF.

            post_doc(
            EXPORTING
               iv_test  = abap_true
            CHANGING
               iv_no_commit = lv_no_commit ).

            DELETE mt_messages WHERE icon = icon_green_light.
            SORT mt_messages BY prog_nr message ASCENDING.
            DELETE ADJACENT DUPLICATES FROM mt_messages COMPARING message prog_nr.

            IF lv_no_commit IS INITIAL.
              CLEAR mt_messages.
              post_doc( iv_test  = abap_false ).
            ENDIF.
            display_msg( iv_no_dock = abap_true ).

          WHEN p_disply.

*            IF lv_error = abap_true.
*              display_msg( iv_no_dock = abap_true ).
*              RETURN.
*            ENDIF.
            display_alv( ).

        ENDCASE.

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

    adjust_cols_msg( iv_no_dock = iv_no_dock ).

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

        DATA(lv_text) = CONV scrtext_m( 'ID lotto' ).
        lo_column ?= lo_cols->get_column( 'PROG_NR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'TIME' ).
        IF iv_no_dock IS NOT INITIAL.
          lo_column->set_technical( ).
        ELSE.
          lo_column->set_key( ).
        ENDIF.

        lo_column ?= lo_cols->get_column( 'BUKRS' ).
        lo_column->set_key( ).

        lv_text = CONV string( 'Number of Line Item Within Accounting Document'(009) ).
        lo_column ?= lo_cols->get_column( 'AWKEY' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'BUZEI' ).
        IF iv_no_dock IS NOT INITIAL.
          lo_column->set_technical( ).
        ELSE.
          lo_column->set_key( ).
        ENDIF.

        lv_text = CONV scrtext_s( 'Status'(011) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(012).
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        IF iv_no_dock IS NOT INITIAL.
          lo_column->set_technical( ).
        ELSE.
          lo_column->set_cell_type(
           value = if_salv_c_cell_type=>hotspot
       ).
        ENDIF.

        lo_cols->get_column( 'GJAHR' )->set_technical( ).

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
             bukrs     TYPE bukrs,
             belnr     TYPE string, " 10
             gjahr     TYPE string, " 4
             budat     TYPE char10, " 10
             bldat     TYPE char10, " 10
             blart     TYPE string, " 2
             bktxt     TYPE string, " 25
             awkey     TYPE string, " 20
             rwcur     TYPE string, " 5
             rhcur     TYPE string, " 5
             rldnr     TYPE string, " 2
             docln     TYPE string, " 6
             sgtxt     TYPE string, " 50
             zuonr     TYPE string, " 18
             bschl     TYPE string, " 2
             racct     TYPE string, " 10
             kunnr     TYPE string, " 10
             lifnr     TYPE string, " 10
             valut     TYPE char10, " 10
             wsl       TYPE string, " 31
             hsl       TYPE string, " 31
             mwskz     TYPE string, " 2
             wt_withcd TYPE string, " 2
             wt_qsshh  TYPE string, " 23
             augbl     TYPE string, " 10
             zzramo    TYPE string, " 3
             zztipolav TYPE string, " 3
             zzptf     TYPE string, " 3
             segment   TYPE string, " 10
             zzantpol  TYPE string, " 3
             zztipoliq TYPE string, " 3
             zzrff     TYPE string, " 6
             zzcciv    TYPE string, " 5
             zzlob     TYPE string, " 3
             rbusa     TYPE string, " 4
           END OF ty_split.

    TYPES: BEGIN OF ty_awkey_buzei,
             awkey TYPE awkey,
             buzei TYPE buzei,
           END OF ty_awkey_buzei.

    TYPES: BEGIN OF ty_tab_for,
             zz_input  TYPE zzinput,
             zz_output TYPE zzoutput,
           END OF ty_tab_for.

    TYPES: BEGIN OF ty_tab_cli,
             zz_input  TYPE zzinput,
             zz_output TYPE zzoutput,
           END OF ty_tab_cli.

    TYPES: BEGIN OF ty_cc,
             zz_input  TYPE zz_i_bschl,
             zz_output TYPE zz_o_bschl,
           END OF ty_cc.

    TYPES: BEGIN OF ty_matrice_ra,
*****      INPUT
             itemno_acc      TYPE posnr_acc,
             racct           TYPE racct,
             zzramo          TYPE zzramo,
             zztipolav       TYPE zztipolav,
             zzptf           TYPE zzptf,
             segment         TYPE fb_segment,
             zzantpol        TYPE zzantpol,
             zztipoliq       TYPE zztipoliq,
             zzcciv          TYPE zzcciv,
             rbusa           TYPE gsber,
***  OUTPUT
             z_out_pargb     TYPE pargb,
             z_out_zzantpol  TYPE ztipo_polizza,
             z_out_zzcciv    TYPE xref1,
             z_out_zzrff     TYPE xref2,
             z_out_rcntr     TYPE kostl,
             z_out_gsber     TYPE gsber,
             z_out_rassc     TYPE rassc,
             z_out_hbkid     TYPE hbkid,
             z_out_hktid     TYPE hktid,
             z_out_ledger_gr TYPE fins_ledger,
             z_out_valut     TYPE valut,
             conto_ifrs_bpm  TYPE hkont,
             conto_ivas_bpm  TYPE hkont,
             dsc_conto_bpm   TYPE char255,
           END OF ty_matrice_ra.

    DATA: lt_strings     TYPE STANDARD TABLE OF string,
          ls_split       TYPE ty_split,
          lv_line        TYPE string,
          lt_filetable   TYPE TABLE OF string,
          lt_awkey_buzei TYPE SORTED TABLE OF ty_awkey_buzei WITH UNIQUE KEY awkey,
          lt_tab_for     TYPE SORTED TABLE OF ty_tab_for WITH UNIQUE KEY zz_input,
          lt_tab_cli     TYPE SORTED TABLE OF ty_tab_cli WITH UNIQUE KEY zz_input,
          lt_tab_cc      TYPE SORTED TABLE OF ty_cc WITH UNIQUE KEY zz_input,
          lt_matrice_ra  TYPE SORTED TABLE OF ty_matrice_ra WITH UNIQUE KEY  racct
                                                                             zzramo
                                                                             zztipolav
                                                                             zzptf
                                                                             segment
                                                                             zzantpol
                                                                             zztipoliq
                                                                             zzcciv
                                                                             rbusa ,
          lv_error       TYPE abap_bool,
          lv_prog_nr     TYPE string.

    cl_gui_frontend_services=>gui_upload( EXPORTING
                                        filename = p_file
                                        filetype = 'ASC'
                                          CHANGING
                                        data_tab = lt_strings ).

    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 1.

      lv_prog_nr += 1.

      ls_split-belnr       = <ls_strings>(10).
      ls_split-gjahr       = <ls_strings>+10(4).
      ls_split-budat       = <ls_strings>+14(10).
      ls_split-bldat       = <ls_strings>+24(10).
      ls_split-blart       = <ls_strings>+34(2).
      ls_split-bktxt       = <ls_strings>+36(25).
      ls_split-awkey       = <ls_strings>+61(20).
      ls_split-rwcur       = <ls_strings>+81(5).
      ls_split-rhcur       = <ls_strings>+86(5).
      ls_split-rldnr       = <ls_strings>+91(2).
      ls_split-docln       = <ls_strings>+93(6).
      ls_split-sgtxt       = <ls_strings>+99(50).
      ls_split-zuonr       = <ls_strings>+149(18).
      ls_split-bschl       = <ls_strings>+167(2).
      ls_split-racct       = <ls_strings>+169(10).
      ls_split-kunnr       = <ls_strings>+179(10).
      ls_split-lifnr       = <ls_strings>+189(10).
      ls_split-valut       = <ls_strings>+199(10).
      ls_split-wsl         = <ls_strings>+209(31).
      ls_split-hsl         = <ls_strings>+240(31).
      ls_split-mwskz       = <ls_strings>+271(2).
      ls_split-wt_withcd   = <ls_strings>+273(2).
      ls_split-wt_qsshh    = <ls_strings>+275(23).
      ls_split-augbl       = <ls_strings>+298(10).
      ls_split-zzramo      = <ls_strings>+308(3).
      ls_split-zztipolav   = <ls_strings>+311(3).
      ls_split-zzptf       = <ls_strings>+314(2).
      ls_split-segment     = <ls_strings>+316(10).
      ls_split-zzantpol    = <ls_strings>+326(3).
      ls_split-zztipoliq   = <ls_strings>+329(3).
      ls_split-zzrff       = <ls_strings>+332(6).
      ls_split-zzcciv      = <ls_strings>+338(5).
      ls_split-zzlob       = <ls_strings>+343(3).
      ls_split-rbusa       = <ls_strings>+346(4).
*      ls_split-rassc       = <ls_strings>+351(6).
*      ls_split-rcntr       = <ls_strings>+357(10).

      ls_split-budat = |{ ls_split-budat+6(4) }{ ls_split-budat+3(2) }{ ls_split-budat(2) }|.
      ls_split-bldat = |{ ls_split-bldat+6(4) }{ ls_split-bldat+3(2) }{ ls_split-bldat(2) }|.
      ls_split-valut = |{ ls_split-valut+6(4) }{ ls_split-valut+3(2) }{ ls_split-valut(2) }|.

      REPLACE ALL OCCURRENCES OF '.' IN ls_split-wsl WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN ls_split-wsl WITH '.'.
      CONDENSE ls_split-wsl NO-GAPS.

      REPLACE ALL OCCURRENCES OF '.' IN ls_split-wt_qsshh WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN ls_split-wt_qsshh WITH '.'.
      CONDENSE ls_split-wt_qsshh NO-GAPS.

      REPLACE ALL OCCURRENCES OF '.' IN ls_split-hsl WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN ls_split-hsl WITH '.'.
      CONDENSE ls_split-hsl NO-GAPS.

*      DATA: lv_belnr TYPE belnr_d,
*            lv_posnr TYPE posnr_acc.
*
*      IF lv_belnr IS INITIAL.
*        lv_belnr = ls_split-belnr.
*      ELSEIF lv_belnr <> ls_split-belnr.
*        lv_belnr = ls_split-belnr.
*        CLEAR lv_posnr.
*      ENDIF.
*
*      lv_posnr += 1.
*      ls_split-itemno_acc =  lv_posnr.

      ls_split-bukrs = 'VVIT'.

*** tab fornitore
      READ TABLE lt_tab_for ASSIGNING FIELD-SYMBOL(<ls_tab_for>) WITH KEY zz_input = ls_split-kunnr BINARY SEARCH.

      IF sy-subrc <> 0.
        INSERT VALUE #( zz_input = ls_split-kunnr ) INTO TABLE lt_tab_for ASSIGNING <ls_tab_for>.
        SELECT SINGLE zzoutput
          FROM zfi_t_forn
          INTO <ls_tab_for>-zz_output
          WHERE zzinput = ls_split-kunnr.
      ENDIF.

      IF <ls_tab_for>-zz_output IS INITIAL.

        APPEND VALUE #(
          prog_nr    = lv_prog_nr
          time       = sy-uzeit
          bukrs      = ls_split-bukrs
          awkey      = ls_split-awkey(12)
          icon       = icon_red_light
          message    = |Errore durante la trascodifica del Customer Number { ls_split-kunnr }|
          belnr      = ls_split-belnr
          gjahr = ls_split-gjahr
           ) TO mt_messages.

        lv_error = abap_true.
      ENDIF.

*** tab cliente

      READ TABLE lt_tab_cli ASSIGNING FIELD-SYMBOL(<ls_tab_cli>) WITH KEY zz_input = ls_split-lifnr BINARY SEARCH.

      IF sy-subrc <> 0.
        INSERT VALUE #( zz_input = ls_split-lifnr ) INTO TABLE lt_tab_cli ASSIGNING <ls_tab_cli>.
        SELECT SINGLE zzoutput
          FROM zfi_t_client
          INTO <ls_tab_cli>-zz_output
          WHERE zzinput = ls_split-lifnr.
      ENDIF.

      IF <ls_tab_cli>-zz_output IS INITIAL.

        APPEND VALUE #(
          prog_nr    = lv_prog_nr
          time       = sy-uzeit
          bukrs      = ls_split-bukrs
          awkey      = ls_split-awkey(12)
          icon       = icon_red_light
          message    = |Errore durante la trascodifica del Account Number of Supplier { ls_split-lifnr }|
          belnr      = ls_split-belnr
          gjahr = ls_split-gjahr
           ) TO mt_messages.

        lv_error = abap_true.
      ENDIF.

****      tab cc

      READ TABLE lt_tab_cc ASSIGNING FIELD-SYMBOL(<ls_tab_cc>) WITH KEY zz_input = ls_split-bschl BINARY SEARCH.

      IF sy-subrc <> 0.
        INSERT VALUE #( zz_input = ls_split-bschl ) INTO TABLE lt_tab_cc ASSIGNING <ls_tab_cc>.
        SELECT SINGLE zzoutput
          FROM zfi_t_cc
          INTO <ls_tab_cc>-zz_output
          WHERE zzinput = ls_split-bschl.
      ENDIF.

      IF <ls_tab_cc>-zz_output IS INITIAL.

        APPEND VALUE #(
          prog_nr    = lv_prog_nr
          time       = sy-uzeit
          bukrs      = ls_split-bukrs
          awkey      = ls_split-awkey(12)
          icon       = icon_red_light
          message    = |Errore durante la trascodifica del Posting Key { ls_split-bschl }|
          belnr      = ls_split-belnr
          gjahr = ls_split-gjahr
           ) TO mt_messages.

        lv_error = abap_true.
      ENDIF.

*** TAB MATRICE RACCORDO

      READ TABLE lt_matrice_ra ASSIGNING FIELD-SYMBOL(<ls_matrice_ra>) WITH KEY racct      = ls_split-racct
                                                                                zzramo     = ls_split-zzramo
                                                                                zztipolav  = ls_split-zztipolav
                                                                                zzptf      = ls_split-zzptf
                                                                                segment    = ls_split-segment
                                                                                zzantpol   = ls_split-zzantpol
                                                                                zztipoliq  = ls_split-zztipoliq
                                                                                zzcciv     = ls_split-zzcciv
                                                                                rbusa      = ls_split-rbusa BINARY SEARCH.

      IF sy-subrc <> 0.
        INSERT VALUE #( racct      = ls_split-racct
                        zzramo     = ls_split-zzramo
                        zztipolav  = ls_split-zztipolav
                        zzptf      = ls_split-zzptf
                        segment    = ls_split-segment
                        zzantpol   = ls_split-zzantpol
                        zztipoliq  = ls_split-zztipoliq
                        zzcciv     = ls_split-zzcciv
                        rbusa      = ls_split-rbusa ) INTO TABLE lt_matrice_ra ASSIGNING <ls_matrice_ra>.

        SELECT SINGLE
             z_out_pargb    ,
             z_out_zzantpol ,
             z_out_zzcciv   ,
             z_out_zzrff    ,
             z_out_rcntr    ,
             z_out_gsber    ,
             z_out_rassc    ,
             z_out_hbkid    ,
             z_out_hktid    ,
             z_out_ledger_gr,
             z_out_valut    ,
             conto_ifrs_bpm ,
             conto_ivas_bpm ,
             dsc_conto_bpm

          FROM zgl_t_ripresa_tc

*          INTO CORRESPONDING FIELDS OF @<ls_matrice_ra>
          INTO @DATA(ls_output)
          WHERE   racct    = @ls_split-racct
          AND   zzramo     = @ls_split-zzramo
          AND   zztipolav  = @ls_split-zztipolav
          AND   zzptf      = @ls_split-zzptf
          AND   segment    = @ls_split-segment
          AND   zzantpol   = @ls_split-zzantpol
          AND   zztipoliq  = @ls_split-zztipoliq
*          AND   zzrff      = @ls_split-zzrff
          AND   zzcciv     = @ls_split-zzcciv
*          AND   zzlob      = @ls_split-zzlob
          AND   rbusa      = @ls_split-rbusa .

        MOVE-CORRESPONDING ls_output TO <ls_matrice_ra>.

        IF sy-subrc <> 0.

          APPEND VALUE #(
          prog_nr  = lv_prog_nr
          time     = sy-uzeit
          bukrs    = ls_split-bukrs
          awkey    = ls_split-awkey(12)
          icon     = icon_red_light
          message  = |Errore durante la trascodifica di Matrice Raccordo.|
          belnr    = ls_split-belnr
          gjahr    = ls_split-gjahr
             ) TO mt_messages.

          lv_error = abap_true.
        ENDIF.

      ENDIF.

      READ TABLE lt_awkey_buzei ASSIGNING FIELD-SYMBOL(<ls_awkey>) WITH KEY awkey = ls_split-awkey(12) BINARY SEARCH.
      IF sy-subrc <> 0.
        INSERT VALUE #( awkey = ls_split-awkey(12) ) INTO TABLE lt_awkey_buzei ASSIGNING <ls_awkey>.
      ENDIF.
      <ls_awkey>-buzei += 1.

      APPEND VALUE #( bukrs      = ls_split-bukrs
                      buzei      =  <ls_awkey>-buzei
                      belnr      =  ls_split-belnr
                      gjahr      =  ls_split-gjahr
                      budat      =  ls_split-budat
                      bldat      =  ls_split-bldat
                      blart      =  ls_split-blart
                      bktxt      =  ls_split-bktxt
                      awkey      =  ls_split-awkey
                      rwcur      =  ls_split-rwcur
                      rhcur      =  ls_split-rhcur
                      rldnr      =  ls_split-rldnr
                      docln      =  ls_split-docln
                      sgtxt      =  ls_split-sgtxt
                      zuonr      =  ls_split-zuonr
                      bschl      =  <ls_tab_cc>-zz_output " ls_split-bschl"
                      racct      =  COND #( WHEN <ls_matrice_ra> IS ASSIGNED
                                            THEN <ls_matrice_ra>-conto_ifrs_bpm )
                      kunnr      =  <ls_tab_for>-zz_output "ls_split-kunnr"
                      lifnr      =  <ls_tab_cli>-zz_output "ls_split-lifnr "
                      valut      =   COND #( WHEN <ls_matrice_ra> IS ASSIGNED
                                            THEN <ls_matrice_ra>-z_out_valut )
                      wsl        =  ls_split-wsl
                      hsl        =  ls_split-hsl
                      mwskz      =  ls_split-mwskz
                      wt_withcd  =  ls_split-wt_withcd
                      wt_qsshh   =  ls_split-wt_qsshh
                      augbl      =  ls_split-augbl
                      zzramo     =  ls_split-zzramo
                      zztipolav  =  ls_split-zztipolav
                      zzptf      =  ls_split-zzptf
                      segment    =  ls_split-segment
                      zzantpol   =   COND #( WHEN <ls_matrice_ra> IS ASSIGNED
                                            THEN <ls_matrice_ra>-z_out_zzantpol )
                      zztipoliq  =  ls_split-zztipoliq
                      zzrff      =  ls_split-zzrff
                      zzcciv     =  COND #( WHEN <ls_matrice_ra> IS ASSIGNED
                                            THEN <ls_matrice_ra>-z_out_zzcciv )
                      zzlob      =  ls_split-zzlob
                      rbusa      =   COND #( WHEN <ls_matrice_ra> IS ASSIGNED
                                            THEN <ls_matrice_ra>-z_out_gsber )
                       ) TO mt_alv_data.


    ENDLOOP.

    IF mt_alv_data IS INITIAL OR lv_error IS NOT INITIAL .
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

      WHERE parent~saknr             = @mt_alv_data-racct
        AND child~saknr             <> @mt_alv_data-racct
        AND t001~bukrs               = @mt_alv_data-bukrs
        AND parent~glaccount_subtype = 'S'
        AND parent~xbilk             = 'X'
        AND parent~glaccount_type    = 'C'
      INTO TABLE @DATA(lt_ska1).
    SORT lt_ska1 BY act_saknr.

    SELECT skb1~bukrs,
           skat~saknr,
           skb1~altkt,
           skat~txt50
      FROM skb1

      JOIN t001
        ON t001~bukrs = skb1~bukrs

      LEFT JOIN skat
      ON skat~saknr = skb1~saknr
        AND skat~ktopl = t001~ktopl
        AND skat~spras = 'I'

      FOR ALL ENTRIES IN @mt_alv_data
      WHERE skb1~saknr  = @mt_alv_data-racct
      AND skb1~bukrs    = @mt_alv_data-bukrs
      INTO TABLE @DATA(lt_acc_and_dsc).

    SORT lt_acc_and_dsc BY bukrs saknr .

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_alv_data>) .

*      IF <ls_alv_data>-int_bank IS NOT INITIAL.
      IF <ls_matrice_ra>-z_out_hbkid IS NOT INITIAL.

        READ TABLE lt_ska1 INTO DATA(ls_ska1)
           WITH KEY act_saknr = <ls_alv_data>-racct BINARY SEARCH.

        IF sy-subrc = 0.
          <ls_alv_data>-hkont_gtran = ls_ska1-opp_saknr.
        ENDIF.

      ENDIF.

      READ TABLE lt_acc_and_dsc INTO DATA(ls_acc_and_dsc)
      WITH KEY bukrs = <ls_alv_data>-bukrs
      saknr = <ls_alv_data>-racct BINARY SEARCH.

      IF sy-subrc = 0.
*        <ls_alv_data>-altkt = ls_acc_and_dsc-altkt.
*        <ls_alv_data>-desc = ls_acc_and_dsc-txt50.
      ENDIF.
    ENDLOOP.

*    SORT mt_alv_data BY awkey.

  ENDMETHOD.                    "EXTRACT_DATA

  METHOD post_doc.
    TYPES: BEGIN OF ty_post_bapi,
             buzei  TYPE buzei,
             header TYPE bapiache09,
             items  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
             exten  TYPE STANDARD TABLE OF bapiparex  WITH DEFAULT KEY,
             currc  TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
           END OF ty_post_bapi.

    TYPES: BEGIN OF ty_belnr,
             buzei TYPE buzei,
             belnr TYPE belnr_d,
           END OF ty_belnr.

    DATA: lt_selected         TYPE salv_t_row,
          lv_error            TYPE abap_bool,
          lv_obj_key          TYPE bapiache09-obj_key,
          lt_post_bapi        TYPE STANDARD TABLE OF ty_post_bapi,
          lr_awkey            TYPE RANGE OF awkey,
          lr_belnr            TYPE RANGE OF belnr_d,
          lr_gjahr            TYPE RANGE OF gjahr,
          lt_belnr            TYPE STANDARD TABLE OF ty_belnr,
          ls_zgl_t_ripresa_mo TYPE zgl_t_ripresa_mo,
          lt_zgl_t_ripresa_mo TYPE STANDARD TABLE OF zgl_t_ripresa_mo,
          lv_prog_nr          TYPE numc5,
          lv_buzei            TYPE posnr_acc,
          lv_char960(960)     TYPE c.

    DATA: ls_ext2     TYPE bapiparex,
          ls_zci_cobl TYPE zci_cobl.

    GET TIME FIELD DATA(lv_uzeit).

    iv_no_commit = abap_false.

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>)
      GROUP BY ( belnr =  <ls_data>-belnr
                 gjahr =  <ls_data>-gjahr
                 bukrs =  <ls_data>-bukrs )
      ASSIGNING FIELD-SYMBOL(<lg_keys>).

      CLEAR: lt_belnr.

      SELECT SINGLE @abap_true
      FROM zgl_t_ripresa_mo
      INTO @DATA(lv_exists)
      WHERE belnr = @<lg_keys>-belnr
      AND gjahr = @<lg_keys>-gjahr.
*      AND bukrs = @<lg_keys>-bukrs.

      IF sy-subrc = 0.

        APPEND VALUE #( prog_nr = 1
                        time    = lv_uzeit
*                        awkey   = <lg_awkey>-awkey
                        icon    = icon_red_light
                        message = 'Record already exists in the database!'(017)
                        belnr   = <lg_keys>-belnr
                        gjahr   = <lg_keys>-gjahr
                      ) TO mt_messages.

        LOOP AT GROUP <lg_keys> ASSIGNING FIELD-SYMBOL(<ls_alv_data>).
          <ls_alv_data>-status = icon_red_light.
        ENDLOOP.

        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post_bapi_main>).

      LOOP AT GROUP <lg_keys> ASSIGNING <ls_alv_data>.

        lv_buzei += 1.

        IF <ls_post_bapi_main>-header IS INITIAL.
          <ls_post_bapi_main>-header = VALUE #(
                        username     = sy-uname
                        comp_code    = <ls_alv_data>-bukrs
                        ledger_group = <ls_alv_data>-rldnr
                        doc_date     = <ls_alv_data>-bldat
                        pstng_date   = <ls_alv_data>-budat
                        doc_type     = <ls_alv_data>-blart
                        fisc_year    = <ls_alv_data>-gjahr
                        header_txt   = <ls_alv_data>-bktxt
                        ref_doc_no   = <ls_alv_data>-awkey(12)
                                  ).
        ENDIF.

        APPEND VALUE bapiacgl09(  itemno_acc      = lv_buzei "<ls_alv_data>-buzei
                                  gl_account      = <ls_alv_data>-racct "hkont
                                  comp_code       = <ls_alv_data>-bukrs
                                  item_text       = <ls_alv_data>-sgtxt
                                  tr_part_ba      = <ls_alv_data>-zzramo && <ls_alv_data>-zztipolav && <ls_alv_data>-zzptf
                                  bus_area        = <ls_alv_data>-rbusa
                                  segment         = <ls_alv_data>-zzlob
*                                  trade_id        = <ls_alv_data>-rassc
*                                  housebankid     = <ls_alv_data>-int_bank
*                                  housebankacctid = <ls_alv_data>-HKTID
                                  alloc_nmbr      = <ls_alv_data>-zuonr
                                  value_date      = <ls_alv_data>-valut
                                  customer        = <ls_alv_data>-kunnr
                                  vendor_no       = <ls_alv_data>-lifnr
                                  pstng_date      = <ls_alv_data>-budat
                                  doc_type        = <ls_alv_data>-blart
                                  tax_code        = <ls_alv_data>-mwskz
                                  ref_key_1       = <ls_alv_data>-zzcciv
                                  ref_key_2       = <ls_alv_data>-zzrff
                                  ) TO <ls_post_bapi_main>-items.

        ls_zci_cobl = VALUE #( zz_tipo_polizza = <ls_alv_data>-zzantpol
                               posnr = CONV posnr( <ls_alv_data>-buzei ) ).

        ls_ext2-structure = 'ZCI_COBL'.

        cl_abap_container_utilities=>fill_container_c(
          EXPORTING
            im_value               = ls_zci_cobl                 " Data for Filling Container
          IMPORTING
            ex_container           = lv_char960                 " Container
          EXCEPTIONS
            illegal_parameter_type = 1                " Invalid type for the parameter IM_VALUE
            OTHERS                 = 2
        ).

        ls_ext2+30 = lv_char960.
        APPEND ls_ext2 TO <ls_post_bapi_main>-exten.

        APPEND VALUE bapiaccr09(
                         itemno_acc = lv_buzei "<ls_alv_data>-buzei
                         curr_type  = '00'
                         currency   = <ls_alv_data>-rwcur "waers
                         amt_doccur = <ls_alv_data>-hsl
*                         exch_rate  = COND #( WHEN <ls_alv_data>-currency <> 'EUR' THEN <ls_alv_data>-hsl / <ls_alv_data>-tsl
*                                              ELSE 1 )
                          ) TO <ls_post_bapi_main>-currc ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).

        IF <ls_alv_data>-hkont_gtran IS INITIAL .
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post>).
        <ls_post>-buzei  = lv_buzei.
        <ls_post>-header =  VALUE #(
                    username     = sy-uname
                    comp_code    = <ls_alv_data>-bukrs
                    doc_date     = <ls_alv_data>-bldat
                    pstng_date   = <ls_alv_data>-budat
                    doc_type     = <ls_alv_data>-blart
                    fisc_year    = <ls_alv_data>-gjahr
                    header_txt   = <ls_alv_data>-bktxt
                    ref_doc_no   = <ls_alv_data>-awkey(12)
                              ).
        <ls_post>-items = VALUE #(

        (  itemno_acc      = 1
           gl_account      = <ls_alv_data>-racct "hkont
           comp_code       = <ls_alv_data>-bukrs
           item_text       = <ls_alv_data>-bktxt
           tr_part_ba      = <ls_alv_data>-zzramo && <ls_alv_data>-zztipolav && <ls_alv_data>-zzptf
           bus_area        = <ls_alv_data>-rbusa
           segment         = <ls_alv_data>-zzlob
*           trade_id        = <ls_alv_data>-rassc
*           housebankid     = <ls_alv_data>-int_bank
*           housebankacctid = <ls_alv_data>-account_id
           alloc_nmbr      = <ls_alv_data>-zuonr
           value_date      = <ls_alv_data>-valut
           customer        = <ls_alv_data>-kunnr
           vendor_no       = <ls_alv_data>-lifnr
           pstng_date      = <ls_alv_data>-budat
           doc_type        = <ls_alv_data>-blart
           tax_code        = <ls_alv_data>-mwskz
           ref_key_1       = <ls_alv_data>-zzcciv
           ref_key_2       = <ls_alv_data>-zzrff
         )
         ( itemno_acc      = 2
           gl_account      = <ls_alv_data>-hkont_gtran "racct
           comp_code       = <ls_alv_data>-bukrs
           item_text       = <ls_alv_data>-sgtxt
           tr_part_ba      = <ls_alv_data>-zzramo && <ls_alv_data>-zztipolav && <ls_alv_data>-zzptf
           bus_area        = <ls_alv_data>-rbusa
           segment         = <ls_alv_data>-zzlob
*           trade_id        = <ls_alv_data>-rassc
*           housebankid     = <ls_alv_data>-int_bank
*           housebankacctid = <ls_alv_data>-account_id
           alloc_nmbr      = <ls_alv_data>-zuonr
           value_date      = <ls_alv_data>-valut
           customer        = <ls_alv_data>-kunnr
           vendor_no       = <ls_alv_data>-lifnr
           pstng_date      = <ls_alv_data>-budat
           doc_type        = <ls_alv_data>-blart
           tax_code        = <ls_alv_data>-mwskz
           ref_key_1       = <ls_alv_data>-zzcciv
           ref_key_2       = <ls_alv_data>-zzrff
         ) ).

        CLEAR: ls_ext2,ls_zci_cobl.

        ls_zci_cobl = VALUE #( zz_tipo_polizza = <ls_alv_data>-zzantpol
                               posnr         = CONV posnr( <ls_alv_data>-buzei ) ).

        ls_ext2-structure = 'ZCI_COBL'.

        cl_abap_container_utilities=>fill_container_c(
          EXPORTING
            im_value               = ls_zci_cobl                 " Data for Filling Container
          IMPORTING
            ex_container           = lv_char960                 " Container
          EXCEPTIONS
            illegal_parameter_type = 1                " Invalid type for the parameter IM_VALUE
            OTHERS                 = 2
        ).

        ls_ext2+30 = lv_char960.
        APPEND ls_ext2 TO <ls_post>-exten.

        CLEAR: ls_ext2,ls_zci_cobl.

        ls_zci_cobl = VALUE #( zz_tipo_polizza = <ls_alv_data>-zzantpol
                               posnr         = CONV posnr( 2 ) ).

        ls_ext2-structure = 'ZCI_COBL'.

        cl_abap_container_utilities=>fill_container_c(
          EXPORTING
            im_value               = ls_zci_cobl                 " Data for Filling Container
          IMPORTING
            ex_container           = lv_char960                 " Container
          EXCEPTIONS
            illegal_parameter_type = 1                " Invalid type for the parameter IM_VALUE
            OTHERS                 = 2
        ).

        ls_ext2+30 = lv_char960.
        APPEND ls_ext2 TO <ls_post>-exten.

        <ls_post>-currc = VALUE #(
        (
          itemno_acc = 1
          curr_type  = '00'
          currency   = <ls_alv_data>-rwcur "WAERS
          amt_doccur = <ls_bapi_accr>-amt_doccur * -1
          exch_rate  = <ls_bapi_accr>-exch_rate
        )
        (
          itemno_acc = 2
          curr_type  = '00'
          currency   = <ls_alv_data>-rwcur "WAERS
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
            iv_progr = lv_prog_nr
            iv_test  = iv_test
            is_head  = <ls_post>-header
            it_glacc = <ls_post>-items
            it_curre = <ls_post>-currc
            it_exten = <ls_post>-exten
            iv_doc   = CONV #( <ls_post>-header-ref_doc_no )
            iv_buzei = lv_buzei "<ls_post>-buzei
            iv_bukrs = <ls_post>-header-comp_code
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

        IF lv_error = abap_true ."IS NOT INITIAL.
          iv_no_commit = abap_true.
*          EXIT.
        ENDIF.

      ENDLOOP.

      LOOP AT GROUP <lg_keys> ASSIGNING <ls_alv_data>.
        IF lv_error IS NOT INITIAL.
          <ls_alv_data>-status = icon_red_light.
          CONTINUE.
        ELSE.
          <ls_alv_data>-status = icon_green_light.
        ENDIF.

        IF iv_test IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        <ls_alv_data>-belnr  = lv_main_belnr.


        READ TABLE lt_belnr ASSIGNING FIELD-SYMBOL(<ls_belnr>) WITH KEY buzei = <ls_alv_data>-buzei.
        IF sy-subrc = 0.
          <ls_alv_data>-belnr_gtran = <ls_belnr>-belnr.
        ENDIF.

        IF ls_zgl_t_ripresa_mo IS INITIAL.

*          DATA: lv_belnr TYPE belnr_d,
*                lv_posnr TYPE posnr_acc.
*
*          IF lv_belnr IS INITIAL.
*          ELSEIF lv_belnr <> <ls_alv_data>-belnr.
*            lv_belnr = <ls_alv_data>-belnr.
*            CLEAR lv_posnr.
*          ENDIF.
*
*          lv_posnr += 1.
*          <ls_alv_data>-itemno_acc = lv_posnr.

          MOVE-CORRESPONDING <ls_alv_data> TO ls_zgl_t_ripresa_mo.
          APPEND ls_zgl_t_ripresa_mo TO lt_zgl_t_ripresa_mo.

        ENDIF.
        CLEAR ls_zgl_t_ripresa_mo.
      ENDLOOP.

      IF iv_test IS INITIAL AND lv_no_commit IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      CLEAR: lv_no_commit, lt_post_bapi.

    ENDLOOP.

    IF iv_test IS INITIAL.
      INSERT zgl_t_ripresa_mo FROM TABLE lt_zgl_t_ripresa_mo.
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

    DATA(lo_funct) = mo_salv->get_functions( ).
    lo_funct->set_all( abap_true ).

    TRY.
        lo_columns = mo_salv->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column ?= lo_columns->get_column( 'STATUS' ).
        lo_column->set_icon( ).
        lo_column->set_short_text( value = 'Stato' ).
        lo_column->set_medium_text( value = 'Stato' ).
        lo_column->set_long_text( value = 'Stato' ).

        lo_column ?= lo_columns->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        lo_column ?= lo_columns->get_column( 'BELNR_GTRAN' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

*        lv_text = 'Conto COGE Giroconto'.
*        lo_column ?= lo_columns->get_column( 'HKONT_GTRAN' ).
*        lo_column->set_short_text( CONV #( lv_text ) ).
*        lo_column->set_medium_text( CONV #( lv_text ) ).
*        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_columns->get_column( columnname = 'HSL' )->set_currency( value = 'EUR' ).
        lo_columns->get_column( columnname = 'WSL' )->set_currency_column( value = 'CURRENCY' ).

      CATCH cx_salv_data_error. " ALV: General Error Class (Checked in Syntax Check)
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        DATA(lo_sorts) = mo_salv->get_sorts( ).
        DATA(lo_sort) =
            lo_sorts->add_sort(
              EXPORTING
                columnname = 'BELNR'                           " ALV CONTROL: FIELD NAME OF INTERNAL TABLE FIELD
                position   = 1
                subtotal   = if_salv_c_bool_sap=>true
            ).

        DATA(lo_aggr) = mo_salv->get_aggregations( ).
        DATA(lo_aggr_col) = lo_aggr->add_aggregation( columnname  = 'HSL' ).
        lo_aggr->add_aggregation( columnname  = 'WSL' ).

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
        default_filename      = 'TXT'
        file_filter           = cl_gui_frontend_services=>filetype_text
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
    DATA: lt_return     TYPE TABLE OF bapiret2,
          lt_glacc      TYPE STANDARD TABLE OF bapiacgl09,
          lt_curre      TYPE STANDARD TABLE OF bapiaccr09,
          lt_extension2 TYPE TABLE OF  bapiparex.

    lt_glacc = it_glacc.
    lt_curre = it_curre.
    lt_extension2 = it_exten.

    IF iv_test = abap_true.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = is_head
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return
          extension2     = lt_extension2.
    ELSE.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = is_head
        IMPORTING
          obj_key        = ev_obj_key
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return
          extension2     = lt_extension2.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    DATA(lv_uzeit) = sy-uzeit.
    DATA(lv_progr) = 0.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
*       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).
*
*      IF <ls_return>-type = 'S'
*        AND <ls_return>-id = 'RW'.
*
*        CASE <ls_return>-number.
*          WHEN 614.
*
*            IF iv_buzei IS INITIAL.
*              <ls_return>-message = 'Document Ready for Registration COTEC'(013) .  " Documento registrato COTEC
*            ELSE.
*              <ls_return>-message = 'Document Ready for Registration Girofondo Transito'(014) . "Documento registrato Girofondo Transitorio
*            ENDIF.
*
*          WHEN 605.
*
*            IF iv_buzei IS INITIAL.
*              <ls_return>-message = 'Registered Document COTEC'(015) .
*            ELSE.
*              <ls_return>-message = 'Registered Document Transitional Girofondo'(016).
*            ENDIF.
*        ENDCASE.
*
*      ENDIF.

      lv_progr += 1.
      APPEND VALUE #( prog_nr = lv_progr
                      time    = lv_uzeit
                      bukrs   = iv_bukrs
                      awkey   = iv_doc
                      buzei   = iv_buzei
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                      belnr   = COND #( WHEN ev_obj_key <> '$' THEN ev_obj_key(10) ) "ev_obj_key(10)
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
            iv_gjahr = <ls_data>-gjahr
            iv_bukrs = <ls_data>-bukrs
        ).
*
      WHEN 'BELNR_GTRAN'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_data>-belnr_gtran
            iv_gjahr = <ls_data>-gjahr
            iv_bukrs = <ls_data>-bukrs
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
            iv_gjahr = <ls_msg>-gjahr
            iv_bukrs = <ls_msg>-bukrs
        ).

    ENDCASE.

  ENDMETHOD.
  METHOD view_belnr.

    IF iv_belnr IS INITIAL.
      RETURN.
    ENDIF.

    SET PARAMETER ID 'GJR' FIELD iv_gjahr.
    SET PARAMETER ID 'BUK' FIELD iv_bukrs.
    SET PARAMETER ID 'BLN' FIELD iv_belnr.

    CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_stock_reg=>value_request( ).

START-OF-SELECTION.
  go_stock_reg = NEW lcl_stock_reg( ).
  go_stock_reg->execute( ).

*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET TITLEBAR 'TB_IVASS'.
  SET PF-STATUS 'PF-STATUS'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA lv_no_commit TYPE abap_bool.
  CLEAR: go_stock_reg->mt_messages.

  CASE gs_screen0001-ok_code.
    WHEN 'FC_SIMU'.
      go_stock_reg->post_doc( iv_test  = abap_true ).
      SORT go_stock_reg->mt_messages BY prog_nr message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_stock_reg->mt_messages COMPARING message prog_nr.
      go_stock_reg->display_msg( ).
      go_stock_reg->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    WHEN 'FC_POST'.

      go_stock_reg->post_doc(
      EXPORTING
         iv_test  = abap_true
      CHANGING
         iv_no_commit = lv_no_commit ).

      DELETE go_stock_reg->mt_messages WHERE icon = icon_green_light.
      SORT go_stock_reg->mt_messages BY prog_nr message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_stock_reg->mt_messages COMPARING message prog_nr.

      IF lv_no_commit IS INITIAL.
        CLEAR go_stock_reg->mt_messages.
        go_stock_reg->post_doc( iv_test  = abap_false ).
      ENDIF.

      go_stock_reg->display_msg( ).
      go_stock_reg->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
