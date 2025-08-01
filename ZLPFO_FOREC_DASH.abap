*&---------------------------------------------------------------------*
*& Report ZLPFO_FOREC_DASH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlpfo_forec_dash.
TABLES: zlpfo_mon_h,
        zlpfo_mon_p.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     p_werks TYPE zlpfo_mon_h-werks OBLIGATORY DEFAULT 'P994'.
SELECT-OPTIONS :s_markt  FOR  zlpfo_mon_h-market,
                s_matnr FOR zlpfo_mon_h-matnr,
                s_extco FOR zlpfo_mon_h-zzcolorex,
                s_intco FOR zlpfo_mon_h-zzsellerx,
                s_optst  FOR  zlpfo_mon_h-zzoptstrx,
                s_forty  FOR  zlpfo_mon_h-fortype,
                s_round  FOR  zlpfo_mon_h-round,
                s_upldt  FOR  zlpfo_mon_h-upldate,
                s_foyer  FOR  zlpfo_mon_h-foryear,
                s_fomon  FOR  zlpfo_mon_p-formonth OBLIGATORY DEFAULT '01' TO '12',
                s_prpnt  FOR  zlpfo_mon_h-chr_plant,
                s_ifdat FOR zlpfo_mon_h-ifdate,
                s_ifres FOR zlpfo_mon_p-ifresult,
                s_appdt FOR zlpfo_mon_h-appr_dt.
SELECTION-SCREEN END OF BLOCK b1.
CLASS lcl_forecast_dashboard DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :execute,
      user_command_0100.
    DATA:
      BEGIN OF ms_screen100,
        ok_code TYPE syucomm,
      END OF ms_screen100.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_display_data,
             status           TYPE zlpfo_mon_h-status,                      " Status
             werks            TYPE zlpfo_mon_h-werks,                       " Plant
             market           TYPE zlpfo_mon_h-market,                      " Market
             matnr            TYPE zlpfo_mon_h-matnr,                       " Model
             descr_veicolo    TYPE zrsdap_psa_pc016-descr_veicolo,          " Model description
             zzcolorex        TYPE zlpfo_mon_h-zzcolorex ,                  " External color
             descr_colore_est TYPE zrsdap_psa_pc030-descr_colore_est ,      " External color description
             zzsellerx        TYPE zlpfo_mon_h-zzsellerx ,                  " Internal color
             descr_selleria   TYPE zrsdap_psa_pc031-descr_selleria ,        " Internal color description
             zzoptstrx        TYPE zlpfo_mon_h-zzoptstrx,                   " OPT string
*            lcdv24                                                         " LCDV24
*            model pn18                                                     " Model PN18
*            model sincom                                                   " Model Sincom
             fortype          TYPE zlpfo_mon_h-fortype ,                    " Forecast
             round            TYPE zlpfo_mon_h-round,                       " Round
             upldate          TYPE zlpfo_mon_h-upldate ,                    " Upload date
             foryear          TYPE zlpfo_mon_h-foryear,                     " Forecast year
             chr_plant        TYPE zlpfo_mon_h-chr_plant ,                  " Factory plant
             appr_dt          TYPE zlpfo_mon_h-appr_dt,                     " Approval date
             ifdate           TYPE zlpfo_mon_h-ifdate,                      " Interface date
             ifresult         TYPE zlpfo_mon_p-ifresult ,                   " Interface result
             sched            TYPE zlpfo_mon_p-sched,                       " Scheduled vehicles
             notsched         TYPE zlpfo_mon_p-notsched,                    " Not scheduled vehicles
             m01              TYPE mseg-erfmg,
             m02              TYPE mseg-erfmg,
             m03              TYPE mseg-erfmg,
             m04              TYPE mseg-erfmg,
             m05              TYPE mseg-erfmg,
             m06              TYPE mseg-erfmg,
             m07              TYPE mseg-erfmg,
             m08              TYPE mseg-erfmg,
             m09              TYPE mseg-erfmg,
             m10              TYPE mseg-erfmg,
             m11              TYPE mseg-erfmg,
             m12              TYPE mseg-erfmg,
             read             TYPE abap_bool,
           END OF ty_display_data,
           tt_display_data TYPE STANDARD TABLE OF ty_display_data.
    DATA: mt_display_data TYPE tt_display_data,
          mo_alv_grid     TYPE REF TO cl_gui_alv_grid,
          mo_cust_grid    TYPE REF TO cl_gui_custom_container.
    METHODS: get_data,
      set_first_display,
      get_fcat_grid RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat,
      approve_data,
      remove_approval.
ENDCLASS.
DATA : go_forecast_dashboard TYPE REF TO lcl_forecast_dashboard.
CLASS lcl_forecast_dashboard IMPLEMENTATION.
  METHOD execute.
    get_data( ).
    set_first_display( ).
  ENDMETHOD.
  METHOD get_data.
    mt_display_data = VALUE #( ( status = 'A' m01 = 2 ) ( status = 'N' m02 = 3 ) ( status = 'R' m04 = 12 ) ).
    RETURN.
    SELECT hdr~status,
           hdr~werks,
           hdr~market,
           hdr~matnr,
           pc016~descr_veicolo,
           hdr~zzcolorex,
           pc030~descr_colore_est,
           hdr~zzsellerx,
           pc031~descr_selleria,
           hdr~zzoptstrx,
           hdr~fortype,
           hdr~round,
           hdr~upldate,
           hdr~foryear,
           hdr~chr_plant,
           hdr~appr_dt,
           hdr~ifdate
      FROM zlpfo_mon_h AS hdr
      LEFT JOIN zrsdap_psa_pc016 AS pc016
       ON hdr~market   = pc016~mercato
      AND hdr~matnr    = pc016~matnr
      AND pc016~lingua = @sy-langu
      LEFT JOIN zrsdap_psa_pc030 AS pc030
       ON hdr~market    = pc030~mercato
      AND hdr~matnr     = pc030~matnr
      AND hdr~zzcolorex = pc030~cod_col_est_psa
      AND pc030~lingua  = @sy-langu
      LEFT JOIN zrsdap_psa_pc031 AS pc031
       ON hdr~market    = pc031~mercato
      AND hdr~matnr     = pc031~matnr
      AND hdr~zzsellerx = pc031~cod_selleria
      AND pc031~lingua  = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @mt_display_data
      WHERE hdr~werks     =  @p_werks
        AND hdr~market    IN @s_markt
        AND hdr~matnr     IN @s_matnr
        AND hdr~zzcolorex IN @s_extco
        AND hdr~zzsellerx IN @s_intco
        AND hdr~zzoptstrx IN @s_optst
        AND hdr~fortype    IN @s_forty
        AND hdr~round      IN @s_round
        AND hdr~upldate    IN @s_upldt
        AND hdr~foryear    IN @s_foyer
        AND hdr~chr_plant  IN @s_prpnt
        AND hdr~appr_dt   IN @s_appdt
        AND hdr~ifdate    IN @s_ifdat
      ORDER BY hdr~werks,
               hdr~market,
               hdr~matnr,
               hdr~zzcolorex,
               hdr~zzsellerx,
               hdr~zzoptstrx,
               hdr~fortype,
               hdr~round,
               hdr~foryear.
    IF mt_display_data IS INITIAL.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SELECT werks,
           market,
           matnr,
           zzcolorex,
           zzsellerx,
           zzoptstrx,
           fortype,
           round,
           foryear,
           formonth,
           qnt,
           ifresult,
           sched,
           notsched
      FROM zlpfo_mon_p
      FOR ALL ENTRIES IN @mt_display_data
      WHERE werks      = @mt_display_data-werks
        AND market     = @mt_display_data-market
        AND matnr      = @mt_display_data-matnr
        AND zzcolorex  = @mt_display_data-zzcolorex
        AND zzsellerx  = @mt_display_data-zzsellerx
        AND zzoptstrx  = @mt_display_data-zzoptstrx
        AND fortype    = @mt_display_data-fortype
        AND round      = @mt_display_data-round
        AND foryear    = @mt_display_data-foryear
        AND formonth  IN @s_fomon
        AND ifresult  IN @s_ifres
     INTO TABLE @DATA(lt_items).
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      READ TABLE mt_display_data ASSIGNING FIELD-SYMBOL(<ls_disp>)
        WITH KEY werks      = <ls_item>-werks
                 market     = <ls_item>-market
                 matnr      = <ls_item>-matnr
                 zzcolorex  = <ls_item>-zzcolorex
                 zzsellerx  = <ls_item>-zzsellerx
                 zzoptstrx  = <ls_item>-zzoptstrx
                 fortype    = <ls_item>-fortype
                 round      = <ls_item>-round
                 foryear    = <ls_item>-foryear BINARY SEARCH.
      IF sy-subrc <> 0. " could never happen
        CONTINUE.
      ENDIF.
      IF <ls_disp>-read IS INITIAL.
<ls_disp>-ifresult  = <ls_item>-ifresult.
<ls_disp>-sched     = <ls_item>-sched.
<ls_disp>-notsched  = <ls_item>-notsched.
<ls_disp>-read  = abap_true.
      ENDIF.
      ASSIGN COMPONENT |M{ <ls_item>-formonth }| OF STRUCTURE <ls_disp> TO FIELD-SYMBOL(<lv_mnum>).
      IF sy-subrc <> 0. " could never happen
        CONTINUE.
      ENDIF.
<lv_mnum> = <ls_item>-qnt.
    ENDLOOP.
*    DELETE mt_display_data WHERE read = abap_false.
  ENDMETHOD.
  METHOD set_first_display.
    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).
    DATA(lt_fcat) = get_fcat_grid( ).
    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                        sel_mode   = 'D'
                                        ).
    mo_alv_grid->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = mt_display_data
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
         invalid_parameter_combination = 1                " Wrong Parameter
         program_error                 = 2                " Program Errors
         too_many_lines                = 3                " Too many Rows in Ready for Input Grid
         OTHERS                        = 4
       ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL SCREEN 100.
  ENDMETHOD.
  METHOD get_fcat_grid.
    rt_fcat = VALUE #(
       ( fieldname = 'STATUS'             ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'WERKS'              ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'MARKET'             ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'MATNR'              ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'DESCR_VEICOLO'      ref_table  = 'ZRSDAP_PSA_PC016' )
       ( fieldname = 'ZZCOLOREX'          ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'DESCR_COLORE_EST'   ref_table  = 'ZRSDAP_PSA_PC030' )
       ( fieldname = 'ZZSELLERX'          ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'DESCR_SELLERIA'     ref_table  = 'ZRSDAP_PSA_PC031' )
       ( fieldname = 'ZZOPTSTRX'          ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'FORTYPE'            ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'ROUND'              ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'UPLDATE'            ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'FORYEAR'            ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'CHR_PLANT'          ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'APPR_DT'            ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'IFDATE'             ref_table  = 'ZLPFO_MON_H'      )
       ( fieldname = 'IFRESULT'           ref_table  = 'ZLPFO_MON_P'      )
       ( fieldname = 'SCHED'              ref_table  = 'ZLPFO_MON_P'      )
       ( fieldname = 'NOTSCHED'           ref_table  = 'ZLPFO_MON_P'      )
    ).
    DO 12 TIMES.
      DATA(lv_index) = sy-index.
      DATA(lv_month) = CONV numc2( lv_index ).
      IF lv_month NOT IN s_fomon.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO rt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
<ls_fcat>-fieldname = |M{ lv_month }|.
<ls_fcat>-quantity  = 'ST'.
<ls_fcat>-no_zero   = abap_true.
<ls_fcat>-ref_field = 'ERFMG'.
<ls_fcat>-ref_table = 'MSEG'.
<ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-reptext = |M{ lv_index }|.
    ENDDO.
  ENDMETHOD.
  METHOD approve_data.
    DATA : lt_sel_data TYPE STANDARD TABLE OF ty_display_data.
    mo_alv_grid->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(lt_sel_rows)
    ).
    IF lt_sel_rows IS INITIAL.
      MESSAGE 'Please, select at least 1 line to continue!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<ls_sel_rows>).
      READ TABLE mt_display_data ASSIGNING FIELD-SYMBOL(<ls_display_data>) INDEX <ls_sel_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <ls_display_data>-status <> 'N' OR <ls_display_data>-status <> 'R' .
        MESSAGE 'Selection contains status different from N and R' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      APPEND <ls_display_data> TO lt_sel_data.
    ENDLOOP.
    LOOP AT lt_sel_data ASSIGNING FIELD-SYMBOL(<ls_sel_data>).
      UPDATE zlpfo_mon_h
      SET status = 'A',
      approver  = @sy-uname,
      appr_dt = @sy-datum
      WHERE werks = @<ls_sel_data>-werks
      AND market = @<ls_sel_data>-market
      AND matnr = @<ls_sel_data>-matnr.
    ENDLOOP.
    MESSAGE 'Approval complated' TYPE 'S'.
  ENDMETHOD.
  METHOD remove_approval.
    DATA : lt_sel_data TYPE STANDARD TABLE OF ty_display_data.
    mo_alv_grid->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(lt_sel_rows)
    ).
    IF lt_sel_rows IS INITIAL.
      MESSAGE 'Please, select at least 1 line to continue!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<ls_sel_rows>).
      READ TABLE mt_display_data ASSIGNING FIELD-SYMBOL(<ls_display_data>) INDEX <ls_sel_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <ls_display_data>-status <> 'A'.
        MESSAGE 'Selection contains status different from A' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      APPEND <ls_display_data> TO lt_sel_data.
    ENDLOOP.
    LOOP AT lt_sel_data ASSIGNING FIELD-SYMBOL(<ls_sel_data>).
      UPDATE zlpfo_mon_h
      SET status = 'N',
      approver  = '',
      appr_dt = ''
      WHERE werks = @<ls_sel_data>-werks
      AND market = @<ls_sel_data>-market
      AND matnr = @<ls_sel_data>-matnr.
    ENDLOOP.
    MESSAGE 'Approval removed' TYPE 'S'.
  ENDMETHOD.
  METHOD user_command_0100.
    CASE ms_screen100-ok_code.
      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'FC_APPROVE'.
        approve_data( ).
      WHEN 'FC_REMOVE'.
        remove_approval( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  CREATE OBJECT go_forecast_dashboard.
  go_forecast_dashboard->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZPF_FOREC_DASH'.
  SET TITLEBAR 'ZT_FOREC_DASH'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_forecast_dashboard->user_command_0100( ).
ENDMODULE.
