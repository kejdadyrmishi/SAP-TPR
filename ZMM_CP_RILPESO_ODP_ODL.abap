*&---------------------------------------------------------------------*
*& Report ZMM_CP_RILPESO_ODP_ODL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_rilpeso_odp_odl.

DATA: BEGIN OF gs_screen100,
        ok_code           TYPE sy-ucomm,
        cont_show         TYPE abap_bool,
        p_pr_ord          TYPE abap_bool VALUE abap_true,
        p_cn_wrk          TYPE abap_bool,
        p_psc1            TYPE abap_bool VALUE abap_true,
        p_psc2            TYPE abap_bool,
        p_zzpeso          TYPE zmm_cp_peso_fasi-zzpeso,
        p_zzpesopietre    TYPE zmm_cp_peso_fasi-zzpesopietre,
        zzpesopietre_desc TYPE makt-maktx,
        p_zzpesoother     TYPE zmm_cp_peso_fasi-zzpesoother,
        zzpesoother_desc  TYPE makt-maktx,
        p_aufnr           TYPE afko-aufnr,
        p_vornr           TYPE zmm_cp_peso_fasi-vornr,
        p_aufnr2          TYPE afko-aufnr,
        p_vornr2          TYPE zmm_cp_peso_fasi-vornr,
        p_ebeln2          TYPE ebeln,
        p_werks           TYPE werks_d,
        p_lgort           TYPE lgort_d,
        waste             TYPE abap_bool,
      END OF gs_screen100.

DATA: BEGIN OF gs_screen101,
        ok_code TYPE sy-ucomm,
        zzpeso  TYPE zmm_cp_peso_fasi-zzpeso,
      END OF gs_screen101.

DATA: BEGIN OF gs_materials,
        wip_other  TYPE matnr VALUE 'WIP_OTHER',
        wip_carati TYPE matnr VALUE 'WIP_CARATI',
      END OF gs_materials.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_waste_tab,
             zmatwaste TYPE ztb_matcomp-zmatwaste,
             maktx     TYPE maktx,
             lgort     TYPE lgort_d,
             lgobe     TYPE lgobe,
             peso      TYPE zmm_cp_peso,
             meins     TYPE meins,
           END OF ty_waste_tab,
           tt_waste_tab TYPE TABLE OF ty_waste_tab.

    DATA: mo_grid       TYPE REF TO cl_gui_alv_grid,
          mt_waste_tab  TYPE tt_waste_tab,
          mt_scrap      TYPE tt_waste_tab,
          mt_scrap_temp TYPE tt_waste_tab,
          mo_cont       TYPE REF TO cl_gui_custom_container,
          mv_exe_gmvt   TYPE abap_bool,
          ms_weight     TYPE zmm_cp_peso_fasi.

    METHODS execute.

    METHODS fill_waste_weight.

    METHODS save.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_waste_mat,
             prgrp     TYPE pgmi-prgrp,
             zmatcomp  TYPE zmm_compteo-zmatcomp,
             ntgew     TYPE zmm_compteo-ntgew,
             zmatwaste TYPE ztb_matcomp-zmatwaste,
           END OF ty_waste_mat,
           tt_mat_waste TYPE TABLE OF ty_waste_mat WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_filler_data,
             steus    TYPE afvc-steus,
             aufpl    TYPE afko-aufpl,
             arbpl    TYPE crhd-arbpl,
             zzcalo   TYPE zmm_cp_perc_amm-zzcalo,
             pernr    TYPE afru-pernr,
             sname    TYPE pa0001-sname,
             lgort    TYPE zmm_cp_artg_cid-lgort,
             werks    TYPE zmm_cp_artg_cid-werks,
             matwaste TYPE tt_mat_waste,
           END OF ty_filler_data.


    METHODS execute_prod_interna.

    METHODS get_waste_materials.

    METHODS get_data_from_document EXPORTING ev_lifnr      TYPE lifnr
                                             ev_pesopietre TYPE zmm_cp_peso
                                             ev_pesoother  TYPE zmm_cp_peso.

    METHODS get_filler_data IMPORTING iv_aufnr       TYPE aufnr
                                      iv_vornr       TYPE vornr
                                      iv_lifnr       TYPE lifnr OPTIONAL
                            EXPORTING ev_matnr       TYPE matnr
                                      es_operation   TYPE bapi_order_operation1
                                      ev_error       TYPE abap_bool
                                      es_filler_data TYPE ty_filler_data.

    METHODS execute_prod_esterna.

    METHODS update_peso_scarti IMPORTING iv_steus TYPE steus
                                         iv_arbpl TYPE arbpl
                                         iv_mblnr TYPE mblnr
                                         iv_mjahr TYPE mjahr .

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.
INCLUDE zmm_cp_rilpeso_odp_odl_pbo.
INCLUDE zmm_cp_rilpeso_odp_odl_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.

    IF sy-sysid = 'BCD'.
      gs_materials-wip_other  = 'WIP_PAA'.
      gs_materials-wip_carati = 'WIP_PC'.
    ENDIF.

    fill_waste_weight( ).

  ENDMETHOD.                    "execute

  METHOD save.
    DATA lr_ebeln TYPE RANGE OF ebeln.

    CASE abap_true.
      WHEN gs_screen100-p_pr_ord.
        DATA(lv_aufnr) = gs_screen100-p_aufnr.
        DATA(lv_vornr) = gs_screen100-p_vornr.

      WHEN gs_screen100-p_cn_wrk.
        lv_aufnr = gs_screen100-p_aufnr2.
        lv_vornr = gs_screen100-p_vornr2.
        lr_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = gs_screen100-p_ebeln2 ) ).

        SELECT SINGLE @abap_true
          FROM ekko
          INTO @DATA(lv_exist)
          WHERE ebeln = @gs_screen100-p_ebeln2.

        IF sy-subrc <> 0.
          MESSAGE s022(zmm_cp_msg) WITH gs_screen100-p_ebeln2 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

    ENDCASE.

    IF lv_aufnr IS INITIAL.
      MESSAGE s000(zmm_cp_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE @abap_true
      FROM afko
      INTO @lv_exist
      WHERE aufnr  = @lv_aufnr.

    IF sy-subrc <> 0.
      MESSAGE s001(zmm_cp_msg) WITH lv_aufnr DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE @abap_true
      FROM afko
      JOIN afvc
        ON afko~aufpl = afvc~aufpl
      INTO @lv_exist
      WHERE afko~aufnr = @lv_aufnr
      AND   afvc~vornr = @lv_vornr.

    IF sy-subrc <> 0.
      MESSAGE s012(zmm_cp_msg) WITH lv_vornr lv_aufnr DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT *
      FROM zmm_cp_peso_fasi
      INTO TABLE @DATA(lt_saved_weights)
      WHERE aufnr = @lv_aufnr
        AND vornr = @lv_vornr
        AND ebeln IN @lr_ebeln
        AND ebelp = ' '.

    IF ( gs_screen100-p_psc1 = abap_true AND lt_saved_weights IS NOT INITIAL ) OR
       ( gs_screen100-p_psc2 = abap_true AND lines( lt_saved_weights ) = 2 ).

      MESSAGE s006(zmm_cp_msg) WITH gs_screen100-p_aufnr gs_screen100-p_vornr DISPLAY LIKE 'E'.
      CLEAR gs_screen100-ok_code.
      RETURN.

    ELSEIF  gs_screen100-p_psc2 = abap_true AND lt_saved_weights IS INITIAL.

      MESSAGE s007(zmm_cp_msg) DISPLAY LIKE 'E'.
      CLEAR gs_screen100-ok_code.
      RETURN.

    ENDIF.

    READ TABLE lt_saved_weights INTO ms_weight INDEX 1.
    IF sy-subrc <> 0 AND gs_screen100-p_psc2 = abap_true.
      RETURN.
    ELSEIF gs_screen100-p_psc2 = abap_true.
      IF ms_weight-zzpeso < gs_screen100-p_zzpeso.
        MESSAGE w018(zmm_cp_msg).
      ENDIF.
    ENDIF.

    IF gs_screen100-p_psc1 = abap_true.
      SELECT vornr, zztipo, zzpeso
        FROM zmm_cp_peso_fasi
        INTO TABLE @DATA(lt_completed_weights)
        WHERE aufnr = @gs_screen100-p_aufnr
        AND   vornr < @gs_screen100-p_vornr
        AND   ebeln IN @lr_ebeln
        AND   ebelp = ' '
        ORDER BY vornr  DESCENDING, zztipo ASCENDING.

      READ TABLE lt_completed_weights ASSIGNING FIELD-SYMBOL(<ls_compl_weight>) INDEX 1.
      IF sy-subrc = 0.

        IF <ls_compl_weight>-zztipo = 'I'.

          MESSAGE s010(zmm_cp_msg) WITH <ls_compl_weight>-vornr DISPLAY LIKE 'E'.
          CLEAR gs_screen100-ok_code.
          RETURN.
        ELSE.

          IF <ls_compl_weight>-zzpeso > gs_screen100-p_zzpeso.

            MESSAGE w011(zmm_cp_msg) WITH <ls_compl_weight>-vornr.

          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    CASE abap_true.

      WHEN gs_screen100-p_pr_ord.

        execute_prod_interna( ).

      WHEN gs_screen100-p_cn_wrk.

        execute_prod_esterna( ).

    ENDCASE.

  ENDMETHOD.

  METHOD execute_prod_esterna.

    DATA: ls_weight      TYPE zmm_cp_peso_fasi,
          ls_operation   TYPE bapi_order_operation1,
          lv_matnr       TYPE matnr,
          lv_pesopietre  TYPE zmm_cp_peso,
          lv_pesoother   TYPE zmm_cp_peso,
          lv_lifnr       TYPE lifnr,
          lv_error       TYPE abap_bool,
          ls_filler_data TYPE ty_filler_data.

    get_data_from_document(
      IMPORTING
        ev_lifnr      = lv_lifnr
        ev_pesopietre = lv_pesopietre
        ev_pesoother  = lv_pesoother ).

    get_filler_data(
      EXPORTING
        iv_aufnr       = gs_screen100-p_aufnr2
        iv_vornr       = gs_screen100-p_vornr2
        iv_lifnr       = lv_lifnr
      IMPORTING
        ev_matnr       = lv_matnr
        es_operation   = ls_operation
        ev_error       = lv_error
        es_filler_data = ls_filler_data  ).

    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE ekpo~ebelp
      FROM eban
      JOIN ekpo
        ON eban~ebeln = ekpo~ebeln
       AND eban~ebelp = ekpo~ebelp
       AND ekpo~loekz = ''
     WHERE eban~banfn = @ls_operation-purchase_req_no
       AND eban~ebeln = @gs_screen100-p_ebeln2
      INTO @DATA(lv_ebelp).
    IF sy-subrc <> 0.
      MESSAGE s025(zmm_cp_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zmm_cp_peso_fasi
      INTO @DATA(ls_peso_fasi)
      WHERE aufnr = @gs_screen100-p_aufnr2
      AND   vornr = @gs_screen100-p_vornr2
      AND   ebeln = @gs_screen100-p_ebeln2
      AND   zztipo = 'I'.

    DATA(lv_peso_neto)      = CONV zmm_cp_peso( gs_screen100-p_zzpeso - lv_pesopietre - lv_pesoother ).
    DATA(lv_calo_pat_abs)   = CONV zmm_cp_peso( abs( lv_peso_neto * ( ls_filler_data-zzcalo / 100 ) ) ).

    ls_weight = VALUE #( aufnr           = gs_screen100-p_aufnr2
                         vornr           = gs_screen100-p_vornr2
                         zztipo          = COND #( WHEN gs_screen100-p_psc1 = abap_true THEN 'I' ELSE 'F' )
                         ebeln           = gs_screen100-p_ebeln2
                         steus           = ls_operation-opr_cntrl_key
                         bname           = sy-uname
                         zzdataril       = sy-datum
                         zzoraril        = sy-uzeit
                         zzpesopietre    = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_pesopietre )
                         zzpesoother     = lv_pesoother
                         zzpesocalofasea = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_calo_pat_abs )
                         zzpesocalofaset = COND #( WHEN ls_peso_fasi IS NOT INITIAL THEN ls_peso_fasi-zzpeso - gs_screen100-p_zzpeso )
                         zzpeso          = gs_screen100-p_zzpeso
                         zzcalo          = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN ls_filler_data-zzcalo )
                         matnr           = lv_matnr
                         lifnr           = lv_lifnr
                         ).

    INSERT zmm_cp_peso_fasi FROM ls_weight.

    IF sy-subrc = 0.
      MESSAGE s002(zmm_cp_msg).

      IF gs_screen100-p_psc2 = abap_true.

        UPDATE zmm_cp_peso_fasi
           SET zzpesoother = 0
         WHERE aufnr   = gs_screen100-p_aufnr2
           AND vornr   = gs_screen100-p_vornr2
           AND zztipo  = 'I'
           AND ebeln   = gs_screen100-p_ebeln2.

      ENDIF.
    ELSE.
      MESSAGE s003(zmm_cp_msg) DISPLAY LIKE 'E'.
    ENDIF.

    CLEAR: ls_weight.

  ENDMETHOD.

  METHOD execute_prod_interna.

    DATA: ls_weight             TYPE zmm_cp_peso_fasi,
          ls_filler_data        TYPE ty_filler_data,
          ls_gm_header          TYPE bapi2017_gm_head_01,
          ls_operation          TYPE bapi_order_operation1,
          ls_goodsmvt_code      TYPE bapi2017_gm_code,
          lt_gm_item            TYPE bapi2017_gm_item_create_t,
          lt_return             TYPE bapiret2_tab,
          lv_mblnr              TYPE mblnr,
          lv_mjahr              TYPE mjahr,
          lv_matnr              TYPE matnr,
          lv_gsgew              TYPE gsgew,
          lv_peso_grande_sfrido TYPE zmm_cp_peso,
          lv_error              TYPE abap_bool,
          lt_msg                TYPE  esp1_message_tab_type.

    mo_grid->check_changed_data( ).
    ls_goodsmvt_code-gm_code = '03'.

    IF gs_screen100-waste = abap_true.
      LOOP AT mt_waste_tab ASSIGNING FIELD-SYMBOL(<ls_waste_row>) .
        lv_peso_grande_sfrido = lv_peso_grande_sfrido + <ls_waste_row>-peso.
      ENDLOOP.

      IF lv_peso_grande_sfrido = 0.

        MESSAGE s004(zmm_cp_msg) DISPLAY LIKE 'E'.
        CLEAR gs_screen100-ok_code.
        RETURN.
      ENDIF.

      IF gs_screen100-p_lgort IS INITIAL
      OR gs_screen100-p_werks IS INITIAL.

        MESSAGE s005(zmm_cp_msg) DISPLAY LIKE 'E'.
        CLEAR gs_screen100-ok_code.
        RETURN.
      ENDIF.

    ELSE.
      lv_peso_grande_sfrido = 0.
    ENDIF.

    get_filler_data(
      EXPORTING
        iv_aufnr       = gs_screen100-p_aufnr
        iv_vornr       = gs_screen100-p_vornr
      IMPORTING
        ev_matnr       = lv_matnr
        es_operation   = ls_operation
        ev_error       = lv_error
        es_filler_data = ls_filler_data  ).

    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT r~rsnum,
           r~rspos,
           r~rsart
      FROM resb AS r
      INTO TABLE @DATA(lt_resb)
      WHERE r~aufnr = @gs_screen100-p_aufnr
        AND r~vornr = @gs_screen100-p_vornr
        AND EXISTS ( SELECT 1 FROM zmara WHERE zmara~matnr = r~matnr AND zmara~zmattyp = 'STONE' ).

    IF lt_resb IS NOT INITIAL.
      SELECT a~rspos, a~mjahr, a~mblnr, a~zeile, a~bwart

        FROM aufm AS a
        FOR ALL ENTRIES IN @lt_resb

       WHERE a~aufnr = @gs_screen100-p_aufnr
         AND a~rsnum = @lt_resb-rsnum
         AND a~rspos = @lt_resb-rspos
         AND a~rsart = @lt_resb-rsart
         AND a~bwart IN ( '261', '262' )
        INTO TABLE @DATA(lt_aufm).
      IF sy-subrc <> 0.
        MESSAGE s019(zmm_cp_msg) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SORT lt_aufm BY rspos
                      mjahr DESCENDING
                      mblnr DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_aufm COMPARING rspos.
      DELETE lt_aufm WHERE bwart = '262'.

      IF lines( lt_resb ) <> lines( lt_aufm ).
        MESSAGE s019(zmm_cp_msg) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SELECT s~gsgew,
             s~mblnr,
             s~mjahr,
             s~zeile
        FROM zmseg AS s
        FOR ALL ENTRIES IN @lt_aufm
        WHERE s~mblnr = @lt_aufm-mblnr
          AND s~mjahr = @lt_aufm-mjahr
          AND s~zeile = @lt_aufm-zeile
        INTO TABLE @DATA(lt_gsgew).

      lv_gsgew = REDUCE #( INIT lv_gw = EXACT zmseg-gsgew( 0 )
                           FOR <ls_gsgew> IN lt_gsgew
                           NEXT lv_gw = lv_gw + <ls_gsgew>-gsgew  ).

      IF lt_gsgew IS INITIAL.
        MESSAGE s019(zmm_cp_msg) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

    DATA lv_test TYPE abap_bool.

    IF gs_screen100-p_psc1 = abap_true.
      DATA(lv_pesopietre)         = CONV zmm_cp_peso( lv_gsgew ).
      DATA(lv_pesoother)          = gs_screen100-p_zzpesoother.
    ELSE.

      lv_pesopietre = gs_screen100-p_zzpesopietre.
      lv_pesoother  = gs_screen100-p_zzpesoother.

    ENDIF.

    IF gs_screen100-p_psc2 = abap_true.

      DATA(lv_peso_finale_pietre) = EXACT zmm_cp_peso( gs_screen100-p_zzpeso -  lv_pesopietre ).
      DATA(lv_calo_registrato)    = EXACT zmm_cp_peso( ms_weight-zzpeso -  lv_peso_finale_pietre - lv_pesoother - lv_peso_grande_sfrido ).
      DATA(lv_calo_ammesso_abs)   = CONV  zmm_cp_peso( abs( ( ( gs_screen100-p_zzpeso - lv_pesopietre - lv_peso_grande_sfrido - lv_pesoother ) * ls_filler_data-zzcalo ) / 100 ) ).
      DATA(lv_peso_sfrido_artig)  = EXACT zmm_cp_peso( lv_calo_registrato -  lv_calo_ammesso_abs ).

      IF ls_filler_data-matwaste IS NOT INITIAL.
        CLEAR mt_scrap.

        IF lines( ls_filler_data-matwaste ) = 1.

          READ TABLE ls_filler_data-matwaste ASSIGNING FIELD-SYMBOL(<ls_matwaste>) INDEX 1.

          IF lv_peso_sfrido_artig <> 0.

            APPEND VALUE #( material   = <ls_matwaste>-zmatwaste
                            plant      = ls_filler_data-werks
                            stge_loc   = ls_filler_data-lgort
                            entry_qnt  = abs( lv_peso_sfrido_artig )
                            move_type  = COND #( WHEN lv_peso_sfrido_artig > 0 THEN '531' ELSE '532')
                            entry_uom  = 'G'
                            no_more_gr = 'X'
                            withdrawn  = 'X'
                            orderid    = gs_screen100-p_aufnr
                            activity   = gs_screen100-p_vornr
                            ) TO lt_gm_item.

            APPEND VALUE #( zmatwaste = <ls_matwaste>-zmatwaste
                            peso      = lv_peso_sfrido_artig
                            meins     = 'G'
                           ) TO mt_scrap.

          ENDIF.

        ELSE.


          DATA(lv_tot_ntgew) = REDUCE ntgew( INIT lv_total = EXACT ntgew( 0 )
                                             FOR <material> IN ls_filler_data-matwaste
                                             NEXT lv_total = <material>-ntgew + lv_total ).

          LOOP AT ls_filler_data-matwaste ASSIGNING <ls_matwaste>.

            DATA(lv_ntgew_perc)  = CONV zmm_cp_peso( ( 100 * <ls_matwaste>-ntgew ) / lv_tot_ntgew ).
            DATA(lv_peso_mat)    = CONV zmm_cp_peso( ( lv_ntgew_perc * lv_peso_sfrido_artig ) / 100 ).

            IF lv_peso_mat <> 0.

              APPEND VALUE #( material   = <ls_matwaste>-zmatwaste
                              plant      = ls_filler_data-werks
                              stge_loc   = ls_filler_data-lgort
                              entry_qnt  = abs( lv_peso_mat )
                              move_type  = COND #( WHEN lv_peso_mat > 0 THEN '531' ELSE '532')
                              entry_uom  = 'G'
                              no_more_gr = 'X'
                              withdrawn  = 'X'
                              orderid    = gs_screen100-p_aufnr
                              activity   = gs_screen100-p_vornr
                              ) TO lt_gm_item.

              APPEND VALUE #( zmatwaste = <ls_matwaste>-zmatwaste
                              peso      = lv_peso_mat
                              meins     = 'G'
                             ) TO mt_scrap.

            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.

      IF gs_screen100-waste IS NOT INITIAL.
        LOOP AT mt_waste_tab ASSIGNING <ls_waste_row> WHERE peso > 0 .
          APPEND VALUE #( material   = <ls_waste_row>-zmatwaste
                          plant      = gs_screen100-p_werks
                          stge_loc   = gs_screen100-p_lgort
                          entry_qnt  = <ls_waste_row>-peso
                          move_type  = '531'
                          entry_uom  = 'G'
                          no_more_gr = 'X'
                          withdrawn  = 'X'
                          orderid    = gs_screen100-p_aufnr
                          activity   = gs_screen100-p_vornr
                          ) TO lt_gm_item.
        ENDLOOP.
      ENDIF.

      gs_screen101-zzpeso = lv_peso_sfrido_artig.

      IF mt_scrap IS NOT INITIAL.
        SELECT SINGLE lgobe
          FROM t001l
          INTO @DATA(lv_lgobe)
          WHERE lgort = @ls_filler_data-lgort
            AND werks = @ls_filler_data-werks.

        LOOP AT mt_scrap ASSIGNING FIELD-SYMBOL(<ls_scrap>).

          <ls_scrap>-lgort = ls_filler_data-lgort.
          <ls_scrap>-lgobe = lv_lgobe.

          SELECT SINGLE k~maktx,
                        m~meins
            FROM mara AS m
            LEFT JOIN makt AS k
                   ON k~matnr = m~matnr
            INTO ( @<ls_scrap>-maktx, @<ls_scrap>-meins )
            WHERE m~matnr = @<ls_scrap>-zmatwaste
            AND k~spras   = @sy-langu.

        ENDLOOP.

        mt_scrap_temp = mt_scrap.

        CALL SCREEN 101 STARTING AT 10 10.

        IF mv_exe_gmvt = abap_false.
          MESSAGE s009(zmm_cp_msg) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDIF.

      IF lt_gm_item IS NOT INITIAL.
        LOOP AT mt_scrap ASSIGNING <ls_scrap>.
          READ TABLE lt_gm_item ASSIGNING FIELD-SYMBOL(<ls_gm_item>) WITH KEY material = <ls_scrap>-zmatwaste.
          IF sy-subrc = 0.
            <ls_gm_item>-entry_qnt = abs( <ls_scrap>-peso ).
          ENDIF.
        ENDLOOP.

        CLEAR mt_scrap.

        ls_gm_header = VALUE #( pstng_date = sy-datum
                                doc_date   = sy-datum  ).

        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = ls_gm_header
            goodsmvt_code    = ls_goodsmvt_code
          IMPORTING
            materialdocument = lv_mblnr
            matdocumentyear  = lv_mjahr
          TABLES
            goodsmvt_item    = lt_gm_item
            return           = lt_return.

        READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_error).
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          lv_error = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          IF gs_screen100-waste = abap_true.
            update_peso_scarti(
              iv_steus = ls_operation-opr_cntrl_key
              iv_arbpl = ls_operation-work_center
              iv_mblnr = lv_mblnr
              iv_mjahr = lv_mjahr   ).
          ENDIF.
        ENDIF.

        lt_msg = VALUE #( FOR <ls_ret> IN lt_return
          ( msgid  = <ls_ret>-id
            msgty  = <ls_ret>-type
            msgno  = <ls_ret>-number
            msgv1  = <ls_ret>-message_v1
            msgv2  = <ls_ret>-message_v2
            msgv3  = <ls_ret>-message_v3
            msgv4  = <ls_ret>-message_v4
            lineno = sy-tabix
            ) ) .

      ENDIF.
    ENDIF.

    IF lv_error = abap_false.

      IF gs_screen100-p_psc2 = abap_true.
        UPDATE zmm_cp_peso_fasi
           SET sname  = ls_filler_data-sname
               pernr  = ls_filler_data-pernr

         WHERE aufnr  = gs_screen100-p_aufnr
           AND vornr  = gs_screen100-p_vornr
           AND ebeln  = ''
           AND ebelp  = ''
           AND zztipo = 'I'.
      ENDIF.

      ls_weight = VALUE #( aufnr           = gs_screen100-p_aufnr
                           vornr           = gs_screen100-p_vornr
                           steus           = ls_operation-opr_cntrl_key
                           arbpl           = ls_operation-work_center
                           zztipo          = COND #( WHEN gs_screen100-p_psc1 = abap_true THEN 'I' ELSE 'F' )
                           bname           = sy-uname
                           zzdataril       = sy-datum
                           zzoraril        = sy-uzeit
                           zzpesopietre    = lv_pesopietre
                           zzpesoother     = lv_pesoother
                           zzpeso          = gs_screen100-p_zzpeso
                           zzcalo          = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN ls_filler_data-zzcalo )
                           matnr           = lv_matnr
                           sname           = ls_filler_data-sname
                           pernr           = ls_filler_data-pernr
                           zzpesocalofaset = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN ms_weight-zzpeso - gs_screen100-p_zzpeso )
                           zzpesocalofasea = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_calo_ammesso_abs )
                           zzpesoxscarti   = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_peso_grande_sfrido )
                           zzpesoxartig    = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_peso_sfrido_artig )
                           mblnr           = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_mblnr )
                           mjahr           = COND #( WHEN gs_screen100-p_psc2 = abap_true THEN lv_mjahr ) ).

      INSERT zmm_cp_peso_fasi FROM ls_weight.

      IF sy-subrc = 0.
        IF lv_mblnr IS INITIAL.
          MESSAGE s002(zmm_cp_msg).
        ELSE.
          MESSAGE s008(zmm_cp_msg) WITH lv_mblnr.
        ENDIF.
      ELSE.
        MESSAGE s003(zmm_cp_msg) DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

    IF lt_msg IS NOT INITIAL.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_msg.
    ENDIF.

    CLEAR: ls_gm_header, lt_gm_item, lt_return, lv_mblnr, lv_mjahr.

  ENDMETHOD.

  METHOD update_peso_scarti.

    DATA lt_peso_fsc  TYPE STANDARD TABLE OF zmm_cp_peso_f_sc.

    LOOP AT mt_waste_tab ASSIGNING FIELD-SYMBOL(<ls_waste_row>) WHERE peso <> 0.

      APPEND VALUE #( aufnr   = gs_screen100-p_aufnr
                      vornr   = gs_screen100-p_vornr
                      steus   = iv_steus
                      arbpl   = iv_arbpl
                      lgort   = gs_screen100-p_lgort
                      werks   = gs_screen100-p_werks
                      zztipo  = 'F'
                      zzpeso  = <ls_waste_row>-peso
                      matnr   = <ls_waste_row>-zmatwaste
                      mblnr   = iv_mblnr
                      mjahr   = iv_mjahr ) TO lt_peso_fsc.

    ENDLOOP.

    INSERT zmm_cp_peso_f_sc FROM TABLE lt_peso_fsc.

  ENDMETHOD.

  METHOD get_filler_data.

    DATA: ls_order_objects TYPE bapi_pp_order_objects,
          lt_header        TYPE STANDARD TABLE OF bapi_order_header1,
          lt_operation     TYPE STANDARD TABLE OF bapi_order_operation1,
          lv_query_arbpl   TYPE string,
          lv_query_lifnr   TYPE string.

    CLEAR es_filler_data.

    ls_order_objects-header = abap_true.
    ls_order_objects-operations = abap_true.

    CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
      EXPORTING
        number        = iv_aufnr
        order_objects = ls_order_objects
      TABLES
        header        = lt_header
        operation     = lt_operation.

    READ TABLE lt_header INTO DATA(ls_header) INDEX 1.
    IF sy-subrc = 0.
      ev_matnr = ls_header-material.
      es_filler_data-werks = ls_header-production_plant.
    ENDIF.

    READ TABLE lt_operation INTO es_operation WITH KEY operation_number = iv_vornr.

    CASE abap_true.
      WHEN gs_screen100-p_pr_ord.
        DATA(lv_lifnr) = EXACT lifnr( '' ).
      WHEN gs_screen100-p_cn_wrk.
        lv_lifnr = iv_lifnr.
    ENDCASE.

    SELECT SINGLE prgrp
      FROM pgmi
      INTO @DATA(lv_prgrp)
      WHERE nrmit = @ev_matnr.

    IF lv_prgrp IS INITIAL.
      lv_prgrp = ev_matnr.
    ENDIF.

    SELECT SINGLE zzcalo
      FROM zmm_cp_perc_amm
      INTO @es_filler_data-zzcalo
      WHERE prgrp = @lv_prgrp
        AND valid_from <= @sy-datum
        AND valid_to   >= @sy-datum
        AND arbpl       = @es_operation-work_center
        AND lifnr       = @lv_lifnr.

    IF gs_screen100-p_cn_wrk IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE aufpl
      FROM afko
      INTO @DATA(lv_aufpl)
      WHERE aufnr = @iv_aufnr.

    IF gs_screen100-p_psc2 IS NOT INITIAL.
      SELECT SINGLE a~pernr, p~sname
        FROM afru AS a
        JOIN pa0001 AS p
        ON p~pernr = a~pernr
        INTO (@es_filler_data-pernr, @es_filler_data-sname)
        WHERE aufpl = @lv_aufpl
          AND satza = 'B40'
          AND stokz = ' '
          AND stzhl = ' '
          AND aufnr = @iv_aufnr
          AND vornr = @iv_vornr.

      IF sy-subrc <> 0.
        ev_error = abap_true.
        MESSAGE s016(zmm_cp_msg) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SELECT valsign AS sign,
             valoption AS option,
             valfrom   AS low,
             valto     AS high
       FROM setleaf
        WHERE setname = 'ZMM_CP_PLANT'
        INTO TABLE @DATA(lr_werks_002).
      IF sy-subrc = 0 AND es_filler_data-werks IN lr_werks_002.
        es_filler_data-lgort = '002'.

      ELSE.
        SELECT SINGLE lgort ", werks
          FROM zmm_cp_artg_cid
          INTO @es_filler_data-lgort
          WHERE pernr = @es_filler_data-pernr.

        IF sy-subrc <> 0.
          ev_error = abap_true.
          MESSAGE s015(zmm_cp_msg) WITH es_filler_data-pernr DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDIF.

    ENDIF.

    SELECT valsign AS sign,
           valoption AS option,
           valfrom   AS low,
           valto     AS high
     FROM setleaf
      WHERE setname = 'ZMM_CP_COMPTYPE'
      INTO TABLE @DATA(lr_mattyp).

    SELECT p~prgrp,
           c~zmatcomp,
           c~ntgew,
           m~zmatwaste
      FROM pgmi AS p
      JOIN zmm_compteo AS c
      ON c~matnr = p~prgrp
      JOIN ztb_matcomp AS m
      ON m~zmatcomp = c~zmatcomp
      INTO TABLE @es_filler_data-matwaste
      WHERE p~nrmit      = @ev_matnr
        AND c~zmattyp   IN @lr_mattyp
        AND m~zmatwaste <> ''.

    IF sy-subrc <> 0 AND gs_screen100-p_psc2 IS NOT INITIAL.
      MESSAGE i021(zmm_cp_msg).
    ENDIF.

  ENDMETHOD.

  METHOD get_data_from_document.

    SELECT SINGLE lifnr
      FROM ekko
      INTO @ev_lifnr
     WHERE ebeln = @gs_screen100-p_ebeln2.

    SELECT DISTINCT l~matnr,
                    l~brgew
*           SUM( l~brgew ) AS brgew
    FROM ekbe AS b

     JOIN lips AS l
       ON l~vbeln = b~belnr
      AND l~vgbel = b~ebeln
      AND right( l~vgpos, 5 ) = b~ebelp

     WHERE b~ebeln = @gs_screen100-p_ebeln2
     AND b~vgabe = '8'
     AND l~matnr IN ( @gs_materials-wip_carati , @gs_materials-wip_other )

*       GROUP BY l~matnr
      INTO TABLE @DATA(lt_lips).

    LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>).

      CASE <ls_lips>-matnr.
        WHEN gs_materials-wip_carati.
          ev_pesopietre = <ls_lips>-brgew.

        WHEN gs_materials-wip_other.
          ev_pesoother  = <ls_lips>-brgew.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD fill_waste_weight.

    DATA: lt_fieldcat TYPE lvc_t_fcat.

    IF mo_grid IS NOT BOUND.

      CREATE OBJECT mo_cont
        EXPORTING
          container_name              = 'CONT_100'              " Name of the Screen CustCtrl Name to Link Container To
          repid                       = sy-repid               " Screen to Which this Container is Linked
          dynnr                       = '0100'                " Report To Which this Container is Linked
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

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = mo_cont
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

    get_waste_materials( ).

    lt_fieldcat = VALUE #( ( fieldname = 'ZMATWASTE' ref_table = 'MARA' ref_field = 'MATNR' )
                           ( fieldname = 'MAKTX'     ref_table = 'MAKT'  )
                           ( fieldname = 'PESO'      edit = abap_true reptext = 'Weight'(007) qfieldname = 'MEINS' ref_table = 'ZMM_CP_PESO_FASI' ref_field = 'ZZPESO' )
                           ( fieldname = 'MEINS'     ref_table = 'MARA' ) ).

    DATA(lt_toolbar_excluding) = VALUE ui_functions(
    ( cl_gui_alv_grid=>mc_fc_loc_cut            )
    ( cl_gui_alv_grid=>mc_fc_loc_append_row     )
    ( cl_gui_alv_grid=>mc_fc_loc_insert_row     )
    ( cl_gui_alv_grid=>mc_fc_loc_delete_row     )
    ( cl_gui_alv_grid=>mc_fc_loc_copy           )
    ( cl_gui_alv_grid=>mc_fc_loc_copy_row       )
    ( cl_gui_alv_grid=>mc_fc_loc_paste          )
    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row  )
    ( cl_gui_alv_grid=>mc_fc_info               )
    ( cl_gui_alv_grid=>mc_fc_check              )
    ( cl_gui_alv_grid=>mc_fc_refresh            )
    ( cl_gui_alv_grid=>mc_fc_graph              )
    ( cl_gui_alv_grid=>mc_fc_print              )
    ( cl_gui_alv_grid=>mc_fc_view_excel         )
    ( cl_gui_alv_grid=>mc_fc_view_grid          )
    ( cl_gui_alv_grid=>mc_fc_detail             )
    ( cl_gui_alv_grid=>mc_fc_call_xxl           )
    ( cl_gui_alv_grid=>mc_fc_html               )
    ( cl_gui_alv_grid=>mc_fc_word_processor     )
    ( cl_gui_alv_grid=>mc_fc_call_abc           )
    ( cl_gui_alv_grid=>mc_fc_send               )
    ( cl_gui_alv_grid=>mc_fc_to_office          )
    ( cl_gui_alv_grid=>mc_fc_filter             )
    ( cl_gui_alv_grid=>mc_fc_current_variant    )
    ).

    mo_grid->set_table_for_first_display(
       EXPORTING
         it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog      = lt_fieldcat
         it_outtab            = mt_waste_tab ).

  ENDMETHOD.

  METHOD get_waste_materials.

    SELECT valsign   AS sign,
           valoption AS option,
           valfrom   AS low,
           valto     AS high
        FROM setleaf
         WHERE setname = 'ZMM_CP_MAT_SFRIDO_GRANDE'
         INTO TABLE @DATA(lr_matnr).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT m~matnr AS zmatwaste,
           k~maktx,
           m~meins
      FROM mara AS m
      LEFT JOIN makt AS k
             ON k~matnr = m~matnr
            AND k~spras = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @mt_waste_tab
      WHERE m~matnr IN @lr_matnr.

  ENDMETHOD.

ENDCLASS.
