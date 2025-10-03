*&---------------------------------------------------------------------*
*& Report ZMM_CP_PRIMARIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_primario.
TABLES: zmm_cp_peso_fasi.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_dt_per FOR zmm_cp_peso_fasi-zzdataril OBLIGATORY,
                  s_ebeln  FOR zmm_cp_peso_fasi-ebeln,
                  s_aufnr  FOR zmm_cp_peso_fasi-aufnr ,
                  s_arbpl  FOR zmm_cp_peso_fasi-arbpl ,
                  s_lifnr  FOR zmm_cp_peso_fasi-lifnr.
SELECTION-SCREEN END OF BLOCK b1.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_primario.
             INCLUDE TYPE zmm_cp_peso_fasi.
    TYPES:   matnr_m  TYPE mseg-matnr,
             menge    TYPE mseg-menge,
             meins    TYPE mseg-meins,
             werks    TYPE mseg-werks,
             lgort    TYPE mseg-lgort,
             name1    TYPE lfa1-name1,
             maktx    TYPE makt-maktx,
             ktext_up TYPE crtx-ktext_up,
           END OF ty_primario,
           tt_primario TYPE TABLE OF ty_primario WITH EMPTY KEY.

    DATA: mt_cp_primario TYPE tt_primario,
          mo_grid        TYPE REF TO cl_gui_alv_grid.

    METHODS execute.

  PRIVATE SECTION.

    METHODS extract_data.

    METHODS init_container.

    METHODS display_alv.

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.

INCLUDE zmm_cp_primario_pbo.
INCLUDE zmm_cp_primario_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.
    display_alv( ).
  ENDMETHOD.                    "execute

  METHOD extract_data.

    TYPES : BEGIN OF ty_cp_primario,
              aufnr  TYPE zmm_cp_peso_fasi-aufnr,
              vornr  TYPE zmm_cp_peso_fasi-vornr,
              ebeln  TYPE zmm_cp_peso_fasi-ebeln,
              zztipo TYPE zmm_cp_peso_fasi-zztipo,
            END OF ty_cp_primario.

    DATA: ls_cp_prim TYPE ty_cp_primario.

    SELECT c~aufnr,
           c~vornr,
           c~arbpl,
           c~ebeln,
           c~zzdataril,
           c~zztipo,
           c~lifnr,
           l~name1,
           c~matnr,
           k~maktx,
           c~zzpeso,
           c~zzcalo,
           c~zzpesopietre,
           c~zzpesoother,
           c~zzpesoxscarti,
           c~zzpesocalofaset,
           c~zzpesocalofasea,
           c~zzpesoxartig,
           c~mblnr,
           c~mjahr,
           m~matnr AS matnr_m,
           CASE WHEN li~brgew IS NULL THEN ( CASE WHEN m~bwart = '532' THEN m~menge * -1 ELSE m~menge END ) ELSE li~brgew  END AS menge,
           CASE WHEN li~gewei IS NULL THEN m~meins ELSE li~gewei END AS meins,
           CASE WHEN m~werks <> ' ' THEN m~werks ELSE substring( c~arbpl, 1, 4 ) END AS werks,
           m~lgort,
           tx~ktext_up

      FROM zmm_cp_peso_fasi AS c

      JOIN aufk ON aufk~aufnr = c~aufnr

      LEFT JOIN lfa1 AS l
      ON l~lifnr = c~lifnr

      LEFT JOIN makt AS k
      ON  k~matnr = c~matnr
      AND k~spras = @sy-langu

      LEFT JOIN crhd
        ON crhd~arbpl = c~arbpl
       AND crhd~werks = aufk~werks

      LEFT JOIN crtx AS tx
        ON crhd~objty = tx~objty
       AND crhd~objid = tx~objid
       AND tx~spras   = @sy-langu

      LEFT JOIN mseg AS m
      ON  m~mblnr = c~mblnr
      AND m~mjahr = c~mjahr
      AND c~zztipo = 'F'

      LEFT JOIN lips AS li
        ON c~ebeln  <> ''
       AND m~bwart  = '543'
       AND m~matnr  LIKE 'WIP%'
       AND li~vgbel = m~ebeln
       AND right( li~vgpos , 5 ) = m~ebelp
       AND li~matnr = m~matnr

      WHERE c~aufnr     IN @s_aufnr
        AND c~arbpl     IN @s_arbpl
        AND c~ebeln     IN @s_ebeln
        AND c~zzdataril IN @s_dt_per
        AND c~lifnr     IN @s_lifnr
        AND ( c~ebeln IS INITIAL
          OR ( c~ebeln IS NOT INITIAL AND m~bwart = '543' AND m~matnr LIKE 'WIP%' )
          OR ( c~ebeln IS NOT INITIAL AND m~bwart = '545'  )
          OR ( c~ebeln IS NOT INITIAL AND c~zztipo = 'I'  )
          )
        ORDER BY c~aufnr,
                 c~vornr,
                 c~ebeln,
                 c~zztipo
       INTO CORRESPONDING FIELDS OF TABLE @mt_cp_primario.

    IF '' IN s_aufnr[] AND '' IN s_arbpl[].

      SELECT ekko~ebeln,
             ekko~lifnr,
             lfa1~name1,
             ekpo~matnr,
             makt~maktx,
             mseg~mblnr,
             mseg~mjahr,
             mseg~budat_mkpf AS zzdataril,
             CASE WHEN mg~werks <> ' ' THEN mg~werks ELSE mseg~werks END AS werks,
             mg~lgort,
             pmm~zzcalo,
             ekkn~aufnr,
             mg~matnr AS matnr_m,
             CASE WHEN ekkn~aufnr IS NOT NULL THEN 'X' ELSE ' ' END AS ekkn_exists,
             CASE WHEN li~brgew IS NULL THEN mg~menge ELSE li~brgew END AS menge,
             CASE WHEN li~gewei IS NULL THEN mg~meins ELSE li~gewei END AS meins

        FROM ekko

        JOIN ekpo
          ON ekpo~ebeln  = ekko~ebeln

        LEFT JOIN ekkn
          ON ekkn~ebeln = ekko~ebeln
         AND ekkn~ebelp = ekpo~ebelp

        JOIN mseg
          ON mseg~ebeln  = ekko~ebeln
          AND mseg~bwart = '101'

        LEFT JOIN pgmi
        ON pgmi~nrmit = mseg~matnr

        LEFT JOIN zmm_cp_perc_amm AS pmm
          ON coalesce( pgmi~prgrp , mseg~matnr ) = pmm~prgrp
         AND pmm~arbpl   = ' '
         AND pmm~lifnr   = ekko~lifnr
         AND valid_from <= mseg~budat_mkpf
         AND valid_to   >= mseg~budat_mkpf

        LEFT JOIN mseg AS mg
         ON mg~mjahr = mseg~mjahr
        AND mg~mblnr = mseg~mblnr
        AND ( ( mg~bwart = '543' AND mg~matnr LIKE 'WIP%' )
            OR ( mg~bwart = '545'  )  )

        LEFT JOIN lips AS li
          ON mg~ebeln  <> ''
         AND mg~bwart  = '543'
         AND mg~matnr  LIKE 'WIP%'
         AND li~vgbel = mg~ebeln
         AND right( li~vgpos , 5 ) = mg~ebelp
         AND li~matnr = mg~matnr

        LEFT JOIN makt
          ON makt~matnr = ekpo~matnr
          AND makt~spras = @sy-langu
        LEFT JOIN lfa1
          ON lfa1~lifnr  = ekko~lifnr

        WHERE ekko~bsart       = 'ZCL'
          AND mseg~bwart       = '101'
          AND mseg~budat_mkpf IN @s_dt_per
          AND ekko~ebeln      IN @s_ebeln
          AND ekko~lifnr      IN @s_lifnr
          AND ekpo~matnr      <> ''
        INTO TABLE @DATA(lt_cp_prim).
      IF sy-subrc = 0.
        DELETE lt_cp_prim WHERE ekkn_exists = 'X' AND aufnr IS INITIAL.
        APPEND LINES OF CORRESPONDING tt_primario( lt_cp_prim ) TO mt_cp_primario.
      ENDIF.
    ENDIF.

    LOOP AT mt_cp_primario ASSIGNING FIELD-SYMBOL(<ls_cp_primario>).

      IF  <ls_cp_primario>-aufnr  = ls_cp_prim-aufnr
      AND <ls_cp_primario>-vornr  = ls_cp_prim-vornr
      AND <ls_cp_primario>-ebeln  = ls_cp_prim-ebeln
      AND <ls_cp_primario>-zztipo = ls_cp_prim-zztipo.

        CLEAR : <ls_cp_primario>-zzpeso ,
                <ls_cp_primario>-zzpesopietre     ,
                <ls_cp_primario>-zzpesoother      ,
                <ls_cp_primario>-zzpesocalofaset  ,
                <ls_cp_primario>-zzpesocalofasea  ,
                <ls_cp_primario>-zzpesoxscarti    ,
                <ls_cp_primario>-zzcalo    ,
                <ls_cp_primario>-zzpesoxartig.
      ELSE.

        MOVE-CORRESPONDING <ls_cp_primario> TO ls_cp_prim.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "extract_data

  METHOD display_alv.

    DATA lt_fcat TYPE lvc_t_fcat.
    DATA(ls_layout)  = VALUE lvc_s_layo( sel_mode = 'A' no_rowmark  = '' cwidth_opt = abap_true ).
    DATA(ls_variant) = VALUE disvariant( report = sy-repid  username = sy-uname ).

    extract_data( ).

    IF mt_cp_primario IS INITIAL.
      MESSAGE s020(zmm_cp_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lt_toolbar_excluding) = VALUE ui_functions(
    ( cl_gui_alv_grid=>mc_fc_loc_cut           ) ( cl_gui_alv_grid=>mc_fc_loc_append_row )
    ( cl_gui_alv_grid=>mc_fc_loc_insert_row    ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
    ( cl_gui_alv_grid=>mc_fc_loc_copy          ) ( cl_gui_alv_grid=>mc_fc_loc_undo       )
    ( cl_gui_alv_grid=>mc_fc_loc_copy_row      ) ( cl_gui_alv_grid=>mc_fc_loc_paste      )
    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ) ( cl_gui_alv_grid=>mc_fc_info           )
    ( cl_gui_alv_grid=>mc_fc_check             ) ( cl_gui_alv_grid=>mc_fc_refresh        )
    ( cl_gui_alv_grid=>mc_fc_graph             ) ).

    lt_fcat = VALUE lvc_t_fcat(  ( fieldname = 'AUFNR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'VORNR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ARBPL'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'KTEXT_UP'                  ref_table = 'CRTX'
                                   scrtext_s = 'W.center desc.'(013)       scrtext_m = 'W.center desc.'(013)  scrtext_l = 'Work center description'(002) reptext = 'Work center description'(002) )
                                 ( fieldname = 'EBELN'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZDATARIL'                 ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZTIPO'                    ref_table = 'ZMM_CP_PESO_FASI'
                                   scrtext_s = 'T.weight'(014)             scrtext_m = 'T.weight'(014)        scrtext_l = 'Type weight'(003) reptext = 'Type weight'(003) )
                                 ( fieldname = 'LIFNR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'NAME1'                     ref_table = 'LFA1'
                                   scrtext_s = 'Supp.name'(015)            scrtext_m = 'Supp.name'(015)       scrtext_l = 'Supplier name'(004) reptext = 'Supplier name'(004) )
                                 ( fieldname = 'MATNR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MAKTX'                     ref_table = 'MAKT' )
                                 ( fieldname = 'ZZPESO'                    ref_table = 'ZMM_CP_PESO_FASI'          quantity  = 'G'
                                   scrtext_s = 'Meas.weight'(016)          scrtext_m = 'Meas.weight'(016)          scrtext_l = 'Measured weight'(005) reptext = 'Measured weight'(005) )
                                 ( fieldname = 'ZZPESOPIETRE'              ref_table = 'ZMM_CP_PESO_FASI'          quantity  = 'G'
                                   scrtext_s = 'Stone.w'(017)              scrtext_m = 'Stone.w'(017)              scrtext_l = 'Stone weight'(006) reptext = 'Stone weight'(006) )
                                 ( fieldname = 'ZZPESOOTHER'               ref_table = 'ZMM_CP_PESO_FASI'          quantity  = 'G'
                                   scrtext_s = 'Non-precious mat.'(018)    scrtext_m = 'Non-precious mat.'(018)    scrtext_l = 'Weight of non-precious material'(007) reptext = 'Weight of non-precious material'(007) )
                                 ( fieldname = 'ZZCALO'                    ref_table = 'ZMM_CP_PESO_FASI'
                                   scrtext_s = '% weight loss'(019)        scrtext_m = '% weight loss'(019)  scrtext_l = 'Percentage of weight loss allowed'(008) reptext = 'Percentage of weight loss allowed'(008) )
                                 ( fieldname = 'ZZPESOXSCARTI'             ref_table = 'ZMM_CP_PESO_FASI'    quantity  = 'G'
                                   scrtext_s = 'Waste.w'(020)              scrtext_m = 'Waste weights'(009)  scrtext_l = 'Waste weights'(009)       reptext = 'Waste weights'(009) )
                                 ( fieldname = 'ZZPESOCALOFASET'           ref_table = 'ZMM_CP_PESO_FASI'    quantity  = 'G'
                                   scrtext_s = 'Tot.loss'(021)             scrtext_m = 'Tot.loss'(021)       scrtext_l = 'Weight total loss'(010)   reptext = 'Weight total loss'(010) )
                                 ( fieldname = 'ZZPESOCALOFASEA'           ref_table = 'ZMM_CP_PESO_FASI'    quantity  = 'G'
                                   scrtext_s = 'Allowed loss'(022)         scrtext_m = 'Allowed loss'(022)   scrtext_l = 'Weight allowed loss'(011) reptext = 'Weight allowed loss'(011) )
                                 ( fieldname = 'ZZPESOXARTIG'              ref_table = 'ZMM_CP_PESO_FASI'    quantity  = 'G'
                                   scrtext_s = 'W.craftsman'(023)          scrtext_m = 'W.craftsman'(023)    scrtext_l = 'Weight to craftsman'(012) reptext = 'Weight to craftsman'(012) )
                                 ( fieldname = 'MBLNR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MJAHR'                     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MATNR_M'                   ref_table = 'MSEG' ref_field  = 'MATNR')
                                 ( fieldname = 'MENGE'                     ref_table = 'MSEG' qfieldname = 'MEINS' )
                                 ( fieldname = 'MEINS'                     ref_table = 'MSEG' )
                                 ( fieldname = 'WERKS'                     ref_table = 'MSEG' )
                                 ( fieldname = 'LGORT'                     ref_table = 'MSEG' ) ).

    init_container( ).

    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_cp_primario ).

    CALL SCREEN 100.

  ENDMETHOD.                    "display_alv

  METHOD init_container.

    DATA: lo_cont      TYPE REF TO cl_gui_custom_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CONT_100'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = lo_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "init_container

ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_report.
  go_report->execute( ).
