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
                  s_aufnr  FOR zmm_cp_peso_fasi-aufnr,
                  s_arbpl  FOR zmm_cp_peso_fasi-arbpl,
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
    TYPES:   matnr_m TYPE mseg-matnr,
             menge   TYPE mseg-menge,
             meins   TYPE mseg-meins,
             werks   TYPE mseg-werks,
             lgort   TYPE mseg-lgort,
             name1   TYPE lfa1-name1,
             maktx   TYPE makt-maktx,
           END OF ty_primario,
           tt_primario TYPE TABLE OF ty_primario.

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
           c~zzpesopietre,
           c~zzpesoother,
           c~zzcalo,
           c~zzpesoxscarti,
           c~zzpesocalofaset,
           c~zzpesocalofasea,
           c~zzpesoxartig,
           c~mblnr,
           c~mjahr,
           m~matnr AS matnr_m,
           m~menge,
           m~meins,
           m~werks,
           m~lgort
      FROM zmm_cp_peso_fasi AS c

      LEFT JOIN lfa1 AS l
      ON l~lifnr = c~lifnr

      LEFT JOIN makt AS k
      ON  k~matnr = c~matnr
      AND k~spras = @sy-langu

      LEFT JOIN mseg AS m
      ON  m~mblnr = c~mblnr
      AND m~mjahr = c~mjahr

      INTO CORRESPONDING FIELDS OF TABLE @mt_cp_primario
      WHERE c~aufnr     IN @s_aufnr
        AND c~arbpl     IN @s_arbpl
        AND c~ebeln     IN @s_ebeln
        AND c~zzdataril IN @s_dt_per
        AND c~lifnr     IN @s_lifnr
        ORDER BY c~aufnr,
                 c~vornr.

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

    lt_fcat = VALUE lvc_t_fcat(  ( fieldname = 'AUFNR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'VORNR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ARBPL'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'EBELN'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZDATARIL'       ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZTIPO'          ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'LIFNR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'NAME1'           ref_table = 'LFA1' reptext = 'Vendor name' )
                                 ( fieldname = 'MATNR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MAKTX'           ref_table = 'MAKT' )
                                 ( fieldname = 'ZZPESO'          ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOPIETRE'    ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOOTHER'     ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZCALO'          ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOXSCARTI'   ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOCALOFASET' ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOCALOFASEA' ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'ZZPESOXARTIG'    ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MBLNR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MJAHR'           ref_table = 'ZMM_CP_PESO_FASI' )
                                 ( fieldname = 'MATNR_M'         ref_table = 'MSEG' ref_field = 'MATNR')
                                 ( fieldname = 'MENGE'           ref_table = 'MSEG' )
                                 ( fieldname = 'MEINS'           ref_table = 'MSEG' )
                                 ( fieldname = 'WERKS'           ref_table = 'MSEG' )
                                 ( fieldname = 'LGORT'           ref_table = 'MSEG' ) ).

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
