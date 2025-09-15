*&---------------------------------------------------------------------*
*& Report ZMM_CP_CALO_SECONDARIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_calo_secondario.

TABLES: zmm_cp_peso_recu.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_dt_per FOR zmm_cp_peso_recu-zzdataa NO-EXTENSION OBLIGATORY,
                  s_arbpl  FOR zmm_cp_peso_recu-arbpl,
                  s_werks  FOR zmm_cp_peso_recu-werks,
                  s_matnr  FOR zmm_cp_peso_recu-matnr.
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

    TYPES: BEGIN OF ty_secondario,
             zzdatada TYPE zmm_cp_dataril,
             zzdataa  TYPE zmm_cp_dataril,
             arbpl    TYPE arbpl,
             matnr    TYPE matnr,
             divisa   TYPE werks_d,
             zzpeso   TYPE zmm_cp_peso,
             calo_p   TYPE zmm_cp_peso,
             calo_e   TYPE zmm_cp_peso,
             mblnr    TYPE mblnr,
             mjahr    TYPE mjahr,
           END OF ty_secondario,
           tt_secondario TYPE TABLE OF ty_secondario.

    DATA: mt_cp_secondario TYPE tt_secondario,
          mo_grid          TYPE REF TO cl_gui_alv_grid.

    METHODS execute.

  PRIVATE SECTION.

    METHODS extract_data.

    METHODS init_container.

    METHODS display_alv.

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.

INCLUDE zmm_cp_secondario_pbo.
INCLUDE zmm_cp_secondario_pai.

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

    DATA: lt_peso TYPE tt_secondario.

    DATA(lv_data_a) = COND d( WHEN s_dt_per-high IS INITIAL THEN s_dt_per-low ELSE '99991231' ).

    SELECT r~zzdataa,
           r~zzdatada,
           r~arbpl,
           r~matnr,
*           r~zzpeso,
           f~mblnr,
           f~mjahr,
           m~menge AS calo_p
      FROM zmm_cp_peso_recu AS r

      JOIN zmm_cp_peso_fasi AS f
      ON  f~zzdataril >= r~zzdatada
      AND f~zzdataril <= r~zzdataa
      AND f~arbpl      = r~arbpl
      AND f~zztipo     = 'F'

      JOIN mseg AS m
      ON  m~mblnr = f~mblnr
      AND m~mjahr = f~mjahr
      AND m~matnr = r~matnr
      AND m~werks = left( r~arbpl, 4 )

      WHERE r~zzdatada >= @s_dt_per-low
        AND r~zzdataa  <= @lv_data_a
        AND r~arbpl    IN @s_arbpl
        AND r~werks    IN @s_werks
        AND r~matnr    IN @s_matnr
        ORDER BY r~matnr, r~arbpl
      INTO CORRESPONDING FIELDS OF TABLE @lt_peso.


    LOOP AT lt_peso ASSIGNING FIELD-SYMBOL(<ls_peso>).
      COLLECT <ls_peso> INTO mt_cp_secondario.
    ENDLOOP.

    LOOP AT mt_cp_secondario ASSIGNING FIELD-SYMBOL(<ls_secondario>).
      SELECT SINGLE zzpeso
      FROM zmm_cp_peso_recu
      WHERE zzdatada >= @s_dt_per-low
        AND zzdataa  <= @lv_data_a
        AND arbpl    IN @s_arbpl
        AND werks    IN @s_werks
        AND matnr    IN @s_matnr
        INTO @<ls_secondario>-zzpeso.
    ENDLOOP.

  ENDMETHOD.                    "extract_data

  METHOD display_alv.

    DATA lt_fcat TYPE lvc_t_fcat.
    DATA(ls_layout)  = VALUE lvc_s_layo( sel_mode = 'A' no_rowmark  = '' cwidth_opt = abap_true ).
    DATA(ls_variant) = VALUE disvariant( report = sy-repid  username = sy-uname ).

    extract_data( ).

    IF mt_cp_secondario IS INITIAL.
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

    lt_fcat = VALUE lvc_t_fcat(  ( fieldname = 'ZZDATAA'      ref_table = 'ZMM_CP_PESO_RECU' )
                                 ( fieldname = 'ZZDATADA'     ref_table = 'ZMM_CP_PESO_RECU' )
                                 ( fieldname = 'ARBPL'        ref_table = 'ZMM_CP_PESO_RECU' )
                                 ( fieldname = 'MATNR'        ref_table = 'ZMM_CP_PESO_RECU' )
                                 ( fieldname = 'DIVISA'       reptext = 'Divisione' )
                                 ( fieldname = 'ZZPESO'       ref_table = 'ZMM_CP_PESO_RECU' )
                                 ( fieldname = 'CALO_P'       ref_table = 'MSEG' ref_field = 'MENGE' reptext = 'Calo primario' )
                                 ( fieldname = 'CALO_E'       ref_table = 'ZMM_CP_PESO_RECU' ref_field = 'ZZPESO' reptext = 'Calo effetivo' )  ).

    init_container( ).

    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_cp_secondario ).

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
