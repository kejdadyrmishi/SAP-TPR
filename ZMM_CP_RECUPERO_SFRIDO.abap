*&---------------------------------------------------------------------*
*& report zmm_cp_recupero_sfrido
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_recupero_sfrido.

DATA: BEGIN OF gs_screen100,
        ok_code  TYPE sy-ucomm,
        p_arbpl  TYPE zmm_cp_peso_recu-arbpl,
        p_datada TYPE zmm_cp_peso_recu-zzdatada,
        p_dataa  TYPE zmm_cp_peso_recu-zzdataa,
        p_zzpeso TYPE zmm_cp_peso_recu-zzpeso,
        p_matnr  TYPE zmm_cp_peso_recu-matnr,
        p_werks  TYPE zmm_cp_peso_recu-werks,
        p_lgort  TYPE zmm_cp_peso_recu-lgort,
      END OF gs_screen100.

*----------------------------------------------------------------------*
*       class lcl_report definition
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS save.

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.
INCLUDE zmm_cp_recupero_sfrido_pbo.
INCLUDE zmm_cp_recupero_sfrido_pai.

*----------------------------------------------------------------------*
*       class lcl_report implementation
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD save.

    DATA: ls_gm_header     TYPE BAPI2017_GM_head_01,
          lv_goodsmvt_code TYPE bapi2017_gm_code VALUE '03',
          lt_gm_item       TYPE bapi2017_gm_item_create_t,
          lt_return        TYPE bapiret2_tab,
          lv_mblnr         TYPE mblnr,
          lv_mjahr         TYPE mjahr,
          lv_error         TYPE abap_bool,
          ls_peso_recu     TYPE zmm_cp_peso_recu.

    lt_gm_item = VALUE #( ( material   = gs_screen100-p_matnr
                            plant      = gs_screen100-p_werks
                            stge_loc   = gs_screen100-p_lgort
                            entry_qnt  = gs_screen100-p_zzpeso
                            move_type  = '531'
                            entry_uom  = 'G'
                            no_more_gr = 'X'
                            withdrawn  = 'X'  ) ).

    ls_gm_header = VALUE #( pstng_date = sy-datum
                            doc_date   = sy-datum  ).

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_gm_header
        goodsmvt_code    = lv_goodsmvt_code
      IMPORTING
        materialdocument = lv_mblnr
        matdocumentyear  = lv_mjahr
      TABLES
        goodsmvt_item    = lt_gm_item
        return           = lt_return.

    IF lt_return IS NOT INITIAL.

      READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_error).

      IF sy-subrc = 0.
        MESSAGE ls_error-message TYPE  'S' DISPLAY LIKE 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        RETURN.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ENDIF.

    ls_peso_recu-arbpl    = gs_screen100-p_arbpl.
    ls_peso_recu-zzdatada = gs_screen100-p_datada.
    ls_peso_recu-zzdataa  = gs_screen100-p_dataa.
    ls_peso_recu-zzpeso   = gs_screen100-p_zzpeso.
    ls_peso_recu-matnr    = gs_screen100-p_matnr.
    ls_peso_recu-werks    = gs_screen100-p_werks.
    ls_peso_recu-lgort    = gs_screen100-p_lgort.
    ls_peso_recu-mjahr    = lv_mjahr.
    ls_peso_recu-mblnr    = lv_mblnr.
    ls_peso_recu-bname    = sy-uname.

    INSERT zmm_cp_peso_recu FROM ls_peso_recu.

    IF sy-subrc = 0.
      MESSAGE 'Data saved successfully' TYPE  'S'.
    ELSE.
      MESSAGE 'Error while saving data' TYPE  'S' DISPLAY LIKE 'E'.
    ENDIF.

    CLEAR ls_peso_recu.

  ENDMETHOD.                    "execute


ENDCLASS.
