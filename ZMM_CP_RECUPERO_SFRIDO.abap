*&---------------------------------------------------------------------*
*& report zmm_cp_recupero_sfrido
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_recupero_sfrido.

DATA: BEGIN OF gs_screen100,
        ok_code   TYPE sy-ucomm,
        p_arbpl   TYPE zmm_cp_peso_recu-arbpl,
        p_datada  TYPE zmm_cp_peso_recu-zzdatada,
        p_dataa   TYPE zmm_cp_peso_recu-zzdataa,
        p_zzpeso  TYPE zmm_cp_peso_recu-zzpeso,
        p_matnr   TYPE zmm_cp_peso_recu-matnr,
        p_werks   TYPE zmm_cp_peso_recu-werks,
        p_frlgort TYPE zmm_cp_peso_recu-lgort_from,
        p_tolgort TYPE zmm_cp_peso_recu-lgort_to,
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

    DATA: lt_return    TYPE bapiret2_tab,
          ls_peso_recu TYPE zmm_cp_peso_recu.

    DATA(lt_gm_item) = VALUE bapi2017_gm_item_create_t(
            ( material   = gs_screen100-p_matnr
              plant      = gs_screen100-p_werks
              stge_loc   = gs_screen100-p_frlgort
              move_stloc = gs_screen100-p_tolgort
              entry_qnt  = gs_screen100-p_zzpeso
              move_type  = '311'
              entry_uom  = 'G'
              no_more_gr = 'X'
              withdrawn  = 'X'  ) ).

    DATA(ls_gm_header) = VALUE bapi2017_gm_head_01(
                  pstng_date = sy-datum
                  doc_date   = sy-datum  ).

    DATA(ls_goodsmvt_code) = VALUE bapi2017_gm_code( gm_code = '04' ).

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_gm_header
        goodsmvt_code    = ls_goodsmvt_code
      IMPORTING
        materialdocument = ls_peso_recu-mblnr
        matdocumentyear  = ls_peso_recu-mjahr
      TABLES
        goodsmvt_item    = lt_gm_item
        return           = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_error).
    IF sy-subrc = 0.
      MESSAGE ls_error-message TYPE 'S' DISPLAY LIKE 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RETURN.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

    ls_peso_recu-arbpl      = gs_screen100-p_arbpl.
    ls_peso_recu-zzdatada   = gs_screen100-p_datada.
    ls_peso_recu-zzdataa    = gs_screen100-p_dataa.

    ls_peso_recu-zzpeso     = gs_screen100-p_zzpeso.
    ls_peso_recu-matnr      = gs_screen100-p_matnr.
    ls_peso_recu-werks      = gs_screen100-p_werks.
    ls_peso_recu-lgort_from = gs_screen100-p_frlgort.
    ls_peso_recu-lgort_to   = gs_screen100-p_tolgort.
    ls_peso_recu-bname      = sy-uname.
    GET TIME STAMP FIELD ls_peso_recu-created_on.

    INSERT zmm_cp_peso_recu FROM ls_peso_recu.
    DATA(lv_msg) = text-001 && ls_peso_recu-mjahr && ls_peso_recu-mblnr .

    CLEAR ls_peso_recu.
    MESSAGE lv_msg TYPE 'S'.
  ENDMETHOD.                    "execute
ENDCLASS.
