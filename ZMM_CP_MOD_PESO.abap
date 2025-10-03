*&---------------------------------------------------------------------*
*& Report ZMM_CP_MOD_PESO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_mod_peso.

DATA: BEGIN OF gs_screen100,
        ok_code    TYPE sy-ucomm,
        aufnr      TYPE zmm_cp_peso_fasi-aufnr,
        vornr      TYPE zmm_cp_peso_fasi-vornr,
        old_aufnr  TYPE zmm_cp_peso_fasi-aufnr,
        old_vornr  TYPE zmm_cp_peso_fasi-vornr,
        peso_vech  TYPE zmm_cp_peso_fasi-zzpeso,
        peso_v_mat TYPE zmm_cp_peso_fasi-zzpesoother,
        zzpeso     TYPE zmm_cp_peso_fasi-zzpeso,
        peso_n_mat TYPE zmm_cp_peso_fasi-zzpesoother,
        meins      TYPE ekpo-meins VALUE 'G',
      END OF gs_screen100.
*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :execute,
      enter.

  PRIVATE SECTION.
    METHODS :save.
ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.
INCLUDE zmm_cp_mod_peso_pbo.
INCLUDE zmm_cp_mod_peso_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.
    save( ).
  ENDMETHOD.                    "execute

  METHOD enter.

    IF gs_screen100-old_aufnr <> gs_screen100-aufnr OR  gs_screen100-old_vornr <> gs_screen100-vornr.

      gs_screen100-old_aufnr = gs_screen100-aufnr.
      gs_screen100-old_vornr = gs_screen100-vornr.

      SELECT SINGLE zzpeso,
             zzpesoother
        FROM zmm_cp_peso_fasi
        INTO ( @gs_screen100-peso_vech , @gs_screen100-peso_v_mat )
        WHERE aufnr = @gs_screen100-aufnr
          AND vornr = @gs_screen100-vornr.

      gs_screen100-zzpeso = gs_screen100-peso_vech.
      gs_screen100-peso_n_mat = gs_screen100-peso_v_mat.

    ENDIF.

  ENDMETHOD.

  METHOD save.

    SELECT *
      FROM zmm_cp_peso_fasi
      INTO TABLE @DATA(lt_peso)
      WHERE aufnr = @gs_screen100-aufnr
        AND vornr = @gs_screen100-vornr.

    IF sy-subrc <> 0.
      MESSAGE s024(zmm_cp_msg) WITH gs_screen100-aufnr gs_screen100-vornr DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF lines( lt_peso ) > 1.
      MESSAGE s023(zmm_cp_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE lt_peso ASSIGNING FIELD-SYMBOL(<ls_peso>) INDEX 1.
    IF sy-subrc = 0.
      <ls_peso>-zzpeso = gs_screen100-zzpeso.
      IF <ls_peso>-ebeln IS INITIAL.
        <ls_peso>-zzpesoother = gs_screen100-peso_n_mat.
      ELSE.
        CLEAR gs_screen100-peso_n_mat.
        MESSAGE i027(zmm_cp_msg).
      ENDIF.

      <ls_peso>-uname  = sy-uname.
      <ls_peso>-datum  = sy-datum.
      <ls_peso>-uzeit  = sy-uzeit.

      UPDATE zmm_cp_peso_fasi FROM <ls_peso>.
      IF sy-subrc = 0.
        MESSAGE s002(zmm_cp_msg).
      ELSE.
        MESSAGE s003(zmm_cp_msg) DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "extract_data

ENDCLASS.
