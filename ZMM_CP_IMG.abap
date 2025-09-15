*&---------------------------------------------------------------------*
*& Report ZMM_CP_IMG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_img.

PARAMETERS: p_rd1 RADIOBUTTON GROUP g1,
            p_rd2 RADIOBUTTON GROUP g1,
            p_rd3 RADIOBUTTON GROUP g1,
            p_rd4 RADIOBUTTON GROUP g1,
            p_rd5 RADIOBUTTON GROUP g1,
            p_rd6 RADIOBUTTON GROUP g1.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS execute.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD execute.

    CASE abap_true.
      WHEN p_rd1.
        DATA(lv_tcode) = EXACT sy-tcode( 'ZLOAD_PERC_AMMESSA' ).
      WHEN p_rd2.
        lv_tcode = 'ZMM_CP_RIL_PESO'.
      WHEN p_rd3.
        lv_tcode = 'ZMM_CP_MOD_PESO'.
      WHEN p_rd4.
        lv_tcode = 'ZMM_CP_REC_SFRIDO'.
      WHEN p_rd5.
        lv_tcode = 'ZMM_CP_PRIMARIO'.
      WHEN p_rd6.
        lv_tcode = 'ZMM_CP_SECONDARIO'.
    ENDCASE.

    IF lv_tcode IS INITIAL.
      MESSAGE 'Transaction ' && lv_tcode && ' does not exist' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_report( )->execute( ).
