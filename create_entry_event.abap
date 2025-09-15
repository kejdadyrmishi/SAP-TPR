*----------------------------------------------------------------------*
***INCLUDE LZMM_CP_PERC_AMMF03.
*----------------------------------------------------------------------*

FORM after_insert_05.

  SELECT SINGLE @abap_true
    FROM zmm_cp_perc_amm
    INTO @DATA(lv_overlap)
    WHERE valid_from <= @zmm_cp_perc_amm-valid_from
      AND valid_to   >= @zmm_cp_perc_amm-valid_from
      AND matnr       = @zmm_cp_perc_amm-matnr
      AND arbpl       = @zmm_cp_perc_amm-arbpl
      and lifnr       = @zmm_cp_perc_amm-lifnr.

  IF sy-subrc = 0.
    MESSAGE 'Validity period overlap with another record' TYPE 'E'.
  ENDIF.

ENDFORM.
