*----------------------------------------------------------------------*
***INCLUDE LZMM_CP_PERC_AMMF01.
*----------------------------------------------------------------------*

FORM form_validation_date_check_01.

  TYPES: BEGIN OF ty_perc_amm.
           INCLUDE TYPE zmm_cp_perc_amm.
           INCLUDE TYPE vimtbflags.
TYPES END OF ty_perc_amm.

  FIELD-SYMBOLS <ls_perc_amm> TYPE ty_perc_amm.

  DATA: lt_perc_amm   TYPE STANDARD TABLE OF ty_perc_amm,
        lv_field_name TYPE fieldname,
        lv_error      TYPE abap_bool.

  LOOP AT total.
    ASSIGN total TO <ls_perc_amm> CASTING.
    <ls_perc_amm>-vim_action = <action>.
    APPEND <ls_perc_amm> TO lt_perc_amm.
  ENDLOOP.

  SORT lt_perc_amm BY matnr arbpl lifnr.

  LOOP AT lt_perc_amm ASSIGNING <ls_perc_amm> WHERE vim_action IS NOT INITIAL.

    IF <ls_perc_amm>-valid_from > <ls_perc_amm>-valid_to.
      lv_error = abap_true.
      MESSAGE 'Valid to should be greater than valid from' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    READ TABLE lt_perc_amm TRANSPORTING NO FIELDS WITH KEY matnr = <ls_perc_amm>-matnr
                                                           arbpl = <ls_perc_amm>-arbpl
                                                           lifnr = <ls_perc_amm>-lifnr BINARY SEARCH.
    DATA(lv_index) = sy-index.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    LOOP AT lt_perc_amm ASSIGNING FIELD-SYMBOL(<ls_perc_group>) FROM lv_index.

      IF <ls_perc_group>-matnr <> <ls_perc_amm>-matnr OR
         <ls_perc_group>-arbpl <> <ls_perc_amm>-arbpl OR
         <ls_perc_group>-lifnr <> <ls_perc_amm>-lifnr.
        EXIT.
      ENDIF.

      IF ( <ls_perc_amm>-valid_from <= <ls_perc_group>-valid_from AND <ls_perc_amm>-valid_to >= <ls_perc_group>-valid_from ) OR
         ( <ls_perc_amm>-valid_from <= <ls_perc_group>-valid_TO   AND <ls_perc_amm>-valid_to >= <ls_perc_group>-valid_to   ) OR
         ( <ls_perc_amm>-valid_from >= <ls_perc_group>-valid_from AND <ls_perc_amm>-valid_to <= <ls_perc_group>-valid_to   ).

        MESSAGE 'Validity period overlap with another record' TYPE 'S' DISPLAY LIKE 'E'.
        lv_error = abap_true.
        DATA(lv_exit) = abap_true.
        EXIT.

      ENDIF.

    ENDLOOP.
    IF lv_exit = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_error = abap_true.
    vim_abort_saving = 'X'.
  ENDIF.

ENDFORM.
