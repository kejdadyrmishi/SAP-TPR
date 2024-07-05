FUNCTION zfi_intent_upload.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_int_tot,
           z_cc      TYPE zfi_intent_uploa-z_cc,
           z_vv      TYPE zfi_intent_uploa-z_vv,
           z_plafond TYPE zfi_intent_uploa-z_plafond,
           z_balance TYPE zfi_intent_uploa-z_balance,
           remain    TYPE zfi_intent_uploa-z_balance,
         END OF ty_int_tot.

  DATA: lr_vat      TYPE RANGE OF bseg-mwskz WITH HEADER LINE,
        lv_message  TYPE string,
        lv_string   TYPE string,
        lv_sum_bseg TYPE bseg-dmbtr,
        lv_lifnr    TYPE lifnr,
        lt_mesg     TYPE STANDARD TABLE OF smesg.

  DATA: lt_int_tot TYPE STANDARD TABLE OF ty_int_tot,
        ls_int_tot TYPE ty_int_tot.

  CLEAR gt_zfi_intent_uploa.

  FIELD-SYMBOLS: <fs_simulate> TYPE any.
  ASSIGN ('(SAPLMR1M)SIMULATION') TO <fs_simulate>.

  SELECT * FROM tvarvc
  INTO TABLE @DATA(lt_tvarvc)
        WHERE name EQ 'ZFI_LETINTTAXCODE'.

  LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
    CLEAR lr_vat.
    lr_vat-high = ls_tvarvc-high.
    lr_vat-low = ls_tvarvc-low.
    lr_vat-sign = ls_tvarvc-sign.
    lr_vat-option = ls_tvarvc-opti.
    APPEND lr_vat.
  ENDLOOP.
  CHECK sy-subrc IS INITIAL.

  READ TABLE t_bkpf INTO DATA(ls_bkpf) INDEX 1.
  CHECK sy-subrc IS INITIAL.

  DATA(lv_bldat_st)  = ls_bkpf-bldat(4) && '0101'.
  DATA(lv_bldat_end) = ls_bkpf-bldat(4) && '1231'.

  SELECT * FROM zfi_intent_uploa
  INTO TABLE @gt_zfi_intent_uploa
    FOR ALL ENTRIES IN @t_bseg
        WHERE z_cc     = @t_bseg-bukrs
          AND z_vv     = @t_bseg-lifnr
          AND z_from  BETWEEN @lv_bldat_st AND @lv_bldat_end
          AND z_to    BETWEEN @lv_bldat_st AND @lv_bldat_end .
  CHECK sy-subrc IS INITIAL.

  SORT gt_zfi_intent_uploa BY z_cc z_vv z_from DESCENDING.

  LOOP AT gt_zfi_intent_uploa ASSIGNING FIELD-SYMBOL(<ls_zfi>) .
    <ls_zfi>-remain = <ls_zfi>-z_plafond - <ls_zfi>-z_balance.

    ls_int_tot = CORRESPONDING #( <ls_zfi> ).
    COLLECT ls_int_tot INTO lt_int_tot.
    CLEAR ls_int_tot.
  ENDLOOP.
  SORT lt_int_tot BY z_cc z_vv.

*  Start date from index 1
  DATA(lt_intent_from) = gt_zfi_intent_uploa.
  SORT lt_intent_from BY z_cc z_vv z_from ASCENDING.

  CASE sy-tcode.
    WHEN 'FB60' OR 'MIRO'.
      LOOP AT t_bkpf INTO ls_bkpf.
        CLEAR:  lv_sum_bseg.

        LOOP AT t_bseg INTO DATA(ls_bseg)
          WHERE bukrs = ls_bkpf-bukrs
            AND belnr = ls_bkpf-belnr
            AND gjahr = ls_bkpf-gjahr
            AND mwskz IN lr_vat
*            AND lifnr IS NOT INITIAL.
            AND lifnr IS INITIAL.
          CLEAR lv_lifnr.
          LOOP AT t_bseg INTO DATA(ls_bseg2) WHERE belnr = ls_bseg-belnr
                                               AND gjahr = ls_bseg-gjahr
                                               AND lifnr <> space.
            lv_lifnr = ls_bseg2-lifnr.
            EXIT.
          ENDLOOP.
          CHECK lv_lifnr IS NOT INITIAL.

          READ TABLE gt_zfi_intent_uploa ASSIGNING FIELD-SYMBOL(<fs_zfi_intent_uploa>)
            WITH KEY z_cc = ls_bseg-bukrs
                     z_vv = lv_lifnr
                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            CASE ls_bseg-shkzg.
              WHEN 'H'.
                lv_sum_bseg = lv_sum_bseg - ls_bseg-dmbtr.
              WHEN OTHERS.
                lv_sum_bseg = lv_sum_bseg + ls_bseg-dmbtr.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        IF <fs_zfi_intent_uploa> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        IF lv_sum_bseg IS INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE lt_int_tot INTO ls_int_tot WITH KEY z_cc = <fs_zfi_intent_uploa>-z_cc
                                                       z_vv = <fs_zfi_intent_uploa>-z_vv BINARY SEARCH.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Information'
            txt1  = |{ TEXT-t01 }   { ls_int_tot-z_plafond CURRENCY = ls_bkpf-waers NUMBER = USER }|
            txt2  = |{ TEXT-t02 }   { lv_sum_bseg + ls_int_tot-z_balance CURRENCY = ls_bkpf-waers NUMBER = USER }|
            txt3  = |{ TEXT-t03 }   { ls_int_tot-z_plafond - ( lv_sum_bseg + ls_int_tot-z_balance ) CURRENCY = ls_bkpf-waers NUMBER = USER }|
            txt4  = |{ TEXT-t04 }   { lv_sum_bseg CURRENCY = ls_bkpf-waers NUMBER = USER }|.

        IF lv_sum_bseg <= <fs_zfi_intent_uploa>-remain.
          <fs_zfi_intent_uploa>-z_balance += lv_sum_bseg.
          <fs_zfi_intent_uploa>-remain    -= lv_sum_bseg.
          CONTINUE.
        ENDIF.

*        lv_sum_bseg -= <fs_zfi_intent_uploa>-z_balance.

        IF ls_int_tot-z_plafond - ( lv_sum_bseg + ls_int_tot-z_balance ) < 0.
          lv_message = TEXT-e01.
          REPLACE '&1' INTO lv_message WITH lv_lifnr.
          MESSAGE lv_message TYPE 'E'.
        ENDIF.

*        lv_sum_bseg = lv_sum_bseg + <fs_zfi_intent_uploa>-z_balance.
        READ TABLE lt_intent_from TRANSPORTING NO FIELDS
          WITH KEY z_cc = ls_int_tot-z_cc
                   z_vv = ls_int_tot-z_vv
                   BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT lt_intent_from ASSIGNING FIELD-SYMBOL(<ls_intent_from>) FROM sy-tabix.
            IF <ls_intent_from>-z_cc <> ls_int_tot-z_cc
              OR <ls_intent_from>-z_vv <> ls_int_tot-z_vv.
              EXIT.
            ENDIF.

            IF lv_sum_bseg = 0.
              EXIT.
            ELSEIF lv_sum_bseg >= <ls_intent_from>-remain.
              lv_sum_bseg -= <ls_intent_from>-remain.
              <ls_intent_from>-z_balance += <ls_intent_from>-remain.
              <ls_intent_from>-remain = 0.
            ELSE.
              <ls_intent_from>-z_balance += lv_sum_bseg.
              <ls_intent_from>-remain -= lv_sum_bseg.
            ENDIF.

            READ TABLE gt_zfi_intent_uploa ASSIGNING <fs_zfi_intent_uploa>
              WITH KEY z_cc   = <ls_intent_from>-z_cc
                       z_vv   = <ls_intent_from>-z_vv
                       z_from = <ls_intent_from>-z_from.
            IF sy-subrc = 0.
              <fs_zfi_intent_uploa>-z_balance = <ls_intent_from>-z_balance.
              <fs_zfi_intent_uploa>-remain    = <ls_intent_from>-remain.
            ENDIF.

          ENDLOOP.
        ENDIF.

      ENDLOOP.


    WHEN 'FB08' OR 'MR8M'.
      LOOP AT t_bkpf INTO ls_bkpf.
        LOOP AT t_bseg INTO ls_bseg
              WHERE bukrs = ls_bkpf-bukrs
              AND belnr = ls_bkpf-belnr
              AND gjahr = ls_bkpf-gjahr
              AND mwskz IN lr_vat
              AND lifnr IS INITIAL.

          CLEAR lv_lifnr.
          LOOP AT t_bseg INTO ls_bseg2  WHERE belnr = ls_bseg-belnr
                                               AND gjahr = ls_bseg-gjahr
                                               AND lifnr <> space.
            lv_lifnr = ls_bseg2-lifnr.
            EXIT.
          ENDLOOP.
          CHECK lv_lifnr IS NOT INITIAL.

          CASE ls_bseg-shkzg.
            WHEN 'H'."'S'.
              ls_bseg-dmbtr *= -1.
          ENDCASE.

          READ TABLE lt_int_tot INTO ls_int_tot WITH KEY z_cc = ls_bseg-bukrs
                                                         z_vv = lv_lifnr BINARY SEARCH.
          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = 'Information'
              txt1  = |{ TEXT-t01 }   { ls_int_tot-z_plafond CURRENCY = ls_bkpf-waers NUMBER = USER }|
              txt2  = |{ TEXT-t02 }   { ls_bseg-dmbtr + ls_int_tot-z_balance CURRENCY = ls_bkpf-waers NUMBER = USER }|
              txt3  = |{ TEXT-t03 }   { ls_int_tot-z_plafond - ( ls_bseg-dmbtr + ls_int_tot-z_balance ) CURRENCY = ls_bkpf-waers NUMBER = USER }|
              txt4  = |{ TEXT-t04 }   { ls_bseg-dmbtr CURRENCY = ls_bkpf-waers NUMBER = USER }|.

          IF ls_int_tot-remain < ls_bseg-dmbtr.
            lv_message = TEXT-e01.
            REPLACE '&1' INTO lv_message WITH lv_lifnr.
            MESSAGE lv_message TYPE 'E'.
          ENDIF.

          READ TABLE gt_zfi_intent_uploa TRANSPORTING NO FIELDS
                WITH KEY z_cc = ls_bseg-bukrs
                         z_vv = lv_lifnr
                BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            LOOP AT gt_zfi_intent_uploa ASSIGNING <fs_zfi_intent_uploa> FROM sy-tabix.
              IF <fs_zfi_intent_uploa>-z_cc <> ls_bseg-bukrs
                OR <fs_zfi_intent_uploa>-z_vv <> lv_lifnr.
                EXIT.
              ENDIF.

              IF <fs_zfi_intent_uploa>-remain = 0.
                CONTINUE.
              ENDIF.

              IF ls_bseg-dmbtr >= <fs_zfi_intent_uploa>-remain.
                ls_bseg-dmbtr -= <fs_zfi_intent_uploa>-remain.
                <fs_zfi_intent_uploa>-z_balance += <fs_zfi_intent_uploa>-remain.
                <fs_zfi_intent_uploa>-remain = 0.
              ELSE.
                <fs_zfi_intent_uploa>-z_balance += ls_bseg-dmbtr.
                <fs_zfi_intent_uploa>-remain -= ls_bseg-dmbtr.
                EXIT.
              ENDIF.

            ENDLOOP.

          ENDIF.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.

  IF <fs_simulate> IS ASSIGNED AND <fs_simulate> IS INITIAL OR <fs_simulate> IS NOT ASSIGNED.
    EXPORT et_int = gt_zfi_intent_uploa TO MEMORY ID 'ZFI_INT_UPL'.
    PERFORM update_zfi_intent_uploa ON COMMIT.
  ENDIF.

ENDFUNCTION.

*----------------------------------------------------------------------*
***INCLUDE LZFI_INTENT_UPLOAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form update_zfi_intent_uploa
*&---------------------------------------------------------------------*
FORM update_zfi_intent_uploa.
  DATA lt_zfi_int TYPE STANDARD TABLE OF zfi_intent_uploa.

  IMPORT et_int = gt_zfi_intent_uploa FROM MEMORY ID 'ZFI_INT_UPL'.
  IF gt_zfi_intent_uploa IS NOT INITIAL.
    FREE MEMORY ID 'ZFI_INT_UPL'.
    MOVE-CORRESPONDING gt_zfi_intent_uploa TO lt_zfi_int.
    MODIFY zfi_intent_uploa FROM TABLE lt_zfi_int.
  ENDIF.
ENDFORM.
