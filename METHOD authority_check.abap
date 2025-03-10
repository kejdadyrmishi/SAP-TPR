METHOD authority_check.
    DATA(lt_auth) = mt_auth.

    LOOP AT lt_auth ASSIGNING FIELD-SYMBOL(<ls_auth>).

      DO.

* Get name of mapping field
        DATA(lv_field) = |<LS_AUTH>-MAP_F| && sy-index.
        ASSIGN (lv_field) TO FIELD-SYMBOL(<la_map>).
        IF sy-subrc EQ 0.
* if name is empty, STOP
          IF <la_map> IS INITIAL.
            EXIT.
          ENDIF.
* get value of the mapped field
          ASSIGN (<la_map>) TO FIELD-SYMBOL(<la_val>).
          IF sy-subrc EQ 0.
* if field is empty ignore it
            IF <la_val> IS INITIAL.
              CLEAR <la_map>.
              CONTINUE.
            ELSE.
              DATA(lv_continue) = abap_true.
            ENDIF.
          ELSE.
            DATA(lv_skip) = abap_true.
            EXIT.
          ENDIF.
* set field value as outside value
          DATA(lv_value) = |<LS_AUTH>-VAL_F| && sy-index.
          ASSIGN (lv_value) TO FIELD-SYMBOL(<la_ath_val>).
          IF sy-subrc EQ 0.
            <la_ath_val> = <la_val>.
          ENDIF.

        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      IF lv_skip IS NOT INITIAL OR lv_continue IS INITIAL.
        CLEAR: lv_skip.
        CONTINUE.
      ENDIF.
      CLEAR lv_continue.

      AUTHORITY-CHECK OBJECT <ls_auth>-objct
       ID <ls_auth>-fiel1  FIELD <ls_auth>-val_f1
       ID <ls_auth>-fiel2  FIELD <ls_auth>-val_f2
       ID <ls_auth>-fiel3  FIELD <ls_auth>-val_f3
       ID <ls_auth>-fiel4  FIELD <ls_auth>-val_f4
       ID <ls_auth>-fiel5  FIELD <ls_auth>-val_f5
       ID <ls_auth>-fiel6  FIELD <ls_auth>-val_f6
       ID <ls_auth>-fiel7  FIELD <ls_auth>-val_f7
       ID <ls_auth>-fiel8  FIELD <ls_auth>-val_f8
       ID <ls_auth>-fiel9  FIELD <ls_auth>-val_f9
       ID <ls_auth>-fiel10 FIELD <ls_auth>-val_f10.
      IF sy-subrc <> 0.
*    You are not authorized for &1
        MESSAGE e034(zmcmf) WITH <ls_auth>-ttext
                                 <ls_auth>-val_f1
         RAISING not_authorized.
      ENDIF.

      IF iv_delete IS NOT INITIAL.
        DELETE mt_auth WHERE objct = <ls_auth>-objct.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
