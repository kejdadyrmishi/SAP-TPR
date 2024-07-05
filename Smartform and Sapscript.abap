REPORT zkd_print.

DATA : lv_ebeln         TYPE ebeln,
       gt_ekko          TYPE STANDARD TABLE OF zkd_print_ekko,
       gs_ekko          TYPE zkd_print_ekko,
       gt_ekpo          TYPE zkd_t_ekpo,
       gs_ekpo          TYPE zkd_s_ekpo,
       gv_fm_name       TYPE rs38l_fnam,
       gt_lines         TYPE TABLE OF tline,
       gs_lines         TYPE tline,
       gv_total         TYPE p DECIMALS 2,
       gv_quantity_char TYPE string,
       gv_net_price_c   TYPE string,
       gv_net_value_c   TYPE string.

CONSTANTS: c_id     TYPE thead-tdid     VALUE 'F01',
           c_langu  TYPE thead-tdspras  VALUE 'D',
           c_object TYPE thead-tdobject VALUE 'EKKO'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ebeln FOR lv_ebeln.

PARAMETERS:p_script RADIOBUTTON GROUP docp DEFAULT 'X' USER-COMMAND u01 ,
           p_sform  RADIOBUTTON GROUP docp .
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM execute.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_EKKO
*&---------------------------------------------------------------------*
FORM get_data_ekko.

  "join the tables ekko, lfa1, and t001 by purchase order number, vendor number, and company code
  SELECT   t001~butxt
           t001~ort01 AS t_ort01
           t001~land1
           ekko~ebeln
           ekko~aedat
           ekko~ernam
           ekko~zterm
           ekko~verkf
           ekko~telf1
           ekko~lifnr
           lfa1~name1
           lfa1~ort01 AS l_ort01
           lfa1~ort02
           ekko~waers
           ekko~wkurs
    FROM ekko
    LEFT JOIN lfa1 ON ekko~lifnr = lfa1~lifnr
    INNER JOIN t001 ON ekko~bukrs = t001~bukrs
    INTO CORRESPONDING FIELDS OF TABLE gt_ekko
    WHERE ekko~ebeln IN s_ebeln
    ORDER BY ekko~ebeln.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_EKPO
*&---------------------------------------------------------------------*
FORM get_data_ekpo .

  "join the tables ekpo and makt by purchase order number and material number
  SELECT   ekpo~ebeln,
           ekpo~ebelp,
           ekpo~txz01,
           ekpo~matnr,
           makt~maktx,
           ekpo~menge,
           ekpo~netpr,
           ekpo~menge * ekpo~netpr AS net_value,
           ekpo~zzxx_comment
    FROM ekpo
    LEFT JOIN makt ON ekpo~matnr = makt~matnr
    AND makt~spras = @sy-langu
    INTO TABLE @gt_ekpo
   WHERE ekpo~ebeln IN @s_ebeln.

  "sort the table by purchase order number and item number
  SORT gt_ekpo ASCENDING BY ebeln ebelp.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_SMARTFORM
*&---------------------------------------------------------------------*
FORM call_smartform .

  DATA gs_control_struc TYPE ssfctrlop.

  LOOP AT gt_ekko INTO gs_ekko.

    DATA(lv_index) = sy-tabix.

    CASE lv_index.
      WHEN 1.
        IF lv_index <> lines( gt_ekko ).
          gs_control_struc-no_open = space.
          gs_control_struc-no_close = 'X'.
        ENDIF.

      WHEN lines( gt_ekko ).
        gs_control_struc-no_open = 'X'.
        gs_control_struc-no_close = space.

      WHEN OTHERS.
        gs_control_struc-no_open   = 'X' .
        gs_control_struc-no_close  = 'X' .
    ENDCASE.

    CALL FUNCTION gv_fm_name
      EXPORTING
        is_ekko            = gs_ekko
        it_ekpo            = gt_ekpo
        control_parameters = gs_control_struc
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR gs_ekko.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SMARTFORM_NAME
*&---------------------------------------------------------------------*
FORM smartform_name .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZKD_SFORM'
    IMPORTING
      fm_name            = gv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_SAPSCRIPT
*&---------------------------------------------------------------------*
FORM call_sapscript .

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      form                        = 'ZKD_SAPSCR'
      language                    = sy-langu
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SCRIPT_PRINT
*&---------------------------------------------------------------------*
FORM script_print .

  LOOP AT gt_ekko INTO gs_ekko .

    PERFORM start_form.
    PERFORM control_form.
    PERFORM address_window.
    PERFORM logo_window.
    PERFORM header_window.
    PERFORM calculate_total.
    PERFORM read_text_scr.
    PERFORM end_form.
  ENDLOOP.

  PERFORM close_form.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTROL_FORM
*&---------------------------------------------------------------------*
FORM control_form .

*   SAP Script Control Commands to the layout set
  IF sy-tabix > 1. " if the current loop index (sy-tabix) is greater than 1, the control command is only executed after processing the first entry in the loop.
    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command   = 'NEW-PAGE PAGE1' "This command instructs the form to start a new page.
      EXCEPTIONS
        unopened  = 1
        unstarted = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADDRESS_WINDOW
*&---------------------------------------------------------------------*
FORM address_window .

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'ADDRESS'
      window                   = 'ADDRES'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOGO_WINDOW
*&---------------------------------------------------------------------*
FORM logo_window .

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'LOGO'
      window                   = 'LOGO'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_WINDOW
*&---------------------------------------------------------------------*
FORM header_window .

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'HEADER'
      window                   = 'HDRDET'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FOOTER
*&---------------------------------------------------------------------*
FORM footer .

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'FOOTER'
      window                   = 'FOOTER'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  END_FORM
*&---------------------------------------------------------------------*
FORM end_form .

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      codepage                 = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM
*&---------------------------------------------------------------------*
FORM close_form .

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      codepage                 = 5
      OTHERS                   = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  START_FORM
*&---------------------------------------------------------------------*
FORM start_form .

  CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = 'ZKD_SAPSCR'
      language    = sy-langu
      startpage   = 'PAGE1'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      codepage    = 7
      OTHERS      = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MAIN_WINDOW
*&---------------------------------------------------------------------*
FORM main_window .

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'ITEM'
      window                   = 'MAIN'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element                  = 'INFO'
      window                   = 'MAIN'
    EXCEPTIONS
      element                  = 1
      function                 = 2
      type                     = 3
      unopened                 = 4
      unstarted                = 5
      window                   = 6
      bad_pageformat_for_print = 7
      spool_error              = 8
      codepage                 = 9
      OTHERS                   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_TOTAL
*&---------------------------------------------------------------------*
FORM calculate_total .

*  *TRANSPORTING NO FIELDS clause ensures that no fields from the table are transported to the work area
  READ TABLE gt_ekpo TRANSPORTING NO FIELDS WITH KEY ebeln = gs_ekko-ebeln BINARY SEARCH.
  IF sy-subrc = 0.

    LOOP AT gt_ekpo INTO gs_ekpo FROM sy-tabix.

      MOVE gs_ekpo-menge TO gv_quantity_char .
      REPLACE '.000' IN gv_quantity_char WITH ' '.

      MOVE gs_ekpo-netpr TO gv_net_price_c .
      MOVE gs_ekpo-net_value TO gv_net_value_c .

      IF gs_ekpo-ebeln <> gs_ekko-ebeln.
        EXIT.
      ENDIF.

      gv_total = gv_total + gs_ekpo-net_value.
      MODIFY gt_ekpo FROM gs_ekpo.

      PERFORM main_window.
      PERFORM footer.

    ENDLOOP.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'TOTAL'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        codepage                 = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR gv_total.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT_SCR
*&---------------------------------------------------------------------*
FORM read_text_scr .

  DATA gv_name TYPE thead-tdname.
  gv_name = gs_ekko-ebeln.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = c_id
      language                = c_langu
      name                    = gv_name
      object                  = c_object
    TABLES
      lines                   = gt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_lines INTO gs_lines.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'READ'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        codepage                 = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  CLEAR gt_lines.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXECUTE
*&---------------------------------------------------------------------*
FORM execute .
  PERFORM get_data_ekko.
  PERFORM get_data_ekpo.

  CASE abap_true.

    WHEN p_script.
      PERFORM call_sapscript.
      PERFORM script_print.

    WHEN p_sform.
      PERFORM smartform_name.
      PERFORM call_smartform .

  ENDCASE.
ENDFORM.
