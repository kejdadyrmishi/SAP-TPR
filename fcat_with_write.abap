*&---------------------------------------------------------------------*
*& Report  ZKD_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zkd_test.

*----------------------------------------------------------------------*
*       CLASS lcl_fcat_write DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fcat_write DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_data,
              matnr TYPE matnr,
              maktx TYPE maktx,
              ebeln TYPE ebeln,
              netwr TYPE bwert,
              waers TYPE waers,
            END OF ty_data.

    DATA: mt_result TYPE STANDARD TABLE OF ty_data,
          mt_fcat   TYPE lvc_t_fcat.

    METHODS:       extract_data,
      display_output,
      get_fcat.

ENDCLASS.                    "

DATA go_fcat_write TYPE REF TO lcl_fcat_write.

*----------------------------------------------------------------------*
*       CLASS lcl_fcat_write IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fcat_write IMPLEMENTATION.

  METHOD execute.
    extract_data( ).
    display_output( ).

  ENDMETHOD.                    "execute

  METHOD extract_data.

    SELECT
      mara~matnr,
      makt~maktx,
      ekpo~ebeln,
      ekpo~netwr,
      ekko~waers
      UP TO 10 ROWS
  INTO TABLE @mt_result
  FROM mara
  LEFT JOIN makt
  ON makt~matnr = mara~matnr
  AND makt~spras = @sy-langu
  JOIN ekpo
  ON ekpo~matnr = mara~matnr
  JOIN ekko
  ON ekko~ebeln = ekpo~ebeln .

  ENDMETHOD.                    "extract_data

  METHOD get_fcat.

    mt_fcat = VALUE lvc_t_fcat(
  ( fieldname = 'MATNR' ref_table = 'MARA'  coltext = 'Material Number' )
  ( fieldname = 'MAKTX' ref_table = 'MAKT'  coltext = 'Material Description' )
  ( fieldname = 'EBELN' ref_table = 'EKPO'  coltext = 'Document Number' )
  ( fieldname = 'NETWR' ref_table = 'EKPO'  coltext = 'Net Order' cfieldname = 'WAERS'  )
  ( fieldname = 'WAERS' ref_table = 'EKKO'  coltext = 'Currency' ) ).

  ENDMETHOD.

  METHOD display_output.

    TYPES:BEGIN OF ty_tab_length,
            fieldname TYPE fname,
            length    TYPE i,
          END OF ty_tab_length.

    FIELD-SYMBOLS: <fs_value>  TYPE any,
                   <ls_result> TYPE any,
                   <ls_fcat>   TYPE lvc_s_fcat.

    DATA: lv_len   TYPE i,
          lv_lines TYPE i,
          lv_index TYPE i.

    DATA: lv_maxlen TYPE i,
          lt_widths TYPE STANDARD TABLE OF ty_tab_length.

    IF mt_result IS INITIAL.
      WRITE: / 'No data to display.'.
      RETURN.
    ENDIF.

    get_fcat( ).

    LOOP AT mt_fcat ASSIGNING <ls_fcat>.
      lv_maxlen = strlen( <ls_fcat>-coltext ).
      LOOP AT mt_result ASSIGNING <ls_result>.
        ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_result> TO <fs_value>.
        IF sy-subrc = 0.
          lv_len = strlen( CONV string( <fs_value> ) ).
          IF lv_len > lv_maxlen.
            lv_maxlen = lv_len + 2.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND VALUE #( fieldname = <ls_fcat>-fieldname
                      length    = lv_maxlen ) TO lt_widths.

    ENDLOOP.

    DATA(lv_total_length) = REDUCE i( INIT lv_acc = 1
                                       FOR <ls_width> IN lt_widths
                                       NEXT lv_acc = lv_acc + <ls_width>-length + 3 ).


*    DATA(lv_total_length) = 1.
*    LOOP AT lt_widths ASSIGNING FIELD-SYMBOL(<ls_width>).
*      lv_total_length = lv_total_length + <ls_width>-length + 3.
*    ENDLOOP.

    WRITE: / '|'.
    LOOP AT mt_fcat ASSIGNING <ls_fcat>.
      READ TABLE lt_widths ASSIGNING FIELD-SYMBOL(<lv_width>) WITH KEY fieldname = <ls_fcat>-fieldname..
      IF sy-subrc = 0.
        WRITE :|{ <ls_fcat>-coltext WIDTH = <lv_width>-length }| , '|'.
      ENDIF.
    ENDLOOP.
    WRITE: / sy-uline(lv_total_length).

    LOOP AT mt_result ASSIGNING <ls_result>.
      WRITE: / '|'.
      LOOP AT mt_fcat ASSIGNING <ls_fcat>.
        READ TABLE lt_widths ASSIGNING <lv_width> WITH KEY fieldname = <ls_fcat>-fieldname.
        IF sy-subrc = 0.
          ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_result> TO <fs_value>.
          IF sy-subrc = 0.
            <fs_value> = |{ <fs_value> ALPHA = OUT }|.
            WRITE :|{ <fs_value> WIDTH = <lv_width>-length }| , '|'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      WRITE: / sy-uline(lv_total_length).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_fcat_write.
  go_fcat_write->execute( ).
