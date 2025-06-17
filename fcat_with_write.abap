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
  ( fieldname = 'MATNR' ref_table = 'MARA'  col_opt = 'X' )
  ( fieldname = 'MAKTX' ref_table = 'MAKT'  col_opt = 'X'  )
  ( fieldname = 'EBELN' ref_table = 'EKPO'  col_opt = 'X'  )
  ( fieldname = 'NETWR' ref_table = 'EKPO'  cfieldname = 'WAERS'  col_opt = 'X'  )
  ( fieldname = 'WAERS' ref_table = 'EKKO'  col_opt = 'X'  ) ).

  ENDMETHOD.


  METHOD display_output.

    DATA: lv_lines TYPE i,
          lv_index TYPE i.

    FIELD-SYMBOLS: <fs_value> TYPE any.

    IF mt_result IS INITIAL.
      WRITE: / 'No data to display.'.
      RETURN.
    ENDIF.

    get_fcat( ).

*    DATA ls_out TYPE ty_data.
*    DATA lo_rtti_struct TYPE REF TO cl_abap_structdescr.
**DATA(lo_rtti_struct) = cl_rodps_odp_rt=>get_structdescr_from_input( ls_out ).
**  CL_GENIOS_CUST_PARAMETER    GET_STRUCT_DESCR
*    DATA(lo_typedescr) = cl_abap_structdescr=>describe_by_data( ls_out ).
*    IF ( lo_typedescr->kind = cl_abap_typedescr=>kind_struct ).
*      lo_rtti_struct ?= lo_typedescr.
*      DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_rtti_struct ).
*    ENDIF.
*    WRITE : / '|'.
*    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_test>).
*      DATA(lv_outputlength) = <ls_test>-outputlen.
**      DATA(lv_string) = CONV string( <ls_test>-scrtext_l ).
**      WRITE:  lv_string , '|'.
*      WRITE : |{ <ls_test>-scrtext_l WIDTH = lv_outputlength }| , '|'.
*    ENDLOOP.

    WRITE : / '|'.
    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(lv_string) = CONV string( <ls_fcat>-coltext ).
      WRITE:   lv_string  , '|'.
    ENDLOOP.

    WRITE : sy-uline.

    DESCRIBE TABLE mt_result LINES lv_lines.

    lv_index = 1.
    DO lv_lines TIMES.
      READ TABLE mt_result ASSIGNING FIELD-SYMBOL(<ls_result>) INDEX lv_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      WRITE : / '|'.
      LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_result> TO <fs_value>.
        IF sy-subrc = 0.
          WRITE : <fs_value> ,'|'.


        ENDIF.
      ENDLOOP.

*      LOOP AT lt_dfies ASSIGNING <ls_test>.
*        ASSIGN COMPONENT <ls_test>-fieldname OF STRUCTURE <ls_result> TO <fs_value>.
*        IF sy-subrc = 0.
**          WRITE : <fs_value> ,'|'.
*          lv_outputlength = <ls_test>-outputlen.
*          WRITE :|{ <fs_value> WIDTH = lv_outputlength }| , '|'.
*
*        ENDIF.
*      ENDLOOP.

      WRITE : sy-uline.
      lv_index = lv_index + 1.
    ENDDO.

  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_fcat_write.
  go_fcat_write->execute( ).
