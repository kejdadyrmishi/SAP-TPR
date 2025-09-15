*&---------------------------------------------------------------------*
*& Report ZMM_CP_LOAD_PERC_AMMESA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_load_perc_ammessa.

TABLES: zmm_cp_perc_amm.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_matnr FOR zmm_cp_perc_amm-matnr,
                  s_arbpl FOR zmm_cp_perc_amm-arbpl,
                  s_lifnr FOR zmm_cp_perc_amm-lifnr,
                  s_val_f FOR zmm_cp_perc_amm-valid_from,
                  s_val_t FOR zmm_cp_perc_amm-valid_to,
                  s_calo  FOR zmm_cp_perc_amm-zzcalo.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_selfields,
             scr_name TYPE char8,
             fld_name TYPE fieldname,
           END OF ty_selfields,
           tt_selfields TYPE STANDARD TABLE OF ty_selfields WITH EMPTY KEY.

    DATA: mt_sellist   TYPE STANDARD TABLE OF vimsellist.

    METHODS execute.

  PRIVATE SECTION.

    METHODS extract_data.

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.
    extract_data( ).

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action      = 'S'
        view_name   = 'ZMM_CP_PERC_AMM'
      TABLES
        dba_sellist = mt_sellist
      EXCEPTIONS
        OTHERS      = 1.

  ENDMETHOD.                    "execute

  METHOD extract_data.

    FIELD-SYMBOLS: <lt_range> TYPE STANDARD TABLE.

    DATA(lt_selfields) = VALUE tt_selfields(
        ( scr_name = 'S_MATNR' fld_name = 'MATNR' )
        ( scr_name = 'S_ARBPL' fld_name = 'ARBPL' )
        ( scr_name = 'S_LIFNR' fld_name = 'LIFNR' )
        ( scr_name = 'S_VAL_F' fld_name = 'VALID_FROM' )
        ( scr_name = 'S_VAL_T' fld_name = 'VALID_TO' )
        ( scr_name = 'S_CALO'  fld_name = 'ZZCALO' ) ).

    LOOP AT lt_selfields ASSIGNING FIELD-SYMBOL(<ls_field>).

      DATA(lv_range) = |{ <ls_field>-scr_name }[]|.

      ASSIGN (lv_range) TO <lt_range>.

      CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
        EXPORTING
          fieldname          = <ls_field>-fld_name
          append_conjunction = 'AND'
        TABLES
          sellist            = mt_sellist
          rangetab           = <lt_range>.

    ENDLOOP.

  ENDMETHOD.                    "extract_data

ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_report.
  go_report->execute( ).
