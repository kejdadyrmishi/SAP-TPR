*&---------------------------------------------------------------------*
*& Report ZFI_INTENT_UPLOA_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_intent_uploa_1.

TABLES : zfi_intent_uploa, acdoca ,tvarv.

DATA: BEGIN OF gs_0001,
        ok_code TYPE sy-ucomm,
      END OF gs_0001.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:s_cc   FOR zfi_intent_uploa-z_cc    ,
                 s_vv   FOR zfi_intent_uploa-z_vv    ,
                 s_from FOR zfi_intent_uploa-z_from  ,
                 s_to   FOR zfi_intent_uploa-z_to    .
SELECTION-SCREEN END OF BLOCK b01.

CLASS lcl_intent_uploa_1 DEFINITION.
  PUBLIC SECTION.

    METHODS execute.
    METHODS output_popup.

    TYPES : BEGIN OF ty_acdoca,
              rbukrs TYPE acdoca-rbukrs,     " Company Code
              gjahr  TYPE acdoca-gjahr,      " Fiscal Year
              budat  TYPE acdoca-budat,      " Invoice Date
              bldat  TYPE acdoca-bldat,      " Registration Date
              belnr  TYPE acdoca-belnr,      " Document Number
              mwskz  TYPE acdoca-mwskz,      " VAT Code
              tsl    TYPE acdoca-tsl,        " Totale Imponibile
            END OF ty_acdoca.

    DATA: mt_acdoca    TYPE STANDARD TABLE OF ty_acdoca,
          mo_alv_popup TYPE REF TO cl_gui_alv_grid.

  PRIVATE SECTION.

    DATA : mo_salv           TYPE REF TO cl_salv_table,
           mt_intent_display TYPE STANDARD TABLE OF zfi_intent_uploa.

    METHODS display_alv.
    METHODS get_data.
    METHODS on_link_click FOR EVENT link_click  "Hotspot Handler
      OF cl_salv_events_table
      IMPORTING row column.
    METHODS on_added_funct FOR EVENT added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

DATA go_intent_uploa_1 TYPE REF TO lcl_intent_uploa_1.

CLASS lcl_intent_uploa_1 IMPLEMENTATION.

  METHOD get_data.

    SELECT *
    FROM zfi_intent_uploa
    INTO TABLE @mt_intent_display
    WHERE z_cc    IN @s_cc
    AND   z_vv    IN @s_vv
    AND   z_from  IN @s_from
    AND   z_to    IN @s_to.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_list.

    IF mt_intent_display IS INITIAL.
      MESSAGE 'No data found'(004) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = cl_gui_container=>screen0
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_intent_display ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        RETURN.
    ENDTRY.

    lo_columns = mo_salv->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( columnname = 'MANDT' ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( columnname = 'Z_VV' ).
        lo_column->set_cell_type(  if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found.
    ENDTRY.

    SET HANDLER: on_link_click
                 on_added_funct FOR mo_salv->get_event( ).

    DATA(lo_funct) = mo_salv->get_functions( ).
    lo_funct->set_all( abap_true ).

    TRY.
        lo_funct->add_function(
          EXPORTING
            name     = 'FC_REFRESH'                 " ALV Function
            icon     = CONV #( icon_refresh )
            tooltip  = 'Aggiorna dati'
            position = if_salv_c_function_position=>left_of_salv_functions                 " Positioning Function
        ).
      CATCH cx_salv_existing.   " ALV: General Error Class (Checked in Syntax Check)
      CATCH cx_salv_wrong_call. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        cl_abap_list_layout=>suppress_toolbar( ).
      CATCH cx_list_already_active. " A list is already active. No call-up allowed
    ENDTRY.

    mo_salv->display( ).
    WRITE space.

  ENDMETHOD.

  METHOD on_link_click.

    DATA: lo_functions   TYPE REF TO cl_salv_functions_list,
          lr_belnr       TYPE RANGE OF acdoca-belnr,
          lr_mwskz       TYPE RANGE OF acdoca-mwskz,
          lr_koart       TYPE RANGE OF acdoca-koart,
          lr_tvarv_belnr TYPE RANGE OF acdoca-belnr.

    CASE column.

      WHEN 'Z_VV'.

        READ TABLE mt_intent_display INTO DATA(ls_intent_display) INDEX row TRANSPORTING z_vv
                                                                                         z_cc
                                                                                         z_to
                                                                                         z_from .

        SELECT sign, opti AS option, low, high
          FROM tvarvc
          INTO TABLE @lr_tvarv_belnr
          WHERE  name = 'Z_INTENT_DOC_NUM' .

        SELECT sign, opti AS option, low, high
          FROM tvarvc
          INTO TABLE @lr_mwskz
          WHERE  name = 'Z_INTENT_VAT_CODE' .

        SELECT sign, opti AS option, low, high
          FROM tvarvc
          INTO TABLE @lr_koart
          WHERE  name = 'Z_INTENT_ACC_TYPE' .

* EXTRACTION 1
        DATA(lv_gjahr) = CONV acdoca-gjahr( ls_intent_display-z_from(4) - 1 ).

        CLEAR lr_belnr.
        SELECT 'I' AS sign,
               'EQ' AS option,
                belnr AS low
        FROM acdoca
        INTO TABLE @lr_belnr
          WHERE lifnr = @ls_intent_display-z_vv
          AND rbukrs = @ls_intent_display-z_cc
          AND gjahr = @lv_gjahr
          AND belnr IN @lr_tvarv_belnr
          AND bldat BETWEEN @ls_intent_display-z_from AND @ls_intent_display-z_to.

        IF lr_belnr IS NOT INITIAL.

          SELECT
          rbukrs
          gjahr
          budat
          bldat
          belnr
          mwskz
          tsl
          FROM acdoca
          INTO CORRESPONDING FIELDS OF TABLE mt_acdoca
          WHERE lifnr = ls_intent_display-z_vv
          AND rbukrs = ls_intent_display-z_cc
          AND gjahr = lv_gjahr
          AND belnr IN lr_belnr
          AND bldat BETWEEN ls_intent_display-z_from AND ls_intent_display-z_to
          AND mwskz IN lr_mwskz
          AND koart IN lr_koart.

        ENDIF.

*          EXTRACTION 2
        CLEAR lr_belnr.
        SELECT 'I' AS sign,
               'EQ' AS option,
                belnr AS low
        FROM acdoca
        INTO TABLE @lr_belnr
        WHERE lifnr = @ls_intent_display-z_vv
        AND rbukrs = @ls_intent_display-z_cc
        AND gjahr = @ls_intent_display-z_from(4)
        AND belnr IN @lr_tvarv_belnr
        AND bldat BETWEEN @ls_intent_display-z_from AND @ls_intent_display-z_to.

        IF lr_belnr IS NOT INITIAL.

          SELECT
          rbukrs
          gjahr
          budat
          bldat
          belnr
          mwskz
          tsl
          FROM acdoca
          INTO CORRESPONDING FIELDS OF TABLE mt_acdoca
          WHERE rbukrs = ls_intent_display-z_cc
          AND gjahr = ls_intent_display-z_from(4)
          AND belnr IN lr_belnr
          AND bldat BETWEEN ls_intent_display-z_from AND ls_intent_display-z_to
          AND mwskz IN lr_mwskz
          AND koart IN lr_koart .

        ENDIF.

        IF mt_acdoca IS NOT INITIAL.

          CALL SCREEN 0001 STARTING AT 10 10
                            ENDING AT 100 30.

        ELSE.
          MESSAGE 'No entries found that match the selection criteria!'(002) TYPE 'I'.
        ENDIF.

      WHEN OTHERS.
        MESSAGE 'Click on a valid vendor!'(003) TYPE 'I'.
    ENDCASE.

  ENDMETHOD.

  METHOD execute.
    get_data( ).
    display_alv( ).
  ENDMETHOD.
  METHOD output_popup.
    DATA lo_cust_cont TYPE REF TO cl_gui_custom_container.

    IF mo_alv_popup IS BOUND.
      mo_alv_popup->refresh_table_display(
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      RETURN.
    ENDIF.

    DATA(lt_fcat) = VALUE lvc_t_fcat(
    ( fieldname = 'RBUKRS' ref_table = 'ACDOCA' )
    ( fieldname = 'GJAHR'  ref_table = 'ACDOCA' )
    ( fieldname = 'BUDAT'  ref_table = 'ACDOCA' )
    ( fieldname = 'BLDAT'  ref_table = 'ACDOCA' )
    ( fieldname = 'BELNR'  ref_table = 'ACDOCA' )
    ( fieldname = 'MWSKZ'  ref_table = 'ACDOCA' )
    ( fieldname = 'TSL'    ref_table = 'ACDOCA' do_sum = abap_true ) ).

    DATA(ls_layout) = VALUE lvc_s_layo( col_opt = abap_true ).

    CREATE OBJECT lo_cust_cont
      EXPORTING
        container_name              = 'POPUP_CONT'
        repid                       = sy-repid                 " Screen to Which this Container is Linked
        dynnr                       = sy-dynnr        " Report To Which this Container is Linked
      EXCEPTIONS
        cntl_error                  = 1                " CNTL_ERROR
        cntl_system_error           = 2                " CNTL_SYSTEM_ERROR
        create_error                = 3                " CREATE_ERROR
        lifetime_error              = 4                " LIFETIME_ERROR
        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CREATE OBJECT mo_alv_popup
      EXPORTING
        i_parent          = lo_cust_cont
      EXCEPTIONS
        error_cntl_create = 1                " Error when creating the control
        error_cntl_init   = 2                " Error While Initializing Control
        error_cntl_link   = 3                " Error While Linking Control
        error_dp_create   = 4                " Error While Creating DataProvider Control
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_alv_popup->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout                 " Layout
      CHANGING
        it_outtab                     = mt_acdoca                " Output Table
        it_fieldcatalog               = lt_fcat              " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD on_added_funct.

    CASE e_salv_function.
      WHEN 'FC_REFRESH'.
        get_data( ).
        mo_salv->refresh( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.               "lcl_INTENT_UPLOA_1

START-OF-SELECTION.

  go_intent_uploa_1 = NEW #( ).
  go_intent_uploa_1->execute( ).
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF_POPUP'.
  SET TITLEBAR 'TB-POPUP'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE gs_0001-ok_code.
    WHEN 'FC_OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module OUTPUT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE output OUTPUT.
  go_intent_uploa_1->output_popup( ).
ENDMODULE.
