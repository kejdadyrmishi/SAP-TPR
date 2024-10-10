*&---------------------------------------------------------------------*
*& Report ZFI_INTENT_UPLOA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_intent_uploa.

INCLUDE <cl_alv_control>.

TABLES zfi_intent_uploa.

DATA: BEGIN OF gs_0001,
        ok_code TYPE sy-ucomm,
      END OF gs_0001.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS:s_cc   FOR zfi_intent_uploa-z_cc         ,
                 s_vv   FOR zfi_intent_uploa-z_vv         ,
                 s_from FOR zfi_intent_uploa-z_from       ,
                 s_to   FOR zfi_intent_uploa-z_to         ,
                 s_plaf FOR zfi_intent_uploa-z_plafond    ,
                 s_bala FOR zfi_intent_uploa-z_balance    ,
                 s_prot FOR zfi_intent_uploa-z_protocol_nr.

  PARAMETERS: p_edit TYPE flag.

SELECTION-SCREEN END OF BLOCK b01.

CLASS lcl_int_maint DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_intent_upl,
             z_cc          TYPE zfi_intent_uploa-z_cc,
             z_vv          TYPE zfi_intent_uploa-z_vv,
             name1         TYPE lfa1-name1,
             z_from        TYPE zfi_intent_uploa-z_from,
             z_to          TYPE zfi_intent_uploa-z_to,
             z_plafond     TYPE zfi_intent_uploa-z_plafond,
             z_balance     TYPE zfi_intent_uploa-z_balance,
             percentage    TYPE char10,
             z_protocol_nr TYPE zfi_intent_uploa-z_protocol_nr,
             style         TYPE lvc_t_styl,
           END OF ty_intent_upl,
           tt_intent_upl TYPE STANDARD TABLE OF ty_intent_upl.

    TYPES: BEGIN OF ty_intent_upl_db.
             INCLUDE TYPE ty_intent_upl.
    TYPES:   action TYPE char1,
             read   TYPE char1,
           END OF ty_intent_upl_db.

    DATA: mt_intent_uplt TYPE tt_intent_upl,
          mo_grid        TYPE REF TO cl_gui_alv_grid,
          mv_edit        TYPE i.

    METHODS maintain.
    METHODS on_save.
    METHODS edit_mode IMPORTING iv_call TYPE abap_bool OPTIONAL.

  PRIVATE SECTION.
    METHODS
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender.

    METHODS
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.

    METHODS
      handle_data_ch FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
          et_good_cells.

ENDCLASS.

DATA go_intent_uploa TYPE REF TO lcl_int_maint.

CLASS lcl_int_maint IMPLEMENTATION.
  METHOD maintain.

    DATA :lv_dec3   TYPE p DECIMALS 3.

    SELECT upl~z_cc,
           upl~z_vv,
           lfa1~name1,
           upl~z_from,
           upl~z_to,
           upl~z_plafond,
           upl~z_balance,
           upl~z_protocol_nr

    FROM zfi_intent_uploa AS upl
    LEFT JOIN lfa1
      ON upl~z_vv = lfa1~lifnr

    INTO CORRESPONDING FIELDS OF TABLE @mt_intent_uplt

    WHERE upl~z_cc           IN @s_cc
      AND upl~z_vv           IN @s_vv
      AND upl~z_from         IN @s_from
      AND upl~z_to           IN @s_to
      AND upl~z_plafond      IN @s_plaf
      AND upl~z_balance      IN @s_bala
      AND upl~z_protocol_nr  IN @s_prot.

    DATA(lt_style) = VALUE lvc_t_styl(

    ( fieldname = 'Z_PLAFOND'          style = cl_gui_alv_grid=>mc_style_enabled )
    ( fieldname = 'Z_BALANCE'          style = cl_gui_alv_grid=>mc_style_enabled )
    ( fieldname = 'Z_PROTOCOL_NR'      style = cl_gui_alv_grid=>mc_style_enabled )
    ( fieldname = 'PERCENTAGE'         style = alv_style_align_right_center )
    ).

    LOOP AT mt_intent_uplt ASSIGNING FIELD-SYMBOL(<ls_int>).
      <ls_int>-style = lt_style.
      IF <ls_int>-z_plafond <> 0.
        lv_dec3 = ( <ls_int>-z_balance * 100 ) / <ls_int>-z_plafond.
      ENDIF.
      <ls_int>-percentage = |{ lv_dec3 } %|.
    ENDLOOP.

    MODIFY mt_intent_uplt FROM VALUE #( style = lt_style ) TRANSPORTING style WHERE style IS INITIAL.

    DATA(lt_fcat) = VALUE lvc_t_fcat(
    ( fieldname = 'Z_CC'          ref_table = 'ZFI_INTENT_UPLOA' key = 'X' )
    ( fieldname = 'Z_VV'          ref_table = 'ZFI_INTENT_UPLOA' key = 'X' )
    ( fieldname = 'NAME1'         ref_table = 'LFA1'             key = 'X' )
    ( fieldname = 'Z_FROM'        ref_table = 'ZFI_INTENT_UPLOA' key = 'X' )
    ( fieldname = 'Z_TO'          ref_table = 'ZFI_INTENT_UPLOA' key = 'X' )
    ( fieldname = 'Z_PLAFOND'     ref_table = 'ZFI_INTENT_UPLOA' )
    ( fieldname = 'Z_BALANCE'     ref_table = 'ZFI_INTENT_UPLOA' )
    ( fieldname = 'PERCENTAGE'    reptext   = '% di Esaurimento' )
    ( fieldname = 'Z_PROTOCOL_NR' ref_table = 'ZFI_INTENT_UPLOA' )
     ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        stylefname  = 'STYLE' ).

    DATA(lo_cont) = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).
    mo_grid = NEW cl_gui_alv_grid( i_parent = lo_cont ).

    IF p_edit IS NOT INITIAL.
      mv_edit = 1.
    ENDIF.

    edit_mode( ).

    SET HANDLER: handle_toolbar
                 handle_ucomm
                 handle_data_ch FOR mo_grid .

    TRY.
        cl_abap_list_layout=>suppress_toolbar( ).
      CATCH cx_list_already_active. " A list is already active. No call-up allowed
    ENDTRY.

    mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    DATA(lt_tb_exc) = VALUE ui_functions(
                ( '&LOCAL&INSERT_ROW'    )
                ( '&LOCAL&CUT'    )
                ( '&LOCAL&COPY'   )
                ( '&LOCAL&PASTE'  )
                ( '&LOCAL&COPY_ROW'  )
                ( '&LOCAL&PASTE_NEW_ROW'  )
                ( '&INFO'         )
    ).

    mo_grid->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout                 " Layout
        it_toolbar_excluding          = lt_tb_exc
      CHANGING
        it_outtab                     = mt_intent_uplt                " Output Table
        it_fieldcatalog               = lt_fcat              " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).

    CALL SCREEN 0001.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD handle_data_ch.

    DATA: lv_dec3  TYPE p DECIMALS 3,
          lv_lifnr TYPE lifnr.

    LOOP AT et_good_cells ASSIGNING FIELD-SYMBOL(<ls_cell>)
      WHERE fieldname = 'Z_PLAFOND'
        OR fieldname  = 'Z_BALANCE'
        OR fieldname  = 'Z_VV'.

      READ TABLE mt_intent_uplt ASSIGNING FIELD-SYMBOL(<ls_int>) INDEX <ls_cell>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE <ls_cell>-fieldname.
        WHEN 'Z_PLAFOND' OR 'Z_BALANCE'.
          IF <ls_int>-z_plafond <> 0.
            lv_dec3 = ( <ls_int>-z_balance * 100 ) / <ls_int>-z_plafond.
          ENDIF.
          <ls_int>-percentage = |{ lv_dec3 } %|.
        WHEN 'Z_VV'.
          lv_lifnr = |{ <ls_cell>-value ALPHA = IN }|.
          SELECT SINGLE name1
            FROM lfa1
            INTO <ls_int>-name1
            WHERE lifnr = lv_lifnr.
      ENDCASE.

    ENDLOOP.
    IF sy-subrc = 0.
      mo_grid->refresh_table_display(
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD handle_ucomm.
    CASE e_ucomm.
      WHEN 'FC_APPEND'.

        APPEND INITIAL LINE TO mt_intent_uplt ASSIGNING FIELD-SYMBOL(<ls_intent>).
        <ls_intent>-percentage = |0 %|.
        <ls_intent>-style = VALUE lvc_t_styl(
          ( fieldname = 'Z_CC'          style = cl_gui_alv_grid=>mc_style_enabled )
          ( fieldname = 'Z_VV'          style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'Z_FROM'        style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'Z_TO'          style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'Z_PLAFOND'     style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'Z_BALANCE'     style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'Z_PROTOCOL_NR' style = cl_gui_alv_grid=>mc_style_enabled  )
          ( fieldname = 'PERCENTAGE'    style = alv_style_align_right_center ) ).

        mo_grid->refresh_table_display(
          EXCEPTIONS
            finished       = 1                " Display was Ended (by Export)
            OTHERS         = 2
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'FC_EDIT'.

        IF mv_edit = 1.
          mv_edit = 0.

*    lock the object
          CALL FUNCTION 'DEQUEUE_E_TRDIR'
            EXPORTING
              name           = sy-repid
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc = 0.
            mo_grid->set_ready_for_input( i_ready_for_input = mv_edit ).
          ENDIF.

        ELSE.
          mv_edit = 1.
          edit_mode( iv_call = abap_true ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.
  METHOD handle_toolbar.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<ls_toolb>) WITH KEY function = '&LOCAL&APPEND'.
    IF sy-subrc = 0.
      <ls_toolb>-function = 'FC_APPEND'.
    ENDIF.

    APPEND VALUE #( function  = 'FC_EDIT'
                    icon      = CONV #( icon_toggle_display_change )
                    quickinfo = 'Edit mode'
     ) TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD on_save.

    DATA: lt_intent_uplt_db TYPE STANDARD TABLE OF ty_intent_upl_db,
          lt_cells          TYPE lvc_t_cell,
          lt_update_int     TYPE STANDARD TABLE OF zfi_intent_uploa,
          lt_insert_int     TYPE STANDARD TABLE OF zfi_intent_uploa,
          lt_delete_int     TYPE STANDARD TABLE OF zfi_intent_uploa.

    SELECT *
      FROM zfi_intent_uploa AS upl
      INTO CORRESPONDING FIELDS OF TABLE @lt_intent_uplt_db
      WHERE upl~z_cc           IN @s_cc
        AND upl~z_vv           IN @s_vv
        AND upl~z_from         IN @s_from
        AND upl~z_to           IN @s_to
        AND upl~z_plafond      IN @s_plaf
        AND upl~z_balance      IN @s_bala
        AND upl~z_protocol_nr  IN @s_prot
      ORDER BY  upl~z_cc,
                upl~z_vv,
                upl~z_from,
                upl~z_to.

    mo_grid->check_changed_data( ).
* Fill the required fields
    LOOP AT mt_intent_uplt ASSIGNING FIELD-SYMBOL(<ls_intent_uplt>).
      DATA(lv_tabix) = sy-tabix.

      IF <ls_intent_uplt>-z_vv      IS INITIAL
        OR <ls_intent_uplt>-z_cc    IS INITIAL
        OR <ls_intent_uplt>-z_to    IS INITIAL
        OR <ls_intent_uplt>-z_from  IS INITIAL.

        MESSAGE 'Fill the required fields first!'(001) TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      READ TABLE lt_intent_uplt_db ASSIGNING FIELD-SYMBOL(<ls_db>)
        WITH KEY z_vv   = <ls_intent_uplt>-z_vv
                 z_cc   = <ls_intent_uplt>-z_cc
                 z_to   = <ls_intent_uplt>-z_to
                 z_from = <ls_intent_uplt>-z_from BINARY SEARCH.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_intent_uplt_db ASSIGNING <ls_db>.
        <ls_db>        = CORRESPONDING #( <ls_intent_uplt> ).
        <ls_db>-action = 'I'.
        <ls_db>-read   = abap_true.

        CONTINUE.
      ENDIF.

      IF <ls_db>-read IS NOT INITIAL.
*        cells red duplicate
        APPEND LINES OF VALUE lvc_t_cell(
        row_id-index = lv_tabix
        ( col_id-fieldname = 'Z_VV' )
        ( col_id-fieldname = 'Z_CC' )
        ( col_id-fieldname = 'Z_TO' )
        ( col_id-fieldname = 'Z_FROM' )
        ) TO lt_cells.
        CONTINUE.
      ENDIF.

      <ls_db>-action = 'U'.
      <ls_db>-read   = abap_true.

    ENDLOOP.

    IF lt_cells IS NOT INITIAL.
      MESSAGE 'Keys are duplicated. Please, fix!'(002) TYPE 'S' DISPLAY LIKE 'E'.
      mo_grid->set_selected_cells( it_cells = lt_cells ).
      RETURN.
    ENDIF.

    LOOP AT lt_intent_uplt_db ASSIGNING <ls_db>.
      CASE <ls_db>-action.
        WHEN 'U'.
          APPEND CORRESPONDING #( <ls_db> ) TO lt_update_int.
        WHEN 'I'.
          APPEND CORRESPONDING #( <ls_db> ) TO lt_insert_int.
        WHEN OTHERS.
          APPEND CORRESPONDING #( <ls_db> ) TO lt_delete_int.
      ENDCASE.
    ENDLOOP.

    IF lt_delete_int IS NOT INITIAL.
      DELETE zfi_intent_uploa FROM TABLE lt_delete_int.
    ENDIF.

    IF lt_update_int IS NOT INITIAL.
      UPDATE zfi_intent_uploa FROM TABLE lt_update_int.
    ENDIF.

    IF lt_insert_int IS NOT INITIAL.
      INSERT zfi_intent_uploa FROM TABLE lt_insert_int.
    ENDIF.

    COMMIT WORK.
    MESSAGE 'The data has been saved successfully!'(003) TYPE 'S'.

  ENDMETHOD.

  METHOD edit_mode.

    DATA: lv_answer TYPE string.

    IF mv_edit IS INITIAL.
      RETURN.
    ENDIF.

*    lock the object
    CALL FUNCTION 'ENQUEUE_E_TRDIR'
      EXPORTING
        name           = sy-repid
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      mo_grid->set_ready_for_input( i_ready_for_input = 1 ).
      RETURN.
    ENDIF.

    IF iv_call IS INITIAL.
      DATA(lv_message) = |The data is locked by user { sy-msgv1 } and can be displayed only. Do you want to display locked data? |.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Locked Data'
          text_question         = lv_message
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CASE lv_answer.
        WHEN '2'.
          LEAVE LIST-PROCESSING.
      ENDCASE.

      RETURN.
    ENDIF.

    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    mv_edit = 0.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  go_intent_uploa = NEW #( ).
  go_intent_uploa->maintain( ).
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF-STATUS'.
  SET TITLEBAR 'ALV-TB'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE gs_0001-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'FC_SAVE'.
      go_intent_uploa->on_save( ).

  ENDCASE.

ENDMODULE.
