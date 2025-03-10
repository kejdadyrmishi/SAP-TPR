*&---------------------------------------------------------------------*
*& Modulpool ZMF_PARAM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zmf_param.

CLASS lcl_param DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: ty_editor(100) TYPE c,
           tt_editor      TYPE STANDARD TABLE OF ty_editor WITH DEFAULT KEY,
           tt_tabs        TYPE TABLE OF rsdstabs WITH DEFAULT KEY,
           tt_rsseldyn    TYPE STANDARD TABLE OF rsseldyn,
           tt_rsdsfldnum  TYPE STANDARD TABLE OF rsdsfldnum.

    TYPES: BEGIN OF ty_hdr,
             process   TYPE zbw_t_process-process,
             add       TYPE icon_d,
             expand(1) TYPE c,
           END OF ty_hdr.

    TYPES: BEGIN OF ty_itm,
             process    TYPE zbw_t_process-process,
             up         TYPE icon_d,
             down       TYPE icon_d,
             sequence   TYPE zbw_t_process-sequence,
             ref        TYPE zbw_t_process-ref,
             from_table TYPE zbw_t_process-from_table,
             to_table   TYPE zbw_t_process-to_table,
             active     TYPE icon_d,
             cont       TYPE icon_d,
             color(4)   TYPE c,
           END OF ty_itm.

    TYPES: BEGIN OF ty_target,
             fieldname TYPE zbw_t_param-fieldname,
             value     TYPE zbw_t_param-low,
           END OF ty_target,
           BEGIN OF ty_where,
             fieldname TYPE zbw_t_param-fieldname,
             sign      TYPE zbw_t_param-sign,
             option    TYPE zbw_t_param-opti,
             low       TYPE zbw_t_param-low,
             high      TYPE zbw_t_param-high,
             more      TYPE icon_d,
           END OF ty_where.

    DATA: mt_hdr TYPE STANDARD TABLE OF ty_hdr,
          mt_itm TYPE STANDARD TABLE OF ty_itm.

    METHODS disp_param IMPORTING iv_index TYPE i.
    METHODS change_active IMPORTING iv_index TYPE i.
    METHODS display.

  PRIVATE SECTION.

    DATA: mo_dock       TYPE REF TO cl_gui_docking_container,
          mo_top_cnt    TYPE REF TO cl_gui_container,
          mo_top_salv   TYPE REF TO cl_salv_table,
          mo_bot_cnt    TYPE REF TO cl_gui_container,
          mo_bot_salv   TYPE REF TO cl_salv_table,
          mt_target     TYPE STANDARD TABLE OF ty_target,
          mt_where      TYPE STANDARD TABLE OF ty_where,
          mt_where_disp TYPE STANDARD TABLE OF ty_where.

    METHODS:
      on_link_click FOR EVENT link_click  "Hotspot Handler
                  OF cl_salv_events_table
        IMPORTING row column.

    METHODS on_added_function               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
                OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

DATA: go_param TYPE REF TO lcl_param.

CLASS lcl_param IMPLEMENTATION.

  METHOD display.

    SELECT process,
           '@0I@' AS up,
           '@0H@' AS down,
           sequence,
           ref,
           from_table,
           to_table,
           CASE WHEN active = 'X' THEN '@01@' ELSE '@02@' END AS active,
           '@3I@' AS cont
     FROM zbw_t_process
     INTO TABLE @mt_itm.

    LOOP AT mt_itm ASSIGNING FIELD-SYMBOL(<ls_itm>)
                    GROUP BY <ls_itm>-process INTO DATA(lv_proc).
      APPEND VALUE #( process = lv_proc
                      add     = '@04@' ) TO mt_hdr.
    ENDLOOP.

    DATA(lt_fcat) = VALUE slis_t_fieldcat_alv(
    ( fieldname = 'PROCESS'    tabname = 'MT_HDR' ref_tabname = 'ZBW_T_PROCESS' )
    ( fieldname = 'ADD'        tabname = 'MT_HDR'
      icon      = abap_true    hotspot = abap_true )
    ( fieldname = 'UP'         tabname = 'MT_ITM'
      icon      = abap_true    hotspot = abap_true )
    ( fieldname = 'DOWN'       tabname = 'MT_ITM'
      icon      = abap_true    hotspot = abap_true )
    ( fieldname = 'SEQUENCE'   tabname = 'MT_ITM' ref_tabname = 'ZBW_T_PROCESS' )
    ( fieldname = 'REF'        tabname = 'MT_ITM' ref_tabname = 'ZBW_T_PROCESS' )
    ( fieldname = 'FROM_TABLE' tabname = 'MT_ITM' ref_tabname = 'ZBW_T_PROCESS' )
    ( fieldname = 'TO_TABLE'   tabname = 'MT_ITM' ref_tabname = 'ZBW_T_PROCESS' )
    ( fieldname = 'ACTIVE'     tabname = 'MT_ITM' ref_tabname = 'ZBW_T_PROCESS'
      icon      = abap_true    hotspot = abap_true )
    ( fieldname = 'CONT'       tabname = 'MT_ITM'
      icon      = abap_true    hotspot = abap_true )
    ( fieldname = 'COLOR'      tabname = 'MT_ITM'  )
    ).

    DATA(ls_keyinfo) = VALUE slis_keyinfo_alv(
                        header01 = 'PROCESS' item01 = 'PROCESS' ) .

    DATA(ls_layout) = VALUE slis_layout_alv(
      zebra             = 'X'  "Zebra looks
      colwidth_optimize = 'X'  "Column width optimized
      expand_fieldname  = 'EXPAND' "Expand operation
      info_fieldname    = 'COLOR' "Color
       ).

    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        i_callback_user_command = 'USER_COMM'
        is_layout               = ls_layout
        it_fieldcat             = lt_fcat
        i_tabname_header        = 'MT_HDR'
        i_tabname_item          = 'MT_ITM'
        is_keyinfo              = ls_keyinfo
      TABLES
        t_outtab_header         = mt_hdr
        t_outtab_item           = mt_itm
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Internal Error' TYPE 'I'.
    ENDIF.

  ENDMETHOD.
  METHOD disp_param.
    DATA lo_col_tab TYPE REF TO cl_salv_column_table.

    CLEAR: mt_target, mt_where, mt_where_disp.

    IF mo_dock IS INITIAL.
      mo_dock = NEW cl_gui_docking_container(
                     side  = 8 " right
                     ratio = 40 ).
      DATA(lo_split) = NEW cl_gui_splitter_container(
                            parent  = mo_dock
                            rows    = 2
                            columns = 1 ).
      mo_top_cnt = lo_split->get_container(
        EXPORTING
          row       = 1
          column    = 1 ).

      mo_bot_cnt = lo_split->get_container(
        EXPORTING
          row       = 2
          column    = 1 ).

    ENDIF.

    MODIFY mt_itm FROM VALUE #( cont = '@3I@' )
    TRANSPORTING color cont
     WHERE color IS NOT INITIAL .
    READ TABLE mt_itm ASSIGNING FIELD-SYMBOL(<ls_itm>) INDEX iv_index.
    IF sy-subrc EQ 0.
      <ls_itm>-color = 'C311'.
      <ls_itm>-cont = '@0Z@'.

      SELECT *
        FROM zbw_t_param
        INTO TABLE @DATA(lt_param)
        WHERE ref = @<ls_itm>-ref.

      LOOP AT lt_param ASSIGNING FIELD-SYMBOL(<ls_param>).
        CASE <ls_param>-setkind.
          WHEN '1'. " target
            APPEND INITIAL LINE TO mt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
            <ls_target>-fieldname = <ls_param>-fieldname.
            <ls_target>-value     = <ls_param>-low.
          WHEN '9'. " where
            APPEND INITIAL LINE TO mt_where ASSIGNING FIELD-SYMBOL(<ls_where>).
            <ls_where>-fieldname = <ls_param>-fieldname.
            <ls_where>-sign      = <ls_param>-sign.
            <ls_where>-option    = <ls_param>-opti.
            <ls_where>-low       = <ls_param>-low.
            <ls_where>-high      = <ls_param>-high.
            IF <ls_where>-low(1) = '{'.
              <ls_where>-more    = '@QB@'.
            ELSE.
              <ls_where>-more    = '@1F@'.
            ENDIF.

            READ TABLE mt_where_disp ASSIGNING FIELD-SYMBOL(<ls_w>)
              WITH KEY fieldname = <ls_where>-fieldname.
            IF sy-subrc NE 0.
              APPEND <ls_where> TO mt_where_disp.
            ELSE.
              <ls_w>-more = '@1E@'.
            ENDIF.

        ENDCASE.
      ENDLOOP.

      IF mo_top_salv IS INITIAL.
        TRY.
            cl_salv_table=>factory(
              EXPORTING
                r_container  = mo_top_cnt
              IMPORTING
                r_salv_table = mo_top_salv
              CHANGING
                t_table      = mt_target ).
          CATCH cx_salv_msg .
        ENDTRY.

        mo_top_salv->get_display_settings( )->set_list_header( |TARGET| ).

        DATA(lo_cols) = mo_top_salv->get_columns( ).
        lo_cols->set_optimize( abap_true ).

        TRY.
            lo_col_tab ?= lo_cols->get_column( 'VALUE' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        TRY.
            mo_top_salv->get_functions( )->add_function(
              name     = 'FC_TOP_ADD'
              icon     = CONV #( icon_positive )
              text     = 'Add'
              tooltip  = 'Add new target'
              position = if_salv_c_function_position=>right_of_salv_functions ).
          CATCH cx_salv_wrong_call cx_salv_existing.
        ENDTRY.

        TRY.
            mo_top_salv->get_functions( )->add_function(
              name     = 'FC_TOP_REMOVE'
              icon     = CONV #( icon_negative )
              text     = 'Remove'
              tooltip  = 'Remove target'
              position = if_salv_c_function_position=>right_of_salv_functions ).
          CATCH cx_salv_wrong_call cx_salv_existing.
        ENDTRY.

        SET HANDLER on_added_function FOR mo_top_salv->get_event( ).

        mo_top_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        mo_top_salv->display( ).
      ELSE.
        mo_top_salv->refresh( ).
      ENDIF.

      IF mo_bot_salv IS INITIAL.
        TRY.
            cl_salv_table=>factory(
              EXPORTING
                r_container  = mo_bot_cnt
              IMPORTING
                r_salv_table = mo_bot_salv
              CHANGING
                t_table      = mt_where_disp ).
          CATCH cx_salv_msg .
        ENDTRY.
        mo_bot_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        TRY.
            mo_bot_salv->get_functions( )->add_function(
              name     = 'FC_BOT_ADD'
              icon     = CONV #( icon_positive )
              text     = 'Add'
              tooltip  = 'Add new target'
              position = if_salv_c_function_position=>right_of_salv_functions ).
          CATCH cx_salv_wrong_call cx_salv_existing.
        ENDTRY.

        TRY.
            mo_bot_salv->get_functions( )->add_function(
              name     = 'FC_BOT_REMOVE'
              icon     = CONV #( icon_negative )
              text     = 'Remove'
              tooltip  = 'Remove target'
              position = if_salv_c_function_position=>right_of_salv_functions ).
          CATCH cx_salv_wrong_call cx_salv_existing.
        ENDTRY.

        SET HANDLER on_added_function FOR mo_top_salv->get_event( ).
        mo_bot_salv->get_display_settings( )->set_list_header( |WHERE| ).
        lo_cols = mo_bot_salv->get_columns( ).
        lo_cols->set_optimize( abap_true ).


        TRY.
            lo_col_tab ?= lo_cols->get_column( 'MORE' ).
            lo_col_tab->set_short_text( 'Additional' ).
            lo_col_tab->set_medium_text( 'Additional' ).
            lo_col_tab->set_long_text( 'Additional' ).
          CATCH cx_salv_not_found.
        ENDTRY.
*
*   Set the HotSpot for MORE Column
        TRY.
            CALL METHOD lo_col_tab->set_cell_type
              EXPORTING
                value = if_salv_c_cell_type=>button.
          CATCH cx_salv_data_error .
        ENDTRY.

*   event handler
        SET HANDLER on_link_click FOR mo_bot_salv->get_event( ).
        SET HANDLER on_added_function FOR mo_bot_salv->get_event( ).

        mo_bot_salv->display( ).
      ELSE.
        mo_bot_salv->refresh( ).
      ENDIF.


    ENDIF.

  ENDMETHOD.
  METHOD on_link_click.
    DATA: lt_range TYPE rsds_selopt_t.

    CASE column.
      WHEN 'MORE'.
        READ TABLE mt_where_disp ASSIGNING FIELD-SYMBOL(<ls_wdisp>) INDEX row.
        IF sy-subrc EQ 0.
          IF <ls_wdisp>-more = '@QB@'.
            MESSAGE 'Dynamic entries can not be changed in the selection' TYPE 'I'.
            RETURN.
          ENDIF.

          lt_range = VALUE #( FOR <ls_where> IN mt_where
                              WHERE ( fieldname = <ls_wdisp>-fieldname )
                              ( sign    = <ls_where>-sign
                                option  = <ls_where>-option
                                low     = <ls_where>-low
                                high    = <ls_where>-high ) ).

          CASE <ls_wdisp>-fieldname.
            WHEN 'TABLE'.
              DATA(ls_tfield) = VALUE rstabfield(
                                  tablename = 'DD03V'
                                  fieldname = 'TABNAME' ).
            WHEN OTHERS.
              ls_tfield = VALUE rstabfield(
                                  tablename = 'ZMCMF_GLODAT10'
                                  fieldname = <ls_wdisp>-fieldname ).
          ENDCASE.

* complex selection dialog
          CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
            EXPORTING
              tab_and_field  = ls_tfield
            TABLES
              range          = lt_range
            EXCEPTIONS
              no_range_tab   = 1
              cancelled      = 2
              internal_error = 3
              OTHERS         = 4.

        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD on_added_function.

    CASE e_salv_function.
      WHEN 'FC_BOT_ADD'.
      WHEN 'FC_TOP_ADD'.
      WHEN 'FC_BOT_REMOVE'.
      WHEN 'FC_TOP_REMOVE'.
    ENDCASE.

  ENDMETHOD.
  METHOD change_active.

    READ TABLE mt_itm ASSIGNING FIELD-SYMBOL(<ls_itm>) INDEX iv_index.
    IF sy-subrc EQ 0.
      <ls_itm>-active = COND #( WHEN <ls_itm>-active = '@01@'
                                THEN '@02@' ELSE '@01@' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  go_param = NEW lcl_param( ).
  go_param->display( ).

FORM user_comm USING uv_ucomm    TYPE sy-ucomm
                     us_selfield TYPE slis_selfield.

  CASE uv_ucomm.
    WHEN '&IC1'. " Hotspot

      CASE us_selfield-fieldname.
        WHEN 'ADD'.
        WHEN 'ACTIVE'.
          go_param->change_active( us_selfield-tabindex ).
        WHEN 'CONT'.
          go_param->disp_param( us_selfield-tabindex ).

      ENDCASE.
      us_selfield-refresh = 'X'.
  ENDCASE.

ENDFORM.
