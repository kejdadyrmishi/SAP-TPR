*&---------------------------------------------------------------------*
*& Report ZMF_AUTO_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmf_auto_check.

DATA: gv_email TYPE ad_smtpadr.
SELECT-OPTIONS: s_email FOR gv_email NO INTERVALS.
PARAMETERS: p_attach AS CHECKBOX.

CLASS lcl_auto_check DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_display,
             status     TYPE icon_d,
             priority   TYPE i,
             e_count    TYPE i,
             execute    TYPE icon_d,
             test_name  TYPE zbw_t_check-test_name,
             test_descr TYPE zbw_t_check-test_descr,
             technical  TYPE icon_d,
             test       TYPE icon_d,
             sql_count  TYPE zbw_t_check-sql_count,
             sql_detail TYPE zbw_t_check-sql_detail,
           END OF ty_display.

    TYPES: BEGIN OF ty_email,
             test_type  TYPE zbw_test_type,
             test_name  TYPE zbw_t_check-test_name,
             test_descr TYPE zbw_t_check-test_descr,
             count      TYPE i,
           END OF ty_email.

    TYPES: BEGIN OF ty_disabled,
             test_name TYPE zbw_t_check-test_name,
           END OF ty_disabled.

    DATA: mt_display   TYPE STANDARD TABLE OF ty_display,
          mo_salv      TYPE REF TO cl_salv_table,
          mo_salv_det  TYPE REF TO cl_salv_table,
          mo_dock_cont TYPE REF TO cl_gui_docking_container,
          mo_dialog    TYPE REF TO cl_gui_dialogbox_container,
          mo_abap      TYPE REF TO cl_gui_abapedit,
          mt_disabled  TYPE STANDARD TABLE OF ty_disabled.

    CONSTANTS cv_sep(4) TYPE c VALUE '#SEP'.

    CONSTANTS: BEGIN OF cs_prio,
                 error    TYPE i VALUE 1,
                 warning  TYPE i VALUE 2,
                 info     TYPE i VALUE 3,
                 success  TYPE i VALUE 4,
                 disabled TYPE i VALUE 5,
               END OF cs_prio.

    METHODS execute IMPORTING iv_refresh TYPE abap_bool OPTIONAL.

  PRIVATE SECTION.
    METHODS start_test IMPORTING iv_test TYPE i.
    METHODS display.
    METHODS set_columns.
    METHODS set_functions.
    METHODS display_technical IMPORTING is_disp TYPE ty_display .
    METHODS display_test IMPORTING is_disp TYPE ty_display .
    METHODS mark_tests CHANGING cs_disp TYPE ty_display .
    METHODS:
      on_single_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
    METHODS:
      close FOR EVENT close OF cl_gui_dialogbox_container.
    METHODS send_email.

    METHODS on_added_function               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
                OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_auto_check IMPLEMENTATION.
  METHOD execute.
    DATA lv_test TYPE i.

    IF sy-batch IS INITIAL AND iv_refresh IS INITIAL.
      lv_test = 0. " Online execution - do not start selections
    ELSE.
      lv_test = 1. " Background execution - start selections
    ENDIF.

    start_test( lv_test ).
    display( ).

    IF iv_refresh IS INITIAL .
      IF s_email[] IS NOT INITIAL.
        send_email( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD start_test.

    SELECT test_type AS status,
           0 AS priority,
           0 AS error,
           '@B1@' AS execute,
           test_name,
           test_descr,
           '@BO@' AS technical,
           '@15@' AS test,
           sql_count,
           sql_detail
      FROM zbw_t_check
      INTO TABLE @mt_display.

    IF iv_test = 0.
      RETURN.
    ENDIF.

    LOOP AT mt_display ASSIGNING FIELD-SYMBOL(<ls_disp>).

      IF line_exists( mt_disabled[ test_name = <ls_disp>-test_name ] ).
        <ls_disp>-execute = '@B2@'.
        <ls_disp>-status  = '@00@'.
        <ls_disp>-priority = cs_prio-disabled.
        CONTINUE.
      ENDIF.

      DATA(lv_sql) = <ls_disp>-sql_count.
      REPLACE ALL OCCURRENCES OF cv_sep IN lv_sql WITH space.
      lv_sql = lv_sql && ' ;'.

* Execute Query
      TRY.
          DATA(lo_sql) = NEW cl_sql_statement( ).
          DATA(lo_result) = lo_sql->execute_query( lv_sql ).

          lo_result->set_param( REF #( <ls_disp>-e_count ) ).
          lo_result->next( ).
          ROLLBACK WORK.
          lo_result->close( ).

          IF <ls_disp>-e_count = 0.
            <ls_disp>-status = '@08@'.
          ENDIF.

        CATCH cx_sql_exception INTO DATA(err).
          <ls_disp>-status = '@03@'.
      ENDTRY.

      <ls_disp>-priority = SWITCH i( <ls_disp>-status
                                     WHEN '@0A@'
                                       OR '@03@' THEN cs_prio-error
                                     WHEN '@09@' THEN cs_prio-warning
                                     WHEN '@0S@' THEN cs_prio-info
                                     WHEN '@08@' THEN cs_prio-success
                                     ).
    ENDLOOP.

    SORT mt_display BY priority ASCENDING
                       e_count  DESCENDING.

  ENDMETHOD.

  METHOD display.

    IF mo_salv IS INITIAL.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container  = cl_gui_container=>screen0
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_display ).
        CATCH cx_salv_msg.
      ENDTRY.

      set_columns( ).
      set_functions( ).

      mo_salv->display( ).
      WRITE '.'.
    ELSE.
      mo_salv->refresh( ).
    ENDIF.

  ENDMETHOD.
  METHOD set_columns.
    DATA lo_col TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv->get_columns( ).
    lo_cols->set_optimize( ).

    TRY.
        lo_col ?= lo_cols->get_column( 'STATUS' ).
        lo_col->set_short_text( 'Status' ).
        lo_col->set_medium_text( 'Status' ).
        lo_col->set_long_text( 'Status' ).

        lo_col ?= lo_cols->get_column( 'E_COUNT' ).
        lo_col->set_short_text( 'Count' ).
        lo_col->set_medium_text( 'Count' ).
        lo_col->set_long_text( 'Count' ).

        lo_col ?= lo_cols->get_column( 'EXECUTE' ).
        lo_col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_col->set_short_text( 'Executed' ).
        lo_col->set_medium_text( 'Executed' ).
        lo_col->set_long_text( 'Executed' ).

        lo_col ?= lo_cols->get_column( 'TECHNICAL' ).
        lo_col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_col->set_short_text( 'Technical' ).
        lo_col->set_medium_text( 'Technical' ).
        lo_col->set_long_text( 'Technical' ).

        lo_col ?= lo_cols->get_column( 'TEST' ).
        lo_col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_col->set_short_text( 'Test' ).
        lo_col->set_medium_text( 'Test' ).
        lo_col->set_long_text( 'Test' ).

        lo_col ?= lo_cols->get_column( 'SQL_COUNT' ).
        lo_col->set_technical( ).

        lo_col ?= lo_cols->get_column( 'SQL_DETAIL' ).
        lo_col->set_technical( ).

        lo_col ?= lo_cols->get_column( 'PRIORITY' ).
        lo_col->set_technical( ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

    TRY.
        DATA(lo_sort) = mo_salv->get_sorts( ).
        lo_sort->add_sort( columnname = 'PRIORITY' ).
        lo_sort->add_sort( columnname = 'STATUS' ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.
  METHOD on_single_click.

    READ TABLE mt_display ASSIGNING FIELD-SYMBOL(<ls_disp>) INDEX row.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'TECHNICAL'.
        display_technical( <ls_disp> ).
      WHEN 'TEST'.
        display_test( <ls_disp> ).
      WHEN 'EXECUTE'.
        mark_tests( CHANGING cs_disp = <ls_disp> ).
    ENDCASE.

  ENDMETHOD.
  METHOD mark_tests.

    IF line_exists( mt_disabled[ test_name = cs_disp-test_name ] ).
      DELETE mt_disabled WHERE test_name = cs_disp-test_name.
      cs_disp-execute = '@B1@'.
    ELSE.
      APPEND cs_disp-test_name TO mt_disabled.
      cs_disp-execute = '@B2@'.
    ENDIF.

    mo_salv->refresh( ).

  ENDMETHOD.
  METHOD display_technical.
    IF mo_dialog IS INITIAL.
      CREATE OBJECT mo_dialog
        EXPORTING
          width   = 500
          height  = 200
          top     = 50
          left    = 100
          caption = is_disp-test_descr.

      SET HANDLER close FOR mo_dialog.

      mo_abap = NEW cl_gui_abapedit( mo_dialog ).
      mo_abap->set_readonly_mode( 1 ).
    ENDIF.

    SPLIT is_disp-sql_count AT cv_sep INTO TABLE DATA(lt_count).
    SPLIT is_disp-sql_detail AT cv_sep INTO TABLE DATA(lt_detail).

    INSERT INITIAL LINE INTO lt_count ASSIGNING FIELD-SYMBOL(<ls_c>) INDEX 1.
    <ls_c> = '*--------- ERROR COUNT ---------*'.

    lt_count = VALUE #( BASE lt_count
                        ( |.| ) ( )
                        ( |*--------- ERROR DETAIL ---------*| ) ).
    APPEND LINES OF lt_detail TO lt_count.
    mo_abap->set_text( lt_count ).

  ENDMETHOD.

  METHOD close.
    IF mo_abap IS NOT INITIAL.
      mo_abap->free( ).
      FREE mo_abap.
    ENDIF.

    IF mo_dialog IS NOT INITIAL.
      mo_dialog->free( ).
      FREE mo_dialog.
    ENDIF.

* finally flush
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.
  METHOD display_test.
    DATA lo_col TYPE REF TO cl_salv_column_table.

    DATA(lv_sql) = is_disp-sql_detail.
    REPLACE ALL OCCURRENCES OF cv_sep IN lv_sql WITH space.

    TRY.
        DATA(lo_sys) = cl_db6_sys=>get_sys_ref( system_id = sy-sysid ).
      CATCH cx_db6_sys.
    ENDTRY.

    TRY.
        cl_dba_sql_executor=>get_instance( lo_sys  )->exec_query_dyn(
        EXPORTING
          im_statement    = lv_sql
          im_cursor_size  = 1000
*          im_check        = abap_true
* Per bypassare il controllo autorizativo in FWP togliamo
* i AUTH CHECK e mettiamo FPW non editabile; Transportare da FWD -> FWP
          im_system       = lo_sys
        IMPORTING
          ex_structdescr  = DATA(lo_struct)
          ex_result_ref   = DATA(lo_result) ).
      CATCH cx_sql_exception INTO DATA(lx_sql).
        MESSAGE lx_sql->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_dba_adbc INTO DATA(lx_2).
        MESSAGE lx_2->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_dba_root INTO DATA(lx_3).
        MESSAGE lx_3->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    ROLLBACK WORK.

    ASSIGN lo_result->* TO FIELD-SYMBOL(<lt_result>).
    IF sy-subrc NE 0.
      MESSAGE 'No data to preview' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF mo_dock_cont IS INITIAL.
      CREATE OBJECT mo_dock_cont
        EXPORTING
          parent                      = cl_gui_container=>screen0
          side                        = 8
          extension                   = 500
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.
    mo_dock_cont->set_visible( '1' ).

    IF mo_salv_det IS INITIAL.
      DATA(lv_first) = abap_true.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container  = mo_dock_cont
            IMPORTING
              r_salv_table = mo_salv_det
            CHANGING
              t_table      = <lt_result> ).
        CATCH cx_salv_msg.
      ENDTRY.

      mo_salv_det->get_functions( )->set_all(  ).
    ELSE.
      TRY.
          mo_salv_det->set_data( CHANGING t_table = <lt_result> ).
        CATCH cx_salv_no_new_data_allowed.
      ENDTRY.
    ENDIF.

    DATA(lo_cols) = mo_salv_det->get_columns( ).
    lo_cols->set_optimize( ).

    DATA(lt_comp) = lo_struct->get_components( ).
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).

      TRY.
          lo_col ?= lo_cols->get_column( CONV #( <ls_comp>-name ) ).
          lo_col->set_short_text( CONV #( <ls_comp>-name ) ).
          lo_col->set_medium_text( CONV #( <ls_comp>-name ) ).
          lo_col->set_long_text( CONV #( <ls_comp>-name ) ).
        CATCH cx_salv_not_found.
      ENDTRY.

    ENDLOOP.

    IF lv_first IS NOT INITIAL.
      mo_salv_det->display( ).
    ELSE.
      mo_salv_det->refresh( ).
    ENDIF.

  ENDMETHOD.
  METHOD set_functions.
* Selections
    mo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>cell ).
    SET HANDLER on_single_click FOR mo_salv->get_event( ).

* Functions
    mo_salv->get_functions( )->set_all( abap_true ).

    TRY.
        mo_salv->get_functions( )->add_function(
          name     = 'FC_REFRESH'
          icon     = CONV #( icon_refresh )
          tooltip  = 'Refresh test'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.

    SET HANDLER on_added_function FOR mo_salv->get_event( ).

*   Set ZEBRA pattern
    mo_salv->get_display_settings( )->set_striped_pattern( abap_true ).
  ENDMETHOD.
  METHOD on_added_function.

    CASE e_salv_function.
      WHEN 'FC_REFRESH'.
        IF mo_dock_cont IS NOT INITIAL.
          mo_dock_cont->set_visible( '0' ).
        ENDIF.
        execute( abap_true ).
    ENDCASE.

  ENDMETHOD.
  METHOD send_email.
    DATA: lt_email TYPE STANDARD TABLE OF ty_email.

    lt_email = VALUE #( FOR <ls_disp> IN mt_display
                         WHERE ( e_count > 0 )
                         (  test_type  = SWITCH #( <ls_disp>-priority
                                           WHEN cs_prio-error   THEN '⛔'
                                           WHEN cs_prio-warning THEN '⚠️'
                                           WHEN cs_prio-info    THEN 'ℹ️'
                                           )
                            test_name  = <ls_disp>-test_name
                            test_descr = <ls_disp>-test_descr
                            count      = <ls_disp>-e_count ) ) .
    IF lt_email IS INITIAL.
      MESSAGE 'No errors found - Email will not be sent' TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(ls_email) = VALUE zmf_s_email(
                  subject     = |MF Checks - Errors|
                  header      = VALUE #(
( |<p>Hello, </p>| )
( |<p>You have received this email due to last checks made in | )
( |Monthly Forecast data on { sy-datum DATE = USER },|
&& | at { sy-uzeit TIME = USER }. </p>| ) )
                  table_data  = REF #( lt_email )
                  footer      = VALUE #(
( |<p>For more information on these checks, please open transaction <b>ZMF_AUTO_CHECK</b>. </p>| )
( |<p>Best regards! </p>| )
( |<p>MF Support Team </p>| )
 )
                  attachment  = p_attach
                  rec_email   = VALUE #( FOR <ls_em> IN s_email[]
                                         WHERE ( sign   = 'I'
                                             AND option = 'EQ' )
                                         ( <ls_em>-low ) )
                  ).

    zcl_mf_utilities=>send_email( CHANGING cs_email = ls_email ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_auto_check( )->execute( ).
