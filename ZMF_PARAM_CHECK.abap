*&---------------------------------------------------------------------*
*& Report ZMF_PARAM_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmf_param_check.

CLASS lcl_check DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: ty_text(500) TYPE c,
           tt_text      TYPE STANDARD TABLE OF ty_text WITH EMPTY KEY.

    DATA: mv_ok_code     TYPE syucomm,
          ms_check       TYPE zbw_t_check,
          ms_check_old   TYPE zbw_t_check,
          mo_cust        TYPE REF TO cl_gui_custom_container,
          mo_count_abap  TYPE REF TO cl_gui_abapedit,
          mo_detail_abap TYPE REF TO cl_gui_abapedit,
          mo_salv_det    TYPE REF TO cl_salv_table,
          mo_dock_cont   TYPE REF TO cl_gui_docking_container,
          mv_edit        TYPE abap_bool,
          ms_request     TYPE trwbo_request_header.

    CONSTANTS: cv_count_start(500)  TYPE c VALUE '* Select to get COUNT of errors',
               cv_detail_start(500) TYPE c VALUE '* Select to get DETAIL of errors',
               cv_separator(5)      TYPE c VALUE '#SEP '.

    METHODS status_0100.
    METHODS user_command_0100.
    METHODS constructor.
    METHODS exit.

  PRIVATE SECTION.
    METHODS change_request.
    METHODS save.
    METHODS delete.
    METHODS display.
    METHODS switch_disp_edit.
    METHODS change_type.
    METHODS test_sql IMPORTING  it_sql         TYPE tt_text
                     EXPORTING  ev_sql         TYPE string
                                eo_structdescr TYPE REF TO cl_abap_structdescr
                                eo_result_ref  TYPE REF TO data
                     EXCEPTIONS cx_error.

    METHODS default_count   IMPORTING it_sql TYPE tt_text OPTIONAL.
    METHODS default_detail  IMPORTING it_sql TYPE tt_text OPTIONAL.
    METHODS display_test IMPORTING it_sql TYPE tt_text .
    METHODS close_test.

    METHODS on_added_function               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
                OF cl_salv_events_table
      IMPORTING e_salv_function.

ENDCLASS.

DATA go_check TYPE REF TO lcl_check.

CLASS lcl_check IMPLEMENTATION.
  METHOD constructor.
    mo_cust = NEW cl_gui_custom_container(
                    container_name = 'CUSTOM_BOX' ).

    DATA(lo_split) = NEW cl_gui_splitter_container(
                          parent  = mo_cust
                          rows    = 1
                          columns = 2 ).
    DATA(lo_count_cnt) = lo_split->get_container(
      EXPORTING
        row       = 1
        column    = 1 ).

    DATA(lo_detail_cnt) = lo_split->get_container(
      EXPORTING
        row       = 1
        column    = 2 ).

    mo_count_abap = NEW cl_gui_abapedit( lo_count_cnt ).
    mo_count_abap->set_readonly_mode( 1 ).
    default_count( ).

    mo_detail_abap = NEW cl_gui_abapedit( lo_detail_cnt ).
    mo_detail_abap->set_readonly_mode( 1 ).
    default_detail( ).

  ENDMETHOD.
  METHOD status_0100.
    SET PF-STATUS 'PF_PARAM'.
    SET TITLEBAR 'TB_PARAM'.
  ENDMETHOD.
  METHOD user_command_0100.
    DATA lt_text TYPE tt_text.

    IF sy-sysid = zcl_mf_c=>cs_sysid-fwp.
* Only display is allowed in production system
      CASE mv_ok_code.
        WHEN 'FC_SAVE'
          OR 'FC_EDIT'
          OR 'FC_DELETE'
          OR 'FC_CSQL'
          OR 'FC_DSQL'.

          MESSAGE 'Changes are not possible in Production system'
             TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDCASE.
    ENDIF.

    CASE mv_ok_code.
      WHEN 'FC_SAVE'.   " Save test
        save( ).
      WHEN 'FC_EDIT'.   " Change to edit or display - abap container
        switch_disp_edit( ).
      WHEN 'FC_DISP'.   " Display test properties
        display( ).
      WHEN 'FC_DELETE'. " Delete test
        delete( ).
      WHEN 'FC_CSQL'.   " Count sql
        mo_count_abap->get_text( IMPORTING table = lt_text ).
        display_test( lt_text ).
      WHEN 'FC_DSQL'.   " Detail sql
        mo_detail_abap->get_text( IMPORTING table = lt_text ).
        display_test( lt_text ).
      WHEN 'FC_TYPE'.
        change_type( ).
    ENDCASE.

  ENDMETHOD.
  METHOD change_type.

    TYPES: BEGIN OF ty_val,
             field(280),
           END OF ty_val.
    DATA: lt_val       TYPE STANDARD TABLE OF ty_val,
          lv_choice(4) TYPE c.

    lt_val = VALUE #( ( |@0A@ - Error Message| )
                      ( |@09@ - Warning Message| )
                      ( |@0S@ - Information Message| )
                       ).
    CALL FUNCTION 'POPUP_WITH_TABLE'
      EXPORTING
        endpos_col   = 40
        endpos_row   = 4
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Type of Message'
      IMPORTING
        choice       = lv_choice
      TABLES
        valuetab     = lt_val
      EXCEPTIONS
        break_off    = 1.
    IF sy-subrc EQ 0.
      ms_check-test_type = lv_choice.
    ENDIF.

  ENDMETHOD.
  METHOD display_test.
    DATA lo_col TYPE REF TO cl_salv_column_table.

    test_sql(
      EXPORTING
        it_sql          = it_sql
      IMPORTING
        eo_result_ref   = DATA(lo_result)
        eo_structdescr  = DATA(lo_struct)
      EXCEPTIONS
        cx_error        = 1 ).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    ASSIGN lo_result->* TO FIELD-SYMBOL(<lt_result>).

    IF mo_dock_cont IS INITIAL.
      CREATE OBJECT mo_dock_cont
        EXPORTING
          parent                      = cl_gui_container=>screen0
          side                        = 4
          ratio                       = 30
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

      TRY.
          mo_salv_det->get_functions( )->add_function(
            name     = 'FC_CLOSE'
            icon     = CONV #( icon_close )
            text     = 'Close'
            tooltip  = 'Close Container'
            position = if_salv_c_function_position=>right_of_salv_functions ).
        CATCH cx_salv_wrong_call cx_salv_existing.
      ENDTRY.

      SET HANDLER on_added_function FOR mo_salv_det->get_event( ).

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
  METHOD save.
    DATA lt_text TYPE tt_text.

    IF ms_check-test_descr IS INITIAL.
      MESSAGE 'Test description can not be empty' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    DO.

      CASE sy-index.
        WHEN 1.
          ASSIGN ms_check-sql_count TO FIELD-SYMBOL(<la_sql>).
          ASSIGN mo_count_abap TO FIELD-SYMBOL(<lo_cnt>).
        WHEN 2.
          ASSIGN ms_check-sql_detail TO <la_sql>.
          ASSIGN mo_detail_abap TO <lo_cnt>.

        WHEN OTHERS.
          EXIT.
      ENDCASE.

      CLEAR <la_sql>.
      <lo_cnt>->get_text( IMPORTING table = lt_text ).

      test_sql(
        EXPORTING
          it_sql   = lt_text
        IMPORTING
          ev_sql   = <la_sql>
        EXCEPTIONS
          cx_error = 1 ).
      IF sy-subrc NE 0.
        DATA(lv_error) = abap_true.
        EXIT.
      ENDIF.

    ENDDO.

    IF lv_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    change_request( ).

    ms_check-mandt = sy-mandt.
    ms_check_old = ms_check.
    IF ms_check-test_type IS INITIAL.
      ms_check-test_type = '@0A@'. " Error
    ENDIF.
    MODIFY zbw_t_check FROM ms_check.
    COMMIT WORK.
    MESSAGE 'Check updated' TYPE 'S'.

  ENDMETHOD.
  METHOD display.
    DATA: lt_text TYPE tt_text.

    IF ms_check_old-test_name = ms_check-test_name.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zbw_t_check
      INTO @DATA(ls_check)
      WHERE test_name = @ms_check-test_name.
    IF sy-subrc EQ 0.
      ms_check = ms_check_old = ls_check.
    ENDIF.

    SPLIT ms_check-sql_count AT cv_separator INTO TABLE lt_text.
    DELETE lt_text WHERE table_line IS INITIAL.
    default_count( lt_text ).

    SPLIT ms_check-sql_detail AT cv_separator INTO TABLE lt_text.
    DELETE lt_text WHERE table_line IS INITIAL.
    default_detail( lt_text ).

  ENDMETHOD.
  METHOD exit.
    CASE mv_ok_code.
      WHEN 'FC_EXIT'.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.
  METHOD switch_disp_edit.

    IF mv_edit IS INITIAL.
      mv_edit = abap_true.
      mo_count_abap->set_readonly_mode( 0 ).
      mo_detail_abap->set_readonly_mode( 0 ).
      mo_count_abap->protect_lines( VALUE #( ( |1| ) ) ).
      mo_detail_abap->protect_lines( VALUE #( ( |1| ) ) ).
    ELSE.
      CLEAR mv_edit.
      mo_count_abap->set_readonly_mode( 1 ).
      mo_detail_abap->set_readonly_mode( 1 ).
    ENDIF.
  ENDMETHOD.
  METHOD delete.
    DATA lv_answ TYPE c.

    IF ms_check-test_name IS INITIAL.
      MESSAGE 'Fill test name' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirm deletion'
        text_question         = 'Would you like to delete test name?'
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_answ
      EXCEPTIONS
        text_not_found        = 1.
    IF lv_answ = '1'.
      DELETE FROM zbw_t_check WHERE test_name = ms_check-test_name.
      COMMIT WORK.
      MESSAGE 'Test deleted sucessfully' TYPE 'S'.

      CLEAR ms_check.
      default_count( ).
      default_detail( ).
    ENDIF.

  ENDMETHOD.
  METHOD test_sql.

    DATA(lt_text) = it_sql.
    DELETE lt_text INDEX 1.
    DELETE lt_text WHERE table_line IS INITIAL.

    LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<ls_text>).
      IF <ls_text>(1) = '*'.
        CONTINUE.
      ENDIF.

      CONCATENATE ev_sql <ls_text>
             INTO ev_sql SEPARATED BY cv_separator.
    ENDLOOP.

    IF ev_sql IS INITIAL.
      MESSAGE 'Sql code can not be empty!' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE cx_error.
    ENDIF.
    DATA(lv_sql) = to_upper( ev_sql ).

    IF lv_sql CS 'DELETE'
      OR lv_sql CS 'UPDATE'
      OR lv_sql CS 'INSERT'
      OR lv_sql CS 'MODIFY'
      OR lv_sql CS 'DROP'
      OR lv_sql CS 'TRUNCATE'
      OR lv_sql CS 'COMMIT'
      OR lv_sql CS 'CALL'
      OR lv_sql CS ';'.
      MESSAGE 'YOU CAN ONLY SELECT!!!!' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE cx_error.
    ENDIF.

    IF lv_sql NS 'SELECT'.
      MESSAGE 'SELECT statement is missing' TYPE 'S' DISPLAY LIKE 'E'.
      RAISE cx_error.
    ENDIF.

    REPLACE ALL OCCURRENCES OF cv_separator IN lv_sql WITH '  '.

    TRY.
        DATA(lo_sys) = cl_db6_sys=>get_sys_ref( system_id = sy-sysid ).
      CATCH cx_db6_sys.
    ENDTRY.

    TRY.
        cl_dba_sql_executor=>get_instance( lo_sys  )->exec_query_dyn(
        EXPORTING
          im_statement    = lv_sql
          im_cursor_size  = 1
          im_check        = abap_true
          im_system       = lo_sys
        IMPORTING
          ex_structdescr  = eo_structdescr
          ex_result_ref   = eo_result_ref ).

      CATCH cx_sql_exception INTO DATA(lx_1).
        MESSAGE lx_1->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RAISE cx_error.
      CATCH cx_dba_adbc INTO DATA(lx_2).
        MESSAGE lx_2->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RAISE cx_error.
      CATCH cx_dba_root INTO DATA(lx_3).
        MESSAGE lx_3->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RAISE cx_error.
    ENDTRY.
    ROLLBACK WORK.

  ENDMETHOD.
  METHOD default_count.

    DATA(lt_text) = VALUE tt_text( ( cv_count_start ) ( ) ).
    APPEND LINES OF it_sql TO lt_text.
    mo_count_abap->set_text( lt_text ).

    IF mv_edit IS NOT INITIAL.
      mo_count_abap->protect_lines( VALUE #( ( |1| ) ) ).
    ENDIF.

  ENDMETHOD.
  METHOD default_detail.

    DATA(lt_text) = VALUE tt_text( ( cv_detail_start ) ( ) ).
    APPEND LINES OF it_sql TO lt_text.
    mo_detail_abap->set_text( lt_text ).

    IF mv_edit IS NOT INITIAL.
      mo_detail_abap->protect_lines( VALUE #( ( |1| ) ) ).
    ENDIF.

  ENDMETHOD.
  METHOD close_test.
    IF mo_dock_cont IS NOT INITIAL.
      mo_dock_cont->set_visible( '0' ).
    ENDIF.
  ENDMETHOD.

  METHOD on_added_function.
    CLEAR mv_ok_code.
    CASE e_salv_function.
      WHEN 'FC_CLOSE'.
        close_test( ).
    ENDCASE.

  ENDMETHOD.
  METHOD change_request.

    DATA(lt_e071) = VALUE tr_objects( (
                            trkorr      = ms_request-trkorr
                            pgmid       = 'R3TR'
                            object      = 'TABU'
                            obj_name    = 'ZBW_T_CHECK'
                            objfunc     = 'K'
                            ) ).


    DATA(lt_e071k) = VALUE tr_keys( (
                            trkorr      = ms_request-trkorr
                            pgmid       = 'R3TR'
                            object      = 'TABU'
                            objname     = 'ZBW_T_CHECK'
                            mastertype  = 'TABU'
                            mastername  = 'ZBW_T_CHECK'
                            tabkey      = '*'
                            ) ).


    CALL FUNCTION 'TR_REQUEST_CHOICE'
      EXPORTING
        iv_request           = ms_request-trkorr
        it_e071              = lt_e071
        it_e071k             = lt_e071k
      IMPORTING
        es_request           = ms_request
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7.
    IF sy-subrc NE 0 AND sy-subrc NE 6.
      MESSAGE 'Entries could not be saved in CR' TYPE 'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF go_check IS INITIAL.
    go_check = NEW lcl_check( ).
  ENDIF.

  go_check->status_0100( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_check->user_command_0100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE exit INPUT.
  go_check->exit( ).
ENDMODULE.
