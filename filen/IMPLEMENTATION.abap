*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_timesheet IMPLEMENTATION.
  METHOD get_data.
    mt_employee = VALUE #( ( id = 1234 name = 'Filan'   surname = 'Fisteku' )
                           ( id = 32   name = 'Filane' surname = 'Fisteke'  )
                           ( id = 54   name = 'Mondi'  surname = 'Nafies'   )
                           ( id = 632  name = 'Limi'   surname = 'Feruzes'  ) ).

    mt_timesheet = VALUE #( project = 'Arts'
                            ( employeeid = 1234 date = '20220202' hours = 8 )
                            ( employeeid = 1234 date = '20220115' hours = 8 )
                            ( employeeid = 1234 date = '20220103' hours = 8 )
                            ( employeeid = 1234 date = '20220101' hours = 5 )
                            ( employeeid = 32   date = '20220101' hours = 5 )
                            ( employeeid = 54   date = '20220101' hours = 5 )
                            ( employeeid = 632  date = '20220101' hours = 5 )
                            ( employeeid = 1234 date = '20220102' hours = '5.25' overtime = abap_true  ) ).

    DATA(lv_record) = |{ p_year }{ p_month }|.

    IF NOT line_exists( mt_timesheet[ date(6) = lv_record ] ).
      MESSAGE 'There are no record' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    gv_mode = SWITCH #( p_hide
                        WHEN abap_true
                        THEN 'S'
                        ELSE 'H' ).
  ENDMETHOD.

  METHOD fill_table.
    DATA: lv_date  TYPE string,
          lv_color TYPE i. " Color value for cell coloring

    mt_fcat = fcat_reuse( ).

    " Loop through the timesheet data grouped by employee ID, project, and overtime
    LOOP AT mt_timesheet ASSIGNING FIELD-SYMBOL(<fs_timesheet>)
         GROUP BY ( employeeid = <fs_timesheet>-employeeid
                    project    = <fs_timesheet>-project
                    overtime   = <fs_timesheet>-overtime )
         ASSIGNING FIELD-SYMBOL(<fg_gr>).

      " Read employee data = to the current employee ID
      READ TABLE mt_employee ASSIGNING FIELD-SYMBOL(<fs_employee>) WITH KEY id = <fg_gr>-employeeid.

      " Append a new line to the display table and assign to a field symbol
      APPEND INITIAL LINE TO mt_display ASSIGNING FIELD-SYMBOL(<fs_display>).
      <fs_display>-fullname = |{ <fs_employee>-name } { <fs_employee>-surname }|.  "  full name
      <fs_display>-project  = <fg_gr>-project.                                    "  project
      <fs_display>-overtime = <fg_gr>-overtime.                                  " Assign overtime

      " Loop through grouped data by date and assign hours to corresponding components
      LOOP AT GROUP <fg_gr> ASSIGNING FIELD-SYMBOL(<fs_date>).

        lv_date = |DAY_{ <fs_date>-date+6(2) }|.  "  component name for hours

        " Assign hours
        ASSIGN COMPONENT lv_date OF STRUCTURE <fs_display> TO FIELD-SYMBOL(<lv_hours>).
        <lv_hours> = <fs_date>-hours.

        IF <fg_gr>-overtime IS NOT INITIAL.
          APPEND VALUE #( color-col = '6' ) TO <fs_display>-cellcolor.  " Append color information to cellcolor field
        ENDIF.

        IF <fg_gr>-overtime IS INITIAL.

          READ TABLE mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = lv_date.
          IF sy-subrc = 0.

            DATA(lv_day) = <ls_fcat>-seltext_m(3).
            "  the color
            IF lv_day = 'Sun' OR lv_day = 'Sat'.
              lv_color = 6.  " red color for weekend days
            ELSE.
              lv_color = 1.
            ENDIF.

            " Apply color based on the chosen output format
            CASE abap_true.

              WHEN p_reuse OR p_grid.  " For ALV reuse or grid output
                APPEND VALUE #( fieldname = lv_date
                                color-col = lv_color ) TO <fs_display>-cellcolor.  " Append color information to cellcolor field
                IF lv_date = 'HOURS'.
                  APPEND VALUE #( fieldname = 'HOURS'
                                  color-col = 1 ) TO <fs_display>-cellcolor.  " Append color for hours field
                ENDIF.

              WHEN p_salv.  " For SALV output
                APPEND VALUE #( fname     = lv_date
                                color-col = lv_color ) TO <fs_display>-color.  " Append color information to color field
                IF lv_date = 'HOURS'.
                  APPEND VALUE #( fname     = 'HOURS'
                                  color-col = 1 ) TO <fs_display>-color.  " Append color for hours field
                ENDIF.
            ENDCASE.
          ENDIF.
        ENDIF.

        " Add hours to the total hours for the display line
        <fs_display>-hours = <fs_display>-hours + <lv_hours>.

      ENDLOOP.

      " Calculate and assign the total hours per day to the display line
      <fs_display>-day = <fs_display>-hours / 8.

    ENDLOOP.
  ENDMETHOD.

  " alv reuse
  METHOD fcat_reuse.
    DATA :lv_month      TYPE t009b-bumon,
          lv_year       TYPE t009b-bdatj,
          lv_month_days TYPE t009b-butag,
          lv_fulldate   TYPE d,
          lv_text       TYPE dtresr-weekday.

    lv_month = p_month.
    lv_year = p_year.

    rt_fcat = VALUE #( tabname = 'mt_display'
                       ( fieldname = 'SEL'        no_out = 'X'  )
                       ( fieldname = 'FULLNAME'   seltext_m = 'Employee' key = abap_true )
                       ( fieldname = 'PROJECT'    seltext_m = 'Project'  key = abap_true )
                       ( fieldname = 'HOURS'      seltext_m = 'Hours'    key = abap_true )
                       ( fieldname = 'DAY'        seltext_m = 'Days'     key = abap_true ) ).

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = lv_month
        par_year  = lv_year
      IMPORTING
        par_days  = lv_month_days.

    DO lv_month_days TIMES.
      APPEND INITIAL LINE TO rt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

      DATA(lv_day) = EXACT numc2( sy-index ).
      <ls_fcat>-fieldname = |DAY_{ lv_day }|.

      lv_fulldate = |{ p_year }{ p_month }{ lv_day }|.

      CALL FUNCTION 'DATE_TO_DAY'
        EXPORTING
          date    = lv_fulldate
        IMPORTING
          weekday = lv_text.

      IF p_hide = abap_true AND lv_text CS 'Sat' OR lv_text CS 'Sun'.
        <ls_fcat>-no_out = abap_true.
      ENDIF.

      <ls_fcat>-seltext_m = |{ lv_text(3) } { lv_day }|.
      <ls_fcat>-no_zero   = abap_true.

    ENDDO.
  ENDMETHOD.

  METHOD user_command_reusealv.
    DATA :lv_total_days  TYPE p LENGTH 3 DECIMALS 2,
          lv_total_hours TYPE p LENGTH 3 DECIMALS 2,
          lv_count       TYPE i.

    CASE iv_ucomm.

      WHEN 'FC_SUM'.

        LOOP AT mt_display ASSIGNING FIELD-SYMBOL(<fs_display>) WHERE sel = abap_true.
          lv_total_days = lv_total_days + <fs_display>-day.
          lv_total_hours = lv_total_hours + <fs_display>-hours.
          lv_count = lv_count + 1.
        ENDLOOP.

        IF lv_count > 1.
          MESSAGE |Sum of days is : { lv_total_days }' Sum of hours is : '{ lv_total_hours }| TYPE 'I'.
        ELSE.
          MESSAGE 'Select at least 2 rows' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.

      WHEN 'grid_show_hide'.
        gv_mode = COND #( WHEN gv_mode = 'H' THEN 'S' ELSE 'H' ). "  hide/show mode
        DATA(lv_no_out) = COND #( WHEN gv_mode = 'S' THEN abap_true ELSE abap_false ).

        update_reuse( EXPORTING iv_out = lv_no_out ).  " Call method to update ALV display
        cs_selfield-refresh = abap_true.  " Refresh selection field
    ENDCASE.

  ENDMETHOD.

  METHOD display_reuse.
    DATA(ls_layout) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                             box_fieldname     = 'SEL'
                                             box_tabname       = 'mt_display'
                                             coltab_fieldname  = 'CELLCOLOR' ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND_REUSEALV'
        is_layout                = ls_layout
        it_fieldcat              = mt_fcat
      TABLES
        t_outtab                 = mt_display
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.

  METHOD update_reuse.
    DATA ls_layout TYPE slis_layout_alv.

    CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
      IMPORTING
        et_fieldcat   = mt_fcat
        es_layout     = ls_layout
      EXCEPTIONS
        no_infos      = 1
        program_error = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      <ls_fcat>-col_pos = sy-tabix.
    ENDLOOP.
    MODIFY mt_fcat FROM VALUE #( no_out = iv_out ) TRANSPORTING no_out WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat'.
    ls_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
      EXPORTING
        it_fieldcat = mt_fcat
        is_layout   = ls_layout.
  ENDMETHOD.

  METHOD color_reuse.
    MODIFY mt_fcat FROM VALUE #( no_out = p_hide ) TRANSPORTING no_out WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat'.

    LOOP AT mt_display ASSIGNING FIELD-SYMBOL(<fs_display>).
      IF <fs_display>-overtime = abap_true.
        APPEND VALUE #( color = VALUE #( col = 6 ) ) TO <fs_display>-cellcolor.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  " alv grid
  METHOD container_grid.
    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).
  ENDMETHOD.

  METHOD get_fcat_grid.
    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

      " Fill the field catalog entry for the ALV grid
      ms_gridfcat-fieldname = <ls_fcat>-fieldname.
      ms_gridfcat-tabname   = 'MT_DISPLAY'.
      ms_gridfcat-scrtext_m = <ls_fcat>-seltext_m.      " Set key and output settings based on fieldname
      ms_gridfcat-no_zero   = 'X'.

      CASE <ls_fcat>-fieldname.
        WHEN 'OVERTIME' OR 'SEL' OR 'EMPLOYEEID'.
          ms_gridfcat-no_out = abap_true.
        WHEN 'FULLNAME' OR 'PROJECT' OR 'HOURS' OR 'DAY'.
          ms_gridfcat-key = abap_true.
      ENDCASE.

      " Append the filled field catalog entry to the output table
      APPEND ms_gridfcat TO mt_gridfcat.
      CLEAR ms_gridfcat.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_layout.
    " Set the layout options for the ALV grid
    rs_layout = VALUE #( cwidth_opt = abap_true
                         box_fname  = 'SEL'
                         sel_mode   = 'D'
                         ctab_fname = 'CELLCOLOR' ).
  ENDMETHOD.

  METHOD set_first_display. " display grid
    " Get the data, container, and field catalog for the ALV grid
    container_grid( ).
    get_fcat_grid( ).

    " Get the layout
    DATA(ls_layout) = get_layout( ).

    " Hide the columns with 'Sun' or 'Sat' in the header text
    LOOP AT mt_gridfcat ASSIGNING FIELD-SYMBOL(<ls_gridfcat>).
      IF <ls_gridfcat>-scrtext_m CS 'Sun' OR <ls_gridfcat>-scrtext_m CS 'Sat'.
        <ls_gridfcat>-no_out = p_hide.
      ENDIF.
    ENDLOOP.

    " Display the ALV grid with the given parameters
    TRY.
        mo_alv_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout   " Layout
                                                  CHANGING  it_outtab       = mt_display  " Output Table
                                                            it_fieldcatalog = mt_gridfcat ). " Field Catalog
        CALL SCREEN 100.  " screen for alv grid
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD grid_sum.
    DATA: lv_total_hours TYPE p LENGTH 3 DECIMALS 2,
          lv_total_days  TYPE p LENGTH 3 DECIMALS 2,
          lv_count       TYPE i.

    " Get the selected rows from the ALV grid
    mo_alv_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_indx_rows) ).

    " Loop at the selected rows and calculate the total hours and days
    LOOP AT lt_indx_rows INTO DATA(lv_row).
      DATA(ls_tab) = mt_display[ lv_row ].
      lv_total_hours = lv_total_hours + ls_tab-hours.
      lv_total_days = lv_total_days + ls_tab-day.
      lv_count = lv_count + 1.
    ENDLOOP.

    " Display a message with the sum of days and hours
    IF lv_count > 1.
      MESSAGE |Sum of days is : { lv_total_days } 'Sum of hours is : '{ lv_total_hours }| TYPE 'I'.
    ELSE.
      MESSAGE 'Select at least 2 rows' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD grid_show_hide.
    " Get the current layout of the ALV grid
    mo_alv_grid->get_frontend_layout( IMPORTING es_layout = DATA(ls_layout) ).

    DATA(lv_show) = EXACT abap_bool( SWITCH  #( gv_mode WHEN 'S' THEN 'X'
                                                        WHEN 'H' THEN ' ' ) ).


    " Hide or show the columns with 'Sun' or 'Sat' in the header text
    LOOP AT mt_gridfcat ASSIGNING FIELD-SYMBOL(<ls_gridfcat>).
      IF <ls_gridfcat>-scrtext_m CS 'Sun' OR <ls_gridfcat>-scrtext_m CS 'Sat'.
        <ls_gridfcat>-no_out = lv_show.
      ENDIF.
    ENDLOOP.

*    " Set the layout option to adjust the column width
    ls_layout-cwidth_opt = abap_true.

    " Update the field catalog and layout of the ALV grid
    mo_alv_grid->set_frontend_fieldcatalog( it_fieldcatalog = mt_gridfcat ).
    mo_alv_grid->set_frontend_layout( is_layout = ls_layout ).

    " Refresh the ALV grid display
    mo_alv_grid->refresh_table_display( EXCEPTIONS finished = 1
                                                   OTHERS   = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  " SALV
  METHOD salv_table.


    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_salv
                                CHANGING  t_table      = mt_display ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    mo_salv->get_functions( )->set_all( abap_true ).
    mo_salv->get_columns( )->set_optimize( abap_true ).

    mo_salv_selection = mo_salv->get_selections( ).
    mo_salv_selection->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    salv_columns( EXPORTING io_salv = mo_salv ).
    salv_status( ).

    DATA(lo_events) = mo_salv->get_event( ).
    SET HANDLER user_command_salv FOR lo_events.

    mo_salv->display( ).
  ENDMETHOD.

  METHOD salv_columns.
    DATA : lv_month       TYPE t009b-bumon,
           lv_year        TYPE t009b-bdatj,
           lv_number_days TYPE t009b-butag,
           lv_fieldname   TYPE slis_fieldcat_alv-fieldname,
           lo_column      TYPE REF TO cl_salv_column_table,
           lv_fulldate    TYPE d,
           lv_text        TYPE dtresr-weekday,
           lv_date        TYPE scrtext_l.

    DATA(lo_columns) = mo_salv->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( 'FULLNAME' ).
        lo_column->set_medium_text( 'Employee' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'HOURS' ).
        lo_column->set_medium_text( 'Hours' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'DAY' ).
        lo_column->set_key( ).
        lo_column->set_medium_text( 'Days' ).

        lo_column ?= lo_columns->get_column( 'PROJECT' ).
        lo_column->set_medium_text( 'Project' ).
        lo_column->set_key( ).
      CATCH cx_salv_not_found.
    ENDTRY.


    lv_month = p_month.
    lv_year = p_year.

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = lv_month
        par_year  = lv_year
      IMPORTING
        par_days  = lv_number_days.

    DO lv_number_days TIMES.

      DATA(lv_day) = EXACT numc2( sy-index ).
      lv_fieldname = |DAY_{ lv_day }|.

      lv_fulldate = |{ p_year }{ p_month }{ lv_day }|.

      CALL FUNCTION 'DATE_TO_DAY'
        EXPORTING
          date    = lv_fulldate
        IMPORTING
          weekday = lv_text.

      IF p_hide = abap_true AND lv_text CS 'Sat' OR lv_text CS 'Sun'.
        ls_fcat-no_out = abap_true.
      ENDIF.

      DATA(lv_name) = EXACT scrtext_l( |{ lv_text(3) } { lv_day }| ).

      TRY.
          lo_column ?= lo_columns->get_column( lv_fieldname ).
          lo_column->set_long_text( lv_name ).
          lo_column->set_zero( abap_false ). " to hide zeros
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column ?= lo_columns->get_column( 'EMPLOYEEID' ).
          lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

          lo_column ?= lo_columns->get_column( 'OVERTIME' ).
          lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

          lo_column ?= lo_columns->get_column( 'SEL' ).
          lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDDO.

    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE seltext_m CS 'Sun' OR seltext_m CS 'Sat'.
      TRY.
          lo_column ?= lo_columns->get_column( <ls_fcat>-fieldname ).

          IF p_hide = abap_true .
            lo_column->set_visible( abap_false ).
          ELSE.
            lo_column->set_visible( ).
          ENDIF.
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDLOOP.

    TRY.
        mo_salv->get_columns( )->set_color_column('COLOR').
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT mt_display ASSIGNING FIELD-SYMBOL(<ls_display>).
      IF <ls_display>-overtime = abap_true.
        APPEND VALUE #( color = VALUE #( col = 6  ) ) TO <ls_display>-color.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD user_command_salv.
    DATA: lv_total_days  TYPE p LENGTH 3 DECIMALS 2,
          lv_total_hours TYPE p LENGTH 3 DECIMALS 2,
          lv_count       TYPE i.

    mo_salv_selection->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    DATA(lt_sel) = mo_salv_selection->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'grid_show_hide'.

        gv_mode = COND #( WHEN gv_mode = 'H' THEN 'S' ELSE 'H' ). "  hide/show mode

        CASE gv_mode.
          WHEN 'S'. " SHOW mode
            CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
              EXPORTING
                name       = icon_display
                text       = 'Show'
                info       = 'Show the Weekends'
                add_stdinf = 'X'
              IMPORTING
                result     = gv_btn_dyn. " result
          WHEN 'H'. " HIDE mode
            CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
              EXPORTING
                name       = icon_read_file
                text       = 'Hide'
                info       = 'Hide the Weekends'
                add_stdinf = 'X'
              IMPORTING
                result     = gv_btn_dyn. " result
        ENDCASE.

        DATA(lv_col_display) = COND #( WHEN gv_mode = 'H' THEN abap_true ELSE abap_false ).  " Set p_hide flag based on mode

        LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
             WHERE seltext_m CS 'Sat' OR seltext_m CS 'Sun'.

          TRY.
              DATA(lo_column) = mo_salv->get_columns( )->get_column( <ls_fcat>-fieldname ).
              lo_column->set_visible( lv_col_display ).

            CATCH cx_salv_not_found.

          ENDTRY.
          update_salv( ).
        ENDLOOP.

        update_salv( ).
        mo_salv->get_columns( )->set_optimize( abap_true ).

      WHEN 'FC_SUM'.
        it_rows = mo_salv_selection->get_selected_rows( ).
        LOOP AT it_rows INTO DATA(ls_row).
          READ TABLE mt_display ASSIGNING FIELD-SYMBOL(<ls_display>) INDEX ls_row.
          lv_total_days = lv_total_days + <ls_display>-day.
          lv_total_hours = lv_total_hours + <ls_display>-hours.
          lv_count = 1 + lv_count.
        ENDLOOP.
        IF lv_count > 1.
          MESSAGE |Sum of days is : { lv_total_days }' Sum of hours is : '{ lv_total_hours }| TYPE 'I'.
        ELSE.
          MESSAGE 'Select at least 2 rows' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD salv_status.
    CASE gv_mode.
      WHEN 'S'. " SHOW mode
        CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
          EXPORTING
            name       = icon_display
            text       = 'Show'
            info       = 'Show the Weekends'
            add_stdinf = 'X'
          IMPORTING
            result     = gv_btn_dyn. " result
      WHEN 'H'. " HIDE mode
        CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
          EXPORTING
            name       = icon_read_file
            text       = 'Hide'
            info       = 'Hide the Weekends'
            add_stdinf = 'X'
          IMPORTING
            result     = gv_btn_dyn. " result
    ENDCASE.
    TRY.
        mo_salv->set_screen_status( report        = sy-repid
                                    pfstatus      = 'SALV' ).

      CATCH cx_salv_object_not_found INTO DATA(lx_msg1).
        MESSAGE lx_msg1->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        mo_salv->get_columns( )->set_optimize( abap_true ).
    ENDTRY.
  ENDMETHOD.
  METHOD update_salv.
    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(count) = sy-tabix.

      DATA(seltext) = <ls_fcat>-seltext_m.
      TRY.
          r_column = mo_salv->get_columns( )->get_column( <ls_fcat>-fieldname ).
        CATCH cx_salv_not_found.
      ENDTRY.

      mo_salv->get_columns( )->set_column_position( columnname = <ls_fcat>-fieldname
                                                        position   = count ).

    ENDLOOP.
    mo_salv->refresh( s_stable = VALUE #( col = abap_true )   ).  " ALV Control: Refresh Stability
  ENDMETHOD.

  " execute all alv
  METHOD execute.
    get_data( ).
    fill_table( ).

    CASE abap_true.
      WHEN p_reuse.
        color_reuse( ).
        display_reuse( ).

      WHEN p_grid.
        set_first_display( ).

      WHEN p_salv.
        salv_table( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
