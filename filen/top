*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_TOP
*&---------------------------------------------------------------------*
REPORT zkd_final.

PARAMETERS: p_year  TYPE n LENGTH 4 OBLIGATORY DEFAULT 2022,
            p_month TYPE monam  AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY DEFAULT '01',
            p_hide  AS CHECKBOX,
            p_salv  RADIOBUTTON GROUP alv DEFAULT 'X',
            p_grid  RADIOBUTTON GROUP alv,
            p_reuse RADIOBUTTON GROUP alv.

DATA: gv_mode    TYPE c LENGTH 1 VALUE 'S', " global variable to store the mode: S for SHOW
      gv_btn_dyn TYPE smp_dyntxt.           " global variable for the pushbutton text and icon

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.
  PERFORM set_values_parameter.

CLASS lcl_timesheet DEFINITION.

  PUBLIC SECTION.
    METHODS execute.

    METHODS user_command_reusealv IMPORTING iv_ucomm    TYPE sy-ucomm
                                  CHANGING  cs_selfield TYPE slis_selfield.

    METHODS grid_show_hide.
    METHODS grid_sum.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_employee,
             id      TYPE i,
             name    TYPE string,
             surname TYPE string,
           END OF ty_employee,
           tt_employee TYPE TABLE OF ty_employee.

    TYPES : BEGIN OF ty_timesheet,
              employeeid TYPE i,
              project    TYPE string,
              overtime   TYPE c LENGTH 1,
              date       TYPE d,
              hours      TYPE p LENGTH 3 DECIMALS 2,
            END OF ty_timesheet,
            tt_timesheet TYPE TABLE OF ty_timesheet.

    TYPES : BEGIN OF ty_display,
              sel        TYPE char1,
              employeeid TYPE i,
              fullname   TYPE c LENGTH 50,
              project    TYPE c LENGTH 50,
              overtime   TYPE c LENGTH 1,
              hours      TYPE p LENGTH 3  DECIMALS 2,
              day        TYPE p LENGTH 3  DECIMALS 2,
              day_01     TYPE p LENGTH 2  DECIMALS 2,
              day_02     TYPE p LENGTH 2  DECIMALS 2,
              day_03     TYPE p LENGTH 2  DECIMALS 2,
              day_04     TYPE p LENGTH 2  DECIMALS 2,
              day_05     TYPE p LENGTH 2  DECIMALS 2,
              day_06     TYPE p LENGTH 2  DECIMALS 2,
              day_07     TYPE p LENGTH 2  DECIMALS 2,
              day_08     TYPE p LENGTH 2  DECIMALS 2,
              day_09     TYPE p LENGTH 2  DECIMALS 2,
              day_10     TYPE p LENGTH 2  DECIMALS 2,
              day_11     TYPE p LENGTH 2  DECIMALS 2,
              day_12     TYPE p LENGTH 2  DECIMALS 2,
              day_13     TYPE p LENGTH 2  DECIMALS 2,
              day_14     TYPE p LENGTH 2  DECIMALS 2,
              day_15     TYPE p LENGTH 2  DECIMALS 2,
              day_16     TYPE p LENGTH 2  DECIMALS 2,
              day_17     TYPE p LENGTH 2  DECIMALS 2,
              day_18     TYPE p LENGTH 2  DECIMALS 2,
              day_19     TYPE p LENGTH 2  DECIMALS 2,
              day_20     TYPE p LENGTH 2  DECIMALS 2,
              day_21     TYPE p LENGTH 2  DECIMALS 2,
              day_22     TYPE p LENGTH 2  DECIMALS 2,
              day_23     TYPE p LENGTH 2  DECIMALS 2,
              day_24     TYPE p LENGTH 2  DECIMALS 2,
              day_25     TYPE p LENGTH 2  DECIMALS 2,
              day_26     TYPE p LENGTH 2  DECIMALS 2,
              day_27     TYPE p LENGTH 2  DECIMALS 2,
              day_28     TYPE p LENGTH 2  DECIMALS 2,
              day_29     TYPE p LENGTH 2  DECIMALS 2,
              day_30     TYPE p LENGTH 2  DECIMALS 2,
              day_31     TYPE p LENGTH 2  DECIMALS 2,
              cellcolor  TYPE slis_t_specialcol_alv,
              color      TYPE lvc_t_scol,
            END OF ty_display,
            tt_display TYPE TABLE OF ty_display.

    DATA: mt_employee       TYPE tt_employee,
          gs_employee       TYPE ty_employee,
          gs_timesheet      TYPE ty_timesheet,
          mt_timesheet      TYPE tt_timesheet,
          mt_display        TYPE tt_display,
          ls_display        TYPE ty_display,
          mt_fcat           TYPE slis_t_fieldcat_alv,
          ls_fcat           TYPE slis_fieldcat_alv,
          mo_salv_selection TYPE REF TO cl_salv_selections,
          r_column          TYPE REF TO cl_salv_column,
          lo_columns        TYPE REF TO cl_salv_columns_table,
          it_rows           TYPE salv_t_row, " Variable for layout settings
          mo_salv           TYPE REF TO cl_salv_table,
          lo_cont           TYPE REF TO cl_gui_custom_container,
          lo_functions      TYPE REF TO cl_salv_functions_list.

    " REUSE = FM REUSE_ALV_GRID_DISPLAY
    METHODS :get_data,
      fill_table,
      fcat_reuse RETURNING VALUE(rt_fcat) TYPE slis_t_fieldcat_alv,
      display_reuse,
      update_reuse IMPORTING iv_out TYPE abap_bool,
      color_reuse.

    " SALV    = Class CL_SALV_TABLE

    METHODS :salv_table,
      update_salv,
      user_command_salv FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

    " GRID    = Class CL_GUI_ALV_GRID in protected section

    DATA: mo_alv_grid  TYPE REF TO cl_gui_alv_grid,         " declare an object
          mo_cust_grid TYPE REF TO cl_gui_custom_container, " declare an object
          ms_gridfcat  TYPE lvc_s_fcat,
          mt_gridfcat  TYPE lvc_t_fcat.

    METHODS : get_fcat_grid,
      get_layout RETURNING VALUE(rs_layout) TYPE lvc_s_layo,
      container_grid,
      set_first_display,
      salv_columns IMPORTING io_salv TYPE REF TO cl_salv_table,
      salv_status.


ENDCLASS.

" screen for alv grid
DATA: BEGIN OF gs_screen100,
        ok_code TYPE syucomm,
      END OF gs_screen100.
