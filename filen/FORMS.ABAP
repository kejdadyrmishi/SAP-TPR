*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_REUSEALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_reusealv USING r_ucomm LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.
  go_timesheet->user_command_reusealv( EXPORTING iv_ucomm    = r_ucomm
                                       CHANGING  cs_selfield = rs_selfield ).
ENDFORM.

FORM pf_status USING rt_extab TYPE slis_t_extab.
  CASE gv_mode.
    WHEN 'H'. " HIDE mode
      CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
        EXPORTING
          name       = icon_read_file
          text       = 'Hide'
          info       = 'Hide the Weekends'
          add_stdinf = 'X'
        IMPORTING
          result     = gv_btn_dyn. " result
    WHEN 'S'. " SHOW mode
      CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
        EXPORTING
          name       = icon_display
          text       = 'Show'
          info       = 'Show the Weekends'
          add_stdinf = 'X'
        IMPORTING
          result     = gv_btn_dyn. " result
  ENDCASE.

  SET PF-STATUS 'STATUS'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  CASE gv_mode.
    WHEN 'H'. " HIDE mode
      CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
        EXPORTING
          name       = icon_read_file
          text       = 'Hide'
          info       = 'Hide the Weekends'
          add_stdinf = 'X'
        IMPORTING
          result     = gv_btn_dyn. " result
    WHEN 'S'. " SHOW mode
      CALL FUNCTION 'ICON_CREATE' " create the pushbutton with icon and text
        EXPORTING
          name       = icon_display
          text       = 'Show'
          info       = 'Show the Weekends'
          add_stdinf = 'X'
        IMPORTING
          result     = gv_btn_dyn. " result

  ENDCASE.
  SET PF-STATUS 'ZKD_GRID'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gs_screen100-ok_code.

    WHEN 'FC_SUM'.

      go_timesheet->grid_sum( ).

    WHEN 'FC_BACK'.

      LEAVE TO SCREEN 0.

    WHEN 'SHOW_HIDE'.

      gv_mode = COND #( WHEN gv_mode = 'S' THEN 'H' ELSE 'S' ). "  hide/show mode
      go_timesheet->grid_show_hide( ).  " Call method to update ALV display

  ENDCASE.

ENDMODULE.
form set_values_parameter.
   DATA: lv_name TYPE vrm_id,
        lt_list TYPE TABLE OF vrm_value.

  lv_name = 'P_MONTH'.

  lt_list = VALUE #( ( key = '01'  text = 'January' )
                     ( key = '02'  text = 'February' )
                     ( key = '03'  text = 'March' )
                     ( key = '04'  text = 'April' )
                     ( key = '05'  text = 'May' )
                     ( key = '06'  text = 'June' )
                     ( key = '07'  text = 'July' )
                     ( key = '08'  text = 'August' )
                     ( key = '09'  text = 'September' )
                     ( key = '10' text = 'October' )
                     ( key = '11' text = 'November' )
                     ( key = '12' text = 'December' )  ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_name
      values          = lt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    " Implement suitable error handling here
  ENDIF.

  endform.
