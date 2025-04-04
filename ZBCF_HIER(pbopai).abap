*----------------------------------------------------------------------*
***INCLUDE ZBCF_HIER_STATUS_100.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TB0101'.
ENDMODULE.


*----------------------------------------------------------------------*
***INCLUDE ZBCF_HIER_USER_COMMAND_100.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

DATA lv_answer TYPE char1.
  CASE gs_screen0100-ok_code.
    WHEN 'FC_BACK'.
      IF gv_changed = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar       = 'Warning'
            text_question  = 'Unsaved data will be lost. Do you want to continue?'
            text_button_1  = 'Yes'
            text_button_2  = 'No'
            default_button = '1'
          IMPORTING
            answer         = lv_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF lv_answer = '1'.
          LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'FC_SAVE'.
      go_double_hierarchy->save_data( ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
CASE gs_screen0101-ok_code.
    WHEN 'FC_OKAY'.
      LEAVE TO SCREEN 0.
    WHEN 'FC_EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
