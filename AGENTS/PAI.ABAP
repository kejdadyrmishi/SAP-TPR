*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_EXAM_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  PERFORM user_command_0001.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.
  CASE gs_screen001-ok_code.
    WHEN 'FC_ENTER' OR 'FC_EXECUTE'.
      PERFORM get_data.  "get data
      CALL SCREEN 2.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.
  PERFORM user_command_0002.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0002 .

  DATA: lv_error  TYPE abap_bool,
        lv_delete TYPE abap_bool.

  CASE gs_screen002-ok_code.

    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'FC_NEW'.
      CLEAR gs_screen003.
      CALL SCREEN 3
      STARTING AT 20 10
      ENDING AT  80 12.

    WHEN 'FC_EDIT'.
      CLEAR gs_screen003.
      PERFORM if_selected CHANGING lv_error. "if row is selected edit the data
      IF lv_error IS INITIAL.

        SELECT SINGLE *  "read the info of the selected agent to the dialog box edit agent
          FROM zkd_employee
          INTO CORRESPONDING FIELDS OF gs_screen003
          WHERE cod = gs_sale-cod.

        CALL SCREEN 3
        STARTING AT 20 10
        ENDING AT 80 12.
      ENDIF.

    WHEN 'FC_SNEW'.
      CLEAR gs_screen004.
      CALL SCREEN 4
      STARTING AT 20 10
      ENDING AT  90 15.

    WHEN 'FC_SEDIT'.
      CLEAR gs_screen004.
      PERFORM if_selected CHANGING lv_error. "if row is selected edit the data
      IF lv_error IS INITIAL.

        SELECT SINGLE cod   " read the info of the selected agent to the dialog box edit sale
                      netwr
                      descr
          FROM zkd_shitjet
          INTO CORRESPONDING FIELDS OF gs_screen004
          WHERE cod     = gs_sale-cod
          AND   smonth  = gs_screen001-month
          AND   syear   = gs_screen001-year
          AND   counter = gs_sale-counter.

        CALL SCREEN 4
         STARTING AT 20 10
         ENDING AT  90 15.
      ENDIF.

    WHEN 'FC_SDEL'.
      PERFORM delete_selected CHANGING lv_delete. "if row or rows are selected
      IF lv_delete IS INITIAL.

        DATA popup_return TYPE char1.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Delete Row From Table'
            text_question         = 'Do You Want To Delete Row From The Table?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '1'
            display_cancel_button = 'X'
          IMPORTING
            answer                = popup_return
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        CASE popup_return.
          WHEN '1'. "  'Yes'
            PERFORM delete_sale. "delete the data
            LEAVE TO SCREEN 2.
          WHEN '2'. "'No' ,
            LEAVE TO SCREEN 2.
        ENDCASE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.
  PERFORM user_command_0003.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0003 .
  CASE gs_screen003-ok_code.
    WHEN 'FC_OK'.
      PERFORM agent. "Create a new agent or edit
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0004 INPUT.
  PERFORM user_command_0004.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0004.
  CASE gs_screen004-ok_code.
    WHEN 'FC_OK'.
      PERFORM new_sales. " Create a new sales order or edit
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE gs_screen003-ok_code.
    WHEN 'FC_CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit1 INPUT.
  CASE gs_screen001-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit2 INPUT.
  CASE gs_screen004-ok_code.
    WHEN 'FC_CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.


*&SPWIZARD: INPUT MODUL FOR TC 'GS_TAB_CONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE GS_TAB_CONT_MARK INPUT.
  DATA: g_GS_TAB_CONT_wa2 like line of GT_SALE.
  if GS_TAB_CONT-line_sel_mode = 1
  and GS_SALE-SEL = 'X'.
    loop at GT_SALE into g_GS_TAB_CONT_wa2
      where SEL = 'X'.
      g_GS_TAB_CONT_wa2-SEL = ''.
      modify GT_SALE
        from g_GS_TAB_CONT_wa2
        transporting SEL.
    endloop.
  endif.
  MODIFY GT_SALE
    FROM GS_SALE
    INDEX GS_TAB_CONT-CURRENT_LINE
    TRANSPORTING SEL.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'GS_TAB_CONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE GS_TAB_CONT_USER_COMMAND INPUT.
  GS_SCREEN002-OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'GS_TAB_CONT'
                              'GT_SALE'
                              'SEL'
                     CHANGING GS_SCREEN002-OK_CODE.
  SY-UCOMM = GS_SCREEN002-OK_CODE.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*
