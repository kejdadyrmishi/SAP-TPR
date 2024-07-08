*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_EXAM_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZKD_PF-STATUS'.
  SET TITLEBAR 'ZKD_TB'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.
  gs_screen002-waers = 'EUR'.
  SET PF-STATUS 'ZKD_PF-STATUS2'.
  SET TITLEBAR 'ZKD_TB2'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.
  SET PF-STATUS 'ZKD_PF-STATUS3'.
  SET TITLEBAR 'ZKD_TB3'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0004 OUTPUT.
  SET PF-STATUS 'ZKD_PF-STATUS4'.
  SET TITLEBAR 'ZKD_TB4'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  NON_EDITABLE_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE non_editable_0003 OUTPUT.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GS_SCREEN003-COD'.
        CASE gs_screen002-ok_code.
          WHEN 'FC_EDIT'.
            screen-input = 0. " Make 'COD' non-editable
          WHEN 'FC_NEW'.
            screen-input = 1.  " Make 'COD' editable
        ENDCASE.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  NON_EDITABLE_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE non_editable_0004 OUTPUT.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GS_SCREEN004-COD'.
        CASE gs_screen002-ok_code.
          WHEN 'FC_SEDIT'.
            screen-input = 0. " Make 'COD' non-editable
          WHEN 'FC_SNEW'.
            screen-input = 1.  " Make 'COD' editable
        ENDCASE.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GS_SCREEN001-MONTH' OR 'GS_SCREEN001-YEAR' OR 'GS_SCREEN002-WAERS'.
        CASE gs_screen002-ok_code.
          WHEN 'FC_SEDIT' OR 'FC_SNEW'.
            screen-input = 0. " Make month year currency non-editable
        ENDCASE.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'GS_TAB_CONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE GS_TAB_CONT_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE GT_SALE LINES GS_TAB_CONT-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'GS_TAB_CONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
*MODULE GS_TAB_CONT_GET_LINES OUTPUT.
**  G_GS_TAB_CONT_LINES = SY-LOOPC.
*ENDMODULE.
