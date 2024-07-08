*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_EXAM_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM get_data.
    SELECT zkd_employee~cod
           zkd_employee~firstname
           zkd_employee~lastname
           zkd_shitjet~netwr
           zkd_shitjet~descr
           zkd_shitjet~counter
      INTO CORRESPONDING FIELDS OF TABLE gt_sale
      FROM zkd_employee
      JOIN zkd_shitjet
        ON zkd_employee~cod = zkd_shitjet~cod
      WHERE zkd_shitjet~smonth = gs_screen001-month
        AND zkd_shitjet~syear = gs_screen001-year
        AND zkd_shitjet~deleted = abap_false.

*Check if there are records
    IF sy-subrc NE 0.
      MESSAGE 'No records found for the selected Year and Month.' TYPE 'I'.
      RETURN.
    ENDIF.

    CLEAR gs_screen002-netwr.
    LOOP AT gt_sale INTO gs_sale.
      gs_sale-bonus = gs_sale-netwr * '0.05'.
      MODIFY gt_sale FROM gs_sale TRANSPORTING bonus.
      gs_screen002-netwr = gs_screen002-netwr + gs_sale-netwr.
    ENDLOOP.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IF_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ERROR  text
*----------------------------------------------------------------------*
  FORM if_selected CHANGING cv_error TYPE abap_bool.

    DATA: lv_count TYPE i.
    cv_error = abap_true.

    LOOP AT gt_sale INTO gs_sale WHERE sel IS NOT INITIAL.
      lv_count = lv_count + 1.
    ENDLOOP.

    CASE lv_count.
      WHEN 0.
        MESSAGE 'Select at least 1 row' TYPE 'S' DISPLAY LIKE 'I'.
      WHEN 1.
        CLEAR cv_error.
        RETURN.
      WHEN OTHERS.
        MESSAGE 'Can not select more than 1 row' TYPE 'S' DISPLAY LIKE 'I'.
    ENDCASE.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_DELETE  text
*----------------------------------------------------------------------*
  FORM delete_selected CHANGING cv_delete TYPE abap_bool.
    READ TABLE gt_sale
    TRANSPORTING NO FIELDS WITH KEY sel = abap_true.

    IF sy-subrc <> 0.
      MESSAGE 'Select at least 1 row' TYPE 'S' DISPLAY LIKE 'I'.
    ENDIF.
  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM agent .

    DATA(ls_agent) = VALUE zkd_employee(  cod        = gs_screen003-cod
                                          firstname  = gs_screen003-firstname
                                          lastname   = gs_screen003-lastname
                                          cnp        = gs_screen003-cnp
                                          hire_date  = gs_screen003-hire_date  ).

    CASE gs_screen002-ok_code.
      WHEN 'FC_NEW'.

        SELECT SINGLE @abap_true
            FROM zkd_employee
            INTO @DATA(lv_exists)
            WHERE cod = @gs_screen003-cod.
        IF lv_exists = abap_true.
          MESSAGE 'This Agent already exists' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

*    Insert the new agent in the database
        INSERT zkd_employee FROM ls_agent.

        IF sy-subrc EQ 0.
          MESSAGE 'Agent is successfully created' TYPE 'S'.
          LEAVE TO SCREEN 0.
        ENDIF.

      WHEN OTHERS.

        MODIFY gt_sale FROM VALUE #( firstname  = gs_screen003-firstname
                                     lastname   = gs_screen003-lastname ) TRANSPORTING firstname lastname
                                      WHERE cod =  gs_screen003-cod.

        MODIFY zkd_employee FROM ls_agent.
        IF sy-subrc EQ 0.
          MESSAGE 'Agent is successfully modified' TYPE 'S'.
          LEAVE TO SCREEN 0.
        ENDIF.
    ENDCASE.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  NEW_SALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM new_sales.
    DATA: ls_order TYPE zkd_shitjet.

    CASE gs_screen002-ok_code.
      WHEN 'FC_SNEW'.

        " Check if agent exists
        SELECT SINGLE cod
          FROM zkd_employee
          INTO ls_order-cod
          WHERE cod = gs_screen004-cod.

        IF sy-subrc <> 0.
          MESSAGE 'Agent does not exist. Please add the agent first' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        " Checking counter value
        SELECT MAX( counter )
            FROM zkd_shitjet
            INTO @ls_order-counter
            WHERE cod = @gs_screen004-cod
            AND smonth = @gs_screen001-month
            AND syear = @gs_screen001-year.

        ls_order-counter = ls_order-counter + 1.

        " If the agent is valid, add it directly to the table control
        ls_order-mandt  = sy-mandt.
        ls_order-cod    = gs_screen004-cod.
        ls_order-smonth = gs_screen001-month.
        ls_order-syear  = gs_screen001-year.
        ls_order-netwr  = gs_screen004-netwr.
        ls_order-waers  = gs_screen002-waers.
        ls_order-descr  = gs_screen004-descr.

        " Insert the new sales order in the database
        INSERT zkd_shitjet FROM ls_order.

        IF sy-subrc EQ 0.
          MESSAGE 'Sales order is successfully created' TYPE 'S'.

          " Get the agent name from the employee table
          SELECT SINGLE firstname,
                        lastname
            FROM zkd_employee
            INTO @DATA(gs_name)
            WHERE cod = @gs_screen004-cod.

          " Append the sales order to the internal table
          APPEND VALUE #(  cod       = gs_screen004-cod
                           firstname = gs_name-firstname
                           lastname  = gs_name-lastname
                           netwr     = gs_screen004-netwr
                           descr     = gs_screen004-descr
                           counter   = ls_order-counter
                       ) TO  gt_sale.

          PERFORM total_value.

          MESSAGE 'Sales order is successfully created' TYPE 'S' DISPLAY LIKE 'I'.
        ELSE.
          MESSAGE 'Sales order creation failed' TYPE 'E'.
        ENDIF.

      WHEN 'FC_SEDIT'.
        ls_order-mandt    = sy-mandt.
        ls_order-cod      = gs_screen004-cod.
        ls_order-smonth   = gs_screen001-month.
        ls_order-syear    = gs_screen001-year.
        ls_order-netwr    = gs_screen004-netwr.
        ls_order-waers    = gs_screen002-waers.
        ls_order-descr    = gs_screen004-descr.
        ls_order-counter  = gs_sale-counter.


        " Modify the sales order in the database
        MODIFY zkd_shitjet FROM ls_order .

        IF sy-subrc NE 0.
          MESSAGE 'Sale edit failed' TYPE 'E'.
          RETURN.
        ENDIF.

        MESSAGE 'Sale is successfully edited' TYPE 'S'.
        READ TABLE gt_sale ASSIGNING FIELD-SYMBOL(<fs_sale>) WITH KEY cod = gs_screen004-cod.
        IF sy-subrc = 0.
          " Update the net value and description
          <fs_sale>-netwr = gs_screen004-netwr.
          <fs_sale>-descr = gs_screen004-descr.
        ENDIF.

        " Calculate the total value of the sales orders
        PERFORM total_value.

        " Go back to the previous screen
        LEAVE TO SCREEN 0.

    ENDCASE.
  ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  TOTAL_VALUE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
  FORM total_value .
    CLEAR gs_screen002-netwr.
    LOOP AT gt_sale INTO gs_sale.
      " Calculate the bonus as 5% of the net value
      gs_sale-bonus = gs_sale-netwr * '0.05'.

      " Modify the internal table
      MODIFY gt_sale FROM gs_sale TRANSPORTING bonus.

      " Add the net value to the total
      gs_screen002-netwr = gs_screen002-netwr + gs_sale-netwr.

    ENDLOOP.
  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_SALE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM delete_sale.

*    DATA: lt_data  TYPE TABLE OF zkd_shitjet,
*          lv_subrc TYPE sy-subrc,
*          lv_index TYPE sy-tabix.

    " Identify and delete selected rows in the table control
    LOOP AT gt_sale INTO gs_sale WHERE sel = abap_true.

      UPDATE zkd_shitjet SET deleted = 'X' WHERE

                         cod         = gs_sale-cod
                         AND smonth  = gs_screen001-month
                         AND syear   = gs_screen001-year
                         AND counter = gs_sale-counter.

      IF sy-subrc EQ 0.
        DELETE gt_sale.
      ENDIF.
    ENDLOOP.

    MESSAGE 'Sale deleted with succes' TYPE 'S'.
    PERFORM total_value.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
  FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                           p_table_name
                           p_mark_name
                  CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
    DATA: l_ok     TYPE sy-ucomm,
          l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
    SEARCH p_ok FOR p_tc_name.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    l_offset = strlen( p_tc_name ) + 1.
    l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
    CASE l_ok.
      WHEN 'INSR'.                      "insert row
        PERFORM fcode_insert_row USING    p_tc_name
                                          p_table_name.
        CLEAR p_ok.

      WHEN 'DELE'.                      "delete row
        PERFORM fcode_delete_row USING    p_tc_name
                                          p_table_name
                                          p_mark_name.
        CLEAR p_ok.

      WHEN 'P--' OR                     "top of list
           'P-'  OR                     "previous page
           'P+'  OR                     "next page
           'P++'.                       "bottom of list
        PERFORM compute_scrolling_in_tc USING p_tc_name
                                              l_ok.
        CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
      WHEN 'MARK'.                      "mark all filled lines
        PERFORM fcode_tc_mark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name   .
        CLEAR p_ok.

      WHEN 'DMRK'.                      "demark all filled lines
        PERFORM fcode_tc_demark_lines USING p_tc_name
                                            p_table_name
                                            p_mark_name .
        CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

    ENDCASE.

  ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
  FORM fcode_insert_row
                USING    p_tc_name           TYPE dynfnam
                         p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
    DATA l_lines_name       LIKE feld-name.
    DATA l_selline          LIKE sy-stepl.
*    DATA l_lastline         TYPE i.
    DATA l_line             TYPE i.
    DATA l_table_name       LIKE feld-name.
    FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
    FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

    ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
    CONCATENATE p_table_name '[]' INTO l_table_name. "table body
    ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
    CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
    ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
    GET CURSOR LINE l_selline.
    IF sy-subrc <> 0.                   " append line to table
      l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
      IF l_selline > <lines>.
        <tc>-top_line = l_selline - <lines> + 1 .
      ELSE.
        <tc>-top_line = 1.
      ENDIF.
    ELSE.                               " insert line into table
      l_selline = <tc>-top_line + l_selline - 1.
*      l_lastline = <tc>-top_line + <lines> - 1.
    ENDIF.
*&SPWIZARD: set new cursor line                                        *
    l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
    INSERT INITIAL LINE INTO <table> INDEX l_selline.
    <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
    SET CURSOR LINE l_line.

  ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
  FORM fcode_delete_row
                USING    p_tc_name           TYPE dynfnam
                         p_table_name
                         p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
    DATA l_table_name       LIKE feld-name.

    FIELD-SYMBOLS <tc>         TYPE cxtab_control.
    FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <wa>.
    FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

    ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
    CONCATENATE p_table_name '[]' INTO l_table_name. "table body
    ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
    DESCRIBE TABLE <table> LINES <tc>-lines.

    LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
      ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

      IF <mark_field> = 'X'.
        DELETE <table> INDEX syst-tabix.
        IF sy-subrc = 0.
          <tc>-lines = <tc>-lines - 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
  FORM compute_scrolling_in_tc USING    p_tc_name
                                        p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
    DATA l_tc_new_top_line     TYPE i.
    DATA l_tc_name             LIKE feld-name.
    DATA l_tc_lines_name       LIKE feld-name.
    DATA l_tc_field_name       LIKE feld-name.

    FIELD-SYMBOLS <tc>         TYPE cxtab_control.
    FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

    ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
    CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
    ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
    IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
      l_tc_new_top_line = 1.
    ELSE.
*&SPWIZARD: no, ...                                                    *
      CALL FUNCTION 'SCROLLING_IN_TABLE'
        EXPORTING
          entry_act      = <tc>-top_line
          entry_from     = 1
          entry_to       = <tc>-lines
          last_page_full = 'X'
          loops          = <lines>
          ok_code        = p_ok
          overlapping    = 'X'
        IMPORTING
          entry_new      = l_tc_new_top_line
        EXCEPTIONS
*         NO_ENTRY_OR_PAGE_ACT  = 01
*         NO_ENTRY_TO    = 02
*         NO_OK_CODE_OR_PAGE_GO = 03
          OTHERS         = 0.
    ENDIF.

*&SPWIZARD: get actual tc and column                                   *
    GET CURSOR FIELD l_tc_field_name
               AREA  l_tc_name.

    IF syst-subrc = 0.
      IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
        SET CURSOR FIELD l_tc_field_name LINE 1.
      ENDIF.
    ENDIF.

*&SPWIZARD: set the new top line                                       *
    <tc>-top_line = l_tc_new_top_line.


  ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
  FORM fcode_tc_mark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
    DATA l_table_name       LIKE feld-name.

    FIELD-SYMBOLS <tc>         TYPE cxtab_control.
    FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <wa>.
    FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

    ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
    CONCATENATE p_table_name '[]' INTO l_table_name. "table body
    ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
    LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
      ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

      <mark_field> = 'X'.
    ENDLOOP.
  ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
  FORM fcode_tc_demark_lines USING p_tc_name
                                   p_table_name
                                   p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
    DATA l_table_name       LIKE feld-name.

    FIELD-SYMBOLS <tc>         TYPE cxtab_control.
    FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
    FIELD-SYMBOLS <wa>.
    FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

    ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
    CONCATENATE p_table_name '[]' INTO l_table_name. "table body
    ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
    LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
      ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

      <mark_field> = space.
    ENDLOOP.
  ENDFORM.                                          "fcode_tc_mark_lines
