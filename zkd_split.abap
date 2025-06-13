*&---------------------------------------------------------------------*
*& Report  ZKD_SPLIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zkd_split.

*----------------------------------------------------------------------*
*       CLASS lcl_split_cont DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_split_cont DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_ekko,
            ebeln TYPE ebeln,
            bukrs TYPE bukrs,
            button TYPE icon_d,
       END OF ty_ekko,

      BEGIN OF ty_ekpo,
            ebeln TYPE ebeln,
            ebelp TYPE ebelp,
            button TYPE icon_d,
        END OF ty_ekpo,

        BEGIN OF ty_eket,
            ebeln TYPE ebeln,
            ebelp TYPE ebelp,
            etenr TYPE eeten,
          END OF ty_eket.

    DATA: mo_tab_split TYPE REF TO cl_gui_splitter_container,
          mo_grid  TYPE REF TO cl_gui_alv_grid,
          mo_salv1 TYPE REF TO cl_salv_table,
          mo_salv2 TYPE REF TO cl_salv_table,
          mo_salv3 TYPE REF TO cl_salv_table,
          mt_ekko TYPE STANDARD TABLE OF ty_ekko,
          mt_ekpo TYPE STANDARD TABLE OF ty_ekpo,
          mt_eket TYPE STANDARD TABLE OF ty_eket.

    METHODS :extract_ekko,
extract_ekpo IMPORTING iv_ebeln TYPE ebeln,
extract_eket IMPORTING iv_ebeln TYPE ebeln
                       iv_ebelp TYPE ebelp,
    start_container,
    display_salv,

     handle_button_click
  FOR EVENT link_click OF cl_salv_events_table
  IMPORTING row column sender.


ENDCLASS.                    "

DATA: BEGIN OF gs_screen100,
  ok_code TYPE syucomm,
  END OF gs_screen100.

DATA go_split_cont TYPE REF TO lcl_split_cont.

*----------------------------------------------------------------------*
*       CLASS lcl_split_cont IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_split_cont IMPLEMENTATION.

  METHOD execute.
    start_container( ).
    extract_ekko( ).
    display_salv( ).


  ENDMETHOD.                    "execute

  METHOD extract_ekko.
    DATA  ls_ekko TYPE ty_ekko.

    SELECT ebeln bukrs
      FROM ekko
      UP TO 10 ROWS
      INTO TABLE mt_ekko.

    ls_ekko-button = icon_execute_object.
    MODIFY mt_ekko FROM ls_ekko TRANSPORTING button WHERE button IS INITIAL.

  ENDMETHOD.                    "extract_EKKO

  METHOD extract_ekpo.
    DATA  ls_ekpo TYPE ty_ekpo.

    SELECT ebeln ebelp
      FROM ekpo
      INTO TABLE mt_ekpo
      WHERE ebeln = iv_ebeln.

    ls_ekpo-button = icon_execute_object.
    MODIFY mt_ekpo FROM ls_ekpo TRANSPORTING button WHERE button IS INITIAL.

    mo_salv2->refresh( ).
    mo_salv3->refresh( ).
  ENDMETHOD.                    "extract_EKPO

  METHOD extract_eket.

    SELECT ebeln ebelp etenr
      FROM eket
      INTO TABLE mt_eket
      WHERE ebeln = iv_ebeln
      AND ebelp = iv_ebelp.

    mo_salv3->refresh( ).

  ENDMETHOD.                    "extract_EKET

  METHOD display_salv.

    DATA: lo_column      TYPE REF TO cl_salv_column_table,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_events TYPE REF TO cl_salv_events_table.

    " Display SALV Table 1
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_tab_split->get_container( row = 1 column = 1 )
          IMPORTING
            r_salv_table = mo_salv1
          CHANGING
            t_table      = mt_ekko ).
      CATCH cx_salv_msg ."INTO data(lx_msg3).
*        MESSAGE lx_msg3->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    mo_salv1->get_columns( ).

    lo_columns = mo_salv1->get_columns( ).
    lo_column ?= lo_columns->get_column( 'BUTTON' ).

    lo_column->set_cell_type( if_salv_c_cell_type=>button ).
    lo_column->set_output_length( 15 ).
    lo_column->set_short_text( 'Pushbutton' ).
    lo_column->set_medium_text( 'Pushbutton' ).
    lo_column->set_long_text( 'Pushbutton' ).

    lo_events = mo_salv1->get_event( ).
    SET HANDLER handle_button_click FOR lo_events.

    mo_salv1->get_functions( )->set_all( abap_true ).
    mo_salv1->get_columns( )->set_optimize( abap_true ).
    mo_salv1->display( ).

    " Display SALV Table 2
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_tab_split->get_container( row = 2 column = 1 )
          IMPORTING
            r_salv_table = mo_salv2
          CHANGING
            t_table      = mt_ekpo ).
      CATCH cx_salv_msg. "INTO data(lx_msg2).
*        MESSAGE lx_msg2->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lo_columns = mo_salv2->get_columns( ).
    lo_column ?= lo_columns->get_column( 'BUTTON' ).

    lo_column->set_cell_type( if_salv_c_cell_type=>button ).
    lo_column->set_output_length( 15 ).
    lo_column->set_short_text( 'Pushbutton' ).
    lo_column->set_medium_text( 'Pushbutton' ).
    lo_column->set_long_text( 'Pushbutton' ).

    lo_events = mo_salv2->get_event( ).
    SET HANDLER handle_button_click FOR lo_events.

    mo_salv2->get_functions( )->set_all( abap_true ).
    mo_salv2->get_columns( )->set_optimize( abap_true ).
    mo_salv2->display( ).

    " Display SALV Table 3
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_tab_split->get_container( row = 3 column = 1 )
          IMPORTING
            r_salv_table = mo_salv3
          CHANGING
            t_table      = mt_eket ).
      CATCH cx_salv_msg ."INTO data(lx_msg3).
*        MESSAGE lx_msg3->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    mo_salv3->get_functions( )->set_all( abap_true ).
    mo_salv3->get_columns( )->set_optimize( abap_true ).
    mo_salv3->display( ).

    CALL SCREEN 100.
  ENDMETHOD.                    "display_salv

  METHOD handle_button_click.

    FIELD-SYMBOLS: <ls_ekko> TYPE ty_ekko.
    FIELD-SYMBOLS: <ls_ekpo> TYPE ty_ekpo.


    IF sender = mo_salv1->get_event( ).

      READ TABLE mt_ekko ASSIGNING <ls_ekko> INDEX row.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CASE column.
        WHEN 'BUTTON'.
          CLEAR :mt_ekpo , mt_eket.

          extract_ekpo( <ls_ekko>-ebeln ).
      ENDCASE.
    ELSE.


      READ TABLE mt_ekpo ASSIGNING <ls_ekpo> INDEX row.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CASE column.
        WHEN 'BUTTON'.
          CLEAR : mt_eket.

          extract_eket( iv_ebeln = <ls_ekpo>-ebeln
          iv_ebelp = <ls_ekpo>-ebelp ).
      ENDCASE.

    ENDIF.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD start_container.

    DATA:  lo_cont   TYPE REF TO cl_gui_custom_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_tab_split
      EXPORTING
        parent            = lo_cont
        rows              = 3
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

  ENDMETHOD.                    "start_container

ENDCLASS.                    "lcl_split_cont IMPLEMENTATION


START-OF-SELECTION.
  CREATE OBJECT go_split_cont.
  go_split_cont->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZKD_PF-STATUS'.
  SET TITLEBAR 'ZKD_TB'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gs_screen100-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
