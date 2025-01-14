REPORT zkd_hier_doc.

INCLUDE <cl_alv_control>.

DATA: BEGIN OF gs_0001,
        ok_code TYPE sy-ucomm,
      END OF gs_0001.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE. " WITH REFERENCE GLOBAL

CLASS lcl_hier DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_desc,
             desc TYPE dd02t-ddtext,
           END OF ty_desc,
           tt_desc TYPE STANDARD TABLE OF ty_desc WITH DEFAULT KEY.

    METHODS : execute.
    METHODS on_save.
    METHODS on_back.

    METHODS edit_mode IMPORTING iv_call TYPE abap_bool OPTIONAL.

    DATA: mv_edit    TYPE i,
          mt_table   TYPE REF TO data,
          mv_tabname TYPE tabname.

  PRIVATE SECTION.

    DATA: mo_tree      TYPE REF TO cl_salv_tree,
          mo_nodes     TYPE REF TO cl_salv_nodes,
          mo_node      TYPE REF TO cl_salv_node,
          mt_nodes     TYPE STANDARD TABLE OF zkd_hier_doc,
          mt_desc      TYPE tt_desc,
          mo_dock      TYPE REF TO cl_gui_docking_container,
          mo_grid      TYPE REF TO cl_gui_alv_grid,
          mo_split_row TYPE REF TO cl_gui_splitter_container,
          mv_changed   TYPE abap_bool.

    METHODS : generate_hierarchy.
    METHODS : generate_nodes.

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING node_key columnname.

    METHODS : display_docking IMPORTING is_tabnodes TYPE zkd_hier_doc.

    METHODS
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive
            sender.

    METHODS
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm.

    METHODS
      handle_data_ch FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
            et_good_cells
            e_modified.


ENDCLASS.

DATA: go_hier TYPE REF TO lcl_hier.

CLASS lcl_hier IMPLEMENTATION.

  METHOD execute.

    generate_hierarchy( ).

  ENDMETHOD.

  METHOD generate_hierarchy.

    DATA: lo_column TYPE REF TO cl_salv_column_tree,
          lr_events TYPE REF TO cl_salv_events_tree.

    cl_salv_tree=>factory(
      IMPORTING
        r_salv_tree   = mo_tree    " ALV: Tree Model
      CHANGING
        t_table       = mt_desc
    ).

    mo_nodes = mo_tree->get_nodes( ).
    mo_tree->get_functions( )->set_all( abap_true ).

    DATA(lo_settings) = mo_tree->get_tree_settings( ).

    lo_settings->set_hierarchy_header( 'Object Name' ).
    lo_settings->set_hierarchy_size( 40 ).

    DATA(lv_title) = sy-title.
    lo_settings->set_header( | { lv_title } | ).

    generate_nodes( ).

    lr_events = mo_tree->get_event( ).
    SET HANDLER on_double_click FOR lr_events.

    mo_tree->display( ).

  ENDMETHOD.

  METHOD generate_nodes.

    DATA : lv_parent        TYPE salv_de_node_key,
           ls_set_hierarchy TYPE ty_desc.
*           lv_ddtext        TYPE as4text,
*           mv_tabname       TYPE tabname.

    CLEAR lv_parent.

    mo_node = mo_nodes->add_node(  related_node   = ' '
                                   text           = 'Main'
                                   folder = 'X'
                                   expander = 'X'
                                   relationship   = ' ' ).

    lv_parent  = mo_node->get_key( ).

    mo_node = mo_nodes->add_node(  related_node   = lv_parent
                                   text           = 'Tables'
*                                     data_row       = ls_set_hierarchy
                                   relationship   = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child ) ).

    lv_parent  = mo_node->get_key( ).

    SELECT
      tabname
      FROM zkd_hier_doc
      INTO CORRESPONDING FIELDS OF TABLE @mt_nodes
      WHERE nodetext = 'Tables'
      ORDER BY nodelevel.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>).

      SELECT SINGLE "dd02l~tabname,
                    dd02t~ddtext
        INTO @ls_set_hierarchy "( @mv_tabname, @lv_ddtext )
        FROM dd02l
        LEFT JOIN dd02t
          ON dd02l~tabname = dd02t~tabname
         AND dd02t~ddlanguage = @sy-langu
        WHERE dd02l~tabname = @<ls_node>-tabname.

      mo_node = mo_nodes->add_node(
        related_node = lv_parent
        text         = CONV lvc_value( <ls_node>-tabname )
        data_row     = ls_set_hierarchy-desc
        relationship = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    DATA: lo_nodes TYPE REF TO cl_salv_nodes,
          lo_node  TYPE REF TO cl_salv_node,
          ls_nodes TYPE zkd_hier_doc.

    lo_nodes = mo_tree->get_nodes( ).
    lo_node = lo_nodes->get_node( node_key ).

    ls_nodes-tabname = lo_node->get_text( ).

    READ TABLE mt_nodes INTO ls_nodes WITH KEY tabname = ls_nodes-tabname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    display_docking( is_tabnodes = ls_nodes ).

  ENDMETHOD.

  METHOD display_docking.

    DATA: lo_dock       TYPE REF TO cl_gui_docking_container,
          lt_fieldcat   TYPE lvc_t_fcat,
          ls_component  TYPE abap_componentdescr,
          lt_component  TYPE abap_component_tab,
          lr_strucdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr TYPE REF TO cl_abap_tabledescr,
          lr_data_table TYPE REF TO data,
          lr_data       TYPE REF TO data,
          lt_style      TYPE lvc_t_styl.

    FIELD-SYMBOLS :   <ls_style> TYPE lvc_t_styl.

    mv_tabname    = is_tabnodes-tabname.

    CREATE DATA mt_table TYPE TABLE OF (mv_tabname).
    ASSIGN mt_table->* TO <fs_table>.

    SELECT *
      FROM (mv_tabname)
      INTO TABLE <fs_table>.

*    SELECT fieldname
* FROM dd03l
* INTO TABLE @DATA(lt_fields)
* WHERE tabname = @mv_tabname.


    IF <fs_table> IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_dock IS INITIAL.
      CREATE OBJECT mo_dock
        EXPORTING
          repid = 'SAPLSLVC_FULLSCREEN'
          dynnr = '0800'
          ratio = 70
          side  = cl_gui_docking_container=>dock_at_right.

      cl_gui_cfw=>set_new_ok_code( '/00' ).
    ENDIF.

    IF mo_grid IS INITIAL.
      CREATE OBJECT mo_grid
        EXPORTING
          i_parent = mo_dock.
    ENDIF.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = mv_tabname
      CHANGING
        ct_fieldcat      = lt_fieldcat
      EXCEPTIONS
        OTHERS           = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Error preparing field catalog' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    edit_mode( ).

    DATA(lo_str) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( p_name = mv_tabname ) ).
    DATA(lt_components) = lo_str->get_components( ).

    APPEND VALUE #( name = 'STYLE'
                    type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( p_name = 'LVC_T_STYL' ) )
                  ) TO lt_components.

    lr_strucdescr = cl_abap_structdescr=>create( lt_components ).
    lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_strucdescr ).

    CREATE DATA lr_data_table TYPE HANDLE lr_tabledescr.

    ASSIGN lr_data_table->* TO <lt_data>.

    SELECT *
      FROM (mv_tabname)
      UP TO 10 ROWS
      INTO CORRESPONDING FIELDS OF TABLE <lt_data>.

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        stylefname  = 'STYLE'
*                                        edit = COND #( WHEN mv_edit IS INITIAL THEN abap_false ELSE abap_true )
                                        ).

    TRY.
        cl_abap_list_layout=>suppress_toolbar( ).
      CATCH cx_list_already_active.
    ENDTRY.

    mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    DATA(lt_tb_exc) = VALUE ui_functions(
                ( '&LOCAL&INSERT_ROW'    )
                ( '&LOCAL&CUT'    )
                ( '&LOCAL&COPY'   )
                ( '&LOCAL&PASTE'  )
                ( '&LOCAL&COPY_ROW'  )
                ( '&LOCAL&PASTE_NEW_ROW'  )
                ( '&INFO'         )
    ).

    SET HANDLER: handle_toolbar
           handle_ucomm
           handle_data_ch FOR mo_grid .

    TRY.
        mo_grid->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layout                 " Layout
          it_toolbar_excluding          = lt_tb_exc
        CHANGING
          it_outtab                     = <lt_data>               " Output Table
          it_fieldcatalog               = lt_fieldcat              " Field Catalog
        EXCEPTIONS
          invalid_parameter_combination = 1                " Wrong Parameter
          program_error                 = 2                " Program Errors
          too_many_lines                = 3                " Too many Rows in Ready for Input Grid
          OTHERS                        = 4
      ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF mo_dock IS NOT INITIAL.
      mo_dock->set_visible( EXPORTING visible = abap_true ). " Keep the docking container visible
    ENDIF.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.

  METHOD on_back.

    DATA: lv_answer  TYPE string.

    IF mv_changed = 'X'.

      DATA(lv_message) = 'Data has been changed. Do you want to save it?'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Save Data'
          text_question         = lv_message
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CASE lv_answer.
        WHEN '1'.
          on_save( ).
          LEAVE TO SCREEN 0.
        WHEN '2'.
          LEAVE TO SCREEN 0.
      ENDCASE.

    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = mv_tabname.

  ENDMETHOD.

  METHOD on_save.


  ENDMETHOD.

  METHOD edit_mode.

    DATA: lv_answer TYPE string.

    IF mv_edit IS INITIAL.
      RETURN.
    ENDIF.

*    lock the object
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = mv_tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF sy-subrc = 0.
      mo_grid->set_ready_for_input(  ).
      RETURN.
    ENDIF.

    IF iv_call IS INITIAL.
      DATA(lv_message) = |The data is locked by user { sy-msgv1 } and can be displayed only. Do you want to display locked data? |.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Locked Data'
          text_question         = lv_message
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CASE lv_answer.
        WHEN '2'.
          LEAVE LIST-PROCESSING.
      ENDCASE.

      RETURN.
    ENDIF.

    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    mv_edit = 0.
  ENDMETHOD.

  METHOD handle_toolbar.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<ls_toolb>) WITH KEY function = '&LOCAL&APPEND'.
    IF sy-subrc = 0.
      <ls_toolb>-function = 'FC_APPEND'.
    ENDIF.

    APPEND VALUE #( function  = 'FC_EDIT'
                    icon      = CONV #( icon_toggle_display_change )
                    quickinfo = 'Edit mode' )   TO e_object->mt_toolbar.

    APPEND VALUE #( function  = 'FC_CLOSE'
                    icon      = CONV #( icon_close )
                    quickinfo = 'Close' )  TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_ucomm.

    CASE e_ucomm.

      WHEN 'FC_APPEND'.

        APPEND INITIAL LINE TO <lt_data> ASSIGNING FIELD-SYMBOL(<fs_table_st>).

        LOOP AT <lt_data> ASSIGNING <fs_table_st>.
*          <fs_table_st>- = cl_gui_alv_grid=>mc_style_enabled.
        ENDLOOP.

        mo_grid->refresh_table_display(
                EXCEPTIONS
                  finished       = 1                " Display was Ended (by Export)
                  OTHERS         = 2
              ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'FC_CLOSE'.

        mo_dock->set_visible(
                EXPORTING
                visible           = abap_false                 " Visible
                EXCEPTIONS
                cntl_error        = 1                " CNTL_ERROR
                cntl_system_error = 2                " CNTL_SYSTEM_ERROR
                OTHERS            = 3 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'FC_EDIT'.

        IF mv_edit = 1.
          mv_edit = 0.

*    lock the object
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = mv_tabname.

          IF sy-subrc = 0.
            mo_grid->set_ready_for_input( i_ready_for_input = mv_edit ).
          ENDIF.

        ELSE.
          mv_edit = 1.
          edit_mode( iv_call = abap_true ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_data_ch.

    IF e_modified IS NOT INITIAL.
      mv_changed = abap_true.
    ELSE.
      mv_changed = abap_false.
    ENDIF.

    IF sy-subrc = 0.
      mo_grid->refresh_table_display(
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  go_hier = NEW #( ).
  go_hier->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZKD_PF_STATUS_DOCK'.
  SET TITLEBAR 'ZKD_MAINTENANCE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE gs_0001-ok_code.
    WHEN 'FC_BACK'.

      go_hier->on_back( ).

    WHEN 'FC_SAVE'.
      go_hier->on_save( ).

  ENDCASE.

ENDMODULE.
