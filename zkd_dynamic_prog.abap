REPORT zkd_dynamic_prog.

INCLUDE <cl_alv_control>.

DATA: BEGIN OF gs_0001,
        ok_code TYPE sy-ucomm,
      END OF gs_0001.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE. " WITH REFERENCE GLOBAL

CLASS lcl_hier DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS : execute,
      on_save,
      on_back,
      start_container,
      get_documentation ,
      free_selection,
      edit_mode IMPORTING iv_call TYPE abap_bool OPTIONAL.

    DATA: mv_edit         TYPE i,
          mv_tabname      TYPE tabname,
          ls_component    TYPE abap_componentdescr,
          mv_show_hide    TYPE char1 VALUE 'H',
          lr_strucdescr   TYPE REF TO cl_abap_structdescr,
          lr_tabledescr   TYPE REF TO cl_abap_tabledescr,
          mo_node         TYPE REF TO cl_salv_node,
          mt_deleted_rows TYPE STANDARD TABLE OF i.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_desc,
             desc TYPE dd02t-ddtext,
           END OF ty_desc,
           tt_desc TYPE STANDARD TABLE OF ty_desc WITH DEFAULT KEY.

    DATA: mo_tree        TYPE REF TO cl_salv_tree,
          mo_nodes       TYPE REF TO cl_salv_nodes,
          mt_nodes       TYPE STANDARD TABLE OF zkd_hier_doc,
          mt_desc        TYPE tt_desc,
          mo_dock        TYPE REF TO cl_gui_docking_container,
          mo_grid        TYPE REF TO cl_gui_alv_grid,
          mv_changed     TYPE abap_bool,
          mo_cont        TYPE REF TO cl_gui_custom_container,
          mo_top_of_page TYPE REF TO cl_dd_document,
          mo_top_area    TYPE REF TO cl_gui_container,
          mo_splitter    TYPE REF TO cl_gui_splitter_container.

    METHODS : generate_hierarchy,
      generate_nodes,
      display_docking IMPORTING is_tabnodes TYPE zkd_hier_doc .

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING node_key columnname.


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

    METHODS: handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
          er_data_changed.

    METHODS top_of_page
         FOR EVENT top_of_page OF cl_gui_alv_grid.

ENDCLASS.

DATA: go_hier TYPE REF TO lcl_hier.

CLASS lcl_hier IMPLEMENTATION.

  METHOD execute.

    start_container( ).
    generate_hierarchy( ).

  ENDMETHOD.

  METHOD start_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT mo_cont
      EXPORTING
        container_name              = 'CUST_CONT'
        repid                       = sy-repid
        dynnr                       = '0001'
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

  ENDMETHOD.

  METHOD handle_data_changed.

    IF er_data_changed->mp_mod_rows IS NOT INITIAL .
      mv_changed = abap_true.
    ELSE.
      mv_changed = abap_false.
    ENDIF.

    LOOP AT er_data_changed->mt_deleted_rows ASSIGNING FIELD-SYMBOL(<ls_delted_rows>).
      APPEND  <ls_delted_rows>-row_id TO mt_deleted_rows.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cell>)
   WHERE fieldname = ls_component-name.

      READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_int>) INDEX <ls_cell>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.

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

    mo_grid->register_edit_event(
  EXPORTING
    i_event_id = cl_gui_alv_grid=>mc_evt_enter
).

  ENDMETHOD.

  METHOD generate_hierarchy.

    DATA lr_events TYPE REF TO cl_salv_events_tree.
    TRY .

        cl_salv_tree=>factory(
        EXPORTING
          r_container     = mo_cont
          IMPORTING
            r_salv_tree   = mo_tree    " ALV: Tree Model
          CHANGING
            t_table       = mt_desc
        ).

      CATCH cx_salv_error.

    ENDTRY.
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
           mo_node          TYPE REF TO cl_salv_node,
           ls_set_hierarchy TYPE ty_desc.

    CLEAR lv_parent.
    TRY .

        mo_node = mo_nodes->add_node(  related_node   = ' '
                                       text           = 'Main'
                                       folder = 'X'
                                       expander = 'X'
                                       relationship   = ' ' ).

        lv_parent  = mo_node->get_key( ).

        mo_node = mo_nodes->add_node(  related_node   = lv_parent
                                       text           = 'Tables'
                                       relationship   = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child ) ).

        lv_parent  = mo_node->get_key( ).

      CATCH cx_salv_msg.

    ENDTRY.
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

      TRY .

          mo_node = mo_nodes->add_node(
            related_node = lv_parent
            text         = CONV lvc_value( <ls_node>-tabname )
            data_row     = ls_set_hierarchy-desc
            relationship = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
          ).

        CATCH cx_salv_msg.

      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    DATA: ls_nodes TYPE zkd_hier_doc.

    TRY .
        mo_nodes = mo_tree->get_nodes( ).
        mo_node = mo_nodes->get_node( node_key ).

        ls_nodes-tabname = mo_node->get_text( ).
      CATCH cx_salv_msg.

    ENDTRY.
    READ TABLE mt_nodes INTO ls_nodes WITH KEY tabname = ls_nodes-tabname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    display_docking( is_tabnodes = ls_nodes ).

  ENDMETHOD.

  METHOD display_docking.

    DATA: lt_table       TYPE REF TO data,
          lt_fieldcat    TYPE lvc_t_fcat,
          lr_data_table  TYPE REF TO data,
          lo_bottom_area TYPE REF TO cl_gui_container.

    mv_tabname    = is_tabnodes-tabname.

    CREATE DATA lt_table   TYPE TABLE OF (mv_tabname).
    ASSIGN lt_table->* TO <fs_table>.

    SELECT *
      FROM (mv_tabname)
      INTO TABLE <fs_table>.

    IF <fs_table> IS NOT ASSIGNED."IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_dock IS INITIAL.
      CREATE OBJECT mo_dock
        EXPORTING
          repid = sy-repid
          dynnr = '0001'
          ratio = 70
          side  = cl_gui_docking_container=>dock_at_right.

      cl_gui_cfw=>set_new_ok_code( '/00' ).
    ENDIF.

    IF mo_splitter IS INITIAL.
      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_dock
          rows    = 2
          columns = 1.
    ENDIF.

    mo_splitter->set_row_sash(   id    = 1
                            type  = cl_gui_splitter_container=>type_sashvisible
                            value = cl_gui_splitter_container=>false ).

*    *    ================================================================
    CASE mv_show_hide.

      WHEN 'D'.
        DATA(lv_height) = 15.
        mo_splitter->set_row_sash(   id    = 1
                                    type  = cl_gui_splitter_container=>type_sashvisible
                                    value = cl_gui_splitter_container=>false ).
      WHEN 'H'.
        lv_height = 0.
        mo_splitter->set_row_sash(   id    = 1
                                    type  = cl_gui_splitter_container=>type_sashvisible
                                    value = cl_gui_splitter_container=>false ).
    ENDCASE.

    mo_splitter->set_row_height(
      EXPORTING
        id                =  1   " Row ID
        height            = lv_height     " Height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

*    ================================================================

    mo_top_area = mo_splitter->get_container( row = 1 column = 1 ).

    IF mo_top_of_page IS INITIAL.
      CREATE OBJECT mo_top_of_page
        EXPORTING
          style = 'ALV_GRID'.
    ENDIF.

    lo_bottom_area = mo_splitter->get_container( row = 2 column = 1 ).

    IF mo_grid IS INITIAL.
      CREATE OBJECT mo_grid
        EXPORTING
          i_parent = lo_bottom_area.
    ENDIF.

    CLEAR lt_fieldcat.
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

    SORT <lt_data> .

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt  = abap_true
                                        stylefname  = 'STYLE'
                                        edit = COND #( WHEN mv_edit IS INITIAL THEN abap_false ELSE abap_true )
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
           handle_data_changed
           top_of_page
            FOR mo_grid .



    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WHERE key <> 'X'.
      <ls_fieldcat>-edit = 'X'.
    ENDLOOP.
    mo_grid->set_ready_for_input( 0 ).

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

    mo_top_of_page->initialize_document( ).
    mo_grid->list_processing_events(
         EXPORTING
           i_event_name = 'TOP_OF_PAGE'
           i_dyndoc_id  = mo_top_of_page ).

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

    DATA: lt_db_data TYPE REF TO data,
          lt_db      TYPE REF TO data,
          lt_delete  TYPE REF TO data,
          lt_keys    TYPE TABLE OF string,
          lv_key     TYPE string.

    " Create a hash table to track seen keys
    DATA: lt_seen_keys       TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
          lt_duplicate_cells TYPE lvc_t_cell,
          lv_key_value       TYPE string.

    FIELD-SYMBOLS: <ls_data>    TYPE any,
                   <ls_db>      TYPE any,
                   <lv_field>   TYPE any,
                   <lt_delete>  TYPE STANDARD TABLE,
                   <lt_db>      TYPE STANDARD TABLE,
                   <lt_db_data> TYPE STANDARD TABLE.


    mo_grid->check_changed_data(  ).

    " Get the structure description dynamically
    lr_strucdescr ?= cl_abap_typedescr=>describe_by_name( mv_tabname ).
    lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_strucdescr ).

    " Create dynamic internal tables
    CREATE DATA lt_db_data TYPE HANDLE lr_tabledescr.
    CREATE DATA lt_db TYPE HANDLE lr_tabledescr.
    CREATE DATA lt_delete TYPE HANDLE lr_tabledescr.

    FIELD-SYMBOLS :   <ls_style> TYPE lvc_t_styl.
    ASSIGN lt_delete->* TO <lt_delete>.
    ASSIGN lt_db->* TO <lt_db>.
    ASSIGN lt_db_data->* TO <lt_db_data>.

    mo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_dyn_fcat) )  .

    SELECT *
         FROM (mv_tabname)
         INTO TABLE <lt_db>.


    SORT <lt_db> ASCENDING.

    " Get key fields dynamically ( KEY = 'X')
    LOOP AT lt_dyn_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) .
      IF <ls_fcat>-key = abap_true.
        APPEND <ls_fcat>-fieldname TO lt_keys.
      ENDIF.
    ENDLOOP.


    IF mt_deleted_rows IS INITIAL.
      " Check for duplicates
      LOOP AT <lt_data> ASSIGNING <ls_data>.
        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <ls_data> TO <ls_style>.
        " Build a concatenated key value
        CLEAR lv_key_value.
        LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<lv_key>).
          IF <lv_key> = 'MANDT'.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT <lv_key> OF STRUCTURE <ls_data> TO <lv_field>.
          IF sy-subrc = 0.
            CONCATENATE lv_key_value <lv_field> INTO lv_key_value SEPARATED BY '|'.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR <ls_style>.
            APPEND VALUE #( fieldname = <lv_key>
                            style = cl_gui_alv_grid=>mc_style_disabled ) TO <ls_style>.
            IF mo_grid->is_ready_for_input( ) = 0.
              mo_grid->set_ready_for_input( 1 ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Check in hash table
        READ TABLE lt_seen_keys WITH TABLE KEY table_line = lv_key_value TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Duplicate found
          LOOP AT lt_dyn_fcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
            APPEND VALUE #( row_id = sy-tabix col_id = <ls_fieldcat>-fieldname ) TO lt_duplicate_cells.
          ENDLOOP.
          CONTINUE.
        ELSE.
          INSERT lv_key_value INTO TABLE lt_seen_keys.
        ENDIF.
      ENDLOOP.

      " Handle duplicates
      IF lt_duplicate_cells IS NOT INITIAL.
        MESSAGE 'Duplicate key(s) found! Please correct them.' TYPE 'E'.
        mo_grid->set_selected_cells( it_cells = lt_duplicate_cells ).
        RETURN.
      ENDIF.

      LOOP AT <lt_data> ASSIGNING <ls_data>.
        ASSIGN COMPONENT 'MANDT' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fs_mandt>).
        IF sy-subrc = 0.
          <fs_mandt> = sy-mandt .
        ENDIF.
        APPEND INITIAL LINE TO <lt_db_data> ASSIGNING FIELD-SYMBOL(<ls_db_md_tab>).
        MOVE-CORRESPONDING <ls_data> TO <ls_db_md_tab>.
      ENDLOOP.

      SORT <lt_db_data> ASCENDING.
      SORT <lt_data> ASCENDING.
    ELSE.

      LOOP AT mt_deleted_rows ASSIGNING FIELD-SYMBOL(<ls_del_rows>).
        READ TABLE <lt_db> ASSIGNING FIELD-SYMBOL(<ls_db_del>) INDEX <ls_del_rows>.
        IF sy-subrc = 0 .
          APPEND <ls_db_del> TO <lt_delete>.
        ENDIF.
      ENDLOOP.

    ENDIF.

    CLEAR mt_deleted_rows.

    IF <lt_delete> IS NOT INITIAL.
      DELETE (mv_tabname) FROM TABLE <lt_delete>.
      UPDATE (mv_tabname) FROM TABLE <lt_db> .
      MESSAGE 'The data has been deleted successfully!' TYPE 'S'.

    ELSE.

      MODIFY (mv_tabname) FROM TABLE <lt_db_data> .
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'The data has been saved successfully!' TYPE 'S'.
      ELSE.
        MESSAGE 'Error while saving data!' TYPE 'E'.
      ENDIF.
    ENDIF.

    CALL METHOD mo_grid->refresh_table_display( ).
    mv_changed = ' ' .

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

        FIELD-SYMBOLS :   <ls_style> TYPE lvc_t_styl.

        APPEND INITIAL LINE TO <lt_data> ASSIGNING FIELD-SYMBOL(<fs_table_st>).

        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_table_st> TO <ls_style>.

        IF sy-subrc = 0 .
          APPEND VALUE #(
                          fieldname = ls_component-name
                          style = cl_gui_alv_grid=>mc_style_enabled ) TO <ls_style>.
        ENDIF.

        mo_grid->refresh_table_display(
                EXCEPTIONS
                  finished       = 1                " Display was Ended (by Export)
                  OTHERS         = 2
              ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        "End of VQ

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
        CLEAR: mv_tabname.
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

  METHOD top_of_page.

    DATA: lv_combined_text TYPE sdydo_text_element,
          lt_text_lines    TYPE TABLE OF tline,
          lv_text_line     TYPE string.

    CLEAR: lv_combined_text, lt_text_lines, lv_text_line.

    SELECT SINGLE a~tdname,
                  a~tabname,
                  b~tdid,
                  b~tdspras,
                  b~tdobject
      INTO @DATA(ls_text_name)
      FROM zkd_hier_doc AS a
      INNER JOIN stxh AS b
        ON a~tdname = b~tdname
      WHERE a~tabname = @mv_tabname
            AND b~tdspras = @sy-langu.

    IF sy-subrc = 0 AND ls_text_name-tdname IS NOT INITIAL.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = ls_text_name-tdid
          language = ls_text_name-tdspras
          name     = ls_text_name-tdname
          object   = ls_text_name-tdobject
        TABLES
          lines    = lt_text_lines
        EXCEPTIONS
          OTHERS   = 1.

      IF lt_text_lines IS NOT INITIAL.
        LOOP AT lt_text_lines ASSIGNING FIELD-SYMBOL(<ls_text_line>).
          lv_text_line = lv_text_line && <ls_text_line>-tdline && cl_abap_char_utilities=>cr_lf.
        ENDLOOP.

        lv_combined_text = CONV sdydo_text_element( lv_text_line ).
      ELSE.
        lv_combined_text = CONV sdydo_text_element( 'This table does not have documentation.' ).
      ENDIF.

    ELSE.
      lv_combined_text = CONV sdydo_text_element( 'This table does not have documentation.' ).
    ENDIF.

    mo_top_of_page->add_text(
      EXPORTING
        text          = lv_combined_text    " Single Text, Up To 255 Characters Long
  ).

    mo_top_of_page->merge_document( ).

    mo_top_of_page->display_document(
      EXPORTING
        reuse_control      = 'X'
        parent             = mo_top_area
      EXCEPTIONS
        html_display_error = 1
        OTHERS             = 2 ).

  ENDMETHOD.

  METHOD get_documentation.

    IF mv_tabname IS INITIAL.
      MESSAGE 'Please select a table before showing documentation.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE mv_show_hide.

      WHEN 'D'.
        DATA(lv_height) = 0.
        mv_show_hide = 'H'.
        mo_splitter->set_row_sash(   id    = 1
                                    type  = cl_gui_splitter_container=>type_sashvisible
                                    value = cl_gui_splitter_container=>false ).
      WHEN 'H'.
        lv_height = 15.
        mv_show_hide = 'D'.
        mo_splitter->set_row_sash(   id    = 1
                                    type  = cl_gui_splitter_container=>type_sashvisible
                                    value = cl_gui_splitter_container=>true ).
    ENDCASE.

    mo_splitter->set_row_height(
      EXPORTING
        id                =  1   " Row ID
        height            = lv_height     " Height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

  ENDMETHOD.

  METHOD free_selection.

    DATA: lt_expressions      TYPE rsds_texpr,
          lv_selection_id     TYPE rsdynsel-selid,
          lt_field_ranges     TYPE rsds_trange,
          lt_where_clauses    TYPE rsds_twhere,
          lt_tables           TYPE STANDARD TABLE OF rsdstabs,
          lt_fields           TYPE STANDARD TABLE OF rsdsfields,
          lv_number_of_fields TYPE i,
          lo_row              TYPE REF TO data.

    FIELD-SYMBOLS: <fs_row> TYPE any.

    IF mv_tabname IS INITIAL.
      MESSAGE 'Please select a table before free selection.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lt_tables = VALUE #( ( prim_tab = mv_tabname ) ).

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'T'
      IMPORTING
        selection_id             = lv_selection_id
      TABLES
        tables_tab               = lt_tables
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = lv_selection_id
        title           = 'Free Selection'
        as_window       = ' '
      IMPORTING
        where_clauses   = lt_where_clauses
      TABLES
        fields_tab      = lt_fields
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CREATE DATA lo_row LIKE LINE OF <fs_table>.
    ASSIGN lo_row->* TO <fs_row>.
    IF <fs_row> IS ASSIGNED.
      IF lines( lt_where_clauses ) > 0.
        DATA(ls_where_clause) = lt_where_clauses[ tablename = mv_tabname ].
        SELECT * FROM (mv_tabname) INTO CORRESPONDING FIELDS OF TABLE <lt_data> WHERE (ls_where_clause-where_tab).
      ELSE.
        SELECT * FROM (mv_tabname) INTO CORRESPONDING FIELDS OF TABLE <lt_data> UP TO 10 ROWS.
      ENDIF.
    ENDIF.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  go_hier = NEW #( ).
  go_hier->execute( ).

  CALL SCREEN 0001.

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

    WHEN 'FC_SELECT'.
      go_hier->free_selection( ).

    WHEN 'FC_TEXT'.
      go_hier->get_documentation( ).

  ENDCASE.

ENDMODULE.
