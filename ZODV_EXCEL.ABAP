*&---------------------------------------------------------------------*
*& Report  ZODV_EXCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zodv_excel.

TABLES: sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS :p_auart  TYPE vbak-auart OBLIGATORY,
            p_file   TYPE string,
            p_test   RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X',
            p_upload RADIOBUTTON GROUP rb.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_odv_excel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_odv_excel DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS :cv_tab_hdr      TYPE tabname   VALUE 'BAPISDHD1',
               cv_tab_itm      TYPE tabname   VALUE 'BAPISDITM',
               cv_tab_so       TYPE tabname   VALUE 'BAPIVBELN',
               cv_tab_partn    TYPE tabname   VALUE 'BAPIPARNR',
               cv_sales_ord    TYPE fieldname VALUE 'VBELN',
               cv_status       TYPE fieldname VALUE 'STATUS',
               cv_itm_number   TYPE fieldname VALUE 'ITM_NUMBER',
               cv_partn_num_we TYPE fieldname VALUE 'PARTN_NUMB_WE',
               cv_partn_num_ag TYPE fieldname VALUE 'PARTN_NUMB_AG',
               cv_partn_num    TYPE fieldname VALUE 'PARTN_NUMB',
               cv_we           TYPE fieldname VALUE 'WE',
               cv_ag           TYPE fieldname VALUE 'AG',
               cv_doc_type     TYPE fieldname VALUE 'DOC_TYPE',
               cv_sales_org    TYPE fieldname VALUE 'SALES_ORG',
               cv_distr_chan   TYPE fieldname VALUE 'DISTR_CHAN',
               cv_division     TYPE fieldname VALUE 'DIVISION',
               cv_material     TYPE fieldname VALUE 'MATERIAL'.

    METHODS :execute,
      at_selection_screen ,
      constructor,
      status_0100,
      user_command_0100,
      get_filename.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_custom_tab,
             auart     TYPE auart,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             position  TYPE i,
           END OF ty_custom_tab,
           tt_custom_tab TYPE STANDARD TABLE OF ty_custom_tab,

           BEGIN OF ty_map_fields,
             tabname   TYPE ddobjname,
             fieldname TYPE fieldname,
           END OF ty_map_fields,
           tt_map_fields TYPE STANDARD TABLE OF ty_map_fields WITH EMPTY KEY,

           BEGIN OF ty_bapi_so,
             vbeln TYPE vbeln,
             hdr   TYPE bapisdhd1,
             hdrx  TYPE bapisdhd1x,
             itm   TYPE STANDARD TABLE OF bapisditm  WITH KEY itm_number,
             itmx  TYPE STANDARD TABLE OF bapisditmx WITH KEY itm_number,
             partn TYPE STANDARD TABLE OF bapiparnr  WITH KEY partn_numb,
           END OF ty_bapi_so,
           tt_bapi_so TYPE SORTED TABLE OF ty_bapi_so WITH UNIQUE KEY vbeln,

           BEGIN OF ty_messages,
             icon        TYPE icon-id,
             sales_order TYPE vbeln,
             excel_so    TYPE vbeln,
             message     TYPE bapi_msg,
           END OF ty_messages,
           tt_messages TYPE STANDARD TABLE OF ty_messages.

    DATA: mt_messages  TYPE tt_messages,
          mo_alv_grid  TYPE REF TO cl_gui_alv_grid,
          mo_cust_grid TYPE REF TO cl_gui_custom_container,
          mt_hdr_map   TYPE tt_map_fields,
          mt_itm_map   TYPE tt_map_fields,
          mt_bapi_so   TYPE tt_bapi_so,
          mo_output    TYPE REF TO data,
          mt_fcat      TYPE lvc_t_fcat,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mo_salv_msg  TYPE REF TO cl_salv_table.

    METHODS :
      get_aufnr_cust,
      get_comp_descr IMPORTING it_map        TYPE tt_map_fields
                     EXPORTING et_comp       TYPE abap_component_tab
                               et_field_desc TYPE zcl_fdt_xl_spreadsheet=>tt_desc,
      download_template,
      upload_excel_and_map,
      create_sales_order IMPORTING iv_test TYPE abap_bool
                                   iv_from TYPE i
                                   iv_to   TYPE i,
      upload_odv_al11,
      display_output,
      create_output_data ,
      get_type IMPORTING iv_name             TYPE string
               RETURNING VALUE(ro_descr_ref) TYPE REF TO cl_abap_datadescr,

      display_msg ,
      adjust_cols_msg,
      handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

ENDCLASS.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE syucomm,
      END OF gs_screen100.
*----------------------------------------------------------------------*
*       CLASS lcl_odv_excel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_odv_excel IMPLEMENTATION.
  METHOD user_command_0100.

    DATA lt_unique_vbeln TYPE SORTED TABLE OF vbeln WITH UNIQUE KEY table_line.

    CASE gs_screen100-ok_code.
      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.

      WHEN 'FC_POST'.

        DATA:lt_selected_rows TYPE lvc_t_roid.
        FIELD-SYMBOLS : <lt_output_tab> TYPE STANDARD TABLE.

        ASSIGN mo_output->* TO <lt_output_tab>.

        CLEAR mt_messages.

        mo_alv_grid->get_selected_rows(
          IMPORTING
            et_row_no     =  lt_selected_rows   " Numeric IDs of Selected Rows
        ).

        IF lt_selected_rows IS INITIAL.
          MESSAGE 'Please, select at least 1 line to continue!'  TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_sel_rows>).
          READ TABLE <lt_output_tab> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_sel_rows>-row_id.

          IF sy-subrc = 0.

            ASSIGN COMPONENT cv_sales_ord OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_vbeln>).
            READ TABLE lt_unique_vbeln TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <lv_vbeln>.

            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.

            INSERT <lv_vbeln> INTO TABLE lt_unique_vbeln.

            READ TABLE mt_bapi_so TRANSPORTING NO FIELDS WITH TABLE KEY vbeln = <lv_vbeln>.

            create_sales_order(
              EXPORTING
                iv_test  = abap_false
                iv_from  = sy-tabix
                iv_to    = sy-tabix
            ).

          ENDIF.
        ENDLOOP.

        display_msg( ).
    ENDCASE.

  ENDMETHOD.
  METHOD status_0100.
    DATA lt_exclude TYPE ui_functions.

    SET PF-STATUS 'ZODV_PF'.
    SET TITLEBAR 'ZODV_TB'.
  ENDMETHOD.
  METHOD execute.

    get_aufnr_cust( ).

    CASE abap_true.
      WHEN p_test.

        create_output_data( ).
        upload_excel_and_map( ).

        create_sales_order(
          EXPORTING
            iv_test  = abap_true
            iv_from = 1
            iv_to = lines( mt_bapi_so )
        ).

        display_msg( ).
        display_output( ).

      WHEN p_upload.
        upload_odv_al11( ). " job in background
    ENDCASE.

  ENDMETHOD.                    "execute

  METHOD get_aufnr_cust.

    DATA lt_auart_map     TYPE tt_custom_tab.

    CLEAR: mt_hdr_map,
           mt_itm_map.

    mt_hdr_map = VALUE #(
      ( fieldname = cv_sales_ord  tabname = cv_tab_so )
      tabname = cv_tab_hdr
      ( fieldname = cv_doc_type   )
      ( fieldname = cv_sales_org  )
      ( fieldname = cv_distr_chan )
      ( fieldname = cv_division   )
    ).

    lt_auart_map = VALUE #(  (  auart = p_auart tabname = 'BAPISDHD1' fieldname = 'COLLECT_NO'  position = 1 )
                             (  auart = p_auart tabname = 'BAPISDHD1' fieldname = 'SALES_OFF'   position = 2 )  ).

    LOOP AT lt_auart_map ASSIGNING FIELD-SYMBOL(<ls_auart_map>).
      APPEND VALUE #( fieldname = <ls_auart_map>-fieldname
                      tabname   = <ls_auart_map>-tabname ) TO mt_hdr_map.
    ENDLOOP.

    APPEND LINES OF VALUE tt_map_fields(
    ( fieldname = cv_partn_num_we tabname   = cv_tab_partn )
    ( fieldname = cv_partn_num_ag tabname   = cv_tab_partn )
    )
     TO mt_hdr_map .

    mt_itm_map = VALUE #(
      ( fieldname = cv_sales_ord tabname = cv_tab_so  )
      ( fieldname = cv_material  tabname = cv_tab_itm )
    ).

    lt_auart_map = VALUE #( (  auart = p_auart tabname = 'BAPISDITM' fieldname = 'PLANT'  position = 1 ) ).

    LOOP AT lt_auart_map ASSIGNING <ls_auart_map>.
      APPEND VALUE #( fieldname = <ls_auart_map>-fieldname
                      tabname   = <ls_auart_map>-tabname ) TO mt_itm_map.
    ENDLOOP.

    APPEND LINES OF VALUE tt_map_fields(
    ( fieldname = cv_partn_num_we tabname   = cv_tab_partn )
    ( fieldname = cv_partn_num_ag tabname   = cv_tab_partn )
    )
     TO mt_itm_map.

  ENDMETHOD.                    "extract_data

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_events    TYPE REF TO cl_salv_events_table.

    CREATE OBJECT mo_dock_msg
      EXPORTING
        repid = 'ZODV_EXCEL'
        dynnr = '100'
        ratio = 30
        side  = cl_gui_docking_container=>dock_at_bottom.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container = mo_dock_msg
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_messages ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    lo_functions = mo_salv_msg->get_functions( ).
    lo_functions->set_all( abap_true ).

    TRY.
        DATA(lv_icon) = icon_close.
        lo_functions->add_function(
          EXPORTING
            name     = 'FC_CLOSE'
            icon     = CONV #( lv_icon )
            tooltip  = 'Close'

            position = if_salv_c_function_position=>right_of_salv_functions
        ).
      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.

    adjust_cols_msg( ).

    lo_events = mo_salv_msg->get_event( ).
    SET HANDLER handle_logs_ucomm FOR lo_events.

    TRY.
        mo_dock_msg->set_visible(
          EXPORTING
            visible = abap_true
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    mo_salv_msg->refresh( ).
    mo_salv_msg->display( ).

  ENDMETHOD.
  METHOD get_filename.

    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             =  cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              =  lt_filetable
        rc                      =  lv_rc
        user_action             =  lv_user
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_filetable IS NOT INITIAL.
      p_file = lt_filetable[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD get_type.

    cl_abap_datadescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name  " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_type)    " Reference to description object
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ro_descr_ref ?= lo_type.
  ENDMETHOD.

  METHOD create_output_data.

    DATA lt_comp  TYPE abap_component_tab.

    APPEND VALUE #( name = cv_status
                    type = get_type( |CHAR8| )
                  ) TO lt_comp .

    mt_fcat = VALUE #(
          key = abap_true
        ( fieldname = cv_status
          reptext   = 'Stato'
          icon      = abap_true
          intlen    = 8
          inttype   = 'C' )
        ( fieldname = cv_sales_ord
          ref_field = cv_sales_ord
          ref_table = cv_tab_so )
        ( fieldname = cv_itm_number
          ref_field = cv_itm_number
          ref_table = cv_tab_itm )
        ).

    LOOP AT mt_hdr_map ASSIGNING FIELD-SYMBOL(<ls_hdr_map>).

      CASE <ls_hdr_map>-tabname.
        WHEN cv_tab_partn.
          DATA(lv_fieldname) = cv_partn_num.

          DATA(lv_parvw) = SWITCH tpar-parvw( <ls_hdr_map>-fieldname
                                              WHEN cv_partn_num_ag THEN cv_ag
                                              WHEN cv_partn_num_we THEN cv_we
                                              ).

          SELECT SINGLE tpart~vtext
            FROM tpar
            LEFT JOIN tpart
              ON tpar~parvw = tpart~parvw
            INTO @DATA(lv_descr)
            WHERE tpar~parvw = @lv_parvw
              AND spras      = @sy-langu.

        WHEN OTHERS.
          lv_fieldname = <ls_hdr_map>-fieldname.
      ENDCASE.


      APPEND VALUE #( name = <ls_hdr_map>-fieldname
                      type = get_type( |{ <ls_hdr_map>-tabname }-{ lv_fieldname }| )
                    ) TO lt_comp .

      APPEND VALUE #( fieldname = <ls_hdr_map>-fieldname
                      ref_field = lv_fieldname
                      ref_table = <ls_hdr_map>-tabname
                      emphasize = 'C700'
                      coltext   = lv_descr
                      scrtext_s = lv_descr
                      scrtext_m = lv_descr
                      scrtext_l = lv_descr
                      ) TO mt_fcat .
      CLEAR lv_descr.
    ENDLOOP.

    APPEND VALUE #( name = cv_itm_number
                    type = get_type( |{ cv_tab_itm }-{ cv_itm_number }| )
                  ) TO lt_comp .

    LOOP AT mt_itm_map ASSIGNING FIELD-SYMBOL(<ls_itm_map>).

      CASE <ls_itm_map>-tabname.
        WHEN cv_tab_partn.

          lv_fieldname = cv_partn_num.

          lv_parvw = SWITCH tpar-parvw( <ls_hdr_map>-fieldname
                                     WHEN cv_partn_num_ag THEN cv_ag
                                     WHEN cv_partn_num_we THEN cv_we
                                     ).

          SELECT SINGLE tpart~vtext
            FROM tpar
            LEFT JOIN tpart
              ON tpar~parvw = tpart~parvw
            INTO @lv_descr
            WHERE tpar~parvw = @lv_parvw
              AND spras      = @sy-langu.

        WHEN OTHERS.
          lv_fieldname = <ls_itm_map>-fieldname.
      ENDCASE.

      DATA(lv_field) = 'P_' && <ls_itm_map>-fieldname.

      APPEND VALUE #( name = lv_field
                      type = get_type( |{ <ls_itm_map>-tabname }-{ lv_fieldname }| )
                    ) TO lt_comp .

      APPEND VALUE #( fieldname = lv_field
                      ref_field = lv_fieldname
                      ref_table = <ls_itm_map>-tabname
                      coltext   = lv_descr
                      scrtext_s = lv_descr
                      scrtext_m = lv_descr
                      scrtext_l = lv_descr
                      ) TO mt_fcat .
      CLEAR lv_descr.
    ENDLOOP.

    DATA(lo_handle_s) = cl_abap_structdescr=>create( lt_comp ).

    TRY.
        DATA(lo_handle_t) =
        cl_abap_tabledescr=>create(
          EXPORTING
            p_line_type          = lo_handle_s    " Line Type
        ).

        CREATE DATA mo_output TYPE HANDLE lo_handle_t.

      CATCH cx_sy_table_creation.
    ENDTRY.

  ENDMETHOD.
  METHOD display_output.

    FIELD-SYMBOLS : <lt_output_tab> TYPE STANDARD TABLE.

    ASSIGN mo_output->* TO <lt_output_tab>.

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid  = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).

    DATA(ls_layout) = VALUE lvc_s_layo(
                          cwidth_opt = abap_true
                          sel_mode   = 'D'  ).

    DATA(lt_sort) = VALUE lvc_t_sort(
                            ( spos      = 1
                              fieldname = cv_sales_ord
                             )
                            ( spos      = 2
                              fieldname = cv_itm_number
                             )
                            ).

    LOOP AT mt_hdr_map ASSIGNING FIELD-SYMBOL(<ls_hdr_map>).
      APPEND VALUE #( spos      = lines( lt_sort ) + 1
                      fieldname = <ls_hdr_map>-fieldname
                    ) TO lt_sort.
    ENDLOOP.

    APPEND VALUE #( spos      = lines( lt_sort ) + 1
                    fieldname = cv_status
                  ) TO lt_sort.

    TRY.
        mo_alv_grid->set_table_for_first_display(
          EXPORTING
            is_layout                     = ls_layout    " Layout
          CHANGING
            it_outtab                     = <lt_output_tab>    " Output Table
            it_fieldcatalog               = mt_fcat    " Field Catalog
            it_sort                       = lt_sort    " Sort Criteria
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL SCREEN 100.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD create_sales_order.

    CONSTANTS cv_where TYPE string VALUE 'vbeln = <ls_bapi_so>-vbeln'.

    DATA: lv_salesdocument TYPE vbeln,
          lt_return        TYPE STANDARD TABLE OF bapiret2.

    FIELD-SYMBOLS <lt_out> TYPE STANDARD TABLE.

    ASSIGN mo_output->* TO <lt_out>.

    LOOP AT mt_bapi_so ASSIGNING FIELD-SYMBOL(<ls_bapi_so>) FROM iv_from TO iv_to.

      CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
        EXPORTING
          order_header_in  = <ls_bapi_so>-hdr
          order_header_inx = <ls_bapi_so>-hdrx
          testrun          = iv_test
        IMPORTING
          salesdocument    = lv_salesdocument
        TABLES
          return           = lt_return
          order_items_in   = <ls_bapi_so>-itm
          order_items_inx  = <ls_bapi_so>-itmx
          order_partners   = <ls_bapi_so>-partn.

      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

        IF <ls_return>-type CA 'EAX'.
          DATA(lv_error_bapi) = abap_true.
        ENDIF.

        APPEND  VALUE #(  icon    = SWITCH #(
                        <ls_return>-type
                          WHEN 'E' THEN icon_red_light
                          WHEN 'S' THEN icon_green_light
                          WHEN 'W' THEN icon_yellow_light )
                          sales_order = lv_salesdocument
                          excel_so    = <ls_bapi_so>-vbeln
                          message     = <ls_return>-message
                         ) TO mt_messages.

      ENDLOOP.

      LOOP AT <lt_out> ASSIGNING FIELD-SYMBOL(<ls_out>) WHERE (cv_where).
        ASSIGN COMPONENT cv_status OF STRUCTURE <ls_out> TO FIELD-SYMBOL(<lv_status>).

        IF lv_error_bapi IS NOT INITIAL.
          <lv_status> = icon_red_light.
        ELSE.
          <lv_status> = icon_green_light.
        ENDIF.
      ENDLOOP.

      IF lv_error_bapi IS NOT INITIAL.
        CLEAR lv_error_bapi.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDLOOP.

  ENDMETHOD.

  METHOD upload_odv_al11.

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        download_template( ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.

    DATA: ls_button TYPE smp_dyntxt.

    ls_button = VALUE #( icon_id   = icon_export
                         icon_text = 'Download Template' ).

    sscrfields-functxt_01 = ls_button.

  ENDMETHOD.

  METHOD get_comp_descr.

    DATA: lv_tabname   TYPE dd03l-tabname,
          lv_fieldname TYPE dd03l-fieldname,
          lt_dfies     TYPE TABLE OF dfies.

    LOOP AT it_map ASSIGNING FIELD-SYMBOL(<ls_map>).

      CASE <ls_map>-tabname.
        WHEN cv_tab_partn.

          lv_fieldname = cv_partn_num.

          APPEND INITIAL LINE TO et_field_desc ASSIGNING FIELD-SYMBOL(<ls_field_desc>).
          <ls_field_desc>-fieldname = <ls_map>-fieldname.

          DATA(lv_parvw) = SWITCH tpar-parvw( <ls_map>-fieldname
                                              WHEN cv_partn_num_ag THEN cv_ag
                                              WHEN cv_partn_num_we THEN cv_we
                                              ).

          SELECT SINGLE tpart~vtext
            FROM tpar
            LEFT JOIN tpart
              ON tpar~parvw = tpart~parvw
            INTO @<ls_field_desc>-descr
            WHERE tpar~parvw = @lv_parvw
              AND spras      = @sy-langu.

        WHEN OTHERS.
          lv_fieldname = <ls_map>-fieldname.

          CALL FUNCTION 'DDIF_FIELDINFO_GET'
            EXPORTING
              tabname        = <ls_map>-tabname
              fieldname      = lv_fieldname
              langu          = sy-langu
            TABLES
              dfies_tab      = lt_dfies
            EXCEPTIONS
              not_found      = 1
              internal_error = 2
              OTHERS         = 3.

          READ TABLE lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>) INDEX 1.
          IF sy-subrc = 0.
            APPEND VALUE #( fieldname = <ls_map>-fieldname
                            descr     = <ls_dfies>-scrtext_l  ) TO et_field_desc.
            CLEAR lt_dfies.
          ENDIF.

      ENDCASE.

      DATA(lo_type) = get_type( |{ <ls_map>-tabname }-{ lv_fieldname }| ).

      APPEND VALUE #( name = <ls_map>-fieldname
                      type = lo_type ) TO et_comp .

    ENDLOOP.

  ENDMETHOD.

  METHOD download_template.

    DATA: lo_hdr      TYPE REF TO data,
          lo_itm      TYPE REF TO data,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_fullpath TYPE string,
          lv_path     TYPE string,
          lt_data     TYPE zcl_fdt_xl_spreadsheet=>tt_sheet_data.

    FIELD-SYMBOLS : <lt_hdr> TYPE STANDARD TABLE,
                    <lt_itm> TYPE STANDARD TABLE.

    get_aufnr_cust( ).

    get_comp_descr(
      EXPORTING
         it_map        = mt_hdr_map
       IMPORTING
         et_comp       = DATA(lt_hdr_comp)
         et_field_desc = DATA(lt_hdr_fdesc)
    ).

    get_comp_descr(
      EXPORTING
         it_map        = mt_itm_map
       IMPORTING
         et_comp       = DATA(lt_itm_comp)
         et_field_desc = DATA(lt_itm_fdesc)
    ).

    DATA(lo_handle_s_hdr) = cl_abap_structdescr=>create( lt_hdr_comp ).

    TRY.
        DATA(lo_handle_t_hdr) =
        cl_abap_tabledescr=>create(
          EXPORTING
            p_line_type          = lo_handle_s_hdr    " Line Type
        ).

        CREATE DATA lo_hdr TYPE HANDLE lo_handle_t_hdr.
        ASSIGN lo_hdr->* TO <lt_hdr>.

      CATCH cx_sy_table_creation.
    ENDTRY.

    DATA(lo_handle_s_itm) = cl_abap_structdescr=>create( lt_itm_comp ).

    TRY.
        DATA(lo_handle_t_itm) =
        cl_abap_tabledescr=>create(
          EXPORTING
            p_line_type          = lo_handle_s_itm    " Line Type
        ).

        CREATE DATA lo_itm TYPE HANDLE lo_handle_t_itm.
        ASSIGN lo_itm->* TO <lt_itm>.

      CATCH cx_sy_table_creation.
    ENDTRY.

    lt_data = VALUE #(
  ( sheet_name = 'Header'
    tab_data = REF #( <lt_hdr> )
    field_desc = lt_hdr_fdesc
  )
  ( sheet_name = 'Items'
    tab_data = REF #( <lt_itm> )
    field_desc = lt_itm_fdesc
  ) ).

    DATA(lv_bin_data) = zcl_fdt_xl_spreadsheet=>create_document(
                        name              = 'Template'
                        it_sheet_data     = lt_data ).

    cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                                   default_file_name = 'Template.xlsx'
                                                   default_extension = 'xlsx'
                                                   file_filter       = |Excel-Datei (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
                                                 CHANGING
                                                   filename          = lv_filename
                                                   path              = lv_path
                                                   fullpath          = lv_fullpath
                                                   user_action       = lv_action ).

    IF lv_action EQ cl_gui_frontend_services=>action_ok.

      DATA(it_raw_data) = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring = lv_bin_data ).

      cl_gui_frontend_services=>gui_download( EXPORTING
                                                filename     = lv_fullpath
                                                filetype     = 'BIN'
                                                bin_filesize = xstrlen( lv_bin_data )
                                              CHANGING
                                                data_tab     = it_raw_data ).

    ENDIF.

  ENDMETHOD.

  METHOD upload_excel_and_map.

    DATA: lv_filesize TYPE w3param-cont_len,
          lt_bin_data TYPE w3mimetabtype,
          lo_wsh_itm  TYPE REF TO data,
          lo_wsh_hdr  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_hdr_excel> TYPE STANDARD TABLE,
                   <lt_itm_excel> TYPE STANDARD TABLE,
                   <lt_output>    TYPE STANDARD TABLE.

    TRY.

        cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename   = p_file
            filetype   = 'BIN'
          IMPORTING
            filelength = lv_filesize
          CHANGING
            data_tab   = lt_bin_data ).

        DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin_data ).
        DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = 'Excel'
                                                    xdocument     = lv_bin_data ).

        lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_wsh_names) ).

        IF lines( lt_wsh_names ) < 2.
          RETURN.
        ENDIF.

        ASSIGN mo_output->*   TO <lt_output>.

        lo_wsh_hdr = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_wsh_names[ 1 ] ).
        ASSIGN lo_wsh_hdr->*  TO <lt_hdr_excel>.

* loop first sheet = Header => Fill bapi
        LOOP AT <lt_hdr_excel> ASSIGNING FIELD-SYMBOL(<ls_hdr_excel>) FROM 2.
          ASSIGN COMPONENT 1 OF STRUCTURE <ls_hdr_excel> TO FIELD-SYMBOL(<lv_vbeln>).
          IF <lv_vbeln> IS INITIAL.
*            vbeln can not be empty
            CONTINUE.
          ENDIF.

*          duplicate vbeln
          READ TABLE mt_bapi_so TRANSPORTING NO FIELDS WITH TABLE KEY vbeln = <lv_vbeln>.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

          INSERT VALUE #( vbeln = <lv_vbeln> ) INTO TABLE mt_bapi_so ASSIGNING FIELD-SYMBOL(<ls_bapi_so>).

          LOOP AT mt_hdr_map ASSIGNING FIELD-SYMBOL(<ls_hdr_map>) FROM 2.

            ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_hdr_excel> TO FIELD-SYMBOL(<lv_field_source>).
            IF sy-subrc <> 0.
*            excel template <> customizing template
              EXIT.
            ENDIF.

            CASE <ls_hdr_map>-tabname.
              WHEN cv_tab_hdr.

                ASSIGN COMPONENT <ls_hdr_map>-fieldname OF STRUCTURE <ls_bapi_so>-hdr TO FIELD-SYMBOL(<lv_field_target>).
                IF sy-subrc <> 0.
*            customizing template with errors - field does not exists in table
                  EXIT.
                ENDIF.

                ASSIGN COMPONENT <ls_hdr_map>-fieldname OF STRUCTURE <ls_bapi_so>-hdrx TO FIELD-SYMBOL(<lv_field_targetx>).

                <lv_field_target>  = <lv_field_source>.
                <lv_field_targetx> = abap_true.

              WHEN cv_tab_partn.

                IF <lv_field_source> IS INITIAL.
                  CONTINUE.
                ENDIF.

                APPEND VALUE #( partn_role = SWITCH #( <ls_hdr_map>-fieldname
                                                      WHEN cv_partn_num_ag THEN cv_ag
                                                      WHEN cv_partn_num_we THEN cv_we
                                                      )
                                partn_numb = |{ <lv_field_source> ALPHA = IN }| ) TO <ls_bapi_so>-partn .

            ENDCASE.
          ENDLOOP.

        ENDLOOP.

        lo_wsh_itm = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_wsh_names[ 2 ] ).
        ASSIGN lo_wsh_itm->*  TO <lt_itm_excel>.

* loop second sheet = items => Fill bapi and output
        LOOP AT <lt_itm_excel> ASSIGNING FIELD-SYMBOL(<ls_itm_excel>) FROM 2.
          ASSIGN COMPONENT 1 OF STRUCTURE <ls_itm_excel> TO <lv_vbeln>.
          IF <lv_vbeln> IS INITIAL.
*            vbeln can not be empty
            CONTINUE.
          ENDIF.

*          vbeln was not inserted in the header but only in items
          READ TABLE mt_bapi_so ASSIGNING <ls_bapi_so> WITH TABLE KEY vbeln = <lv_vbeln>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          APPEND INITIAL LINE TO <lt_output>  ASSIGNING FIELD-SYMBOL(<ls_output>).
          MOVE-CORRESPONDING <ls_bapi_so>-hdr TO <ls_output>.

          ASSIGN COMPONENT cv_sales_ord OF STRUCTURE <ls_output> TO FIELD-SYMBOL(<lv_out_vbeln>).
          <lv_out_vbeln> = <lv_vbeln>.

          APPEND INITIAL LINE TO <ls_bapi_so>-itm  ASSIGNING FIELD-SYMBOL(<ls_bapi_itm>).
          APPEND INITIAL LINE TO <ls_bapi_so>-itmx ASSIGNING FIELD-SYMBOL(<ls_bapi_itmx>).

          <ls_bapi_itm>-itm_number   = lines( <ls_bapi_so>-itm ) * 10.
          <ls_bapi_itmx>-itm_number  = <ls_bapi_itm>-itm_number.

          ASSIGN COMPONENT cv_itm_number OF STRUCTURE <ls_output> TO FIELD-SYMBOL(<lv_out_posnr>).
          <lv_out_posnr> = <ls_bapi_itm>-itm_number.

* Fill output partner header
          LOOP AT <ls_bapi_so>-partn ASSIGNING FIELD-SYMBOL(<ls_partn>).

            DATA(lv_field) = SWITCH fieldname( <ls_partn>-partn_role
                                               WHEN cv_ag THEN cv_partn_num_ag
                                               WHEN cv_we THEN cv_partn_num_we
                                               ).

            ASSIGN COMPONENT lv_field OF STRUCTURE <ls_output> TO FIELD-SYMBOL(<lv_out_target>).
            IF sy-subrc = 0.
              <lv_out_target> = <ls_partn>-partn_numb.
            ENDIF.

          ENDLOOP.

* Fill output and bapi for items
          LOOP AT mt_itm_map ASSIGNING FIELD-SYMBOL(<ls_itm_map>) FROM 2.

            ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_itm_excel> TO <lv_field_source>.
            IF sy-subrc <> 0.
*            excel template <> customizing template
              EXIT.
            ENDIF.

            CASE <ls_itm_map>-tabname.
              WHEN cv_tab_itm.

                ASSIGN COMPONENT <ls_itm_map>-fieldname OF STRUCTURE <ls_bapi_itm> TO <lv_field_target>.
                IF sy-subrc <> 0.
*            customizing template with errors - field does not exists in table
                  EXIT.
                ENDIF.

                ASSIGN COMPONENT <ls_itm_map>-fieldname OF STRUCTURE <ls_bapi_itmx> TO <lv_field_targetx>.

                <lv_field_target>  = <lv_field_source>.
                <lv_field_targetx> = abap_true.

                ASSIGN COMPONENT |P_{ <ls_itm_map>-fieldname }| OF STRUCTURE <ls_output> TO <lv_out_target>.
                <lv_out_target> = <lv_field_source>.

              WHEN cv_tab_partn.

                IF <lv_field_source> IS INITIAL.
                  CONTINUE.
                ENDIF.

                APPEND VALUE #( itm_number = <ls_bapi_itm>-itm_number
                                partn_role = SWITCH #( <ls_itm_map>-fieldname
                                                      WHEN cv_partn_num_ag THEN cv_ag
                                                      WHEN cv_partn_num_we THEN cv_we
                                                      )
                                partn_numb = |{ <lv_field_source> ALPHA = IN }| ) TO <ls_bapi_so>-partn .

                ASSIGN COMPONENT |P_{ <ls_itm_map>-fieldname }| OF STRUCTURE <ls_output> TO <lv_out_target>.
                <lv_out_target> = <lv_field_source>.

            ENDCASE.

          ENDLOOP.

        ENDLOOP.


      CATCH cx_root INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_logs_ucomm.
    CASE e_salv_function.
      WHEN 'FC_CLOSE'.

        mo_dock_msg->set_visible(
      EXPORTING
        visible           = abap_false                 " Visible
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD adjust_cols_msg.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        DATA(lv_text) = CONV scrtext_l( 'Stato' ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( lv_text ).
        lo_column->set_icon( ).

        lv_text = CONV scrtext_l( 'Excel Sales Document' ).
        lo_column ?= lo_cols->get_column( 'EXCEL_SO' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( lv_text ).

      CATCH cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

DATA go_odv_excel TYPE REF TO lcl_odv_excel.

INITIALIZATION.
  CREATE OBJECT go_odv_excel.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  go_odv_excel->get_filename( ).

AT SELECTION-SCREEN.
  go_odv_excel->at_selection_screen( ).

START-OF-SELECTION.
  go_odv_excel->execute( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  go_odv_excel->status_0100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_odv_excel->user_command_0100( ).

ENDMODULE.
