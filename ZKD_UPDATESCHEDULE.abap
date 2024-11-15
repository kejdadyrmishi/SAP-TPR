REPORT zkd_updateschedule.

TABLES: sscrfields.

PARAMETERS: p_file  TYPE string .
SELECTION-SCREEN FUNCTION KEY 1.

CLASS lcl_updateschedule DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS : execute,
      get_filename,
      at_selection_screen ,
      constructor.

  PRIVATE SECTION .

    TYPES: BEGIN OF ty_excel_data,
             sales_order_nr TYPE vbak-vbeln,
             itm_number     TYPE vbap-posnr,
             sched_line     TYPE vbep-etenr,
             req_date       TYPE vbep-edatu,
             date_type      TYPE vbep-prgrs,
             req_qty        TYPE vbep-wmeng,
             sched_type     TYPE vbep-etart,
             dlvschedno     TYPE labnk,
             dlvscheddate   TYPE abrdt,
           END OF ty_excel_data,
           tt_excel_data TYPE STANDARD TABLE OF ty_excel_data WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_buffer,
             sales_order_nr TYPE vbak-vbeln,
             itm_number     TYPE vbap-posnr,
             sched_line     TYPE vbep-etenr,
           END OF ty_buffer,
           tt_buffer TYPE SORTED TABLE OF ty_buffer WITH UNIQUE KEY sales_order_nr itm_number.

    TYPES: BEGIN OF ty_return,
             sales_order TYPE vbak-vbeln,
             id          TYPE i,
             type        TYPE string,
             message     TYPE string,
           END OF ty_return,
           tt_return TYPE STANDARD TABLE OF ty_return.

    DATA: mt_excel_data TYPE tt_excel_data.

    METHODS: read_excel_data ,
      display_message_return IMPORTING it_return TYPE tt_return,
      sales_doc_change,
      download_template.

ENDCLASS.

CLASS lcl_updateschedule IMPLEMENTATION.

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

  METHOD read_excel_data.

    DATA: lt_data  TYPE solix_tab,
          lv_day   TYPE c LENGTH 2,
          lv_month TYPE c LENGTH 2.

    FIELD-SYMBOLS: <lt_excel_upload> TYPE STANDARD TABLE.

    IF p_file IS INITIAL.
      MESSAGE 'Enter a file first' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = p_file
            filetype                = 'BIN'
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19
        ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = p_file
                                               xdocument     = lv_bin_data ).


    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet_names) ).

    IF lt_worksheet_names IS INITIAL.

      MESSAGE 'Problem with excel file' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_itab) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ 1 ] ).
    ASSIGN lo_worksheet_itab->* TO <lt_excel_upload>.
    DELETE <lt_excel_upload> INDEX 1.

    LOOP AT <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_excel_upload>).

      APPEND INITIAL LINE TO mt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel_new>).
      DO.

        DATA(lv_index) =  sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upload> TO FIELD-SYMBOL(<fs>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_new> TO FIELD-SYMBOL(<fs2>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CASE lv_index .

          WHEN 4 OR 9.

            SPLIT <fs> AT '.' INTO lv_day lv_month DATA(lv_year).

            UNPACK lv_day TO lv_day.
            UNPACK lv_month TO lv_month.

            <fs2> = lv_year && lv_month && lv_day.

            CONTINUE.

          WHEN OTHERS.

            <fs2> =   <fs> .
        ENDCASE.
      ENDDO.
    ENDLOOP.

    IF mt_excel_data IS INITIAL.
      MESSAGE 'The excel file does not have any data' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD sales_doc_change.

    DATA:
      lv_salesdocument    TYPE bapivbeln-vbeln,                      "Sales and Distribution Document Number
      lt_return           TYPE STANDARD TABLE OF bapiret2,           "Return Code
      lt_del_schedule_in  TYPE STANDARD TABLE OF bapisddelsched_in, "Communication fields: SD Release
      lt_del_schedule_inx TYPE STANDARD TABLE OF bapisddelsched_inx, "Checkbox for SD Release Maintenance.
      lt_schedule_in      TYPE STANDARD TABLE OF bapischdl,               " Schedule Lines
      lt_schedule_inx     TYPE STANDARD TABLE OF bapischdlx,            "Schedule Line Checkbox
      lt_display_return   TYPE tt_return,
      ls_display_return   TYPE ty_return,
      lv_count            TYPE i,
      ls_order_header_inx TYPE bapisdhd1x,
      lv_sched_line       TYPE vbep-etenr,
      lv_vbeln            TYPE vbeln,
      lt_buffer           TYPE tt_buffer.

    ls_order_header_inx-updateflag = 'U'.

    LOOP AT mt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel_data>)
      GROUP BY ( sales_order_nr = <ls_excel_data>-sales_order_nr )
      ASSIGNING FIELD-SYMBOL(<lg_sales_order_nr>).

      CLEAR: lt_return,
             lt_schedule_in,
             lt_schedule_inx,
             lt_del_schedule_in,
             lt_del_schedule_inx,
             lv_sched_line.

      lv_count = 1.

      LOOP AT GROUP <lg_sales_order_nr> ASSIGNING FIELD-SYMBOL(<ls_sales_order_nr>).

        IF <ls_sales_order_nr>-sched_line IS INITIAL.

          READ TABLE lt_buffer ASSIGNING FIELD-SYMBOL(<ls_buffer>) WITH KEY sales_order_nr = <ls_excel_data>-sales_order_nr  itm_number = <ls_excel_data>-itm_number .

          IF sy-subrc <> 0.

            INSERT VALUE #( sales_order_nr = <ls_excel_data>-sales_order_nr
                            itm_number = <ls_excel_data>-itm_number  ) INTO TABLE lt_buffer ASSIGNING <ls_buffer>.

            SELECT MAX( etenr )
                         FROM vbep
                         INTO <ls_buffer>-sched_line
                         WHERE vbeln = <ls_sales_order_nr>-sales_order_nr
                           AND posnr = <ls_sales_order_nr>-itm_number.

          ENDIF.

          <ls_buffer>-sched_line = <ls_buffer>-sched_line + 1 .
          <ls_sales_order_nr>-sched_line = <ls_buffer>-sched_line.
        ENDIF.

        lv_vbeln =   |{ <ls_sales_order_nr>-sales_order_nr ALPHA = IN }| .

        APPEND VALUE #( itm_number = <ls_sales_order_nr>-itm_number
                                                sched_line      = <ls_sales_order_nr>-sched_line
                                                req_date        = <ls_sales_order_nr>-req_date
                                                date_type       = <ls_sales_order_nr>-date_type
                                                req_qty         = <ls_sales_order_nr>-req_qty
                                                sched_type      = 'L1'
                                                rel_type        = '1'
                                                plan_sched_type = <ls_sales_order_nr>-sched_type ) TO lt_schedule_in.


        APPEND VALUE #( itm_number = <ls_sales_order_nr>-itm_number
                                                     sched_line      = COND #( WHEN <ls_sales_order_nr>-sched_line  IS INITIAL THEN lv_sched_line  ELSE <ls_sales_order_nr>-sched_line  )
                                                     updateflag      = COND #( WHEN <ls_sales_order_nr>-sched_line  IS NOT INITIAL THEN 'U' ELSE 'I')
                                                     req_date        = 'X'
                                                     date_type       = 'X'
                                                     req_qty         = 'X'
                                                     sched_type      = 'X'
                                                     rel_type        = 'X'
                                                     plan_sched_type = 'X' ) TO lt_schedule_inx .

        IF <ls_sales_order_nr>-dlvschedno IS NOT INITIAL AND <ls_sales_order_nr>-dlvscheddate IS NOT INITIAL.

          APPEND VALUE bapisddelsched_in(   itm_number   = <ls_sales_order_nr>-itm_number
                                            rel_type     = '1'
                                            dlvschedno   = <ls_sales_order_nr>-dlvschedno
                                            dlvscheddate = <ls_sales_order_nr>-dlvscheddate ) TO lt_del_schedule_in.


          APPEND VALUE bapisddelsched_inx(  itm_number   = <ls_sales_order_nr>-itm_number
                                            rel_type     = '1'
                                            dlvschedno   = 'X'
                                            dlvscheddate = 'X'
                                            updateflag   = 'U'  ) TO lt_del_schedule_inx.
        ELSE.
          CONTINUE.
        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'SD_SALESDOCUMENT_CHANGE'
        EXPORTING
          salesdocument    = lv_vbeln
          order_header_inx = ls_order_header_inx
        TABLES
          return           = lt_return
          schedule_in      = lt_schedule_in
          schedule_inx     = lt_schedule_inx
          del_schedule_in  = lt_del_schedule_in
          del_schedule_inx = lt_del_schedule_inx.

      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

        IF <ls_return>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.

        CASE <ls_return>-type.
          WHEN 'E'.
            DATA(lv_status) = icon_red_light.
          WHEN 'S'.
            lv_status = icon_green_light.
          WHEN 'W'.
            lv_status = icon_yellow_light.
        ENDCASE.

        APPEND  VALUE #( message = <ls_return>-message
                                    type    = lv_status
                                    id      = lv_count
                                    sales_order = lv_vbeln ) TO lt_display_return.

        lv_count = lv_count + 1.

      ENDLOOP.

      IF lv_error IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDLOOP.
    display_message_return( lt_display_return ).
  ENDMETHOD.

  METHOD display_message_return.

    DATA: lo_salv     TYPE REF TO cl_salv_table,
          lo_column   TYPE REF TO cl_salv_column_table,
          lo_tooltips TYPE REF TO cl_salv_tooltips,
          lv_value    TYPE lvc_value.

    DATA(lt_output) = it_return.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = lt_output ).


        lo_salv->get_columns( )->set_optimize( ).
        lo_salv->get_functions( )->set_all( ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'SALES_ORDER' ).
        lo_column->set_medium_text( 'Sales Order' ).
        lo_column->set_key( ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'ID' ).
        lo_column->set_medium_text( 'Line' ).
        lo_column->set_key( ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'TYPE' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'MESSAGE' ).
        lo_column->set_medium_text( 'Message' ).


*    Tooltips

        lo_tooltips = lo_salv->get_functional_settings( )->get_tooltips( ).

        lv_value = icon_green_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Success' ).

        lv_value = icon_yellow_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Warning' ).

        lv_value = icon_red_light.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = lv_value
          tooltip = 'Error' ).


      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_existing.

    ENDTRY.

* display the table
    lo_salv->display( ).

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        download_template( ).
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.

    DATA: ls_button TYPE smp_dyntxt.

    ls_button = VALUE #( icon_id =  icon_last_page
                         icon_text = 'Download Template' ).

    sscrfields-functxt_01 = ls_button.

  ENDMETHOD.

  METHOD download_template.

    DATA: lt_template TYPE tt_excel_data,
          lv_filename TYPE string,
          lt_raw_data TYPE xml_rawdata,
          lv_size     TYPE i,
          lv_path     TYPE string,
          lv_fullpath TYPE string,
          lv_action   TYPE i,
          lo_column   TYPE REF TO cl_salv_column_table.

    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = DATA(lo_salv)
         CHANGING
           t_table      = lt_template ).


        DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

        IF xstrlen( lv_xml ) > 0.

          cl_gui_frontend_services=>file_save_dialog(
            EXPORTING
              default_extension         = 'xlsx'    " Default Extension
              default_file_name         = 'Template.xlsx'   " Default File Name
              file_filter               = cl_gui_frontend_services=>filetype_excel  " File Type Filter Table
              prompt_on_overwrite       = 'X'
            CHANGING
              filename                  = lv_filename     " File Name to Save
              path                      = lv_path   " Path to File
              fullpath                  = lv_fullpath   " Path + File Name
            EXCEPTIONS
              cntl_error                = 1
              error_no_gui              = 2
              not_supported_by_gui      = 3
              invalid_default_file_name = 4
              OTHERS                    = 5
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.


          IF lv_action EQ cl_gui_frontend_services=>action_ok.

            cl_scp_change_db=>xstr_to_xtab(
              EXPORTING
                im_xstring = lv_xml    " XSTRING
              IMPORTING
                ex_xtab    = lt_raw_data   " Table of Type X
                ex_size    = lv_size    " Size of XSTRING
            ).

          ENDIF.

          cl_gui_frontend_services=>gui_download(
            EXPORTING
              filename                  =  lv_filename   " Name of file
              filetype                  = 'BIN'    " File type (ASCII, binary ...)
              bin_filesize              =  lv_size
            CHANGING
              data_tab                  =  lt_raw_data  " Transfer table
            EXCEPTIONS
              file_write_error          = 1
              no_batch                  = 2
              gui_refuse_filetransfer   = 3
              invalid_type              = 4
              no_authority              = 5
              unknown_error             = 6
              header_not_allowed        = 7
              separator_not_allowed     = 8
              filesize_not_allowed      = 9
              header_too_long           = 10
              dp_error_create           = 11
              dp_error_send             = 12
              dp_error_write            = 13
              unknown_dp_error          = 14
              access_denied             = 15
              dp_out_of_memory          = 16
              disk_full                 = 17
              dp_timeout                = 18
              file_not_found            = 19
              dataprovider_exception    = 20
              control_flush_error       = 21
              not_supported_by_gui      = 22
              error_no_gui              = 23
              OTHERS                    = 24
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_existing.

    ENDTRY.

  ENDMETHOD.
  METHOD execute.
    read_excel_data( ).
    sales_doc_change( ).
  ENDMETHOD.

ENDCLASS.

DATA: go_updateschedule TYPE REF TO lcl_updateschedule.

INITIALIZATION.
  CREATE OBJECT go_updateschedule.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  go_updateschedule->get_filename( ).

AT SELECTION-SCREEN.
  go_updateschedule->at_selection_screen( ).

START-OF-SELECTION.
  go_updateschedule->execute( ).
