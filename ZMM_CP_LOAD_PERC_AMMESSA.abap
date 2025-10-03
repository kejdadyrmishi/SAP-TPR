*&---------------------------------------------------------------------*
*& Report ZMM_CP_LOAD_PERC_AMMESA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_cp_load_perc_ammessa.

TABLES: zmm_cp_perc_amm , sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_prgrp FOR zmm_cp_perc_amm-prgrp,
                  s_arbpl FOR zmm_cp_perc_amm-arbpl,
                  s_lifnr FOR zmm_cp_perc_amm-lifnr,
                  s_val_f FOR zmm_cp_perc_amm-valid_from,
                  s_val_t FOR zmm_cp_perc_amm-valid_to,
                  s_calo  FOR zmm_cp_perc_amm-zzcalo.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_selfields,
             scr_name TYPE char8,
             fld_name TYPE fieldname,
           END OF ty_selfields,
           tt_selfields TYPE STANDARD TABLE OF ty_selfields WITH EMPTY KEY.

    TYPES: BEGIN OF ty_excel_data,
             prgrp      TYPE zmm_cp_perc_amm-prgrp,
             arbpl      TYPE zmm_cp_perc_amm-arbpl,
             lifnr      TYPE zmm_cp_perc_amm-lifnr,
             valid_from TYPE zmm_cp_perc_amm-valid_from,
             valid_to   TYPE zmm_cp_perc_amm-valid_to,
             zzcalo     TYPE zmm_cp_perc_amm-zzcalo,
             excel_row  TYPE i,
           END OF ty_excel_data,
           tt_excel_data TYPE STANDARD TABLE OF ty_excel_data WITH EMPTY KEY.

    DATA: mt_sellist  TYPE STANDARD TABLE OF vimsellist,
          mv_filename TYPE string.

    METHODS :execute,
      at_selection_screen ,
      constructor.

  PRIVATE SECTION.

    METHODS : extract_data,
      upload_data,
      read_excel_data,
      download_template.

ENDCLASS.                    "

DATA go_report TYPE REF TO lcl_report.

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.
    extract_data( ).

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action      = 'S'
        view_name   = 'ZMM_CP_PERC_AMM'
      TABLES
        dba_sellist = mt_sellist
      EXCEPTIONS
        OTHERS      = 1.

  ENDMETHOD.                    "execute

  METHOD extract_data.

    FIELD-SYMBOLS: <lt_range> TYPE STANDARD TABLE.

    DATA(lt_selfields) = VALUE tt_selfields(
        ( scr_name = 'S_PRGRP' fld_name = 'PRGRP' )
        ( scr_name = 'S_ARBPL' fld_name = 'ARBPL' )
        ( scr_name = 'S_LIFNR' fld_name = 'LIFNR' )
        ( scr_name = 'S_VAL_F' fld_name = 'VALID_FROM' )
        ( scr_name = 'S_VAL_T' fld_name = 'VALID_TO' )
        ( scr_name = 'S_CALO'  fld_name = 'ZZCALO' ) ).

    LOOP AT lt_selfields ASSIGNING FIELD-SYMBOL(<ls_field>).

      DATA(lv_range) = |{ <ls_field>-scr_name }[]|.

      ASSIGN (lv_range) TO <lt_range>.

      CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
        EXPORTING
          fieldname          = <ls_field>-fld_name
          append_conjunction = 'AND'
        TABLES
          sellist            = mt_sellist
          rangetab           = <lt_range>.

    ENDLOOP.

  ENDMETHOD.                    "extract_data

  METHOD constructor.

    DATA: lt_buttons TYPE STANDARD TABLE OF smp_dyntxt.

    lt_buttons = VALUE #(
    ( icon_id =  icon_export  icon_text = 'Download Template'(002) )
    ( icon_id =  icon_import  icon_text = 'Upload'(003) ) ).

    LOOP AT lt_buttons ASSIGNING FIELD-SYMBOL(<ls_buttons>).
      CASE sy-tabix.
        WHEN 1.
          sscrfields-functxt_01 = <ls_buttons>.
        WHEN 2.
          sscrfields-functxt_02 = <ls_buttons>.
      ENDCASE. .
    ENDLOOP.
  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        download_template( ).
      WHEN 'FC02'.
        upload_data( ).
        read_excel_data( ).
    ENDCASE.
  ENDMETHOD.

  METHOD download_template.

    DATA: lv_filename    TYPE string,
          lt_raw_data    TYPE xml_rawdata,
          lv_size        TYPE i,
          lv_path        TYPE string,
          lv_fullpath    TYPE string,
          lv_action      TYPE i,
          lo_column      TYPE REF TO cl_salv_column_table,
          lt_excel_templ TYPE tt_excel_data.

    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = DATA(lo_salv)
         CHANGING
           t_table      = lt_excel_templ ).

        TRY.
            lo_salv->get_columns( )->get_column(
              EXPORTING
                columnname =  'EXCEL_ROW'
            )->set_technical( ).

          CATCH cx_salv_not_found. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.

        DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

        IF xstrlen( lv_xml ) > 0.

          cl_gui_frontend_services=>file_save_dialog(
            EXPORTING
              default_extension         = 'xlsx'
              default_file_name         = 'Template.xlsx'
              file_filter               = |Excel-Datei (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
              prompt_on_overwrite       = 'X'
            CHANGING
              filename                  = lv_filename
              path                      = lv_path
              fullpath                  = lv_fullpath
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
                im_xstring = lv_xml
              IMPORTING
                ex_xtab    = lt_raw_data
                ex_size    = lv_size
            ).

          ENDIF.

          cl_gui_frontend_services=>gui_download(
            EXPORTING
              filename                  =  lv_filename
              filetype                  = 'BIN'
              bin_filesize              =  lv_size
            CHANGING
              data_tab                  =  lt_raw_data
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

  METHOD upload_data.

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
      mv_filename = lt_filetable[ 1 ]-filename.
    ENDIF.

  ENDMETHOD.

  METHOD read_excel_data.
    TYPES: tt_perc_amm TYPE STANDARD TABLE OF zmm_cp_perc_amm WITH DEFAULT KEY.
    DATA: lt_data        TYPE solix_tab,
          lv_day         TYPE c LENGTH 2,
          lv_month       TYPE c LENGTH 2,
          lt_perc_amm    TYPE tt_perc_amm,
          lt_messages    TYPE esp1_message_tab_type,
          lt_excel_check TYPE tt_excel_data.

    FIELD-SYMBOLS: <lt_excel_upload> TYPE STANDARD TABLE.

    IF mv_filename IS INITIAL.
      MESSAGE 'Enter a file first'(013) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = mv_filename
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
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = mv_filename
                                               xdocument     = lv_bin_data ).


    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet_names) ).

    IF lt_worksheet_names IS INITIAL.

      MESSAGE 'Problem with excel file'(004) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_itab) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ 1 ] ).
    ASSIGN lo_worksheet_itab->* TO <lt_excel_upload>.
    DELETE <lt_excel_upload> INDEX 1.

    LOOP AT <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_excel_upload>).

      APPEND INITIAL LINE TO lt_excel_check ASSIGNING FIELD-SYMBOL(<ls_excel_check>).

      <ls_excel_check>-excel_row = lines( lt_excel_check ).

      DO 6 TIMES.

        DATA(lv_index) =  sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upload> TO FIELD-SYMBOL(<lv_exc_cell>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_check> TO FIELD-SYMBOL(<lv_exc_check_cell>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CASE lv_index .

          WHEN 4 OR 5.

            SPLIT <lv_exc_cell> AT '.' INTO lv_day lv_month DATA(lv_year).

            UNPACK lv_day TO lv_day.
            UNPACK lv_month TO lv_month.

            <lv_exc_check_cell> = lv_year && lv_month && lv_day.

            CONTINUE.

          WHEN 6. " curr

            REPLACE  ',' IN <lv_exc_cell> WITH '.'.

            <lv_exc_check_cell> = <lv_exc_cell>.

            CONTINUE.

          WHEN OTHERS.

            <lv_exc_check_cell> =   <lv_exc_cell> .
        ENDCASE.
      ENDDO.

      <ls_excel_check>-prgrp = |{ <ls_excel_check>-prgrp ALPHA = IN }|.
      <ls_excel_check>-lifnr = |{ <ls_excel_check>-lifnr ALPHA = IN }|.

      IF <ls_excel_check>-prgrp IS INITIAL
      OR <ls_excel_check>-valid_from IS INITIAL
      OR <ls_excel_check>-valid_to IS INITIAL.

        APPEND VALUE #(
              msgty     = 'E'
              msgid     = 'DB'
              msgno     = '000'
              msgv1     = TEXT-005 && <ls_excel_check>-excel_row
              msgv2     = TEXT-006
      ) TO lt_messages.

        CONTINUE.
      ENDIF.

      IF <ls_excel_check>-arbpl IS INITIAL AND <ls_excel_check>-lifnr IS INITIAL.

        APPEND VALUE #(
              msgty     = 'E'
              msgid     = 'DB'
              msgno     = '000'
              msgv1     = TEXT-005 && <ls_excel_check>-excel_row
              msgv2     = TEXT-007
      ) TO lt_messages.

        CONTINUE.
      ENDIF.

    ENDLOOP.

    IF lt_excel_check IS INITIAL.
      MESSAGE 'The excel file does not have any data'(008) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lt_messages IS NOT INITIAL.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_messages.

      RETURN.
    ENDIF.

    SELECT *
      FROM zmm_cp_perc_amm
      INTO TABLE @DATA(lt_existing)
      FOR ALL ENTRIES IN @lt_excel_check
      WHERE prgrp       = @lt_excel_check-prgrp
      AND   arbpl       = @lt_excel_check-arbpl
      AND   lifnr       = @lt_excel_check-lifnr.

    SORT lt_existing BY prgrp arbpl lifnr valid_from.

    LOOP AT lt_excel_check ASSIGNING <ls_excel_check>.

      READ TABLE lt_existing TRANSPORTING NO FIELDS
        WITH KEY prgrp      =  <ls_excel_check>-prgrp
                 arbpl      =  <ls_excel_check>-arbpl
                 lifnr      =  <ls_excel_check>-lifnr
                 valid_from =  <ls_excel_check>-valid_from BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_existing INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF CORRESPONDING tt_excel_data( lt_existing ) TO lt_excel_check.
    CLEAR lt_existing.

    SORT lt_excel_check BY prgrp arbpl lifnr valid_from.

    LOOP AT lt_excel_check ASSIGNING <ls_excel_check>.

      DATA(lv_next) = sy-tabix + 1.

      READ TABLE lt_excel_check ASSIGNING FIELD-SYMBOL(<ls_next>) INDEX lv_next.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_excel_check>-prgrp <> <ls_next>-prgrp
        OR <ls_excel_check>-arbpl <> <ls_next>-arbpl
        OR <ls_excel_check>-lifnr <> <ls_next>-lifnr.
        CONTINUE.
      ENDIF.

      IF <ls_excel_check>-excel_row IS NOT INITIAL.
        DATA(lv_exc_row) = <ls_excel_check>-excel_row.
      ELSE.
        lv_exc_row = <ls_next>-excel_row.
      ENDIF.

      IF <ls_excel_check>-valid_from = <ls_next>-valid_from.

        APPEND VALUE #(
                      msgty     = 'E'
                      msgid     = 'DB'
                      msgno     = '000'
                      msgv1     = TEXT-005 && lv_exc_row
                      msgv2     = TEXT-009
                      ) TO lt_messages.
        CONTINUE.
      ENDIF.

      IF <ls_excel_check>-valid_to >= <ls_next>-valid_from.

        APPEND VALUE #(
            msgty     = 'E'
            msgid     = 'DB'
            msgno     = '000'
            msgv1     = TEXT-005 && lv_exc_row
            msgv2     = TEXT-010
            ) TO lt_messages.

        CONTINUE.
      ENDIF.

    ENDLOOP.

    IF lt_messages IS NOT INITIAL.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_messages.

      RETURN.
    ENDIF.

    APPEND LINES OF CORRESPONDING tt_perc_amm( lt_excel_check ) TO lt_perc_amm.
    MODIFY zmm_cp_perc_amm FROM TABLE lt_perc_amm.
    IF sy-subrc = 0.
      MESSAGE 'Data saved successfully'(011) TYPE 'S'.
    ELSE.
      MESSAGE 'Data could not be saved'(012) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  CREATE OBJECT go_report.

AT SELECTION-SCREEN.
  go_report->at_selection_screen( ).

START-OF-SELECTION.
  go_report->execute( ).
