REPORT zkd_email.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_smartf AS CHECKBOX,
            p_report AS CHECKBOX,
            p_image  AS CHECKBOX,
            p_alv    AS CHECKBOX,
            p_query  AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_email DEFINITION.

  PUBLIC SECTION.

    DATA : mv_xstring TYPE xstring.

    METHODS:execute,
      send_email,
      attach_smartform CHANGING cv_xstring TYPE xstring,
      attach_alv CHANGING cv_alv_xstring TYPE xstring,
      attach_image CHANGING ct_data_tab   TYPE solix_tab,
      attach_report CHANGING cv_xstring TYPE xstring,
      attach_query CHANGING cv_query_xstring TYPE xstring.

    METHODS : build_message
      IMPORTING
        iv_sender         TYPE sy-uname
      CHANGING
        cv_pdf_count      TYPE i
        cv_excel_count    TYPE i
        cv_image_count    TYPE i
      RETURNING
        VALUE(rv_message) TYPE string.
ENDCLASS.

CLASS lcl_email IMPLEMENTATION.

  METHOD execute.

    IF p_smartf = abap_true OR p_report = abap_true OR
   p_image  = abap_true OR p_alv  = abap_true OR
   p_query  = abap_true.
      send_email( ).
    ELSE.
      MESSAGE: 'No email sent as no options were selected' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.
  METHOD build_message.

    IF p_smartf = abap_true.
      cv_pdf_count = cv_pdf_count + 1.
    ENDIF.

    IF p_report = abap_true.
      cv_pdf_count = cv_pdf_count + 1.
    ENDIF.

    IF p_image = abap_true.
      cv_image_count = cv_image_count + 1.
    ENDIF.

    IF p_alv = abap_true.
      cv_excel_count = cv_excel_count + 1.
    ENDIF.

    IF p_query = abap_true.
      cv_excel_count = cv_excel_count + 1.
    ENDIF.

    rv_message = |Hello { iv_sender },| &&
                 |<br><br>Please find attached the following documents.<br><br>Best regards,<br>Sap Team<br><br><br><br>| &&
                 '<table style="border: 1px solid black; border-collapse: collapse;">' &&
                 '<tr>' &&
                 '<th style="border: 1px solid black;">Document Type</th>' &&
                 '<th style="border: 1px solid black;">Count</th>' &&
                 '</tr>' &&
                 '<tr>' &&
                 '<td style="border: 1px solid black;">PDF</td>' &&
                 '<td style="border: 1px solid black;">' && cv_pdf_count && '</td>' &&
                 '</tr>' &&
                 '<tr>' &&
                 '<td style="border: 1px solid black;">Excel</td>' &&
                 '<td style="border: 1px solid black;">' && cv_excel_count && '</td>' &&
                 '</tr>' &&
                 '<tr>' &&
                 '<td style="border: 1px solid black;">Image</td>' &&
                 '<td style="border: 1px solid black;">' && cv_image_count && '</td>' &&
                 '</tr>' &&
                 '</table>'.
  ENDMETHOD.

  METHOD attach_smartform.
    DATA :  lt_job_output   TYPE ssfcrescl,
            lt_lines        TYPE TABLE OF tline,
            lt_otf          TYPE STANDARD TABLE OF itcoo,
            lt_bin_filesize TYPE so_obj_len.

    SUBMIT zkd_smartform AND RETURN.
    IMPORT  job_output_info-otfdata TO lt_job_output-otfdata FROM MEMORY ID 'OTF'.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lt_bin_filesize
        bin_file              = cv_xstring
      TABLES
        otf                   = lt_job_output-otfdata
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

  ENDMETHOD.

  METHOD attach_alv.

    DATA: lv_xstring  TYPE xstring,
          lo_alv_data TYPE REF TO data,
          lo_salv     TYPE REF TO cl_salv_table.

    cl_salv_bs_runtime_info=>set(
      EXPORTING
        display        = abap_false
        metadata       = abap_false
        data           = abap_true ).

    SUBMIT zkd_alv_tot_salv AND RETURN.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING
        r_data                     = lo_alv_data ).

        ASSIGN lo_alv_data->* TO FIELD-SYMBOL(<ft_alv>).
      CATCH cx_salv_bs_sc_runtime_info.    "
        MESSAGE 'Unable to retrieve ALV data' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = <ft_alv> ).

      CATCH cx_salv_msg
    cx_salv_not_found
    cx_salv_existing.
    ENDTRY.

    DATA(lt_fieldcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      EXPORTING
        r_columns      =  lo_salv->get_columns( )    " ALV Filter
        r_aggregations =  lo_salv->get_aggregations( )  ).

    DATA(lo_salv_ex_res) = cl_salv_ex_util=>factory_result_data_table(
                              r_data                 = lo_alv_data
                              t_fieldcatalog         = lt_fieldcat ).

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )    " XML Version to be Selected
        r_result_data = lo_salv_ex_res
        xml_type      = if_salv_bs_xml=>c_type_xlsx    " XML Type as SALV Constant
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui     " Constant
      IMPORTING
        xml           = cv_alv_xstring ).

  ENDMETHOD.

  METHOD attach_image.

    DATA: lv_filename   TYPE string VALUE 'C:\Users\Perdorues\OneDrive\Desktop\images.jpeg',
          lv_filelength TYPE i.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filelength
      TABLES
        data_tab                = ct_data_tab
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
        OTHERS                  = 17.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.

  METHOD attach_report.

    DATA: lt_out_parameters TYPE pri_params,
          lv_spool_id       TYPE tsp01_sp0r-rqid_char,
          lv_bin_length     TYPE i,
          lv_xstring_pdf    TYPE xstring.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        no_dialog              = abap_true
      IMPORTING
        out_parameters         = lt_out_parameters
      EXCEPTIONS
        archive_info_not_found = 1
        invalid_print_params   = 2
        invalid_archive_params = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
      RETURN.
    ENDIF.

    " Submit the report to generate spool
    SUBMIT zkd_detyre3 TO SAP-SPOOL SPOOL PARAMETERS lt_out_parameters
      WITHOUT SPOOL DYNPRO AND RETURN.

    GET PARAMETER ID 'SPI' FIELD lv_spool_id.

    " Convert spool to PDF
    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
      EXPORTING
        src_spoolid              = CONV tsp01-rqident( lv_spool_id )
        no_dialog                = abap_true
        pdf_destination          = 'X' " xstring
        get_size_from_format     = abap_true
      IMPORTING
        pdf_bytecount            = lv_bin_length
        bin_file                 = cv_xstring
      EXCEPTIONS
        err_no_abap_spooljob     = 1
        err_no_spooljob          = 2
        err_no_permission        = 3
        err_conv_not_possible    = 4
        err_bad_destdevice       = 5
        user_cancelled           = 6
        err_spoolerror           = 7
        err_temseerror           = 8
        err_btcjob_open_failed   = 9
        err_btcjob_submit_failed = 10
        err_btcjob_close_failed  = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD attach_query.

    TYPES:BEGIN OF ty_data,
            ebeln TYPE string,
            bukrs TYPE string,
            ernam TYPE string,
            ebelp TYPE string,
          END OF ty_data,
          tt_data TYPE STANDARD TABLE OF ty_data.

    DATA:  lt_ldata      TYPE STANDARD TABLE OF rsaqldata,
           lt_data_tab   TYPE tt_data,
           ls_data       TYPE ty_data,
           lo_salv       TYPE REF TO cl_salv_table,
           lo_query_data TYPE REF TO data,
           lo_column     TYPE REF TO cl_salv_column_table.

    CALL FUNCTION 'RSAQ_REMOTE_QUERY_CALL'
      EXPORTING
        workspace                   = 'G'
        query                       = CONV aqadef-quname( 'ZKD_QUERY' )
        usergroup                   = CONV aqadef-bgname( 'ZKD_USER_GR' )
        variant                     = ' '
        dbacc                       = 20
        skip_selscreen              = 'X'
        data_to_memory              = 'X'
        external_presentation       = ' '
      TABLES
        ldata                       = lt_ldata
      EXCEPTIONS
        no_usergroup                = 1
        no_query                    = 2
        query_locked                = 3
        generation_cancelled        = 4
        no_selection                = 5
        no_variant                  = 6
        just_via_variant            = 7
        no_submit_auth              = 8
        no_data_selected            = 9
        data_to_memory_not_possible = 10
        OTHERS                      = 11.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT  lt_ldata ASSIGNING FIELD-SYMBOL(<ls_ldata>) .

      SPLIT <ls_ldata> AT ';' INTO TABLE DATA(lt_excel_data).

      LOOP AT lt_excel_data ASSIGNING FIELD-SYMBOL(<ls_split>).
        IF ( sy-tabix < 21 ) .

          SPLIT <ls_split> AT ',' INTO : ls_data-ebeln ls_data-bukrs ls_data-ernam ls_data-ebelp.

          ls_data-ebeln = ls_data-ebeln+4.
          ls_data-bukrs = ls_data-bukrs+4.
          ls_data-ernam = ls_data-ernam+4.
          ls_data-ebelp = ls_data-ebelp+4.

          APPEND ls_data TO lt_data_tab.
          CLEAR ls_data.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    GET REFERENCE OF lt_data_tab INTO lo_query_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = lt_data_tab ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'EBELN' ).
        lo_column->set_medium_text( 'Document Number' ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'BUKRS' ).
        lo_column->set_medium_text( 'Company Code' ).


        lo_column ?= lo_salv->get_columns( )->get_column( 'ERNAM' ).
        lo_column->set_medium_text( 'Name of Person' ).

        lo_column ?= lo_salv->get_columns( )->get_column( 'EBELP' ).
        lo_column->set_medium_text( 'Item Number' ).

      CATCH cx_salv_msg
    cx_salv_not_found
    cx_salv_existing.
    ENDTRY.

    DATA(lt_fieldcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      EXPORTING
        r_columns      =  lo_salv->get_columns( )    " ALV Filter
        r_aggregations =  lo_salv->get_aggregations( )  ).

    DATA(lo_salv_ex_res) = cl_salv_ex_util=>factory_result_data_table(
                              r_data                 = lo_query_data
                              t_fieldcatalog         = lt_fieldcat ).

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )    " XML Version to be Selected
        r_result_data = lo_salv_ex_res
        xml_type      = if_salv_bs_xml=>c_type_xlsx    " XML Type as SALV Constant
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui     " Constant
      IMPORTING
        xml           = cv_query_xstring ).

  ENDMETHOD.

  METHOD send_email.

    DATA: lv_sender             TYPE uname,               " User sending the email
          lv_subject            TYPE string,
          lv_message            TYPE string,              " Email body message
          lv_rcp_email          TYPE ad_smtpadr,          " Recipient's email address
          ls_user_address       TYPE bapiaddr3,           " User's address details
          lt_return             TYPE STANDARD TABLE OF bapiret2,   " Return table from BAPI_USER_GET_DETAIL
          lv_html_table         TYPE string,
          lt_att_content        TYPE soli_tab,
          lv_pdf_count          TYPE i,
          lv_excel_count        TYPE i,
          lv_image_count        TYPE i,
          lt_binary_table       TYPE solix_tab,
          lt_binary_table_query TYPE solix_tab,
          lt_binary_tab         TYPE solix_tab,
          lt_data_tab           TYPE solix_tab,
          lv_bin_filesize       TYPE i.

    " Get user details
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        address  = ls_user_address
      TABLES
        return   = lt_return.

    lv_sender = sy-uname.
    lv_rcp_email = ls_user_address-e_mail.
    lv_rcp_email = 'test@gmail.com'.
    lv_subject =  'Test E-mail'.

    lv_message = build_message(  EXPORTING iv_sender = lv_sender
                        CHANGING cv_pdf_count = lv_pdf_count
                        cv_excel_count = lv_excel_count
                        cv_image_count = lv_image_count ).

    " Convert email body text to internal table
    DATA(it_body_txt) = cl_document_bcs=>string_to_soli( ip_string = lv_message ).

    TRY.
        " Create BCS(Business Communication Services) document for email
        DATA(lo_document) = cl_document_bcs=>create_document( i_type = 'HTM'
                                                             i_text = it_body_txt
                                                             i_subject = CONV so_obj_des( lv_subject ) ).

        " Add attachments to the email
        IF p_smartf = abap_true. "SMARTFORM
          attach_smartform( CHANGING cv_xstring = mv_xstring ).

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = mv_xstring
            TABLES
              binary_tab = lt_binary_table.

          lo_document->add_attachment( i_attachment_type = 'PDF'
                                        i_attachment_subject = CONV so_obj_des( 'SmartForm' )
                                        i_att_content_hex = lt_binary_table ).

        ENDIF.

        IF p_report = abap_true.  "SIMPLE REPORT
          attach_report( CHANGING cv_xstring = mv_xstring ).

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = mv_xstring
            TABLES
              binary_tab = lt_binary_tab.

          lo_document->add_attachment( i_attachment_type = 'PDF'
                                        i_attachment_subject = CONV so_obj_des( 'Report' )
                                        i_att_content_hex = lt_binary_tab ).

        ENDIF.

        IF p_image = abap_true. "IMAGE
          attach_image( CHANGING ct_data_tab = lt_data_tab ).

          lo_document->add_attachment( i_attachment_type = 'JPG'
                                        i_attachment_subject = CONV so_obj_des( 'Image' )
                                        i_att_content_hex = lt_data_tab ).

        ENDIF.

        IF p_alv = abap_true.  "ALV REPORT
          attach_alv( CHANGING cv_alv_xstring = mv_xstring ).

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = mv_xstring
            TABLES
              binary_tab = lt_binary_table.

          DATA :lv_filename  TYPE string,
                lv_text_line TYPE soli,
                lt_att_head  TYPE soli_tab,
                lv_size      TYPE i.

          lv_filename = 'alvreport.xlsx'.
          CONCATENATE '&SO_FILENAME=' lv_filename INTO lv_text_line.
          APPEND lv_text_line TO lt_att_head.

          lo_document->add_attachment( i_attachment_type = 'BIN'
                                        i_attachment_subject = CONV so_obj_des( 'ALVreport' )
                                        i_attachment_size = CONV sood-objlen( lv_size )
                                        i_att_content_hex = lt_binary_table
                                        i_attachment_header = lt_att_head ).

        ENDIF.

        IF p_query = abap_true.  "QUERY
          attach_query( CHANGING cv_query_xstring = mv_xstring ).

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = mv_xstring
            TABLES
              binary_tab = lt_binary_table_query.

          DATA :lv_filename_query  TYPE string,
                lv_text_line_query TYPE soli,
                lt_att_head_query  TYPE soli_tab,
                lv_size_query      TYPE i.

          lv_filename = 'query.xlsx'.
          CONCATENATE '&SO_FILENAME=' lv_filename INTO lv_text_line_query.
          APPEND lv_text_line_query TO lt_att_head_query.

          lo_document->add_attachment( i_attachment_type = 'BIN'
                                        i_attachment_subject = CONV so_obj_des( 'Query' )
                                        i_attachment_size = CONV sood-objlen( lv_size )
                                        i_att_content_hex = lt_binary_table_query
                                        i_attachment_header = lt_att_head_query ).
        ENDIF.

        " Create BCS send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).
        lo_send_request->set_message_subject( ip_subject = lv_subject ).
        lo_send_request->set_document( lo_document ).

        " Set sender's information
        DATA(lo_sender) = cl_sapuser_bcs=>create( lv_sender ).
        lo_send_request->set_sender( lo_sender ).

        " Set recipient's information
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( lv_rcp_email ).
        lo_send_request->add_recipient( i_recipient = lo_recipient ).

        " Send the email
        lo_send_request->send( i_with_error_screen = abap_true ).
        COMMIT WORK.
*    *    " Check if the email was sent successfully
        IF sy-subrc = 0.
          MESSAGE 'The email was sent successfully!' TYPE 'S'.
        ENDIF.
      CATCH cx_root INTO DATA(e_text).
        MESSAGE 'Error!' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(go_email) = NEW lcl_email( ).
  go_email->execute( ).
