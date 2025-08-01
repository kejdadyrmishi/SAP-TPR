*&---------------------------------------------------------------------*
*& Report ZLPFO_FOREC_MON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlpfo_forec_mon.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_werks TYPE werks_d MODIF ID bl1,
            p_forty TYPE zlpfo_fortype MODIF ID bl1,
            p_round TYPE zlpfo_round MODIF ID bl1,
            p_foyer TYPE zlpfo_foryear MODIF ID bl1,
            p_prpnt TYPE werks_d MODIF ID bl1,
            r_exprt RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X', " Export file structure
            r_uplfi RADIOBUTTON GROUP rb, " Upload file
            r_exfil TYPE string MODIF ID bl1. " Local file path and name

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_forecast_upload DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS :execute.
    CLASS-METHODS : get_filename.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_template_file,
             market    TYPE zlpfo_mon_h-market,
             matnr     TYPE zlpfo_mon_h-matnr,
             zzcolorex TYPE zlpfo_mon_h-zzcolorex,
             zzsellerx TYPE zlpfo_mon_h-zzsellerx,
             zzoptstrx TYPE zlpfo_mon_h-zzoptstrx,
             m01       TYPE zlpfo_mon_p-qnt,
             m02       TYPE zlpfo_mon_p-qnt,
             m03       TYPE zlpfo_mon_p-qnt,
             m04       TYPE zlpfo_mon_p-qnt,
             m05       TYPE zlpfo_mon_p-qnt,
             m06       TYPE zlpfo_mon_p-qnt,
             m07       TYPE zlpfo_mon_p-qnt,
             m08       TYPE zlpfo_mon_p-qnt,
             m09       TYPE zlpfo_mon_p-qnt,
             m10       TYPE zlpfo_mon_p-qnt,
             m11       TYPE zlpfo_mon_p-qnt,
             m12       TYPE zlpfo_mon_p-qnt,
           END OF ty_template_file,
           tt_template_file TYPE STANDARD TABLE OF ty_template_file,

           BEGIN OF ty_upload_data,
             status    TYPE icon_d,
             werks     TYPE zlpfo_mon_h-werks,
             fortype   TYPE zlpfo_mon_h-fortype,
             round     TYPE zlpfo_mon_h-round,
             foryear   TYPE zlpfo_mon_h-foryear,
             chr_plant TYPE zlpfo_mon_h-chr_plant,
             market    TYPE zlpfo_mon_h-market,
             matnr     TYPE zlpfo_mon_h-matnr,
             zzcolorex TYPE zlpfo_mon_h-zzcolorex,
             zzsellerx TYPE zlpfo_mon_h-zzsellerx,
             zzoptstrx TYPE zlpfo_mon_h-zzoptstrx,
             m01       TYPE zlpfo_mon_p-qnt,
             m02       TYPE zlpfo_mon_p-qnt,
             m03       TYPE zlpfo_mon_p-qnt,
             m04       TYPE zlpfo_mon_p-qnt,
             m05       TYPE zlpfo_mon_p-qnt,
             m06       TYPE zlpfo_mon_p-qnt,
             m07       TYPE zlpfo_mon_p-qnt,
             m08       TYPE zlpfo_mon_p-qnt,
             m09       TYPE zlpfo_mon_p-qnt,
             m10       TYPE zlpfo_mon_p-qnt,
             m11       TYPE zlpfo_mon_p-qnt,
             m12       TYPE zlpfo_mon_p-qnt,
             error     TYPE abap_bool,
             error_msg TYPE char50,
           END OF ty_upload_data,
           tt_upload_data TYPE STANDARD TABLE OF ty_upload_data,

           BEGIN OF ty_horizon,
             forfrom TYPE zlpfo_forfrom,
             forto   TYPE zlpfo_forto,
           END OF ty_horizon.

    DATA : mt_export_data     TYPE tt_template_file,
           mt_upload_data     TYPE tt_upload_data,
           mo_bottom          TYPE REF TO cl_gui_container,
           mo_alv_grid        TYPE REF TO cl_gui_alv_grid,
           mo_cust_grid       TYPE REF TO cl_gui_custom_container,
           mo_top             TYPE REF TO cl_dd_document,
           mo_splitter        TYPE REF TO cl_gui_splitter_container,
           mo_parent_top      TYPE REF TO cl_gui_container,
           mt_gridfcat        TYPE lvc_t_fcat,
           mt_uploaded_months TYPE STANDARD TABLE OF numc2,
           mv_no_months       TYPE i,
           ms_horizon         TYPE ty_horizon,
           mt_excluded_months TYPE STANDARD TABLE OF numc2.

    METHODS : export_file,
      upload_file,
      set_first_display,
      get_fcat_grid,
      adjust_data,
      check_data,
      top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid,
      init,
      insert_data,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender,
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.

ENDCLASS.

DATA : go_forecast_upload TYPE REF TO lcl_forecast_upload,
       BEGIN OF gs_screen100,
         ok_code TYPE syucomm,
       END OF gs_screen100.

CLASS lcl_forecast_upload IMPLEMENTATION.

  METHOD execute.

    IF p_werks  IS INITIAL OR
       p_forty  IS INITIAL OR
       p_round  IS INITIAL OR
       p_foyer  IS INITIAL OR
       p_prpnt  IS INITIAL OR
       r_exfil  IS INITIAL.

      MESSAGE 'Please fill the obligatory fields!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    init( ).
    CASE abap_true.
      WHEN r_exprt. "Export file structure
        export_file( ).
      WHEN r_uplfi. " Upload file
        upload_file( ).
        check_data( ).
        adjust_data( ).
        set_first_display( ).
    ENDCASE.

  ENDMETHOD.

  METHOD init.

    SELECT SINGLE forfrom,
                  forto
     FROM zlpfo_param
     INTO  @ms_horizon
     WHERE werks     = @p_werks
     AND   chr_plant = @p_prpnt
     AND   fortype   = @p_forty
     AND   round     = @p_round
     AND   foryear   = @p_foyer.

    IF ms_horizon IS INITIAL.
      MESSAGE 'No data found!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD export_file.

    DATA: lt_template        TYPE tt_template_file,
          lv_filename        TYPE string,
          lt_raw_data        TYPE xml_rawdata,
          lv_size            TYPE i,
          lv_path            TYPE string,
          lv_fullpath        TYPE string,
          lv_action          TYPE i,
          lo_column          TYPE REF TO cl_salv_column_table,
          lr_forecast_months TYPE RANGE OF numc2,
          lv_month           TYPE numc2.

    IF ms_horizon-forfrom < ms_horizon-forto.

      lr_forecast_months = VALUE #( ( sign = 'I'
                          option = 'BT'
                          low = ms_horizon-forfrom
                          high = ms_horizon-forto ) ).

    ELSE.

      lr_forecast_months = VALUE #( ( sign = 'E'
                  option = 'BT'
                  low = ms_horizon-forfrom
                  high = ms_horizon-forto ) ).

    ENDIF.

    DO 12 TIMES.

      lv_month = sy-index.

      IF lv_month NOT IN lr_forecast_months.
        APPEND lv_month TO mt_excluded_months.
      ENDIF.

    ENDDO.

    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table = DATA(lo_salv)
         CHANGING
           t_table      = lt_template ).

        DATA(lo_cols) =  lo_salv->get_columns( ).

        TRY.
            lo_column ?= lo_cols->get_column( columnname = 'M01' ).
            lo_column->set_short_text( 'M01' ).
            lo_column->set_medium_text( 'M01' ).
            lo_column->set_long_text( 'M01' ).

            lo_column ?= lo_cols->get_column( columnname = 'M02' ).
            lo_column->set_short_text( 'M02' ).
            lo_column->set_medium_text( 'M02' ).
            lo_column->set_long_text( 'M02' ).

            lo_column ?= lo_cols->get_column( columnname = 'M03' ).
            lo_column->set_short_text( 'M03' ).
            lo_column->set_medium_text( 'M03' ).
            lo_column->set_long_text( 'M03' ).

            lo_column ?= lo_cols->get_column( columnname = 'M04' ).
            lo_column->set_short_text( 'M04' ).
            lo_column->set_medium_text( 'M04' ).
            lo_column->set_long_text( 'M04' ).

            lo_column ?= lo_cols->get_column( columnname = 'M05' ).
            lo_column->set_short_text( 'M05' ).
            lo_column->set_medium_text( 'M05' ).
            lo_column->set_long_text( 'M05' ).

            lo_column ?= lo_cols->get_column( columnname = 'M06' ).
            lo_column->set_short_text( 'M06' ).
            lo_column->set_medium_text( 'M06' ).
            lo_column->set_long_text( 'M06' ).

            lo_column ?= lo_cols->get_column( columnname = 'M07' ).
            lo_column->set_short_text( 'M07' ).
            lo_column->set_medium_text( 'M07' ).
            lo_column->set_long_text( 'M07' ).

            lo_column ?= lo_cols->get_column( columnname = 'M08' ).
            lo_column->set_short_text( 'M08' ).
            lo_column->set_medium_text( 'M08' ).
            lo_column->set_long_text( 'M08' ).

            lo_column ?= lo_cols->get_column( columnname = 'M09' ).
            lo_column->set_short_text( 'M09' ).
            lo_column->set_medium_text( 'M09' ).
            lo_column->set_long_text( 'M09' ).

            lo_column ?= lo_cols->get_column( columnname = 'M10' ).
            lo_column->set_short_text( 'M10' ).
            lo_column->set_medium_text( 'M10' ).
            lo_column->set_long_text( 'M10' ).

            lo_column ?= lo_cols->get_column( columnname = 'M11' ).
            lo_column->set_short_text( 'M11' ).
            lo_column->set_medium_text( 'M11' ).
            lo_column->set_long_text( 'M11' ).

            lo_column ?= lo_cols->get_column( columnname = 'M12' ).
            lo_column->set_short_text( 'M12' ).
            lo_column->set_medium_text( 'M12' ).
            lo_column->set_long_text( 'M12' ).

            LOOP AT mt_excluded_months ASSIGNING FIELD-SYMBOL(<lv_excluded_months>).

              DATA(lv_field) = CONV lvc_fname( |M{ <lv_excluded_months> }| ).
              lo_column ?= lo_cols->get_column( lv_field ).
              lo_column->set_technical( ).
            ENDLOOP.

          CATCH cx_salv_not_found.

        ENDTRY.

        DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

        IF xstrlen( lv_xml ) > 0.

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
              filename                  =  r_exfil   " Name of file
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

  METHOD upload_file.

    DATA: lt_data   TYPE solix_tab,
          lt_upload TYPE tt_template_file.

    FIELD-SYMBOLS : <lt_excel_upload> TYPE STANDARD TABLE.

    IF r_exfil IS INITIAL.
      MESSAGE 'Enter a file name first!' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = r_exfil            " Name of file
        filetype                = 'BIN'            " File Type (ASCII, Binary)
      CHANGING
        data_tab                = lt_data                 " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No upload authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not enough memory in data provider
        disk_full               = 15               " Storage medium is full.
        dp_timeout              = 16               " Data provider timeout
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
      document_name = r_exfil
      xdocument     = lv_bin_data
    ).

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheet_names) ).

    IF lt_worksheet_names IS INITIAL.

      MESSAGE 'Problem with excel file' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_itab) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ 1 ] ).
    ASSIGN lo_worksheet_itab->* TO <lt_excel_upload>.
    READ TABLE <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_header>) INDEX 1.


    LOOP AT <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_excel_upload>) FROM 2.

      APPEND INITIAL LINE TO lt_upload ASSIGNING FIELD-SYMBOL(<ls_excel_new>).
      DO.

        DATA(lv_index) =  sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upload> TO FIELD-SYMBOL(<lv_month_excel>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF sy-index < 6.
          ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_new> TO FIELD-SYMBOL(<lv_month>).

          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          <lv_month> = <lv_month_excel>.
          CONTINUE.

        ENDIF.

        IF <lv_month_excel> CN '0123456789'.
          MESSAGE 'Excel contains non numeric values in quantity' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_header> TO FIELD-SYMBOL(<lv_header_field>).

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <lv_header_field> NP 'M++'.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <lv_header_field> OF STRUCTURE <ls_excel_new> TO <lv_month>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        <lv_month> = <lv_month_excel>.

      ENDDO.
    ENDLOOP.

    DATA(lo_str) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( p_data = <ls_header> ) ).
    DATA(lt_components) = lo_str->get_components( ).

    mv_no_months = lines( lt_components ) - 5.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_header> TO <lv_header_field>.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <lv_header_field> NP 'M++'.
        CONTINUE.
      ENDIF.
      APPEND CONV numc2( <lv_header_field>+1(2) )  TO mt_uploaded_months.

    ENDDO.

    IF lt_upload IS INITIAL.
      MESSAGE 'The excel file does not have any data' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    mt_upload_data = CORRESPONDING #( lt_upload ).

  ENDMETHOD.

  METHOD check_data.

    TYPES :BEGIN OF ty_duplicate_key,
             market    TYPE zlpbns_market_nsc,
             matnr     TYPE vlc_matnr,
             zzcolorex TYPE zmmw0_col,
             zzsellerx TYPE zmmw0_sel,
             zzoptstrx TYPE zmmw0_optstrg,
             modified  TYPE abap_bool,
           END OF ty_duplicate_key.

    DATA : lt_dd07v_tab       TYPE STANDARD TABLE OF dd07v,
           lr_forecast_months TYPE RANGE OF numc2,
           lv_month           TYPE numc2,
           lt_check_upl_data  TYPE tt_upload_data,
           ls_last_key        TYPE ty_upload_data,
           ls_duplicate_key   TYPE ty_duplicate_key,
           lt_duplicate_key   TYPE SORTED TABLE OF ty_duplicate_key WITH UNIQUE KEY market
                                                                                    matnr
                                                                                    zzcolorex
                                                                                    zzsellerx
                                                                                    zzoptstrx.

    IF ms_horizon-forfrom < ms_horizon-forto.

      lr_forecast_months = VALUE #( ( sign = 'I'
                          option = 'BT'
                          low = ms_horizon-forfrom
                          high = ms_horizon-forto ) ).

    ELSE.

      lr_forecast_months = VALUE #( ( sign = 'E'
                  option = 'BT'
                  low = ms_horizon-forfrom
                  high = ms_horizon-forto ) ).

    ENDIF.

    DO 12 TIMES.

      lv_month = sy-index.

      IF lv_month NOT IN lr_forecast_months.
        APPEND lv_month TO mt_excluded_months.
      ENDIF.

    ENDDO.

    LOOP AT mt_uploaded_months ASSIGNING FIELD-SYMBOL(<lv_uploaded_months>).

      IF <lv_uploaded_months> NOT IN lr_forecast_months .

        DATA(lv_excl_month) = abap_true.
        EXIT.
      ENDIF.

*        IF <ls_mandatory> <= 0.
*          <ls_upload_data>-status = icon_incomplete.
*          <ls_upload_data>-error = abap_true.
*          <ls_upload_data>-error_msg = 'All forecasts must be numeric' .
*        ELSE.
*          <ls_upload_data>-status = icon_checked .
*        ENDIF.

    ENDLOOP.

    IF lv_excl_month IS NOT INITIAL.
      MODIFY mt_upload_data FROM VALUE #( status    = icon_incomplete
                                    error     = abap_true
                                    error_msg = 'Uploaded excel contains months in excluded range' )
                                 TRANSPORTING status
                                              error
                                              error_msg
                                 WHERE status IS INITIAL.
      RETURN.
    ENDIF.


    lt_check_upl_data = mt_upload_data.

    SORT lt_check_upl_data BY werks
                              chr_plant
                              fortype
                              round
                              foryear
                              market
                              matnr
                              zzcolorex
                              zzsellerx
                              zzoptstrx.

    LOOP AT lt_check_upl_data ASSIGNING FIELD-SYMBOL(<ls_check_upl_data>).

      READ TABLE lt_duplicate_key ASSIGNING FIELD-SYMBOL(<ls_duplicate_key>) WITH TABLE KEY market    = <ls_check_upl_data>-market
                                                                                            matnr     =  <ls_check_upl_data>-matnr
                                                                                            zzcolorex =  <ls_check_upl_data>-zzcolorex
                                                                                            zzsellerx =  <ls_check_upl_data>-zzsellerx
                                                                                            zzoptstrx =  <ls_check_upl_data>-zzoptstrx .
      IF sy-subrc <> 0.
        INSERT CORRESPONDING ty_duplicate_key( <ls_check_upl_data> ) INTO TABLE lt_duplicate_key.
        CONTINUE.
      ENDIF.

      IF <ls_duplicate_key>-modified IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      <ls_duplicate_key>-modified = abap_true.
      MODIFY mt_upload_data FROM VALUE #( status    = icon_incomplete
                                          error_msg = 'Duplicate Record'
                                          error     = abap_true )
                                 TRANSPORTING status error_msg error
                                 WHERE market    = <ls_check_upl_data>-market
                                   AND matnr     =  <ls_check_upl_data>-matnr
                                   AND zzcolorex =  <ls_check_upl_data>-zzcolorex
                                   AND zzsellerx =  <ls_check_upl_data>-zzsellerx
                                   AND zzoptstrx =  <ls_check_upl_data>-zzoptstrx.

    ENDLOOP.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZLVMM_MARKET_NSC'
      TABLES
        dd07v_tab      = lt_dd07v_tab
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT mt_upload_data ASSIGNING FIELD-SYMBOL(<ls_upload_data>).

      IF <ls_upload_data>-status = icon_incomplete.
        CONTINUE.
      ENDIF.

      LOOP AT mt_uploaded_months ASSIGNING <lv_uploaded_months>.
        DATA(lv_field) = CONV lvc_fname( |M{ <lv_uploaded_months> }| ).

        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_upload_data> TO FIELD-SYMBOL(<ls_mandatory>).

        IF <ls_mandatory> IS ASSIGNED AND <ls_mandatory> IS INITIAL.
          <ls_upload_data>-status = icon_incomplete.
          <ls_upload_data>-error = abap_true.
          <ls_upload_data>-error_msg = 'All values are mandatory' .
        ENDIF.
      ENDLOOP.

      READ TABLE lt_dd07v_tab WITH KEY domvalue_l = <ls_upload_data>-market TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        <ls_upload_data>-status = icon_incomplete.
        <ls_upload_data>-error = abap_true.
        <ls_upload_data>-error_msg = 'Market does not exist. Please check!'  .
        CONTINUE.
      ELSE.
        <ls_upload_data>-status = icon_checked .
      ENDIF.

      SELECT SINGLE matnr
        FROM zrsdap_psa_pc016
        INTO @DATA(lv_material)
        WHERE matnr = @<ls_upload_data>-matnr
        AND mercato = @<ls_upload_data>-market.

      IF sy-subrc <> 0.
        <ls_upload_data>-status = icon_incomplete.
        <ls_upload_data>-error = abap_true.
        <ls_upload_data>-error_msg = 'Model does not exist. Please check!' .
        CONTINUE.
      ELSE.
        <ls_upload_data>-status = icon_checked .
      ENDIF.

      SELECT SINGLE cod_col_est_psa
         FROM zrsdap_psa_pc030
         INTO @DATA(lv_ext_code)
         WHERE matnr = @<ls_upload_data>-matnr
         AND mercato = @<ls_upload_data>-market.

      IF sy-subrc <> 0.
        <ls_upload_data>-status = icon_incomplete.
        <ls_upload_data>-error = abap_true.
        <ls_upload_data>-error_msg = 'External color is incorrect. Please check!' .
        CONTINUE.
      ELSE.
        <ls_upload_data>-status = icon_checked .
      ENDIF.

      SELECT SINGLE cod_selleria
         FROM zrsdap_psa_pc031
         INTO @DATA(lv_int_code)
         WHERE matnr = @<ls_upload_data>-matnr
         AND mercato = @<ls_upload_data>-market.

      IF sy-subrc <> 0.
        <ls_upload_data>-status = icon_incomplete.
        <ls_upload_data>-error = abap_true.
        <ls_upload_data>-error_msg = 'Internal color is incorrect. Please check!' .
        CONTINUE.
      ELSE.
        <ls_upload_data>-status = icon_checked .
      ENDIF.

      SELECT SINGLE cod_col_est_psa
         FROM zrsdap_psa_pc022
         INTO @DATA(lv_code_match)
         WHERE matnr = @<ls_upload_data>-matnr
         AND mercato = @<ls_upload_data>-market
         AND cod_col_est_psa = @<ls_upload_data>-zzcolorex.

      IF sy-subrc <> 0.
        <ls_upload_data>-status = icon_incomplete.
        <ls_upload_data>-error = abap_true.
        <ls_upload_data>-error_msg = 'Configuration does not exist. Please check!' .
        CONTINUE.
      ELSE.
        <ls_upload_data>-status = icon_checked .
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD adjust_data.
    LOOP AT mt_upload_data ASSIGNING FIELD-SYMBOL(<ls_upload_data>).
      <ls_upload_data>-werks   = p_werks.
      <ls_upload_data>-fortype = p_forty.
      <ls_upload_data>-round   = p_round.
      <ls_upload_data>-foryear = p_foyer.
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_data.
    DATA: ls_mon_h        TYPE zlpfo_mon_h,
          ls_mon_p        TYPE zlpfo_mon_p,
          lt_selected_row TYPE lvc_t_roid,
          lv_has_error    TYPE abap_bool,
          lt_sel_data     TYPE STANDARD TABLE OF ty_upload_data.

    mo_alv_grid->get_selected_rows(
      IMPORTING
        et_row_no     = lt_selected_row
    ).

    IF lt_selected_row IS INITIAL.
      MESSAGE 'Please, select at least 1 line to continue!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_selected_row ASSIGNING FIELD-SYMBOL(<ls_sel_row>).

      READ TABLE mt_upload_data ASSIGNING FIELD-SYMBOL(<ls_upload_data>) INDEX <ls_sel_row>-row_id.

      IF <ls_upload_data>-error = abap_true.
        MESSAGE 'Selection contains errors!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      APPEND <ls_upload_data> TO lt_sel_data.
    ENDLOOP.

    LOOP AT lt_sel_data ASSIGNING FIELD-SYMBOL(<ls_sel_data>).

      ls_mon_h = VALUE #( werks     =  p_werks
                          market    =  <ls_sel_data>-market
                          matnr     =  <ls_sel_data>-matnr
                          zzcolorex =  <ls_sel_data>-zzcolorex
                          zzsellerx =  <ls_sel_data>-zzsellerx
                          zzoptstrx =  <ls_sel_data>-zzoptstrx
                          fortype   =  p_forty
                          round     =  p_round
                          foryear   =  p_foyer
                          upldate   =  sy-datum
                          status    =  'N'
                          chr_plant =  p_prpnt  ).

      INSERT zlpfo_mon_h FROM ls_mon_h.
      IF sy-subrc = 0.
        <ls_sel_data>-status = icon_action_success.
        <ls_sel_data>-error = abap_false.
        <ls_sel_data>-error_msg = '' .
      ELSE.
        <ls_sel_data>-status = icon_action_fault .
        <ls_sel_data>-error = abap_true.
        <ls_sel_data>-error_msg = 'Error in header level' .
        ROLLBACK WORK.
        CONTINUE.
      ENDIF.

      LOOP AT mt_uploaded_months ASSIGNING FIELD-SYMBOL(<lv_uploaded_months>).

        DATA(lv_field) = CONV lvc_fname( |M{ <lv_uploaded_months> }| ).

        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_sel_data> TO FIELD-SYMBOL(<ls_data>).

        ls_mon_p = VALUE #( werks     =  p_werks
                            market    =  <ls_sel_data>-market
                            matnr     =  <ls_sel_data>-matnr
                            zzcolorex =  <ls_sel_data>-zzcolorex
                            zzsellerx =  <ls_sel_data>-zzsellerx
                            zzoptstrx =  <ls_sel_data>-zzoptstrx
                            fortype   =  p_forty
                            round     =  p_round
                            foryear   =  p_foyer
                            formonth  = lv_field+1
                            qnt       = <ls_data>
                            ) .


        INSERT zlpfo_mon_p FROM ls_mon_p.
        IF sy-subrc = 0.
          <ls_sel_data>-status = icon_action_success.
          <ls_sel_data>-error = abap_false.
          <ls_sel_data>-error_msg = '' .
        ELSE.
          <ls_sel_data>-status = icon_action_fault .
          <ls_sel_data>-error = abap_true.
          <ls_sel_data>-error_msg = 'Error in position level' .
          lv_has_error = abap_true.
          ROLLBACK WORK.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF lv_has_error IS INITIAL.
        COMMIT WORK.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_filename.

    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             = |Excel (*.xlsx)\|*.xlsx|
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
        user_action             = lv_user
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
      r_exfil = lt_filetable[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD set_first_display.

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CUST_CONT' ).

    CREATE OBJECT mo_splitter
      EXPORTING
        parent            = mo_cust_grid
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1                  " See Superclass
        cntl_system_error = 2                  " See Superclass
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_parent_top = mo_splitter->get_container( row = 1  column  = 1  ).

    CREATE OBJECT mo_top
      EXPORTING
        style = 'ALV_GRID'.

    mo_bottom = mo_splitter->get_container( row = 2  column  = 1  ).

    mo_splitter->set_row_height(
      EXPORTING
        id                = 1                 " Row ID
        height            = 25                " Height
      EXCEPTIONS
        cntl_error        = 1                " See CL_GUI_CONTROL
        cntl_system_error = 2                " See CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_bottom ).

    get_fcat_grid( ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                        sel_mode = 'D').

    SET HANDLER top_of_page
                handle_toolbar
                handle_ucomm FOR mo_alv_grid.

    TRY.
        mo_alv_grid->set_table_for_first_display(
          EXPORTING
            is_layout                     = ls_layout
          CHANGING
            it_outtab                     = mt_upload_data
            it_fieldcatalog               = mt_gridfcat ).

        mo_top->initialize_document( ).
        mo_alv_grid->list_processing_events(
          EXPORTING
            i_event_name      =  'TOP_OF_PAGE'
            i_dyndoc_id       =  mo_top  ).

        CALL SCREEN 100.

      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD get_fcat_grid.

    mt_gridfcat = VALUE #(
                         ( fieldname = 'STATUS'     coltext   = 'Status'      )
                         ( fieldname = 'WERKS'      ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'FORTYPE'    ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'ROUND'      ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'FORYEAR'    ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'CHR_PLANT'  ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'MARKET'     ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'MATNR'      ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'ZZCOLOREX'  ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'ZZSELLERX'  ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'ZZOPTSTRX'  ref_table = 'ZLPFO_MON_H' )
                         ( fieldname = 'M01'        ref_table = 'ZLPFO_MON_P' coltext = 'M1'  no_zero = abap_true )
                         ( fieldname = 'M02'        ref_table = 'ZLPFO_MON_P' coltext = 'M2'  no_zero = abap_true )
                         ( fieldname = 'M03'        ref_table = 'ZLPFO_MON_P' coltext = 'M3'  no_zero = abap_true )
                         ( fieldname = 'M04'        ref_table = 'ZLPFO_MON_P' coltext = 'M4'  no_zero = abap_true )
                         ( fieldname = 'M05'        ref_table = 'ZLPFO_MON_P' coltext = 'M5'  no_zero = abap_true )
                         ( fieldname = 'M06'        ref_table = 'ZLPFO_MON_P' coltext = 'M6'  no_zero = abap_true )
                         ( fieldname = 'M07'        ref_table = 'ZLPFO_MON_P' coltext = 'M7'  no_zero = abap_true )
                         ( fieldname = 'M08'        ref_table = 'ZLPFO_MON_P' coltext = 'M8'  no_zero = abap_true )
                         ( fieldname = 'M09'        ref_table = 'ZLPFO_MON_P' coltext = 'M9'  no_zero = abap_true )
                         ( fieldname = 'M10'        ref_table = 'ZLPFO_MON_P' coltext = 'M10' no_zero = abap_true )
                         ( fieldname = 'M11'        ref_table = 'ZLPFO_MON_P' coltext = 'M11' no_zero = abap_true )
                         ( fieldname = 'M12'        ref_table = 'ZLPFO_MON_P' coltext = 'M12' no_zero = abap_true )
                         ( fieldname = 'ERROR'      coltext   = 'Error'       )
                         ( fieldname = 'ERROR_MSG'  coltext   = 'Error message' )  ).

    LOOP AT mt_excluded_months ASSIGNING FIELD-SYMBOL(<lv_excluded_months>).
      DATA(lv_field) = CONV lvc_fname( |M{ <lv_excluded_months> }| ).

      READ TABLE mt_gridfcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = lv_field .
      IF sy-subrc = 0.
        <ls_fcat>-tech = abap_true.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD top_of_page.

    DATA: lt_html_tab  TYPE sdydo_html_table,
          lt_file      TYPE STANDARD TABLE OF string,
          lv_rec_error TYPE i,
          lv_success   TYPE i.

    SPLIT r_exfil AT '\' INTO TABLE lt_file.
    IF lt_file IS NOT INITIAL.
      DATA(lv_filename) = lt_file[ lines( lt_file ) ].
    ENDIF.

    LOOP AT mt_upload_data ASSIGNING FIELD-SYMBOL(<ls_upl_data>).
      IF <ls_upl_data>-error = abap_true.
        lv_rec_error = lv_rec_error + 1.

      ELSEIF <ls_upl_data>-error = abap_false.
        lv_success = lv_success + 1.
      ENDIF.
    ENDLOOP.

    lt_html_tab = VALUE #( ( line = '<HTML>' )
                           ( line = '<BODY>' )
                           ( line = '<H2> Mode: Execution - Upload forecast </H2>' )
                           ( line = |<P> File Name: { lv_filename } <BR>| )
                           ( line = |Tot.Rec.Imported: { lines( mt_upload_data ) } <BR>| )
                           ( line = |Tot.Rec.In Error: { lv_rec_error } <BR>| )
                           ( line = |Tot.Rec.OK/Warning: { lv_success } </P>| )
                           ( line = '</BODY>' )
                           ( line = '</HTML>' ) ).

    mo_top->add_static_html( table_with_html  = lt_html_tab ).

    mo_top->display_document(
      EXPORTING
        reuse_control      =  'X'
        parent             =  mo_parent_top                " Contain Object Already Exists
      EXCEPTIONS
        html_display_error = 1                " Error Displaying the Document in the HTML Control
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD handle_toolbar.

    APPEND VALUE #( function = 'FC_INSERT'
                    icon = icon_insert_multiple_lines
                    quickinfo = 'Insert data'
                    text = 'Insert data' ) TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_ucomm.

    CASE e_ucomm.
      WHEN 'FC_INSERT'.
        insert_data( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'BL1'.
        screen-required = 2.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR r_exfil.
  lcl_forecast_upload=>get_filename( ).

START-OF-SELECTION.

  CREATE OBJECT go_forecast_upload.
  go_forecast_upload->execute( ).

  INCLUDE zlpfo_forec_mon_user_commani01.

  INCLUDE zlpfo_forec_mon_status_0100o01.
