METHOD add_email_attachment.

    DATA: lv_xlen TYPE i,
          lt_bin  TYPE solix_tab.
    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.


    ASSIGN cs_email-table_data->* TO <lt_data>.

    TRY.
        cl_salv_table=>factory(
           IMPORTING
             r_salv_table = DATA(lo_salv)
           CHANGING
             t_table      = <lt_data> ).
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(lv_xstring) = lo_salv->to_xml( if_salv_bs_xml=>c_type_xlsx ).
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_xlen
      TABLES
        binary_tab    = lt_bin.

    TRY.
        co_doc_bcs->add_attachment(
            i_attachment_type    = 'EXT'
            i_attachment_subject = |{ cs_email-subject }.xlsx|
            i_attachment_size    = CONV #( lv_xlen )
            i_att_content_hex    = lt_bin ).
      CATCH cx_bcs.
    ENDTRY.

  ENDMETHOD.
