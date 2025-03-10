METHOD send_email.

* Create sender
    TRY.
        DATA(lo_req) = cl_bcs=>create_persistent( ).
        lo_req->set_sender( cl_sapuser_bcs=>create( sy-uname ) ).
      CATCH cx_send_req_bcs
            cx_address_bcs.
    ENDTRY.

* Add receivers in the e-mail
    LOOP AT cs_email-rec_email ASSIGNING FIELD-SYMBOL(<la_rec>).

      TRY.
          DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( <la_rec> ).
        CATCH cx_address_bcs.
          CONTINUE.
      ENDTRY.

      TRY.
          lo_req->add_recipient(
          EXPORTING
            i_recipient = lo_recipient
            i_express   = 'X' ).
        CATCH cx_send_req_bcs.
      ENDTRY.

    ENDLOOP.

    DATA(lt_body) = email_body_html( cs_email ).

* Create document to be sent
    TRY.
        DATA(lo_doc) = cl_document_bcs=>create_document(
                          i_type    = 'HTM'
                          i_text    = lt_body
                          i_subject = cs_email-subject ).
      CATCH cx_document_bcs.
    ENDTRY.

    IF cs_email-attachment IS NOT INITIAL.
      add_email_attachment(
         CHANGING
           co_doc_bcs = lo_doc
           cs_email   = cs_email ).
    ENDIF.

*  "Send email
    TRY.
        lo_req->set_document( lo_doc ).
        DATA(lv_result) =  lo_req->send( ).

        IF lv_result IS NOT INITIAL.
          COMMIT WORK.
        ENDIF.
      CATCH cx_send_req_bcs.
    ENDTRY.

  ENDMETHOD.
