METHOD create_document.
  DATA:
    xblank_document  TYPE xstring,
    xsheet           TYPE xstring,
    obj_ssheet_ooxml TYPE REF TO zcl_fdt_xl_spreadsheet,
    ws_tab           TYPE STANDARD TABLE OF string,
    ws               TYPE string,
    xml_doc          TYPE REF TO if_ixml_document,
    iv_call_type     TYPE i,
    lt_sheet_data    TYPE tt_sheet_data.

  FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

* Enabling iXML support
  TYPE-POOLS: ixml.
  CLASS cl_ixml DEFINITION LOAD.

  xblank_document = zcl_fdt_xl_spreadsheet=>create_blank_document_struct(
                      it_sheet_data = it_sheet_data ).

  CREATE OBJECT obj_ssheet_ooxml
    EXPORTING
      document_name = name
      xdocument     = xblank_document.
  obj_ssheet_ooxml->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = ws_tab ).


  LOOP AT it_sheet_data ASSIGNING FIELD-SYMBOL(<ls_sheet_data>).

    xsheet = obj_ssheet_ooxml->get_worksheet_by_name( <ls_sheet_data>-sheet_name ).

    xml_doc = obj_ssheet_ooxml->parse_xml( xsheet ).
    IF xml_doc IS INITIAL.
      CONTINUE.
    ENDIF.

    APPEND <ls_sheet_data> TO lt_sheet_data.

    obj_ssheet_ooxml->set_worksheet_sheetdata(
      EXPORTING
        sheet_xml_document = xml_doc
        it_sheet_data      = lt_sheet_data
        itab               = <ls_sheet_data>-tab_data
        iv_call_type       = iv_call_type
    ).

    CLEAR lt_sheet_data.

    obj_ssheet_ooxml->overwrite_worksheet_by_name(
      worksheet_name = <ls_sheet_data>-sheet_name
      xsheet        = obj_ssheet_ooxml->render_xml( xml_doc )
    ).

    CLEAR xml_doc.
  ENDLOOP.

* Set the shared strings finally
  obj_ssheet_ooxml->set_shared_strings( ).

* returning the document as XSTRING
  xdocument = obj_ssheet_ooxml->if_fdt_document~get_document( ).
ENDMETHOD.
