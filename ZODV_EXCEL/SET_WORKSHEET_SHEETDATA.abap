METHOD set_worksheet_sheetdata.

  DATA: itab_row            TYPE REF TO data,
        columns_descr       TYPE REF TO cl_abap_structdescr,
        t_col               TYPE abap_component_tab,
        s_col               LIKE LINE OF t_col,
        column              TYPE if_fdt_doc_spreadsheet=>s_column,
        col_count           TYPE i,
        rows                TYPE i,
        dimension           TYPE string,
        lv_group            TYPE int4,
        lv_max_group        TYPE int4,
        lv_max_group_string TYPE string,
        sheetdata_node      TYPE REF TO if_ixml_node,
        row_element         TYPE REF TO if_ixml_element,  " <row>
        row_count           TYPE int4,                    " Row count as string
        lv_row_number       TYPE int4.

  FIELD-SYMBOLS: <itab>      TYPE STANDARD TABLE,
                 <itab_row>  TYPE any,
                 <itab_row1> TYPE any.

* Enabling iXML support
  TYPE-POOLS: ixml.
  CLASS cl_ixml DEFINITION LOAD.

  IF itab IS NOT INITIAL.
* Prepare field symbols and getting table info
    ASSIGN itab->* TO <itab>.
    CREATE DATA itab_row LIKE LINE OF <itab>.
    ASSIGN itab_row->* TO <itab_row>.
*    CREATE DATA itab_row1 LIKE LINE OF <itab>.
*    ASSIGN itab_row1->* TO <itab_row1>.
    DESCRIBE TABLE <itab> LINES rows.
    rows = rows + 1. " One row more in the spreadsheet for the column headings

    IF it_sheet_data IS NOT INITIAL.
      LOOP AT it_sheet_data ASSIGNING FIELD-SYMBOL(<ls_sheet_data>).
        IF <ls_sheet_data>-field_desc IS NOT INITIAL.

          columns_descr = CAST cl_abap_structdescr(
              cl_abap_typedescr=>describe_by_data( <itab_row> ) ).
          DATA(t_components) = columns_descr->get_components( ).

          CLEAR t_col.

          LOOP AT <ls_sheet_data>-field_desc ASSIGNING FIELD-SYMBOL(<ls_desc>).
            CLEAR s_col.

            IF <ls_desc>-descr IS NOT INITIAL.
              s_col-name = <ls_desc>-descr.
            ENDIF.

            READ TABLE t_components ASSIGNING FIELD-SYMBOL(<ls_component>)
              INDEX sy-tabix.
            IF sy-subrc = 0.
              s_col-type = <ls_component>-type.
            ENDIF.

            APPEND s_col TO t_col.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
*    ENDIF.
    ELSE."If columns are not yet defined
      TRY.
          columns_descr ?= cl_abap_typedescr=>describe_by_data( <itab_row> ).
          t_col = columns_descr->get_components( ).      "#EC CI_BOX_OK
        CATCH cx_root.
          EXIT.
      ENDTRY.


    ENDIF.
    DESCRIBE TABLE t_col LINES col_count.
  ELSE.
* No data, leaving...
    EXIT.
  ENDIF.

* Prepare XML
  TRY.
      dimension = 'A1:' && me->convert_i_to_column( col_count ) && rows.
      me->set_worksheet_dimension( sheet_xml_document = sheet_xml_document dimension = dimension ).
      me->set_worksheet_cols( sheet_xml_document = sheet_xml_document
                              column_description_tab = t_col
                              is_alv_document = is_alv_document
                              iv_call_type = iv_call_type
                              iv_column_dectab = iv_column_dectab ).

      sheetdata_node = me->clear_worksheet_sheetdata( sheet_xml_document ).
    CATCH cx_root.
      EXIT.
  ENDTRY.

  IF sheetdata_node IS INITIAL.
    EXIT.
  ENDIF.

* Set column headings
  row_element = sheet_xml_document->create_element_ns( 'row' ).
  row_element->set_attribute_ns( name = 'r' value = '1' ).
  row_element->set_attribute_ns( name = 'spans' value = '1:' && col_count ).
  row_element->set_attribute_ns( name = 's' value = '1' ). " Bold
  row_element->set_attribute_ns( name = 'customFormat' value = '1' ).

  create_cell_elements_header( io_row_element        = row_element
*                               it_column             = columns
                               it_column_description = t_col
                               io_sheet_xml_document = sheet_xml_document
                               iv_call_type          = iv_call_type
                               iv_is_alv_document    = is_alv_document ).
  sheetdata_node->append_child( row_element ).

* Create spreadsheet
  lv_row_number = 2.
  LOOP AT <itab> INTO <itab_row>.
    row_count = sy-tabix + 1.

    IF iv_call_type NE if_fdt_doc_spreadsheet=>gc_call_new_lean_trace.
      create_worksheet_row( EXPORTING is_row                = <itab_row>
                                      iv_row_number         = row_count
*                                      it_column             = columns
                                      it_column_description = t_col
                                      io_sheet_xml_document = sheet_xml_document
                                      io_sheet_data_node     = sheetdata_node
                                      iv_call_type          = iv_call_type
                                      iv_is_alv_document    = is_alv_document
                             CHANGING cv_group_number       = lv_group
                                      cv_group_number_max   = lv_max_group ).
    ELSE.
      create_worksheet_row_ntl( EXPORTING it_data               = <itab>
                                          is_row                = <itab_row>
                                          iv_row_index          = ( row_count - 1 )
*                                          it_column             = columns
                                          it_column_description = t_col
                                          io_sheet_xml_document = sheet_xml_document
                                          io_sheet_data_node    = sheetdata_node
                                          iv_is_alv_document    = is_alv_document
                                 CHANGING cv_row_number         = lv_row_number
                                          cv_group_number_max   = lv_max_group ).
    ENDIF.
  ENDLOOP.

  IF iv_call_type EQ if_fdt_doc_spreadsheet=>gc_call_trace
     OR iv_call_type EQ if_fdt_doc_spreadsheet=>gc_call_new_lean_trace.
    lv_max_group_string = lv_max_group.
    set_worksheet_formatpr( sheet_xml_document = sheet_xml_document outlinelevel = lv_max_group_string ).
  ENDIF.

ENDMETHOD.
