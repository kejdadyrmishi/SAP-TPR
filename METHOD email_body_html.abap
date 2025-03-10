METHOD email_body_html.
    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lo_table  TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    IF is_email-attachment IS NOT INITIAL.
      APPEND LINES OF is_email-header TO rt_body.
      APPEND LINES OF is_email-footer TO rt_body.
      RETURN.
    ENDIF.

* create body based on the messages
    rt_body = VALUE soli_tab(
       ( line = '<head>                                           ' )
       ( line = '<style>                                          ' )
       ( line = '#messages {                                      ' )
       ( line = '  font-family: Arial, Helvetica, sans-serif;     ' )
       ( line = '  border-collapse: collapse;                     ' )
       ( line = '}                                                ' )
       ( line = '                                                 ' )
       ( line = '#messages td, #messages th {                     ' )
       ( line = '  border: 1px solid #ddd;                        ' )
       ( line = '  padding: 8px;                                  ' )
       ( line = '}                                                ' )
       ( line = '                                                 ' )
       ( line = '#messages tr:nth-child(even){                    ' )
       ( line = 'background-color: #f2f2f2;}                      ' )
       ( line = '#messages tr:hover {background-color: #B4E2F7;}  ' )
       ( line = '                                                 ' )
       ( line = '#messages th {                                   ' )
       ( line = '  padding-top: 12px;                             ' )
       ( line = '  padding-bottom: 12px;                          ' )
       ( line = '  text-align: left;                              ' )
       ( line = '  background-color: #1C91C5;                     ' )
       ( line = '  color: white;                                  ' )
       ( line = '}                                                ' )
       ( line = '</style>                                         ' )
       ( line = '</head>                                          ' )
       ( line = '<body>                                           ' )
 ).


    APPEND LINES OF is_email-header TO rt_body.
    APPEND |<table id="messages"> | TO rt_body.

    ASSIGN is_email-table_data->* TO <lt_data>.

* Insert headers
    lo_table ?= cl_abap_typedescr=>describe_by_data_ref( REF #( <lt_data> ) ).
    lo_struct ?= lo_table->get_table_line_type( ).
    DATA(lt_comp) = lo_struct->get_components( ).

    APPEND INITIAL LINE TO rt_body ASSIGNING FIELD-SYMBOL(<ls_body>).
    <ls_body>-line = '<tr>'.

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).

      <ls_comp>-type->get_ddic_object(
      RECEIVING
        p_object      = DATA(lt_ddic)
      EXCEPTIONS
        not_found     = 1
        no_ddic_type  = 2 ).
      IF sy-subrc EQ 0.
        DATA(ls_ddic) = lt_ddic[ 1 ].

        SELECT SINGLE ddtext
          FROM dd04t
          INTO @DATA(lv_name)
          WHERE rollname   = @ls_ddic-tabname
            AND ddlanguage = 'E'
            AND as4local   = 'A'.

      ELSE.
        lv_name = <ls_comp>-name.
      ENDIF.

      APPEND INITIAL LINE TO rt_body ASSIGNING <ls_body>.
      <ls_body>-line = |<th>| && lv_name && |</th>|.
    ENDLOOP.
    <ls_body>-line = <ls_body>-line && '</tr>'.


    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
* Start of row
      APPEND INITIAL LINE TO rt_body ASSIGNING <ls_body>.
      <ls_body>-line = |<tr>|.

* Diplay cells inside the row
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<la_val>).
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO rt_body ASSIGNING <ls_body> .
        <ls_body>-line = |<td>{ <la_val> }</td>|.
      ENDDO.

* Close row
      APPEND INITIAL LINE TO rt_body ASSIGNING <ls_body>.
      <ls_body>-line = |</tr>|.

* Close table
      AT LAST.
        <ls_body>-line = |{ <ls_body>-line }</table>|.
      ENDAT.
    ENDLOOP.


    APPEND LINES OF is_email-footer TO rt_body.
    APPEND |</body>| TO rt_body.

  ENDMETHOD.
