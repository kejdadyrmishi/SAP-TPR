*&---------------------------------------------------------------------*
*& Report  ZKD_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zkd_test.

DATA: lt_data    TYPE REF TO data,
      lt_fcat    TYPE lvc_t_fcat,
      lv_lines   TYPE i,
      lv_idx     TYPE i,
      lv_col_idx TYPE i,
      ls_fcat    TYPE lvc_s_fcat.

FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
               <fs_field> TYPE any,
               <fs_line>  TYPE any,
               <fs_value> TYPE any.


lt_fcat = VALUE lvc_t_fcat(
  ( fieldname = 'MATNR' coltext = 'Material' outputlen = 10 )
  ( fieldname = 'MTART' coltext = 'Type' outputlen = 5 )
).

cl_alv_table_create=>create_dynamic_table(
  EXPORTING
*    i_style_table             =     " Add Style Table
    it_fieldcatalog           = lt_fcat    " Field Catalog
*    i_length_in_byte          =     " Boolean Variable (X=True, Space=False)
  IMPORTING
    ep_table                  = lt_data    " Pointer to Dynamic Data Table
*    e_style_fname             =     " ALV Control: Field Name of Internal Table Field
  EXCEPTIONS
    generate_subpool_dir_full = 1
    OTHERS                    = 2
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

ASSIGN lt_data->* TO <lt_table>.

DATA: ls_wa TYPE REF TO data.
CREATE DATA ls_wa LIKE LINE OF <lt_table>.
ASSIGN ls_wa->* TO <fs_line>.

ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_field>.
<fs_field> = 'MAT1'.

ASSIGN COMPONENT 'MTART' OF STRUCTURE <fs_line> TO <fs_field>.
<fs_field> = 'FERT'.

APPEND <fs_line> TO <lt_table>.

ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_field>.
<fs_field> = 'MAT2'.

ASSIGN COMPONENT 'MTART' OF STRUCTURE <fs_line> TO <fs_field>.
<fs_field> = 'FERT2'.

APPEND <fs_line> TO <lt_table>.

DESCRIBE TABLE <lt_table> LINES lv_lines.

lv_idx = 1.
DO lv_lines TIMES.
  READ TABLE <lt_table> ASSIGNING <fs_line> INDEX lv_idx.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  LOOP AT lt_fcat INTO ls_fcat.
    ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fs_value>.
    IF sy-subrc = 0.
      WRITE <fs_value>.
      WRITE ' '.
    ENDIF.
  ENDLOOP.

  NEW-LINE.
  lv_idx = lv_idx + 1.
ENDDO.
