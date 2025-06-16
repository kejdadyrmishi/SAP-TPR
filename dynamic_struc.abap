*&---------------------------------------------------------------------*
*& Report ZKD_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkd_test.

DATA : lo_struc      TYPE REF TO cl_abap_structdescr,
       lt_dfies      TYPE TABLE OF dfies,
       lv_where      TYPE string,
       ls_mara       TYPE mara,
       lt_components TYPE abap_component_tab.

DATA(lv_tabname) = 'MARA'.

DATA(it_filter_select_options)  = VALUE  /iwbep/t_mgw_select_option(
( property = 'MATNR' select_options = VALUE #( ( sign = 'I'
                                               option = 'EQ'
                                               low  = '10005465'   ) ) )
( property = 'MATKL' select_options = VALUE #( ( sign = 'I'
                                               option = 'EQ'
                                               low  = 'E012007--9100'   ) ) ) ).

LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>).

  cl_abap_datadescr=>describe_by_name(
    EXPORTING
      p_name         = '/IWBEP/T_COD_SELECT_OPTIONS'   " Type name
    RECEIVING
      p_descr_ref    = DATA(lo_type)    " Reference to description object
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2
  ).
  IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  APPEND VALUE #( name = <ls_filter>-property
                  type = CAST #( lo_type ) ) TO lt_components.
ENDLOOP.

DATA(lo_comp) = cl_abap_structdescr=>create( lt_components ).

START-OF-SELECTION.

  DATA lr_filter TYPE REF TO data.
  CREATE DATA lr_filter TYPE HANDLE lo_comp.
  ASSIGN lr_filter->* TO FIELD-SYMBOL(<ls_filter_dyn>).

  LOOP AT it_filter_select_options ASSIGNING <ls_filter>.


    ASSIGN COMPONENT <ls_filter>-property OF STRUCTURE <ls_filter_dyn> TO FIELD-SYMBOL(<lr_filt_range>).
*    <lr_filt_range> = CORRESPONDING #( <ls_filter>-select_options ) .
    MOVE-CORRESPONDING <ls_filter>-select_options TO <lr_filt_range>.


    IF lv_where IS INITIAL.
      lv_where = |{ <ls_filter>-property } IN  @<ls_filter_dyn>-{ <ls_filter>-property } |.
    ELSE.
      lv_where = |{ lv_where } AND { <ls_filter>-property } IN  @<ls_filter_dyn>-{ <ls_filter>-property } |.
    ENDIF.

  ENDLOOP.

  SELECT matnr , matkl
    UP TO 10 ROWS
    FROM mara
    INTO TABLE @DATA(lt_mara)
    WHERE (lv_where).

  cl_demo_output=>display(
    EXPORTING
      data = lt_mara    " Text or Data
*      name =
  ).
