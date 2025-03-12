*&---------------------------------------------------------------------*
*& Report  ZTEST_BCF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ztest_bcf2.

CLASS lcl_cr_char DEFINITION.
  PUBLIC SECTION.

    TYPES : BEGIN OF ty_data,
              charact    TYPE char30,
              hier_name  TYPE lvc_value,
              node_pid   TYPE lvc_nkey,
              tech_id    TYPE lvc_nkey,
              tech_pid   TYPE lvc_nkey,
              tech_value TYPE lvc_value,
              conv_value TYPE lvc_value,
            END OF ty_data.

    TYPES : BEGIN OF ty_tech_hier,
              charact    TYPE char30,
              id         TYPE lvc_nkey,
              pid        TYPE lvc_nkey,
              tech_value TYPE lvc_value,
              tech_type  TYPE char30,
              conv_value TYPE lvc_value,
            END OF ty_tech_hier.

    TYPES tt_hier_val TYPE STANDARD TABLE OF bapicharactvaluesdescr WITH DEFAULT KEY.

    DATA : mt_tech_hier  TYPE STANDARD TABLE OF ty_tech_hier,
           mt_data_hier1 TYPE STANDARD TABLE OF ty_data,
           mt_data_hier2 TYPE STANDARD TABLE OF ty_data,
           mt_out1       TYPE STANDARD TABLE OF ty_data,
           mt_out2       TYPE STANDARD TABLE OF ty_data.



    DATA mt_header  TYPE slis_t_listheader.


    DATA : mo_tree1 TYPE REF TO cl_gui_alv_tree,
           mo_tree2 TYPE REF TO cl_gui_alv_tree,
           mo_cust  TYPE REF TO cl_gui_custom_container,
           mo_split TYPE REF TO cl_gui_splitter_container.

    METHODS execute.
    METHODS display.

    METHODS fill_data.

    METHODS get_hierarchy_vals
      IMPORTING iv_charactname TYPE bapicharactkey-charactname
      EXPORTING ev_descr       TYPE bapicharactdescr-description
                et_hier_val    TYPE tt_hier_val.

    METHODS get_nodes.

    METHODS use_tech_hier
      IMPORTING iv_charact TYPE char30
                iv_pid     TYPE lvc_nkey.

ENDCLASS.

CLASS lcl_cr_char IMPLEMENTATION.

  METHOD execute.

    fill_data( ).
    display( ).

  ENDMETHOD.


  METHOD fill_data.

   mt_tech_hier = VALUE #(
    charact = 'ATWRT1'
 ( id =  1           tech_value = 'ATWRT1'     tech_type  = 'H0'       )
 ( id =  2  pid = 1  tech_value = 'C_TL_BCF'   tech_type  = 'H'        )
 ( id =  3  pid = 2  tech_value = '1'          conv_value = 'TOR'      )
 ( id =  4  pid = 2  tech_value = '2'          tech_type  = 'H1'       )
 ( id =  5  pid = 4  tech_value = 'C_TTR_BCF'  tech_type  = 'H'        )
 ( id =  6  pid = 5  tech_value = 'SS'         conv_value = 'SUS'      )
 ( id =  7  pid = 5  tech_value = 'SC'         conv_value = 'SUS'      )
 ( id =  8  pid = 5  tech_value = 'SU'         tech_type  = 'H1'       )
 ( id =  9  pid = 5  tech_value = '/SU'        tech_type  = 'H1'       )
 ( id = 10  pid = 8  tech_value = 'C_TF_BCF'   tech_type  = 'H'  conv_value = 'SUP FRISE' )
 ( id = 11  pid = 9  tech_value = 'C_TF_BCF'   tech_type  = 'H'  conv_value = 'SUP FRISE' )
 ( id = 12  pid = 10 tech_value = 'Z'          conv_value = 'SUP'      )
 ( id = 13  pid = 11 tech_value = 'Z'          conv_value = 'SUP'      )
 ( id = 14  pid = 2  tech_value = '3'          conv_value = 'INT'      )
 ( id = 15  pid = 2  tech_value = '4'          conv_value = 'ROCC SSM' )
 ( id = 16  pid = 2  tech_value = '5'          conv_value = 'MAG'      )
 ( id = 17  pid = 2  tech_value = '6'          tech_type  = 'H1'       )
 ( id = 18  pid = 17 tech_value = 'C_TTI_BCF'  tech_type  = 'H'    conv_value = 'SUP' )
 ( id = 19  pid = 18 tech_value = 'KDK'        conv_value = 'TIN'      )
 ( id = 20  pid = 2  tech_value = '7'          conv_value = 'STI'      )
 ( id = 21  pid = 2  tech_value = '8'          conv_value = 'ROC'      )
 ( id = 22  pid = 2  tech_value = '9'          conv_value = 'BIN'      )
 ( id = 23  pid = 1  tech_value = 'BASE'       tech_type  = 'H0' conv_value = 'BASE'      )

  charact = 'ATWRT2'
( id =  1           tech_value = 'ATWRT2'     tech_type  = 'H0' conv_value = 'RIL' )
 ( id =  2  pid = 1  tech_value = 'BASE'       tech_type  = 'H0' conv_value = 'C_COLORE'       )
 ( id =  3  pid = 2  tech_value = 'C_PO_BCF'   tech_type  = 'H'         )
 ( id =  4  pid = 3  tech_value = 'PP'         tech_type  = 'H1'        )
 ( id =  5  pid = 4  tech_value = 'C_COLORE'   tech_type  = 'H'  conv_value = 'PP ALTRO'      )
 ( id =  6  pid = 5  tech_value = 'TM'         conv_value = 'PP MONO'   )
 ( id =  7  pid = 5  tech_value = 'TF'         conv_value = 'PP MONO'   )
 ( id =  8  pid = 5  tech_value = 'TE'         conv_value = 'PP MONO'   )
 ( id =  9  pid = 5  tech_value = 'TR'         conv_value = 'PP TRIPLY' )
 ( id = 10  pid = 5  tech_value = 'TC'         conv_value = 'PP TRIPLY' )
 ( id = 11  pid = 5  tech_value = 'XT'         conv_value = 'PP TRIPLY' )
 ( id = 12  pid = 5  tech_value = 'TH'         conv_value = 'PP TRIPLY' )
 ( id = 13  pid = 3  tech_value = 'MTX'        conv_value = 'MTX'       )
 ( id = 14  pid = 3  tech_value = 'PBT'        conv_value = 'CORTERRA'  )
 ( id = 15  pid = 3  tech_value = 'PLA'        conv_value = 'PLA'       )

 ( id = 16  pid = 2  tech_value = 'C_COLORE'   tech_type  = 'H1' conv_value = 'C_TIT_BCF' )

( id = 17  pid = 16 tech_value = 'C_TIT_BCF'  tech_type  = 'H' conv_value = 'TM ALTRO' )
 ( id = 18  pid = 17 tech_value = '1000'       conv_value = 'TM TP' )
 ( id = 19  pid = 17 tech_value = '1020'       conv_value = 'TM TP' )
 ( id = 20  pid = 17 tech_value = '1350'       conv_value = 'TM CC' )
 ( id = 21  pid = 17 tech_value = '700'        conv_value = 'TM 700' )
 ( id = 22  pid = 17 tech_value = '1300'       tech_type  = 'H1' )
 ( id = 23  pid = 22 tech_value = 'C_NBA_BCF'  tech_type  = 'H' conv_value = 'TM ALTRO' )
 ( id = 24  pid = 23 tech_value = '128'        conv_value = 'TM AUTO' )
 ( id = 25  pid = 23 tech_value = '84'         conv_value = 'TM AUTO' )

 ( id = 26  pid = 2  tech_value = 'C_TIT_BCF'  tech_type  = 'H' conv_value = 'GG ALTRO' )
 ( id = 27  pid = 26 tech_value = '1000'       conv_value = 'GG LEN' )
 ( id = 28  pid = 26 tech_value = '1200'       conv_value = 'GG LEN' )
 ( id = 29  pid = 26 tech_value = '1330'       conv_value = 'GG LEN' )
 ( id = 30  pid = 26 tech_value = '700'        conv_value = 'GG VEL' )
 ( id = 31  pid = 26 tech_value = '900'        conv_value = 'GG VEL' )
 ( id = 32  pid = 26 tech_value = '1300'       conv_value = 'GG VEL' )
 ( id = 33  pid = 26 tech_value = '650'        tech_type  = 'H1' )
 ( id = 34  pid = 33 tech_value = 'C_NBA_BCF'  tech_type  = 'H' conv_value = 'GG ALTRO' )
 ( id = 35  pid = 34 tech_value = '64'         conv_value = 'GG VEL' )
 ( id = 36  pid = 34 tech_value = '42'         conv_value = 'GG LEN' )


( id = 37  pid = 1  tech_value = 'ROCC SSM'   tech_type  = 'H0' conv_value = 'C_COLORE' )
( id = 38  pid = 37 tech_value = 'C_COLORE'  tech_type  = 'H1'  conv_value = 'C_TI_BCF' )
( id = 39  pid = 38 tech_value = 'C_TI_BCF'   tech_type  = 'H'  conv_value = 'TM ALTRO' )

( id = 40  pid = 39 Tech_value = '1000'       conv_value = 'TM TP' )
 ( id = 41  pid = 39 tech_value = '1020'       conv_value = 'TM TP' )
 ( id = 42  pid = 39 tech_value = '1350'       conv_value = 'TM CC' )
 ( id = 43  pid = 39 tech_value = '700'        conv_value = 'TM 700' )
 ( id = 44  pid = 39 tech_value = '1300'        tech_type  = 'H1' )
 ( id = 45  pid = 44 tech_value = 'C_NB_BCF'  tech_type  = 'H' conv_value = 'TM ALTRO' )
 ( id = 46  pid = 45 tech_value = '128'        conv_value = 'TM AUTO' )
 ( id = 47  pid = 45 tech_value = '84'         conv_value = 'TM AUTO' )

 ( id = 48  pid = 38 tech_value = 'C_TI_BCF'  tech_type  = 'H' conv_value = 'GG ALTRO' )
 ( id = 49  pid = 48 tech_value = '1000'       conv_value = 'GG LEN' )
 ( id = 50  pid = 48 tech_value = '1200'       conv_value = 'GG LEN' )
 ( id = 51  pid = 48 tech_value = '1330'       conv_value = 'GG LEN' )
 ( id = 52  pid = 48 tech_value = '700'        conv_value = 'GG VEL' )
 ( id = 53  pid = 48 tech_value = '900'        conv_value = 'GG VEL' )
 ( id = 54  pid = 48 tech_value = '1300'       conv_value = 'GG VEL' )
 ( id = 55  pid = 48 tech_value = '650'        tech_type  = 'H1' )
 ( id = 56  pid = 55 tech_value = 'C_NB_BCF'  tech_type  = 'H' conv_value = 'GG ALTRO' )
 ( id = 57  pid = 56 tech_value = '64'         conv_value = 'GG VEL' )
 ( id = 58  pid = 56 tech_value = '42'         conv_value = 'GG LEN' )

 ( id = 59  pid = 1 tech_value = 'RIL'  tech_type  = 'H0'  conv_value = 'RIL' )


).

    SORT mt_tech_hier BY charact pid id.

    use_tech_hier(
      EXPORTING
        iv_charact = 'ATWRT1'
        iv_pid     = ''
    ).

    use_tech_hier(
      EXPORTING
        iv_charact = 'ATWRT2'
        iv_pid     = ''
    ).

  ENDMETHOD.

  METHOD use_tech_hier.

    READ TABLE mt_tech_hier TRANSPORTING NO FIELDS
      WITH KEY charact = iv_charact
               pid     = iv_pid BINARY SEARCH.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE iv_charact.
      WHEN 'ATWRT1'.
        DATA(lv_last_id) = lines( mt_data_hier1 ).
      WHEN OTHERS.
        lv_last_id = lines( mt_data_hier2 ).
    ENDCASE.

    LOOP AT mt_tech_hier INTO DATA(ls_data) FROM sy-tabix.
      IF ls_data-charact <> iv_charact
      OR ls_data-pid     <> iv_pid.
        EXIT.
      ENDIF.

      IF ls_data-conv_value IS NOT INITIAL AND ls_data-tech_type IS INITIAL .
        CONTINUE.
      ENDIF.

     CASE ls_data-tech_type.
*        WHEN 'H1'.
*
*          use_tech_hier(
*            EXPORTING
*              iv_charact = iv_charact
*              iv_pid     = ls_data-id
*          ).
 WHEN 'H1'.
        " Process sub-hierarchy node
        DATA(ls_data_hier) = VALUE ty_data(
          charact    = ls_data-charact
          hier_name  = ls_data-tech_value
          node_pid   = lv_last_id
          tech_id    = ls_data-id
          tech_pid   = ls_data-pid
          tech_value = ls_data-tech_value
          conv_value = ls_data-conv_value
        ).

        CASE iv_charact.
          WHEN 'ATWRT1'.
            APPEND ls_data_hier TO mt_data_hier1.
            DATA(lv_h_key) = lines( mt_data_hier1 ).
          WHEN OTHERS.
            APPEND ls_data_hier TO mt_data_hier2.
            lv_h_key = lines( mt_data_hier2 ).
        ENDCASE.

        " Recursively process children of this node
        use_tech_hier(
          EXPORTING
            iv_charact = iv_charact
            iv_pid     = ls_data-id
        ).

        WHEN 'H'.

          get_hierarchy_vals(
            EXPORTING
              iv_charactname = CONV #( ls_data-tech_value )
            IMPORTING
              ev_descr       = DATA(lv_descr)
              et_hier_val    = DATA(lt_hier)
          ).


        IF   ls_data-tech_type = 'H'  AND  lt_hier IS INITIAL.
          LOOP AT mt_tech_hier INTO DATA(ls_child) WHERE Pid = ls_data-id  AND charact = 'ATWRT2'.

            APPEND VALUE #(
              value_char  = ls_child-tech_value
              description = ls_child-tech_value
            ) TO lt_hier.

          ENDLOOP.
        ENDIF.

          ls_data_hier = VALUE ty_data(
            charact    = ls_data-charact
            hier_name  = lv_descr
            node_pid   = lv_last_id
            tech_id    = ls_data-id
            tech_pid   = ls_data-pid
            tech_value = ls_data-tech_value
            conv_value = ls_data-conv_value
          ).

          CASE iv_charact.
            WHEN 'ATWRT1'.
              APPEND ls_data_hier TO mt_data_hier1.
              lv_h_key = lines( mt_data_hier1 ).
            WHEN OTHERS.
              APPEND ls_data_hier TO mt_data_hier2.
              lv_h_key = lines( mt_data_hier2 ).
          ENDCASE.


            LOOP AT lt_hier ASSIGNING FIELD-SYMBOL(<ls_hier>).

            READ TABLE mt_tech_hier INTO DATA(ls_hier_val)
              WITH KEY charact    = iv_charact
                       pid        = ls_data-id
                       tech_value = <ls_hier>-value_char.

            ls_data_hier = VALUE ty_data(
              charact    = ls_data-charact
              hier_name  = <ls_hier>-description
              node_pid   = lv_h_key
              tech_id    = ''
              tech_pid   = ls_data-pid
              tech_value = <ls_hier>-value_char
              conv_value = ls_hier_val-conv_value
            ).

            CASE iv_charact.
              WHEN 'ATWRT1'.
                APPEND ls_data_hier TO mt_data_hier1.
              WHEN OTHERS.
                APPEND ls_data_hier TO mt_data_hier2.
            ENDCASE.

            IF ls_hier_val-tech_type IS NOT INITIAL.
              use_tech_hier(
                EXPORTING
                  iv_charact = iv_charact
                  iv_pid     = ls_hier_val-id
              ).
            ENDIF.

            CLEAR ls_hier_val.


          ENDLOOP.

        WHEN 'H0'.

          ls_data_hier = VALUE ty_data(
             charact    = ls_data-charact
             hier_name  = COND #( WHEN ls_data-id = 1 AND iv_charact = 'ATWRT1'
                                  THEN 'Característica 1'
                                  WHEN ls_data-id = 1 AND iv_charact = 'ATWRT2'
                                  THEN 'Característica 2'
                                  ELSE ls_data-tech_value )
             node_pid   = iv_pid
             tech_id    = ls_data-id
             tech_pid   = ls_data-pid
             tech_value = ls_data-tech_value
             conv_value = ls_data-conv_value
           ).

          CASE iv_charact.
            WHEN 'ATWRT1'.
              APPEND ls_data_hier TO mt_data_hier1.
            WHEN OTHERS.
              APPEND ls_data_hier TO mt_data_hier2.
          ENDCASE.

          use_tech_hier(
            EXPORTING
              iv_charact = iv_charact
              iv_pid     = ls_data-id
          ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_hierarchy_vals.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2,
          lt_descr  TYPE STANDARD TABLE OF bapicharactdescr.

    DATA: lt_hier LIKE mt_tech_hier,
      lv_color TYPE string,
      ls_hier LIKE LINE OF mt_tech_hier.

    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
      EXPORTING
        charactname        = iv_charactname
        language           = 'I'
      TABLES
        charactdescr       = lt_descr
        charactvaluesdescr = et_hier_val
        return             = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF lt_descr IS NOT INITIAL.
      ev_descr = lt_descr[ 1 ]-description.
    ENDIF.

    loop at mt_tech_hier ASSIGNING FIELD-SYMBOL(<ls_tech_hier>).


      endloop.

  ENDMETHOD.
  METHOD get_nodes.

    LOOP AT mt_data_hier1 ASSIGNING FIELD-SYMBOL(<ls_hier>).


      mo_tree1->add_node(
        EXPORTING
          i_relat_node_key     = <ls_hier>-node_pid
          i_relationship       = cl_gui_column_tree=>relat_last_child
          is_outtab_line       = <ls_hier>
          i_node_text          = <ls_hier>-hier_name
        IMPORTING
          e_new_node_key       = DATA(lv_key)
        EXCEPTIONS
          relat_node_not_found = 1
          node_not_found       = 2
          OTHERS               = 3
      ).
    ENDLOOP.

    LOOP AT mt_data_hier2 ASSIGNING <ls_hier>.

      mo_tree2->add_node(
        EXPORTING
          i_relat_node_key     = <ls_hier>-node_pid
          i_relationship       = cl_gui_column_tree=>relat_last_child
          is_outtab_line       = <ls_hier>
          i_node_text          = <ls_hier>-hier_name
        IMPORTING
          e_new_node_key       = lv_key
        EXCEPTIONS
          relat_node_not_found = 1
          node_not_found       = 2
          OTHERS               = 3
      ).
    ENDLOOP.

  ENDMETHOD.
  METHOD display.

    DATA: lt_events      TYPE cntl_simple_events,
          ls_node_layout TYPE lvc_s_layn.

    CREATE OBJECT mo_cust
      EXPORTING
        container_name              = 'TREE_CONT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_split
      EXPORTING
        parent            = mo_cust
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    CREATE OBJECT mo_tree1
      EXPORTING
        parent                      = mo_split->get_container( row = 1 column = 1 )
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_tree2
      EXPORTING
        parent                      = mo_split->get_container( row = 1 column = 2 )
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    DATA(lt_fcat) = VALUE lvc_t_fcat(
      tabname = 'MT_OUT'
      ( fieldname = 'TECH_VALUE' scrtext_s = 'Tech.Value' scrtext_m = 'Tech. Value'  outputlen = 30 )
      ( fieldname = 'CONV_VALUE' scrtext_s = 'End Value'  scrtext_m = 'End Value'    outputlen = 30 )
  ).

    DATA(ls_header) = VALUE treev_hhdr(
      heading = 'Description'
      tooltip = 'Description'
      width   = 40
    ).

    mo_tree1->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header  = ls_header
      CHANGING
        it_outtab            = mt_out1
        it_fieldcatalog      = lt_fcat
    ).

    mo_tree2->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header  = ls_header
      CHANGING
        it_outtab            = mt_out2
        it_fieldcatalog      = lt_fcat
    ).

    get_nodes( ).


    mo_tree1->frontend_update( ).
    mo_tree2->frontend_update( ).


    CALL SCREEN 0100.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_cr_char( )->execute( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF1'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'FC_BACK' OR 'FC_EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
