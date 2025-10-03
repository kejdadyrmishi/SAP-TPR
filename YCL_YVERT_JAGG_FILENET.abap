class YCL_YVERT_JAGG_FILENET definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_attr,
        filenet_doc_type TYPE yvert_jag_fnet-filenet_doc_type,
        bukrs            TYPE bukrs,
        cig              TYPE zplt03_cont_head-cig,
      END OF ty_attr .
  types:
    BEGIN OF ty_form_data,
        key     TYPE string,
        str_val TYPE string,
        file    TYPE xstring,
      END OF ty_form_data .
  types:
    tt_alleg TYPE STANDARD TABLE OF yvert_coda_alleg .
  types:
    tt_rub_head TYPE STANDARD TABLE OF zplt03_rubr_head .
  types:
    tt_rub_att TYPE STANDARD TABLE OF zplt03_rub_attac .
  types:
    tt_form_data TYPE STANDARD TABLE OF ty_form_data WITH EMPTY KEY .
  types:
    BEGIN OF ty_coda_ret.
        INCLUDE TYPE yvert_coda_alleg.
        TYPES: filenet_doc_type TYPE yvert_jag_fnet-filenet_doc_type,
        filenet_guid     TYPE yvert_jag_fnet-filenet_guid,
        file_name        TYPE yvert_jag_fnet-file_name,
        message          TYPE char255,
      END OF ty_coda_ret .
  types:
    tt_coda_ret TYPE STANDARD TABLE OF ty_coda_ret .

  data MT_CODA_RET type TT_CODA_RET read-only .

  methods CONSTRUCTOR
    importing
      !IT_ALLEG type TT_ALLEG optional .
  class-methods GET_JAGGAER_ATTACHMENT
    importing
      !IV_FILE_ID type YVERT_JAG_FNET-FILE_ID
      !IV_FILE_NAME type YVERT_JAG_FNET-FILE_NAME
    returning
      value(RV_CONTENT) type XSTRING
    raising
      YCX_YVERT_JAGG_FILENET .
  class-methods GET_SAP_TABLE_KEY_STRUCT
    importing
      !IV_TABNAME type TABNAME
    returning
      value(RO_KEY_STRUCT) type ref to DATA .
  methods UPLOAD_ALL_TO_FILENET .
  class-methods GET_JAGGAER_AUTH_TOKEN
    returning
      value(RV_TOKEN) type STRING
    raising
      YCX_YVERT_JAGG_FILENET .
  class-methods POST_FORM_TO_JAGGAER
    importing
      !IT_FORM type TT_FORM_DATA
    raising
      YCX_YVERT_JAGG_FILENET .
  class-methods CREATE_CLIENT
    importing
      !IV_TIPO_SERVIZIO type YVERT_URL_API-TIPO_SERVIZIO
      !IV_URL_ADDON type STRING optional
      !IV_METHOD type STRING
      !IV_HEAD_ACCEPT type STRING default '*/*'
      !IV_HEAD_CONT_TYPE type STRING default '*/*'
      !IV_HEAD_AUTH type STRING optional
      !IV_ACCEPT_COOKIE type I default IF_HTTP_CLIENT=>CO_DISABLED
      !IV_PROXY_HOST type STRING default '10.222.204.170'
      !IV_PROXY_SERVICE type STRING default '8097'
    returning
      value(RO_CLIENT) type ref to IF_HTTP_CLIENT .
  class-methods CLIENT_SEND_RECEIVE
    importing
      !IO_CLIENT type ref to IF_HTTP_CLIENT
      !IV_BIN type FLAG optional
    exporting
      !EV_HTTP_CODE type I
      !EV_HTTP_RESPONSE type STRING
      !EV_ASC type STRING
      !EV_BIN type XSTRING .
  class-methods GET_JAGGAER_CONTR_BY_ECM
    importing
      !IV_ECM type ZPLT03_CONT_HEAD-REFERENCE_CODE
    exporting
      value(ES_JAGG_CONTRACT) type YVERT_BS_CONTRAT
      !EV_MESSAGE type BAPIRETURN-MESSAGE .
  class-methods GET_JAGGAER_CONTR_BY_FOLDER
    importing
      !IV_CARTELLA type ZPLT03_CONT_HEAD-TENDER_REFERENCE
    exporting
      value(ET_JAGG_CONTRACTS) type YVERT_BS_CONTRACT_TAB
      !EV_MESSAGE type BAPIRETURN-MESSAGE .
  class-methods UPDATE_JAGGAER_CONTR_VALUE
    importing
      !IV_ECM type ZPLT03_CONT_HEAD-REFERENCE_CODE
      !IV_ECM_VALUE type KTWRT
    returning
      value(RV_MESSAGE) type BAPIRETURN-MESSAGE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_jag_fnet.
        INCLUDE TYPE yvert_jag_fnet.
        TYPES: read_file  TYPE abap_bool,
        error_file TYPE abap_bool,
      END OF ty_jag_fnet .

    DATA mt_alleg TYPE tt_alleg .
    DATA mt_rub_attac TYPE tt_rub_att .
    DATA mt_rub_head TYPE tt_rub_head .
    DATA:
      mt_jag_fnet TYPE STANDARD TABLE OF ty_jag_fnet .
    DATA cv_icon_ok TYPE icon_d VALUE icon_okay ##NO_TEXT.
    DATA cv_icon_ko TYPE icon_d VALUE icon_cancel ##NO_TEXT.
    CLASS-DATA ms_token TYPE yvert_jag_token .

    METHODS post_to_filenet
      IMPORTING
        !is_attr      TYPE ty_attr
        !iv_content   TYPE xstring
      CHANGING
        !cs_file_prop TYPE ty_jag_fnet
      RAISING
        ycx_yvert_jagg_filenet .
    METHODS get_where_coda_alleg
      IMPORTING
        !iv_tabname     TYPE yvert_coda_alleg-tabname
        !iv_tabkey      TYPE yvert_coda_alleg-tabkey
      CHANGING
        VALUE(cv_where) TYPE string
        !co_data        TYPE REF TO data .
    METHODS get_metadata_doc_contrattuale
      IMPORTING
        !is_attributes     TYPE ty_attr
      RETURNING
        VALUE(rt_metadata) TYPE yvert_api_go2doc_ty_attribut_t .
    METHODS get_metadata_contratto
      IMPORTING
        !is_attributes     TYPE ty_attr
      RETURNING
        VALUE(rt_metadata) TYPE yvert_api_go2doc_ty_attribut_t .
    METHODS get_metadata
      IMPORTING
        !is_attributes     TYPE ty_attr
      RETURNING
        VALUE(rt_metadata) TYPE yvert_api_go2doc_ty_attribut_t .
ENDCLASS.



CLASS YCL_YVERT_JAGG_FILENET IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>CLIENT_SEND_RECEIVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CLIENT                      TYPE REF TO IF_HTTP_CLIENT
* | [--->] IV_BIN                         TYPE        FLAG(optional)
* | [<---] EV_HTTP_CODE                   TYPE        I
* | [<---] EV_HTTP_RESPONSE               TYPE        STRING
* | [<---] EV_ASC                         TYPE        STRING
* | [<---] EV_BIN                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD client_send_receive.

    io_client->send(
     EXCEPTIONS
      http_communication_failure = 1                  " Communication Error
      http_invalid_state         = 2                  " Invalid state
      http_processing_failed     = 3                  " Error when processing method
      http_invalid_timeout       = 4                  " Invalid Time Entry
      OTHERS                     = 5 ).

    IF sy-subrc = 0.
      io_client->receive(
    EXCEPTIONS
      http_communication_failure = 1                " Communication Error
      http_invalid_state         = 2                " Invalid state
      http_processing_failed     = 3                " Error when processing method
      OTHERS                     = 4
  ).
    ENDIF.

    io_client->response->get_status(
        IMPORTING
          code   = ev_http_code
          reason = ev_http_response ).

    IF iv_bin IS NOT INITIAL.
      ev_bin = io_client->response->get_data( ).
    ELSE.
      ev_asc = io_client->response->get_cdata( ).
    ENDIF.

    io_client->close( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_YVERT_JAGG_FILENET->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ALLEG                       TYPE        TT_ALLEG(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    IF it_alleg IS SUPPLIED.
      mt_alleg = it_alleg.
    ELSE.

      SELECT *
        FROM yvert_coda_alleg
        WHERE finished IS INITIAL
        INTO TABLE @mt_alleg.

    ENDIF.

    IF mt_alleg IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM zplt03_rub_attac
      FOR ALL ENTRIES IN @mt_alleg
      WHERE fileid = @mt_alleg-file_id
      INTO TABLE @mt_rub_attac.
    IF sy-subrc = 0.
      SORT mt_rub_attac BY fileid.

      SELECT *
        FROM zplt03_rubr_head
        FOR ALL ENTRIES IN @mt_rub_attac
        WHERE id_rub = @mt_rub_attac-id_rub
        INTO TABLE @mt_rub_head.

      SORT mt_rub_head BY id_rub.
    ENDIF.

    SELECT *
      FROM yvert_jag_fnet
      FOR ALL ENTRIES IN @mt_alleg
      WHERE file_id = @mt_alleg-file_id
      ORDER BY PRIMARY KEY
      INTO TABLE @mt_jag_fnet.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>CREATE_CLIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TIPO_SERVIZIO               TYPE        YVERT_URL_API-TIPO_SERVIZIO
* | [--->] IV_URL_ADDON                   TYPE        STRING(optional)
* | [--->] IV_METHOD                      TYPE        STRING
* | [--->] IV_HEAD_ACCEPT                 TYPE        STRING (default ='*/*')
* | [--->] IV_HEAD_CONT_TYPE              TYPE        STRING (default ='*/*')
* | [--->] IV_HEAD_AUTH                   TYPE        STRING(optional)
* | [--->] IV_ACCEPT_COOKIE               TYPE        I (default =IF_HTTP_CLIENT=>CO_DISABLED)
* | [--->] IV_PROXY_HOST                  TYPE        STRING (default ='10.222.204.170')
* | [--->] IV_PROXY_SERVICE               TYPE        STRING (default ='8097')
* | [<-()] RO_CLIENT                      TYPE REF TO IF_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_client.

    SELECT SINGLE host, url
      FROM yvert_url_api
      INTO @DATA(ls_link)
      WHERE tipo_servizio = @iv_tipo_servizio.

    DATA(lv_calling_url) = ls_link-host && ls_link-url && iv_url_addon.



** aggiornamento 13092025
    DATA w_rfcdes TYPE rfcdest.
* esempio valore
*H=prep-reg01.api.jaggaer.com,D=ADSUSER_ASQ,G=proxy,g=3128,I=443,W=Y,B=N,
    DATA: lv_rfcoptions TYPE rfcoptions,
          lv_token      TYPE string,
          lt_tokens     TYPE STANDARD TABLE OF string.

    DATA lv_proxy_host TYPE string.
    DATA lv_proxy_service TYPE string.

    SELECT SINGLE rfcoptions
      FROM rfcdes
      INTO lv_rfcoptions
      WHERE rfcdest = 'JAGGAER'.

    IF sy-subrc = 0.
      SPLIT lv_rfcoptions AT ',' INTO TABLE lt_tokens.
      READ  TABLE lt_tokens INTO lv_token INDEX 3.
      lv_proxy_host = lv_token+2. " dopo 'H='

      READ  TABLE lt_tokens INTO lv_token INDEX 4.
      lv_proxy_service = lv_token+2. " dopo 'S='
    ENDIF.






    cl_http_client=>create_by_url(
     EXPORTING
       url                = lv_calling_url
       ssl_id             = 'ANONYM'
*       proxy_host         = iv_proxy_host
*       proxy_service      = iv_proxy_service
       proxy_host         = lv_proxy_host
       proxy_service      = lv_proxy_service

     IMPORTING
       client             = ro_client
     EXCEPTIONS
       argument_not_found = 1
       plugin_not_active  = 2
       internal_error     = 3
       OTHERS             = 4 ).

    IF sy-subrc <> 0.
      IF sy-msgty IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      RETURN.
    ENDIF.

    ro_client->request->set_method( iv_method ).

    IF iv_head_accept IS NOT INITIAL.
      ro_client->request->set_header_field( name  = 'Accept' value = iv_head_accept ).
    ENDIF.

    IF iv_head_cont_type IS NOT INITIAL.
      ro_client->request->set_header_field(  name  = 'Content-Type' value = iv_head_cont_type ).
    ENDIF.

    IF iv_head_auth IS NOT INITIAL.
      ro_client->request->set_header_field(  name  = 'Authorization' value = iv_head_auth ).
    ENDIF.

    ro_client->propertytype_accept_cookie = iv_accept_cookie.
    ro_client->propertytype_logon_popup   = if_http_client=>co_disabled.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>GET_JAGGAER_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_ID                     TYPE        YVERT_JAG_FNET-FILE_ID
* | [--->] IV_FILE_NAME                   TYPE        YVERT_JAG_FNET-FILE_NAME
* | [<-()] RV_CONTENT                     TYPE        XSTRING
* | [!CX!] YCX_YVERT_JAGG_FILENET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_jaggaer_attachment.

    DATA(lv_token) = get_jaggaer_auth_token( ) .

    DATA(lv_file_id) = |{ iv_file_id }|.
    CONDENSE lv_file_id NO-GAPS.

    DATA(lv_url_addon) = |?fileId={ lv_file_id }|    &&
                         |&fileName={ iv_file_name }| .

    DATA(lo_http_client) =
    create_client(
      EXPORTING
        iv_tipo_servizio  = 'JAG_GET_AT'                             " Tipo servizio
        iv_url_addon      = lv_url_addon
        iv_method         = if_http_request=>co_request_method_get  " GET/POST etc..
        iv_head_accept    = '*/*'                                   " Accept json/zip/* etc
        iv_head_cont_type = '*/*'                                   " Content type
        iv_head_auth      = lv_token                                " Authorization token
        iv_accept_cookie  = if_http_client=>co_enabled              " Accept cookie
    ).

    client_send_receive(
     EXPORTING
       io_client        = lo_http_client     " HTTP Client
       iv_bin           = abap_true
     IMPORTING
       ev_http_code     = DATA(lv_http_code) " HTTP Status Code
       ev_http_response = DATA(lv_reason)    " HTTP status description
       ev_bin           = rv_content         " Response in BIN
   ).

    IF lv_http_code  <> 200.
      RAISE EXCEPTION TYPE ycx_yvert_jagg_filenet
        EXPORTING
          textid = ycx_yvert_jagg_filenet=>request_failed
          reason = CONV #( lv_reason )
          status = lv_http_code.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>GET_JAGGAER_AUTH_TOKEN
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TOKEN                       TYPE        STRING
* | [!CX!] YCX_YVERT_JAGG_FILENET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_jaggaer_auth_token.

    TYPES: BEGIN OF ty_response,
             access_token       TYPE string,
             expires_in         TYPE i,
             refresh_expires_in TYPE i,
             token_type         TYPE string,
             not_before_policy  TYPE i,
             scope              TYPE string,
           END OF ty_response.

    DATA: ls_response  TYPE ty_response,
          lv_timst_in  TYPE timestamp,
          lv_timst_out TYPE timestamp.

    IF ms_token IS INITIAL.
      SELECT SINGLE *
        FROM yvert_jag_token
        INTO @ms_token
        WHERE token_id = 'JAG_TOKEN'.
    ENDIF.

    lv_timst_in = sy-datum && sy-uzeit.
    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in    = lv_timst_in
        duration        = 10
      IMPORTING
        timestamp_out   = lv_timst_out
      EXCEPTIONS
        timestamp_error = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF ms_token-expire_datetime > lv_timst_out.
      rv_token = ms_token-token_value.
      RETURN.
    ENDIF.

    DATA(lo_http_client) =
    create_client(
      EXPORTING
        iv_tipo_servizio  = 'JAG_TOKEN'                             " Tipo servizio
        iv_method         = if_http_request=>co_request_method_post " GET/POST etc..
        iv_head_accept    = 'application/json'                      " Accept json/zip/* etc
        iv_head_cont_type = 'application/x-www-form-urlencoded'     " Content type
        iv_accept_cookie  = if_http_client=>co_enabled              " Accept cookie
    ).

    lo_http_client->request->set_form_field( name = 'client_id'     value = |{ ms_token-client_id }| ).
    lo_http_client->request->set_form_field( name = 'client_secret' value = |{ ms_token-client_secret }| ).
    lo_http_client->request->set_form_field( name = 'grant_type'    value = |client_credentials| ).

    client_send_receive(
      EXPORTING
        io_client        = lo_http_client     " HTTP Client
      IMPORTING
        ev_http_code     = DATA(lv_http_code) " HTTP Status Code
        ev_http_response = DATA(lv_reason)    " HTTP status description
        ev_asc           = DATA(lv_cdata)     " Response in ASC
    ).

    IF lv_http_code <> 200.

      RAISE EXCEPTION TYPE ycx_yvert_jagg_filenet
        EXPORTING
          textid = ycx_yvert_jagg_filenet=>token_not_retrieved
          reason = CONV #( lv_reason ).
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_cdata
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_response ).

    ms_token-token_value = |{ ls_response-token_type } { ls_response-access_token }|.

    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in    = lv_timst_in
        duration        = ls_response-expires_in
      IMPORTING
        timestamp_out   = lv_timst_out
      EXCEPTIONS
        timestamp_error = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ms_token-expire_datetime = lv_timst_out.

    UPDATE yvert_jag_token FROM ms_token.
    COMMIT WORK.

    rv_token = ms_token-token_value.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>GET_JAGGAER_CONTR_BY_ECM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ECM                         TYPE        ZPLT03_CONT_HEAD-REFERENCE_CODE
* | [<---] ES_JAGG_CONTRACT               TYPE        YVERT_BS_CONTRAT
* | [<---] EV_MESSAGE                     TYPE        BAPIRETURN-MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_jaggaer_contr_by_ecm.
    DATA: lv_xstring_auth TYPE xstring,
          lv_base64_auth  TYPE string,
          ls_contr_json   TYPE yvert_bs_web_service_response.

    SELECT SINGLE username, pw
      FROM yvert_cpi_pw
      INTO (@DATA(lv_username), @DATA(lv_password) ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = |{ lv_username }:{ lv_password }|
      IMPORTING
        buffer = lv_xstring_auth
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_xstring_auth
        IMPORTING
          output = lv_base64_auth.
    ENDIF.

    DATA(lo_http_client) =
     create_client(
       EXPORTING
         iv_tipo_servizio  = |CONTRACT|
         iv_method         = if_http_request=>co_request_method_get
         iv_head_accept    = |application/json, text/plain, */*|
         iv_head_cont_type = |application/json;charset=UTF-8|
         iv_accept_cookie  = if_http_client=>co_enabled
         iv_head_auth      = |Basic { lv_base64_auth }|
         iv_proxy_host     = |proxy|
         iv_proxy_service  = |3128|
     ).

    IF lo_http_client IS INITIAL.
      ev_message = 'Oggetto HTTP con errore'.
      RETURN.
    ENDIF.

    lo_http_client->request->set_form_field( name = 'start' value = |{ 1 }| ).
    lo_http_client->request->set_form_field(
      name  = 'flt'
      value = |contractReferenceCode=={ to_lower( iv_ecm ) };deletedStatus==NOT-DELETED| ).

    lo_http_client->request->set_form_field( name = 'deFlt'           value = |CTR_FLAG_INVIO==1| ).

    client_send_receive(
      EXPORTING
        io_client        = lo_http_client
      IMPORTING
        ev_http_code     = DATA(lv_http_code)      " HTTP Status Code
        ev_http_response = DATA(lv_http_response)  " HTTP status description
        ev_asc           = DATA(lv_json)           " Response in ASC
    ).

    IF lv_http_code <> 200.
      ev_message = |{ lv_http_code }: { lv_http_response }|.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json        = lv_json
                pretty_name = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING data        = ls_contr_json ).

    IF ls_contr_json-return_code <> 0 OR ls_contr_json-returned_records = 0.
      ev_message = ls_contr_json-return_message.
      RETURN.
    ENDIF.

    READ TABLE ls_contr_json-contract_list-item INTO es_jagg_contract INDEX 1.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>GET_JAGGAER_CONTR_BY_FOLDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CARTELLA                    TYPE        ZPLT03_CONT_HEAD-TENDER_REFERENCE
* | [<---] ET_JAGG_CONTRACTS              TYPE        YVERT_BS_CONTRACT_TAB
* | [<---] EV_MESSAGE                     TYPE        BAPIRETURN-MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_jaggaer_contr_by_folder.
    DATA: lv_xstring_auth TYPE xstring,
          lv_base64_auth  TYPE string,
          ls_contr_json   TYPE yvert_bs_web_service_response,
          lv_start        TYPE i VALUE 1.

    SELECT SINGLE username, pw
      FROM yvert_cpi_pw
      INTO (@DATA(lv_username), @DATA(lv_password) ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = |{ lv_username }:{ lv_password }|
      IMPORTING
        buffer = lv_xstring_auth
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_xstring_auth
        IMPORTING
          output = lv_base64_auth.
    ENDIF.

    DO.
      DATA(lo_http_client) =
       create_client(
         EXPORTING
           iv_tipo_servizio  = |CONTRACT|
           iv_method         = if_http_request=>co_request_method_get
           iv_head_accept    = |application/json, text/plain, */*|
           iv_head_cont_type = |application/json;charset=UTF-8|
           iv_accept_cookie  = if_http_client=>co_enabled
           iv_head_auth      = |Basic { lv_base64_auth }|
           iv_proxy_host     = |proxy|
           iv_proxy_service  = |3128|
       ).

      IF lo_http_client IS INITIAL.
        ev_message = 'Oggetto HTTP con errore'.
        EXIT.
      ENDIF.

      lo_http_client->request->set_form_field( name = 'start' value = |{ lv_start }| ).
      lo_http_client->request->set_form_field(
        name  = 'flt'
        value = |TENDER_REFERENCE_CODE=={ to_lower( iv_cartella ) };deletedStatus==NOT-DELETED| ).

      client_send_receive(
        EXPORTING
          io_client        = lo_http_client
        IMPORTING
          ev_http_code     = DATA(lv_http_code)      " HTTP Status Code
          ev_http_response = DATA(lv_http_response)  " HTTP status description
          ev_asc           = DATA(lv_json)           " Response in ASC
      ).

      IF lv_http_code <> 200.
        ev_message = |{ lv_http_code }: { lv_http_response }|.
        EXIT.
      ENDIF.

      /ui2/cl_json=>deserialize(
        EXPORTING json        = lv_json
                  pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data        = ls_contr_json ).

      IF ls_contr_json-return_code <> 0 OR ls_contr_json-returned_records = 0.
        ev_message = ls_contr_json-return_message.
        EXIT.
      ENDIF.

      APPEND LINES OF ls_contr_json-contract_list-item TO et_jagg_contracts.

      lv_start += ls_contr_json-returned_records.

      IF ( lv_start - 1 ) = ls_contr_json-tot_records.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_YVERT_JAGG_FILENET->GET_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ATTRIBUTES                  TYPE        TY_ATTR
* | [<-()] RT_METADATA                    TYPE        YVERT_API_GO2DOC_TY_ATTRIBUT_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_metadata.

    CASE is_attributes-filenet_doc_type.
      WHEN 'DocumentazioneContrattuale'.
        rt_metadata = get_metadata_doc_contrattuale( is_attributes = is_attributes ).

      WHEN 'Contratto'.
        rt_metadata = get_metadata_contratto( is_attributes = is_attributes ).

      WHEN 'InserimentoAppoggioBancario'.
      WHEN 'Attachment'.
      WHEN 'RicevutaPagamentoContributoQualificazione'.

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_YVERT_JAGG_FILENET->GET_METADATA_CONTRATTO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ATTRIBUTES                  TYPE        TY_ATTR
* | [<-()] RT_METADATA                    TYPE        YVERT_API_GO2DOC_TY_ATTRIBUT_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_metadata_contratto.

    rt_metadata = VALUE #(
  ( name = 'AnnoEsercizio'              val = sy-datum(4) )
  ( name = 'CFPIVADestinatario'         val = '-' )
  ( name = 'CFPIVAMittente'             val = '-' )
  ( name = 'CIG'                        val = COND #( WHEN is_attributes-cig IS NOT INITIAL THEN is_attributes-cig ELSE '-' ) )
  ( name = 'CodiceSottotipo'            val = 'CONTRATTO' )
  ( name = 'CodiceTipo'                 val = 'GESTIONE_CONTRATTI' )
  ( name = 'ConformitaCopie'            val = 'TRUE' )
  ( name = 'Contraente'                 val = '-' )
  ( name = 'DataRegistrazione'          val = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum+0(4) } 00:00:00| )
  ( name = 'Destinatario'               val = 'Destinatario' )
  ( name = 'FirmatoDigitalmente'        val = 'FALSE' )
  ( name = 'Formazione'                 val = 'A' )
  ( name = 'IDGara'                     val = '-' )
  ( name = 'IDPianoGare'                val = '-' )
  ( name = 'MarcaturaTemporale'         val = 'TRUE' )
  ( name = 'Mese'                       val = sy-datum+4(2) )
  ( name = 'Mittente'                   val = 'Mittente' )
  ( name = 'NumeroAllegati'             val = '0' )
  ( name = 'NumeroRubrica'              val = '-' )
  ( name = 'Oggetto'                    val = '-' )
  ( name = 'OLDGUID'                    val = 'NA' )
  ( name = 'ProgressivoDocumento'       val = '-' )
  ( name = 'RagSocDestinatario'         val = '-' )
  ( name = 'RagSocMittente'             val = '-' )
  ( name = 'RIA'                        val = '-' )
  ( name = 'Riservato'                  val = 'FALSE' )
  ( name = 'Rubrica'                    val = '-' )
  ( name = 'RubricaRiferimento'         val = '-' )
  ( name = 'SigillatoElettronicamente'  val = 'FALSE' )
  ( name = 'Societa'                    val = is_attributes-bukrs )
  ( name = 'StatoDocumento'             val = '0' )
  ( name = 'StrutturaOrganizzativa'     val = '-' )
  ( name = 'TempoConservazione'         val = '9999' )
  ( name = 'TipoContratto'              val = 'documentoautorizzato' )
  ( name = 'TipoFlusso'                 val = 'I' )
  ( name = 'TipoRegistro'               val = 'Nessuno' )
  ( name = 'VersioneDocumento'          val = '1' )
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_YVERT_JAGG_FILENET->GET_METADATA_DOC_CONTRATTUALE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ATTRIBUTES                  TYPE        TY_ATTR
* | [<-()] RT_METADATA                    TYPE        YVERT_API_GO2DOC_TY_ATTRIBUT_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_metadata_doc_contrattuale.

    rt_metadata = VALUE #(
      ( name = 'AnnoEsercizio'              val = sy-datum(4) )
      ( name = 'CFPIVADestinatario'         val = '-' )
      ( name = 'CFPIVAMittente'             val = '-' )
      ( name = 'CIG'                        val = COND #( WHEN is_attributes-cig IS NOT INITIAL THEN is_attributes-cig ELSE '-' ) )
      ( name = 'CodiceSottotipo'            val = 'DOCCONTRATTUALE' )
      ( name = 'CodiceTipo'                 val = 'GESTIONE_CONTRATTI' )
      ( name = 'ConformitaCopie'            val = 'TRUE' )
      ( name = 'Contraente'                 val = '-' )
      ( name = 'DataRegistrazione'          val = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum+0(4) } 00:00:00| )
      ( name = 'Destinatario'               val = 'Destinatario' )
      ( name = 'FirmatoDigitalmente'        val = 'FALSE' )
      ( name = 'Formazione'                 val = 'A' )
      ( name = 'IDGara'                     val = '-' )
      ( name = 'IDPianoGare'                val = '-' )
      ( name = 'MarcaturaTemporale'         val = 'TRUE' )
      ( name = 'Mese'                       val = sy-datum+4(2) )
      ( name = 'Mittente'                   val = 'Mittente' )
      ( name = 'NumeroAllegati'             val = '0' )
      ( name = 'NumeroRubrica'              val = '-' )
      ( name = 'Oggetto'                    val = '-' )
      ( name = 'OLDGUID'                    val = 'NA' )
      ( name = 'ProgressivoDocumento'       val = '-' )
      ( name = 'RagSocDestinatario'         val = '-' )
      ( name = 'RagSocMittente'             val = '-' )
      ( name = 'RIA'                        val = '-' )
      ( name = 'Riservato'                  val = 'FALSE' )
      ( name = 'Rubrica'                    val = '-' )
      ( name = 'RubricaRiferimento'         val = '-' )
      ( name = 'SigillatoElettronicamente'  val = 'FALSE' )
      ( name = 'Societa'                    val = is_attributes-bukrs )
      ( name = 'StatoDocumento'             val = '0' )
      ( name = 'StrutturaOrganizzativa'     val = '-' )
      ( name = 'TempoConservazione'         val = '9999' )
      ( name =  'TipoDocumContrattuale'     val = 'documentoautorizzato' )
      ( name = 'TipoFlusso'                 val = 'I' )
      ( name = 'TipoRegistro'               val = 'Nessuno' )
      ( name = 'VersioneDocumento'          val = '1' )
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>GET_SAP_TABLE_KEY_STRUCT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABNAME                     TYPE        TABNAME
* | [<-()] RO_KEY_STRUCT                  TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sap_table_key_struct.
    DATA: lt_comp TYPE abap_component_tab.

    SELECT fieldname,
           leng
      FROM dd03l
      WHERE tabname = @iv_tabname
        AND keyflag = @abap_true
      ORDER BY position ASCENDING
      INTO TABLE @DATA(lt_key_fields).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_key_fields ASSIGNING FIELD-SYMBOL(<ls_keys>).
      APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      <ls_comp>-name = <ls_keys>-fieldname.
      <ls_comp>-type ?= cl_abap_elemdescr=>get_c( CONV i( <ls_keys>-leng ) ).
    ENDLOOP.

    TRY.
        DATA(lo_struc) = cl_abap_structdescr=>create(
                          EXPORTING
                            p_components = lt_comp ).
      CATCH cx_sy_struct_creation .
    ENDTRY.

    CREATE DATA ro_key_struct TYPE HANDLE lo_struc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_YVERT_JAGG_FILENET->GET_WHERE_CODA_ALLEG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABNAME                     TYPE        YVERT_CODA_ALLEG-TABNAME
* | [--->] IV_TABKEY                      TYPE        YVERT_CODA_ALLEG-TABKEY
* | [<-->] CV_WHERE                       TYPE        STRING
* | [<-->] CO_DATA                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_where_coda_alleg.

    DATA: lt_comp_char TYPE abap_component_tab,
          lo_char_data TYPE REF TO data.

    CLEAR cv_where.

    SELECT fieldname,
           leng
      FROM dd03l
      WHERE tabname   = @iv_tabname
        AND keyflag   = @abap_true
      ORDER BY position ASCENDING
      INTO TABLE @DATA(lt_key_fields).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_key_fields ASSIGNING FIELD-SYMBOL(<ls_keys>).
      APPEND INITIAL LINE TO lt_comp_char ASSIGNING FIELD-SYMBOL(<ls_comp_char>).
      <ls_comp_char>-name = <ls_keys>-fieldname.
      <ls_comp_char>-type ?= cl_abap_elemdescr=>get_c( CONV i( <ls_keys>-leng ) ).

      IF <ls_keys>-fieldname = 'MANDT'.
        CONTINUE.
      ENDIF.

      DATA(lv_cond) = |{ <ls_keys>-fieldname } = @<ls_tab_al>-{ <ls_keys>-fieldname }|.
      IF cv_where IS INITIAL.
        cv_where = lv_cond.
      ELSE.
        cv_where = |{ cv_where } AND { lv_cond }|.
      ENDIF.
    ENDLOOP.

    TRY.
        DATA(lo_char_key) = cl_abap_structdescr=>create(
                          EXPORTING
                            p_components = lt_comp_char ).
      CATCH cx_sy_struct_creation .
    ENDTRY.

    CREATE DATA lo_char_data TYPE HANDLE lo_char_key.
    ASSIGN lo_char_data->* TO FIELD-SYMBOL(<ls_char_data>).

    <ls_char_data> = iv_tabkey.

    ASSIGN co_data->* TO FIELD-SYMBOL(<ls_real_data>).
    <ls_real_data> = CORRESPONDING #( <ls_char_data> ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>POST_FORM_TO_JAGGAER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_FORM                        TYPE        TT_FORM_DATA
* | [!CX!] YCX_YVERT_JAGG_FILENET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD post_form_to_jaggaer.

    DATA: BEGIN OF ls_response,
            return_code    TYPE i,
            return_message TYPE string,
          END OF ls_response.

    DATA(lv_token) = get_jaggaer_auth_token( ).

    DATA(lo_http_client) =
    create_client(
      EXPORTING
        iv_tipo_servizio  = 'JAG_POST'                               " Tipo servizio
        iv_method         = if_http_request=>co_request_method_post  " GET/POST etc..
        iv_head_cont_type = 'multipart/form-data'                    " Content type
        iv_head_auth      = lv_token
        iv_accept_cookie  = if_http_client=>co_enabled               " Accept cookie
    ).

    LOOP AT it_form ASSIGNING FIELD-SYMBOL(<ls_form>).

      DATA(lo_part_file) = lo_http_client->request->if_http_entity~add_multipart(  ).
      DATA(lv_file) = |form-data; name="{ <ls_form>-key }";|.

      IF <ls_form>-str_val IS NOT INITIAL.

        lo_part_file->set_header_field(
           EXPORTING
             name  = 'content-disposition'
             value = lv_file ).

        lo_part_file->set_content_type(
         EXPORTING
           content_type = 'application/json' ).

        lo_part_file->set_cdata( data = <ls_form>-str_val ).

      ELSE.

        lo_part_file->set_header_field(
         EXPORTING
           name  = 'content-disposition'
           value = lv_file ).

        lo_part_file->set_content_type(
                EXPORTING
                  content_type = 'application/bin' ).

        lo_part_file->set_data( data = <ls_form>-file ).

      ENDIF.

    ENDLOOP.

    client_send_receive(
      EXPORTING
        io_client        = lo_http_client     " HTTP Client
      IMPORTING
        ev_http_code     = DATA(lv_http_code) " HTTP Status Code
        ev_http_response = DATA(lv_reason)    " HTTP status description
        ev_asc           = DATA(lv_cdata)     " Response in ASC
    ).

    IF lv_http_code <> 200.

      IF lv_reason IS INITIAL.
        lv_reason = lv_cdata.
      ENDIF.

      RAISE EXCEPTION TYPE ycx_yvert_jagg_filenet
        EXPORTING
          textid = ycx_yvert_jagg_filenet=>request_failed
          status = lv_http_code
          reason = CONV #( lv_reason ).
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_cdata
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_response ).

    IF ls_response-return_code <> 0.
      RAISE EXCEPTION TYPE ycx_yvert_jagg_filenet
        EXPORTING
          textid = ycx_yvert_jagg_filenet=>request_failed
          status = ls_response-return_code
          reason = CONV #( ls_response-return_message ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_YVERT_JAGG_FILENET->POST_TO_FILENET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ATTR                        TYPE        TY_ATTR
* | [--->] IV_CONTENT                     TYPE        XSTRING
* | [<-->] CS_FILE_PROP                   TYPE        TY_JAG_FNET
* | [!CX!] YCX_YVERT_JAGG_FILENET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD post_to_filenet.

    DATA: ls_request     TYPE yvert_api_go2doc_ty_reqdoc_s,
          lt_return      TYPE bapiret2_t,
          lv_response    TYPE string,
          lv_http_status TYPE i,
          lv_reason      TYPE string,
          lt_attr        TYPE yvert_api_go2doc_ty_attribut_t,
          lv_doc_type    TYPE string.

    ls_request-document-content-name  = cs_file_prop-file_name.

    READ TABLE mt_rub_attac ASSIGNING FIELD-SYMBOL(<ls_att>)
                              WITH KEY fileid = cs_file_prop-file_id BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE mt_rub_head ASSIGNING FIELD-SYMBOL(<ls_head>)
                                WITH KEY id_rub = <ls_att>-id_rub BINARY SEARCH.
    ENDIF.

    IF <ls_head> IS ASSIGNED.

      CALL FUNCTION 'YVERT_ESTRAI_METADATA_4_SIGN'
        EXPORTING
          im_ria        = <ls_head>        " Tabella Rubrica Testata
          im_att        = <ls_att>        " Paperless - Rubrica Tabella attachment da interf. Contract
        CHANGING
          tb_attributes = lt_attr
          document_type = lv_doc_type.      " Tabella metadati

      ls_request-document-document_type = lv_doc_type.
      ls_request-document-attributes    = lt_attr.

    ELSE.
*DocumentazioneContrattuale
      ls_request-document-document_type = cs_file_prop-filenet_doc_type.
      ls_request-document-attributes = get_metadata( is_attributes = is_attr ).

    ENDIF.

    CALL FUNCTION 'YVERT_API_GO2DOC_DOC_CREATE'
      EXPORTING
        json_data_body = ls_request
        file           = iv_content
      IMPORTING
        document_id    = cs_file_prop-filenet_guid
        et_return      = lt_return
        ev_response    = lv_response
        ev_http_status = lv_http_status
        ev_reason      = lv_reason.

    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE ycx_yvert_jagg_filenet
        EXPORTING
          textid = ycx_yvert_jagg_filenet=>request_failed
          reason = CONV #( lv_reason )
          status = lv_http_status.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_YVERT_JAGG_FILENET=>UPDATE_JAGGAER_CONTR_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ECM                         TYPE        ZPLT03_CONT_HEAD-REFERENCE_CODE
* | [--->] IV_ECM_VALUE                   TYPE        KTWRT
* | [<-()] RV_MESSAGE                     TYPE        BAPIRETURN-MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_jaggaer_contr_value.

    TYPES: BEGIN OF ty_contr,
             value          TYPE zplt03_cont_head-value,
             reference_code TYPE zplt03_cont_head-reference_code,
           END OF ty_contr.

    TYPES: BEGIN OF ty_post,
             contract       TYPE ty_contr,
             operation_code TYPE numc3,
           END OF ty_post.

    DATA: lv_xstring_auth TYPE xstring,
          lv_base64_auth  TYPE string,
          ls_post         TYPE ty_post.


    TRY.
        DATA(lv_token) = get_jaggaer_auth_token( ).
      CATCH ycx_yvert_jagg_filenet INTO DATA(lx_jagg).
        rv_message = lx_jagg->get_longtext( ).
        RETURN.
    ENDTRY.

    DATA(lo_http_client) =
     ycl_yvert_jagg_filenet=>create_client(
       EXPORTING
         iv_tipo_servizio  = |JAG_RIA_PS|
         iv_method         = if_http_request=>co_request_method_post
         iv_head_auth      = lv_token
     ).

    IF lo_http_client IS INITIAL.
      rv_message = 'Oggetto HTTP con errore'.
      EXIT.
    ENDIF.

    ls_post = VALUE #(
      contract       = VALUE #( value          = iv_ecm_value
                                reference_code = to_lower( iv_ecm )
                                )
      operation_code = '004' ).

    DATA(lv_post_json) =
    /ui2/cl_json=>serialize(
      EXPORTING
        data             = ls_post
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        numc_as_string   = abap_true
    ).

    lo_http_client->request->set_cdata(
      EXPORTING
        data   = lv_post_json
    ).

    client_send_receive(
      EXPORTING
        io_client        = lo_http_client
      IMPORTING
        ev_http_code     = DATA(lv_http_code)      " HTTP Status Code
        ev_http_response = DATA(lv_http_response)  " HTTP status description
        ev_asc           = DATA(lv_json)           " Response in ASC
    ).

    IF lv_http_code <> 200.
      rv_message = |{ lv_http_code }: { lv_http_response }|.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_YVERT_JAGG_FILENET->UPLOAD_ALL_TO_FILENET
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_all_to_filenet.

    DATA: ls_attr  TYPE ty_attr,
          lo_tab   TYPE REF TO data,
          lv_where TYPE string,
          lv_file  TYPE xstring,
          lv_ok    TYPE i,
          lv_ko    TYPE i.

    FIELD-SYMBOLS: <ls_tab_al> TYPE any,
                   <lv_val>    TYPE any.

    DATA(lv_tot_lines) = lines( mt_alleg ).
    LOOP AT mt_alleg ASSIGNING FIELD-SYMBOL(<ls_alleg>).

      CLEAR: ls_attr,
             lo_tab,
             lv_where.

      UNASSIGN: <ls_tab_al>, <lv_val>.

      cl_progress_indicator=>progress_indicate(
          i_text                = |Processing: { sy-tabix }/{ lv_tot_lines }; { lv_ok } OK - { lv_ko } KO |
          i_processed           = sy-tabix
          i_total               = lv_tot_lines
          i_output_immediately  = abap_true ).

      lv_ko += 1.

      APPEND INITIAL LINE TO mt_coda_ret ASSIGNING FIELD-SYMBOL(<ls_coda_ret>).
      <ls_coda_ret> = CORRESPONDING #( <ls_alleg> ).

      IF <ls_alleg>-tabname  IS NOT INITIAL
       AND <ls_alleg>-fname  IS NOT INITIAL
       AND <ls_alleg>-tabkey IS NOT INITIAL.

        TRY.
            CREATE DATA lo_tab TYPE (<ls_alleg>-tabname).
          CATCH cx_root INTO DATA(lx_root).
            <ls_coda_ret>-message = lx_root->get_longtext( ).
            CONTINUE.
        ENDTRY.

        ASSIGN lo_tab->* TO <ls_tab_al>.

        get_where_coda_alleg(
          EXPORTING
            iv_tabname = <ls_alleg>-tabname   " Nome tabella
            iv_tabkey  = <ls_alleg>-tabkey   " Chiave della tabella dove si salva docid
          CHANGING
            cv_where   = lv_where
            co_data    = lo_tab
        ).

        SELECT SINGLE *
          FROM (<ls_alleg>-tabname)
          WHERE (lv_where)
          INTO @<ls_tab_al>.
        IF sy-subrc <> 0.
          <ls_coda_ret>-message = |{ cv_icon_ko }Chiave non trovata su { <ls_alleg>-tabname }|.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <ls_alleg>-fname OF STRUCTURE <ls_tab_al> TO <lv_val>.
        IF sy-subrc <> 0.
          <ls_coda_ret>-message = |{ cv_icon_ko }Campo { <ls_alleg>-fname } non trovato su { <ls_alleg>-tabname }|.
          CONTINUE.
        ENDIF.

        CASE <ls_alleg>-tabname.
          WHEN 'ZPLT03_CONT_HEAD'.
            ASSIGN COMPONENT 'CIG' OF STRUCTURE <ls_tab_al> TO FIELD-SYMBOL(<lv_cig>).
            ls_attr-cig = <lv_cig>.

            UNASSIGN <lv_cig>.

          WHEN 'ZPLT02_CONT_ATT'.
            ASSIGN COMPONENT 'ID'             OF STRUCTURE <ls_tab_al> TO FIELD-SYMBOL(<lv_cont_id>).
            ASSIGN COMPONENT 'REFERENCE_CODE' OF STRUCTURE <ls_tab_al> TO FIELD-SYMBOL(<lv_cont_ecm>).

            SELECT cig
              FROM zplt03_cont_head
              INTO @ls_attr-cig
              UP TO 1 ROWS
              WHERE id             = @<lv_cont_id>
                AND reference_code = @<lv_cont_ecm>
              ORDER BY dataimp DESCENDING, oraimp DESCENDING.
            ENDSELECT.

            UNASSIGN: <lv_cont_id>, <lv_cont_ecm>.

          WHEN 'ZPLT02_PDG' OR 'ZPLT03_PDG_ATT'.
            ASSIGN COMPONENT 'TENDERCODE'     OF STRUCTURE <ls_tab_al> TO FIELD-SYMBOL(<lv_tendercode>).

            SELECT cig
              FROM zplt03_cont_head
              INTO @ls_attr-cig
              UP TO 1 ROWS
              WHERE tendercode = @<lv_tendercode>
              ORDER BY dataimp DESCENDING, oraimp DESCENDING.
            ENDSELECT.

            UNASSIGN <lv_tendercode>.

        ENDCASE.

      ENDIF.

      READ TABLE mt_jag_fnet ASSIGNING FIELD-SYMBOL(<ls_jag_fnet>)
      WITH KEY file_id = <ls_alleg>-file_id BINARY SEARCH.

      IF sy-subrc <> 0.
        <ls_coda_ret>-message = |{ cv_icon_ko }YVERT_JAG_FNET: File non trovato|.
        CONTINUE.
      ENDIF.

      <ls_coda_ret>-file_name        = <ls_jag_fnet>-file_name.
      <ls_coda_ret>-filenet_doc_type = <ls_jag_fnet>-filenet_doc_type.

      IF <ls_jag_fnet>-file_name IS INITIAL.
        <ls_coda_ret>-message = |{ cv_icon_ko }YVERT_JAG_FNET: File name e vuoto|.
        CONTINUE.
      ENDIF.


      IF <ls_jag_fnet>-filenet_doc_type IS INITIAL.
        <ls_coda_ret>-message = |{ cv_icon_ko }YVERT_JAG_FNET: FileNet doc. type e vuoto|.
        CONTINUE.
      ENDIF.

      IF <ls_jag_fnet>-bukrs IS INITIAL.
        <ls_coda_ret>-message = |{ cv_icon_ko }YVERT_JAG_FNET: SocietÃ  e vuota|.
        CONTINUE.
      ENDIF.

      IF <ls_jag_fnet>-error_file IS NOT INITIAL.
        <ls_coda_ret>-message = |{ cv_icon_ko }File errato: Controlla i messaggi precedenti|.
        CONTINUE.
      ENDIF.

      IF <ls_jag_fnet>-filenet_guid IS NOT INITIAL
        AND <ls_jag_fnet>-read_file IS INITIAL.
* Make sure it still exists in FileNet
        ##TO_DO
*   Is there an api just to do a check and not get the WHOLE file???
        CLEAR lv_file.
        CALL FUNCTION 'YVERT_API_GO2DOC_DOC_GET'
          EXPORTING
            document_id   = <ls_jag_fnet>-filenet_guid
            document_type = <ls_jag_fnet>-filenet_doc_type
          IMPORTING
            file          = lv_file.
        IF lv_file IS INITIAL.
          CLEAR <ls_jag_fnet>-filenet_guid.
        ENDIF.

        <ls_jag_fnet>-read_file = abap_true.
      ENDIF.

      IF <ls_jag_fnet>-filenet_guid IS INITIAL.
        TRY.
* Get Jaggaer content file
            DATA(lv_content) =
             get_jaggaer_attachment(
               EXPORTING
                 iv_file_id      = <ls_jag_fnet>-file_id
                 iv_file_name    = <ls_jag_fnet>-file_name
             ).
          CATCH ycx_yvert_jagg_filenet INTO DATA(lx_filenet). " Classe messaggi Jaggaer/Filenet GET/POST .

            <ls_coda_ret>-message = |{ cv_icon_ko }JAGGAER: { lx_filenet->get_longtext( ) }|.

            IF lx_filenet->if_t100_message~t100key = lx_filenet->token_not_retrieved.
              EXIT.
            ENDIF.

            <ls_jag_fnet>-error_file = abap_true.
            CONTINUE.
        ENDTRY.

        ls_attr-bukrs = <ls_jag_fnet>-bukrs.
        ls_attr-filenet_doc_type = <ls_jag_fnet>-filenet_doc_type.

*  Post it to Filenet
        TRY.
            post_to_filenet(
            EXPORTING
              is_attr       = ls_attr
              iv_content    = lv_content
            CHANGING
              cs_file_prop  = <ls_jag_fnet> ).
          CATCH ycx_yvert_jagg_filenet INTO lx_filenet. " Classe messaggi Jaggaer/Filenet GET/POST .
            <ls_coda_ret>-message = |{ cv_icon_ko }FILENET: { lx_filenet->get_longtext( ) }|.
            <ls_jag_fnet>-error_file = abap_true.
            CONTINUE.
        ENDTRY.

        CLEAR lv_content.

        <ls_jag_fnet>-data_upload = sy-datum.
        <ls_jag_fnet>-ora_upload  = sy-uzeit.
        <ls_coda_ret>-message     = |{ cv_icon_ok }File caricato su FileNet|.
      ENDIF.

      IF <lv_val> IS ASSIGNED.
        <lv_val> = <ls_jag_fnet>-filenet_guid.
        UPDATE (<ls_alleg>-tabname) FROM <ls_tab_al>.
      ENDIF.

      <ls_alleg>-finished = abap_true.
      UPDATE yvert_coda_alleg FROM <ls_alleg>.
      COMMIT WORK.

      <ls_coda_ret>-finished      = abap_true.
      <ls_coda_ret>-filenet_guid  = <ls_jag_fnet>-filenet_guid.

      IF <ls_coda_ret>-message IS INITIAL.
        <ls_coda_ret>-message     = |{ cv_icon_ok }File trovato in buffer|.
      ENDIF.

      lv_ko -= 1.
      lv_ok += 1.

    ENDLOOP.

    UPDATE yvert_jag_fnet FROM TABLE mt_jag_fnet.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
