
METHOD create_blank_document_struct.

  DATA: lo_blank_doc TYPE REF TO cl_abap_zip,
        lo_converter TYPE REF TO cl_abap_conv_out_ce.

  DATA: xml           TYPE string,
        timestamp     TYPE timestamp,
        utc           TYPE string,
        utc_str       TYPE string,
        lv_sheet_name TYPE string.

* Create the empty zip package
  CREATE OBJECT lo_blank_doc.

* Create the obligatory [Content_Types].xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">`. "#EC NOTEXT
  xml = xml && `<Override PartName="/xl/theme/theme1.xml" ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>`. "#EC NOTEXT
  xml = xml && `<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>`. "#EC NOTEXT
  xml = xml && `<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`. "#EC NOTEXT
  xml = xml && `<Default Extension="xml" ContentType="application/xml"/>`. "#EC NOTEXT
  xml = xml && `<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>`. "#EC NOTEXT
  xml = xml && `<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>`. "#EC NOTEXT

  LOOP AT it_sheet_data ASSIGNING FIELD-SYMBOL(<ls_sheet_data>).

    DATA(lv_sheet_num) = sy-tabix.

    xml = xml && '<Override PartName="/xl/worksheets/sheet'
    && lv_sheet_num
    && '.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'. "#EC NOTEXT

  ENDLOOP.

  xml = xml && `<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>`. "#EC NOTEXT
  xml = xml && `<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>`. "#EC NOTEXT
  xml = xml && `</Types>`.                                  "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = '[Content_Types].xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory _rels/.rels
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`. "#EC NOTEXT
  xml = xml && `<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>`. "#EC NOTEXT
  xml = xml && `<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>`. "#EC NOTEXT
  xml = xml && `<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`. "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = '_rels/.rels' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory docProps/app.xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" `. "#EC NOTEXT
  xml = xml && `xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">`. "#EC NOTEXT
  xml = xml && `<Application>SAP NetWeaver Application Server</Application>`. "#EC NOTEXT
  xml = xml && `<DocSecurity>0</DocSecurity>`.              "#EC NOTEXT
  xml = xml && `<TitlesOfParts>`.                           "#EC NOTEXT
  xml = xml && `<vt:vector size="2" baseType="lpstr"><vt:lpstr>` && lv_sheet_name && `</vt:lpstr>`. "#EC NOTEXT
  xml = xml && `<vt:lpstr>HELP</vt:lpstr></vt:vector>`.     "#EC NOTEXT
  xml = xml && `</TitlesOfParts>`.                          "#EC NOTEXT
  xml = xml && `<Company>SAP</Company>`.                    "#EC NOTEXT
  xml = xml && `<AppVersion>7.1000</AppVersion>`.           "#EC NOTEXT
  xml = xml && `</Properties>`.                             "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'docProps/app.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory docProps/core.xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" `. "#EC NOTEXT
  xml = xml && `xmlns:dc="http://purl.org/dc/elements/1.1/" `. "#EC NOTEXT
  xml = xml && `xmlns:dcterms="http://purl.org/dc/terms/" `. "#EC NOTEXT
  xml = xml && `xmlns:dcmitype="http://purl.org/dc/dcmitype/" `. "#EC NOTEXT
  xml = xml && `xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">`. "#EC NOTEXT
  xml = xml && `<dc:title>` && meta_title && `</dc:title>`. "#EC NOTEXT
  xml = xml && `<dc:subject>` && meta_subject && `</dc:subject>`. "#EC NOTEXT
  xml = xml && `<dc:creator>` && sy-uname && `</dc:creator>`. "#EC NOTEXT
  xml = xml && `<cp:keywords>` && meta_keywords && `</cp:keywords>`. "#EC NOTEXT
  xml = xml && `<dc:description>` && meta_description && `</dc:description>`. "#EC NOTEXT
  GET TIME STAMP FIELD timestamp. " Get current UTC
  utc = timestamp.
  CONCATENATE utc+0(4) '-' utc+4(2) '-' utc+6(2) 'T' utc+8(2) ':' utc+10(2) ':' utc+12(2) 'Z'  INTO utc_str. "#EC NOTEXT
  xml = xml && `<dcterms:created xsi:type="dcterms:W3CDTF">` && utc_str && `</dcterms:created>`. "#EC NOTEXT
  xml = xml && `<dcterms:modified xsi:type="dcterms:W3CDTF">` && utc_str && `</dcterms:modified>`. "#EC NOTEXT
  xml = xml && `</cp:coreProperties>`.                      "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'docProps/core.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory xl/_rels/workbook.xml.rels
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`. "#EC NOTEXT

  xml = xml && `<Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>`. "#EC NOTEXT

    LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.
      lv_sheet_num = sy-tabix.

      xml = xml && `<Relationship Id="rId` && lv_sheet_num &&
            `" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" ` &&
            `Target="worksheets/sheet` && lv_sheet_num && `.xml"/>`. "#EC NOTEXT
    ENDLOOP.

  xml = xml && `<Relationship Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>`. "#EC NOTEXT

  xml = xml && `</Relationships>`.                          "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'xl/_rels/workbook.xml.rels' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

  LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.

    lv_sheet_num = sy-tabix.

    xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
    xml = xml && `<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">`. "#EC NOTEXT
    xml = xml && `<dimension ref="A1"/>`.                   "#EC NOTEXT
    xml = xml && `<sheetViews><sheetView workbookViewId="0"/></sheetViews>`. "#EC NOTEXT
    xml = xml && `<sheetFormatPr defaultRowHeight="15"/>`.  "#EC NOTEXT
    xml = xml && `<sheetData/>`.                            "#EC NOTEXT
    xml = xml && `<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>`. "#EC NOTEXT
    xml = xml && `</worksheet>`.                            "#EC NOTEXT
    lo_converter = cl_abap_conv_out_ce=>create( ).
    lo_converter->write( data = xml ).
    lo_blank_doc->add( name = 'xl/worksheets/sheet' && lv_sheet_num && '.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
    FREE lo_converter.

  ENDLOOP.

* Create the obligatory xl/sharedStrings.xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"></sst>`. "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'xl/sharedStrings.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory xl/styles.xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">`. "#EC NOTEXT
  xml = xml && `<fonts count="2">`.                         "#EC NOTEXT
  xml = xml && `<font><sz val="11"/><color theme="1"/><name val="Calibri"/><family val="2"/><scheme val="minor"/></font>`. "#EC NOTEXT
  xml = xml && `<font><b/><sz val="11"/><color theme="1"/><name val="Calibri"/><family val="2"/><scheme val="minor"/></font>`. "#EC NOTEXT
  xml = xml && `</fonts>`.                                  "#EC NOTEXT
  xml = xml && `<fills>`.                                   "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="none"/></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="gray125" /></fill>`. "#EC NOTEXT
  "xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="0" tint="-0.14999847407452621" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFDBD5BF" /></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFE2EBF1" /></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFF3E9BE" /></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFEDF4F8" /></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="9" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFC6868" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="4" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor theme="6" tint="0.59999389629810485" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFFFF00" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FFFF3300" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `<fill><patternFill patternType="solid"><fgColor rgb="FF92D050" /><bgColor indexed="64"/></patternFill></fill>`. "#EC NOTEXT
  xml = xml && `</fills>`.                                  "#EC NOTEXT
  xml = xml && '<borders count="2"><border><left/><right/><top/><bottom/><diagonal/></border>'. "#EC NOTEXT
  xml = xml && '<border><left style="thin"><color rgb="FFFFFFFF" /></left><right style="thin"><color rgb="FFFFFFFF" /></right>'. "#EC NOTEXT
  xml = xml && '<top style="thin"><color rgb="FFFFFFFF" /></top><bottom style="thin"><color rgb="FFFFFFFF" /></bottom><diagonal /></border>'. "#EC NOTEXT
  xml = xml && '</borders>'.                                "#EC NOTEXT
  xml = xml && `<cellStyleXfs>`.                            "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="0" fillId="0" borderId="0"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="2" borderId="1" applyFont="0"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="3" borderId="1" applyFont="0"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="4" borderId="1" applyFont="0"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" applyProtection="0" fontId="1" fillId="5" borderId="1" applyFont="0"/>`. "#EC NOTEXT
  xml = xml && `</cellStyleXfs>`.                           "#EC NOTEXT
*  IF is_alv_document = abap_true.
  xml = xml && `<cellXfs>`.                                 "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="0" fillId="0" borderId="0" xfId="0"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="2" borderId="0" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="3" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="4" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="5" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="6" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="7" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="8" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="9" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="10" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="11" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `<xf numFmtId="49" fontId="1" fillId="12" borderId="1" xfId="0" applyFont="1" />`. "#EC NOTEXT
  xml = xml && `</cellXfs>`.                                "#EC NOTEXT
*  ELSE.
*  xml = xml && `<cellXfs count="2"><xf numFmtId="49" fontId="0" fillId="0" borderId="0" xfId="0"/><xf numFmtId="49" fontId="1" fillId="2" borderId="0" xfId="0" applyFont="1"applyFill="1" /></cellXfs>`. "#EC NOTEXT
*  ENDIF.
  xml = xml && `<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>`. "#EC NOTEXT
  xml = xml && `<dxfs count="0"/><tableStyles count="0" defaultTableStyle="TableStyleMedium9" defaultPivotStyle="PivotStyleLight16"/>`. "#EC NOTEXT
  "xml = xml && '<colors><mruColors><color rgb="FFC6FBC6" /></mruColors></colors>'.
  xml = xml && `</styleSheet>`.                             "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'xl/styles.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

* Create the obligatory xl/workbook.xml
  xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`. "#EC NOTEXT
  xml = xml && `<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">`. "#EC NOTEXT
  xml = xml && `<fileVersion appName="xl" lastEdited="1" lowestEdited="1" rupBuild="4505"/>`. "#EC NOTEXT
  xml = xml && `<workbookPr filterPrivacy="1" defaultThemeVersion="124226"/>`. "#EC NOTEXT
  xml = xml && `<bookViews><workbookView xWindow="240" yWindow="105" windowWidth="14805" windowHeight="8010"/></bookViews>`. "#EC NOTEXT
  xml = xml && `<sheets>`.                                  "#EC NOTEXT

  LOOP AT it_sheet_data ASSIGNING <ls_sheet_data>.
    lv_sheet_num = sy-tabix.

    xml = xml && `<sheet name="` && <ls_sheet_data>-sheet_name && `" sheetId="` && lv_sheet_num && `" r:id="rId` && lv_sheet_num && `"/>`. "#EC NOTEXT
  ENDLOOP.

  xml = xml && `</sheets>`.                                 "#EC NOTEXT
  xml = xml && `<calcPr calcId="124519"/>`.                 "#EC NOTEXT
  xml = xml && `</workbook>`.                               "#EC NOTEXT
  lo_converter = cl_abap_conv_out_ce=>create( ).
  lo_converter->write( data = xml ).
  lo_blank_doc->add( name = 'xl/workbook.xml' content = lo_converter->get_buffer( ) ). "#EC NOTEXT
  FREE lo_converter.

  xblank_document = lo_blank_doc->save( ).
ENDMETHOD.
