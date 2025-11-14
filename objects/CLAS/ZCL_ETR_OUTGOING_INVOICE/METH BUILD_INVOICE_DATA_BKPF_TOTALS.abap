  METHOD build_invoice_data_bkpf_totals.
    DATA: lt_hkont       TYPE RANGE OF hkont,
          lv_amount      TYPE wrbtr_cs,
          lv_base_amount TYPE wrbtr_cs.

    LOOP AT ms_invoice_ubl-invoiceline INTO DATA(ls_invoice_line).
      ms_invoice_ubl-legalmonetarytotal-lineextensionamount-content += ls_invoice_line-lineextensionamount-content.
    ENDLOOP.

    lv_base_amount = ms_invoice_ubl-legalmonetarytotal-lineextensionamount-content.

    LOOP AT ms_accdoc_data-accounts INTO DATA(ls_accounts) USING KEY by_accty WHERE accty = 'D'.
      APPEND VALUE #( sign = 'I'  option = 'EQ' low = ls_accounts-saknr ) TO lt_hkont.
    ENDLOOP.
    IF lt_hkont IS NOT INITIAL.
      LOOP AT ms_accdoc_data-bseg INTO DATA(ls_bseg) USING KEY by_koart WHERE koart = 'S'
                                                                    AND shkzg = 'S'.
        IF ls_bseg-lokkt IS NOT INITIAL .
          CHECK ls_bseg-lokkt IN lt_hkont .
        ELSE.
          CHECK ls_bseg-hkont IN lt_hkont .
        ENDIF.
        IF ls_bseg-wrbtr IS INITIAL AND ls_bseg-dmbtr IS NOT INITIAL.
          ls_bseg-wrbtr = ls_bseg-dmbtr.
        ENDIF.
        ms_invoice_ubl-legalmonetarytotal-allowancetotalamount-content += ls_bseg-wrbtr.
        lv_base_amount -= ls_bseg-wrbtr.
      ENDLOOP.
    ENDIF.

*    fill_common_tax_totals( ).

**********************************************************************
    CLEAR lt_hkont.
    LOOP AT ms_accdoc_data-accounts INTO ls_accounts USING KEY by_accty WHERE accty = 'O'.
      APPEND INITIAL LINE TO lt_hkont ASSIGNING FIELD-SYMBOL(<ls_hkont>).
      <ls_hkont>-sign = 'I'.
      <ls_hkont>-option = 'EQ'.
      <ls_hkont>-low = ls_accounts-saknr.
    ENDLOOP.
    IF lt_hkont IS NOT INITIAL.
      LOOP AT ms_accdoc_data-bseg INTO ls_bseg USING KEY by_koart WHERE koart = 'S'
                                                                    AND shkzg = 'H'.
        IF ls_bseg-lokkt IS NOT INITIAL .
          CHECK ls_bseg-lokkt IN lt_hkont .
        ELSE.
          CHECK ls_bseg-hkont IN lt_hkont .
        ENDIF.
        IF ls_bseg-wrbtr IS INITIAL AND ls_bseg-dmbtr IS NOT INITIAL.
          ls_bseg-wrbtr = ls_bseg-dmbtr.
        ENDIF.

        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ls_bseg-mwskz
          INTO @DATA(ls_tax_match).

        APPEND INITIAL LINE TO ms_invoice_ubl-taxtotal ASSIGNING FIELD-SYMBOL(<ls_tax_total>).
        <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_types
          WHERE TaxType = @ls_tax_match-taxty
          INTO @DATA(ls_tax_data).

        IF ls_tax_match-txtyp IS INITIAL.
          <ls_tax_total>-taxamount-content = ls_bseg-wrbtr.

          APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING FIELD-SYMBOL(<ls_tax_subtotal>).
          <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.
          IF ls_bseg-wrbtr IS INITIAL OR ms_document-invty = 'IHRACKAYIT'.
            IF ms_document-taxex IS NOT INITIAL.
              SELECT SINGLE *
                FROM zetr_ddl_i_tax_exemptions
                WHERE ExemptionCode = @ms_document-taxex
                INTO @DATA(ls_tax_exemption).
              <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ms_document-taxex.
            ELSE.
              SELECT SINGLE *
                FROM zetr_ddl_i_tax_exemptions
                WHERE ExemptionCode = @ls_tax_match-taxex
                INTO @ls_tax_exemption.
              <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ls_tax_match-taxex.
            ENDIF.
            <ls_tax_subtotal>-taxcategory-taxexemptionreason-content = ls_tax_exemption-Description.
          ENDIF.
          <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
          <ls_tax_subtotal>-taxamount-content = ls_bseg-wrbtr.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        ELSE.
          lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( 1 - ls_tax_match-taxrt / 100 ).
          <ls_tax_total>-taxamount-content = lv_amount.
          APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          SELECT SINGLE *
            FROM zetr_ddl_i_tax_types
            WHERE TaxType = @ls_tax_match-txtyp
            INTO @DATA(ls_parent_tax_data).
          <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_parent_tax_data-LongDescription.
          <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-txtyp.
          <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-txrtp.
          lv_amount = ( lv_base_amount * ls_tax_match-txrtp ) / 100.
          <ls_tax_subtotal>-taxamount-content = lv_amount.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.

          IF ls_tax_data-TaxCategory = 'TEV'.
            APPEND INITIAL LINE TO ms_invoice_ubl-withholdingtaxtotal ASSIGNING <ls_tax_total>.
            lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( ls_tax_match-taxrt / 100 ).
            <ls_tax_total>-taxamount-content = lv_amount.
            <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
            APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          ELSE.
            APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          ENDIF.
          IF ms_document-taxty IS NOT INITIAL.
            <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ms_document-taxty.
            SELECT SINGLE *
              FROM zetr_ddl_i_tax_types
              WHERE TaxType = @ms_document-taxty
              INTO @ls_tax_data.
            <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          ELSE.
            <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.
            <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          ENDIF.
          lv_amount = ( lv_base_amount * ls_tax_match-txrtp ) / 100.
          <ls_tax_subtotal>-taxableamount-content = lv_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
          lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( ls_tax_match-taxrt / 100 ).
          <ls_tax_subtotal>-taxamount-content = lv_amount.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        ENDIF.
      ENDLOOP.
    ENDIF.
*    fill_common_tax_totals( ).

    " if tax line not found then fill exemption
    IF ms_invoice_ubl-taxtotal IS INITIAL.
      IF ms_accdoc_data-bseg_partner-mwskz IS INITIAL.
        LOOP AT ms_accdoc_data-bseg INTO ls_bseg WHERE mwskz IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ls_bseg-mwskz
          INTO @ls_tax_match.
      ELSE.
        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ms_accdoc_data-bseg_partner-mwskz
          INTO @ls_tax_match.
      ENDIF.

      APPEND INITIAL LINE TO ms_invoice_ubl-taxtotal ASSIGNING <ls_tax_total>.
      <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
      <ls_tax_total>-taxamount-content = 0.

      APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
      SELECT SINGLE *
        FROM zetr_ddl_i_tax_types
        WHERE TaxType = @ls_tax_match-taxty
        INTO @ls_tax_data.
      <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
      <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.

      IF ms_document-taxex IS NOT INITIAL.
        <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ms_document-taxex.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_exemptions
          WHERE ExemptionCode = @ms_document-taxex
          INTO @ls_tax_exemption.
      ELSE.
        <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ls_tax_match-taxex.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_exemptions
          WHERE ExemptionCode = @ls_tax_match-taxex
          INTO @ls_tax_exemption.
      ENDIF.
      <ls_tax_subtotal>-taxcategory-taxexemptionreason-content = ls_tax_exemption-Description.
      <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
      <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
      <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
      <ls_tax_subtotal>-taxamount-content = 0.
      <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
    ENDIF.

    TYPES BEGIN OF ty_taxtotal.
    TYPES tax_code   TYPE string.
    TYPES tax_name   TYPE string.
    TYPES tax_rate   TYPE string.
    TYPES exp_code   TYPE string.
    TYPES exp_name   TYPE string.
    TYPES taxtotal  TYPE wrbtr_cs.
    TYPES taxamount  TYPE wrbtr_cs.
    TYPES tax_base   TYPE wrbtr_cs.
    TYPES witholding TYPE abap_boolean.
    TYPES END OF ty_taxtotal .
    DATA: lt_taxtotal TYPE TABLE OF ty_taxtotal,
          ls_taxtotal TYPE ty_taxtotal.

    IF lines( ms_invoice_ubl-taxtotal ) > 1 OR lines( ms_invoice_ubl-withholdingtaxtotal ) > 1.
      LOOP AT ms_invoice_ubl-taxtotal ASSIGNING <ls_tax_total>.
        LOOP AT <ls_tax_total>-taxsubtotal INTO DATA(ls_taxsubtotal).
          ls_taxtotal-tax_code  = ls_taxsubtotal-taxcategory-taxscheme-taxtypecode-content.
          ls_taxtotal-tax_name  = ls_taxsubtotal-taxcategory-taxscheme-name-content.
          ls_taxtotal-tax_rate  = ls_taxsubtotal-percent-content.
          ls_taxtotal-exp_code  = ls_taxsubtotal-taxcategory-taxexemptionreasoncode-content.
          ls_taxtotal-exp_name  = ls_taxsubtotal-taxcategory-taxexemptionreason-content.
          ls_taxtotal-taxamount = ls_taxsubtotal-taxamount-content.
          ls_taxtotal-taxtotal = <ls_tax_total>-taxamount-content.
          ls_taxtotal-tax_base  = ls_taxsubtotal-taxableamount-content.
          COLLECT ls_taxtotal INTO lt_taxtotal.
          CLEAR ls_taxtotal.
        ENDLOOP.
      ENDLOOP.

      LOOP AT ms_invoice_ubl-withholdingtaxtotal ASSIGNING <ls_tax_total>.
        LOOP AT <ls_tax_total>-taxsubtotal INTO ls_taxsubtotal.
          ls_taxtotal-tax_code  = ls_taxsubtotal-taxcategory-taxscheme-taxtypecode-content.
          ls_taxtotal-tax_name  = ls_taxsubtotal-taxcategory-taxscheme-name-content.
          ls_taxtotal-tax_rate  = ls_taxsubtotal-percent-content.
          ls_taxtotal-exp_code  = ls_taxsubtotal-taxcategory-taxexemptionreasoncode-content.
          ls_taxtotal-exp_name  = ls_taxsubtotal-taxcategory-taxexemptionreason-content.
          ls_taxtotal-taxamount = ls_taxsubtotal-taxamount-content.
          ls_taxtotal-taxtotal = <ls_tax_total>-taxamount-content.
          ls_taxtotal-tax_base  = ls_taxsubtotal-taxableamount-content.
          ls_taxtotal-witholding = 'X'.
          COLLECT ls_taxtotal INTO lt_taxtotal.
          CLEAR ls_taxtotal.
        ENDLOOP.
      ENDLOOP.

      CLEAR: ms_invoice_ubl-taxtotal, ms_invoice_ubl-withholdingtaxtotal.
      LOOP AT lt_taxtotal INTO ls_taxtotal.
        IF ls_taxtotal-witholding IS NOT INITIAL.
          APPEND INITIAL LINE TO ms_invoice_ubl-withholdingtaxtotal ASSIGNING FIELD-SYMBOL(<ls_taxtotal>).
        ELSE.
          APPEND INITIAL LINE TO ms_invoice_ubl-taxtotal ASSIGNING <ls_taxtotal>.
          ms_invoice_ubl-legalmonetarytotal-taxinclusiveamount-content = ms_invoice_ubl-legalmonetarytotal-taxinclusiveamount-content + ls_taxtotal-taxamount.
        ENDIF.
        <ls_taxtotal>-taxamount-content = ls_taxtotal-taxtotal.
        <ls_taxtotal>-taxamount-currencyid = ms_invoice_ubl-documentcurrencycode-content.

        APPEND INITIAL LINE TO <ls_taxtotal>-taxsubtotal ASSIGNING FIELD-SYMBOL(<ls_taxsubtotal>).
        <ls_taxsubtotal>-taxcategory-taxscheme-name-content = ls_taxtotal-tax_name.
        <ls_taxsubtotal>-taxcategory-taxscheme-taxtypecode-content = ls_taxtotal-tax_code.
        <ls_taxsubtotal>-taxcategory-taxexemptionreasoncode-content = ls_taxtotal-exp_code.
        <ls_taxsubtotal>-taxcategory-taxexemptionreason-content = ls_taxtotal-exp_name.
        <ls_taxsubtotal>-taxableamount-content = ls_taxtotal-tax_base.
        <ls_taxsubtotal>-taxableamount-currencyid =  ms_invoice_ubl-documentcurrencycode-content.
        <ls_taxsubtotal>-percent-content = ls_taxtotal-tax_rate.
        <ls_taxsubtotal>-taxamount-content = ls_taxtotal-taxamount.
        <ls_taxsubtotal>-taxamount-currencyid =  ms_invoice_ubl-documentcurrencycode-content.
      ENDLOOP.
    ENDIF.

**********************************************************************

    IF ms_invoice_ubl-legalmonetarytotal-allowancetotalamount-content IS NOT INITIAL.
      ms_invoice_ubl-legalmonetarytotal-allowancetotalamount-currencyid = ms_accdoc_data-bkpf-waers.
    ENDIF.
    ms_invoice_ubl-legalmonetarytotal-lineextensionamount-currencyid = ms_accdoc_data-bkpf-waers.
    ms_invoice_ubl-legalmonetarytotal-taxexclusiveamount-content = lv_base_amount.
    ms_invoice_ubl-legalmonetarytotal-taxexclusiveamount-currencyid = ms_accdoc_data-bkpf-waers.
    ms_invoice_ubl-legalmonetarytotal-taxinclusiveamount-content = ms_invoice_ubl-legalmonetarytotal-taxinclusiveamount-content +
                                                                   ms_invoice_ubl-legalmonetarytotal-lineextensionamount-content.
    ms_invoice_ubl-legalmonetarytotal-taxinclusiveamount-currencyid = ms_accdoc_data-bkpf-waers.
    ms_invoice_ubl-legalmonetarytotal-payableamount-content = ms_accdoc_data-bseg_partner-wrbtr.
    ms_invoice_ubl-legalmonetarytotal-payableamount-currencyid = ms_accdoc_data-bkpf-waers.
  ENDMETHOD.