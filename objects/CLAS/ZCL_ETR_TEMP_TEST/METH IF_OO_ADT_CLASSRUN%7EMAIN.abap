  METHOD if_oo_adt_classrun~main.
*    SELECT * FROM zetr_t_ogdlv ORDER BY belnr DESCENDING INTO @DATA(ls_ogdlv) UP TO 1 ROWS .
*      ls_ogdlv-belnr += 1.
*      CONDENSE ls_ogdlv-belnr.
*      ls_ogdlv-stacd = '5'.
*    ENDSELECT.
*    CHECK ls_ogdlv IS NOT INITIAL.
*    INSERT zetr_t_ogdlv FROM @ls_ogdlv.
*    CHECK sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*update zetr_t_oginv set invno = 'AR82024000000001' WHERE invno = 'ARS8202400000001'.
    DATA: lv_belnr   TYPE belnr_d,
          lv_docui   TYPE sysuuid_c22,
          lv_confirm TYPE abap_bool.

    " Debug modda değerleri girin:
    " lv_belnr, lv_docui, lv_confirm = abap_true
    "  BREAK-POINT.

    IF lv_belnr IS INITIAL OR lv_docui IS INITIAL.
      out->write( 'HATA: Belge numarası ve UUID değeri girilmelidir!' ).
      RETURN.
    ENDIF.

    IF lv_confirm = abap_false.
      out->write( 'UYARI: Silme işlemi onaylanmadı. lv_confirm = abap_true yapın!' ).
      RETURN.
    ENDIF.

    DATA(lv_result) = delete_delivery_record(
      iv_belnr = lv_belnr
      iv_docui = lv_docui
      iv_confirm_delete = lv_confirm
    ).

    IF lv_result = abap_true.
      out->write( |Kayıt başarıyla silindi - BELNR: { lv_belnr }, DOCUI: { lv_docui }| ).
    ELSE.
      out->write( 'Kayıt silinemedi veya bulunamadı.' ).
    ENDIF.

  ENDMETHOD.