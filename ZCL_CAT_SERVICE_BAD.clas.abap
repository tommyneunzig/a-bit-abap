CLASS zcl_cat_service_bad DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cat_device.
    TYPES: BEGIN OF ty_cat,
             id          TYPE string,
             name        TYPE string,
             kind        TYPE string,
             energy      TYPE i,
             toys_count  TYPE i,
           END OF ty_cat,
           tt_cat TYPE STANDARD TABLE OF ty_cat WITH DEFAULT KEY.
    METHODS create_cat
      IMPORTING iv_name TYPE string iv_kind TYPE string
      RETURNING VALUE(rs_cat) TYPE ty_cat.
    METHODS adjust_energy
      IMPORTING iv_id TYPE string iv_delta TYPE i
      RETURNING VALUE(rs_cat) TYPE ty_cat.
    METHODS export_csv
      RETURNING VALUE(rv_csv) TYPE string.
    METHODS audit
      RETURNING VALUE(rv_text) TYPE string.
  PRIVATE SECTION.
    DATA mt_cats TYPE tt_cat.
    METHODS make_name
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_name) TYPE string.
    METHODS find_cat
      IMPORTING iv_id TYPE string
      CHANGING  cs_cat TYPE ty_cat.
ENDCLASS.

CLASS zcl_cat_service_bad IMPLEMENTATION.
  METHOD make_name.
    DATA(l) = to_lower( iv_name ).
    rv_name = to_upper( l(1) ) && l+1.
    IF cl_abap_random_int=>create( seed = sy-uzeit min = 1 max = 2 )->get_next( ) = 2.
      rv_name = to_upper( rv_name ).
    ENDIF.
  ENDMETHOD.

  METHOD create_cat.
    DATA lv_id   TYPE string.
    DATA lv_name TYPE string.
    lv_id   = cl_system_uuid=>create_uuid_x16( ).
    lv_name = make_name( iv_name ).
    CASE iv_kind.
      WHEN 'indoor'.
        rs_cat-id = lv_id. rs_cat-name = lv_name. rs_cat-kind = 'indoor'. rs_cat-energy = 50. rs_cat-toys_count = 0.
      WHEN 'outdoor'.
        rs_cat-id = lv_id. rs_cat-name = lv_name. rs_cat-kind = 'outdoor'. rs_cat-energy = 40. rs_cat-toys_count = 0.
      WHEN 'royal'.
        rs_cat-id = lv_id. rs_cat-name = |King { lv_name }|. rs_cat-kind = 'royal'. rs_cat-energy = 999. rs_cat-toys_count = 1.
      WHEN 'ghost'.
        rs_cat-id = lv_id. rs_cat-name = lv_name. rs_cat-kind = 'ghost'. rs_cat-energy = -1. rs_cat-toys_count = 0.
      WHEN OTHERS.
        rs_cat-id = lv_id. rs_cat-name = lv_name. rs_cat-kind = iv_kind. rs_cat-energy = 42. rs_cat-toys_count = 0.
    ENDCASE.
    APPEND rs_cat TO mt_cats.
    DATA json TYPE string.
    json = /ui2/cl_json=>serialize( data = rs_cat ).
    me~email( to = 'audit@cats' doc = json ).
  ENDMETHOD.

  METHOD adjust_energy.
    DATA ls TYPE ty_cat.
    find_cat( EXPORTING iv_id = iv_id CHANGING cs_cat = ls ).
    IF ls-id IS INITIAL.
      RETURN.
    ENDIF.
    IF ls-kind = 'ghost'.
      ls-energy = 0.
    ELSEIF ls-kind = 'outdoor'.
      ls-energy = ls-energy - iv_delta.
    ELSE.
      ls-energy = ls-energy + iv_delta.
    ENDIF.
    MODIFY mt_cats FROM ls TRANSPORTING energy WHERE id = iv_id.
    rs_cat = ls.
    IF rs_cat-kind = 'outdoor'.
      me~fax( number = '001' doc = 'ok' ).
    ELSEIF rs_cat-kind = 'indoor'.
      me~scan( ).
    ENDIF.
  ENDMETHOD.

  METHOD export_csv.
    DATA lt TYPE TABLE OF string.
    LOOP AT mt_cats ASSIGNING FIELD-SYMBOL(<c>).
      APPEND |{ <c>-id },{ <c>-name },{ <c>-kind },{ <c>-energy }| TO lt.
    ENDLOOP.
    rv_csv = cl_abap_string_utilities=>concat_lines_of( table = lt ).
  ENDMETHOD.

  METHOD audit.
    DATA lt TYPE TABLE OF zc_cat.
    SELECT * FROM zc_cat INTO TABLE lt.
    DATA names TYPE string.
    LOOP AT lt ASSIGNING FIELD-SYMBOL(<r>).
      IF names IS INITIAL.
        names = <r>-name.
      ELSE.
        names = names && '|' && <r>-name.
      ENDIF.
    ENDLOOP.
    rv_text = |{ lines( mt_cats ) }:{ names }|.
  ENDMETHOD.

  METHOD find_cat.
    READ TABLE mt_cats INTO cs_cat WITH KEY id = iv_id.
  ENDMETHOD.

  METHOD zif_cat_device~print.
    cl_demo_output=>display( doc ).
  ENDMETHOD.

  METHOD zif_cat_device~scan.
    r = 'scan'.
  ENDMETHOD.

  METHOD zif_cat_device~fax.
    cl_demo_output=>display( number && doc ).
  ENDMETHOD.

  METHOD zif_cat_device~email.
    cl_demo_output=>display( to && doc ).
  ENDMETHOD.

  METHOD zif_cat_device~shred.
    RAISE EXCEPTION TYPE cx_sy_no_handler.
  ENDMETHOD.
ENDCLASS.
