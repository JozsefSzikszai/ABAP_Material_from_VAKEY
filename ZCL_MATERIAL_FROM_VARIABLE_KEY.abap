class ZCL_MATERIAL_FROM_VARIABLE_KEY definition
  public
  final
  create private .

public section.

  interfaces ZIF_MATERIAL_FROM_VARIABLE_KEY .

  aliases GET_MATERIAL
    for ZIF_MATERIAL_FROM_VARIABLE_KEY~GET_MATERIAL .

  class-methods GET_INSTANCE
    returning
      value(RO_MATERIAL_FROM_VARIABLE_KEY) type ref to ZIF_MATERIAL_FROM_VARIABLE_KEY
    raising
      CX_SY_MOVE_CAST_ERROR .
protected section.
private section.

  types:
    BEGIN OF ty_key,
      kotabnr TYPE kotabnr,
      offset  TYPE i,
    END OF ty_key .
  types:
    tt_keys TYPE HASHED TABLE OF ty_key
                     WITH UNIQUE KEY kotabnr .
  types:
    BEGIN OF ty_key_field,
      gstru TYPE tmc1k-gstru,
      lfnr2 TYPE tmc1k-lfnr2,
      stfna TYPE tmc1k-stfna,
      leng  TYPE dd03l-leng,
    END OF ty_key_field .
  types:
    tt_key_fields TYPE SORTED TABLE OF ty_key_field
                           WITH UNIQUE KEY gstru
                                           lfnr2 .

  class-data MO_MATERIAL_FROM_VARIABLE_KEY type ref to ZIF_MATERIAL_FROM_VARIABLE_KEY .
  data MT_KEYS type TT_KEYS .
  data MT_TMC1K type TT_KEY_FIELDS .

  methods CALCULATE_OFFSET
    importing
      !IV_KOTABNR type KOTABNR .
  methods CONSTRUCTOR .
  methods RETRIEVE_MATERIAL_BY_OFFSET
    importing
      !IV_KOTABNR type KOTABNR
      !IV_VAKEY type VAKEY_LONG
    returning
      value(RV_MATNR) type MATNR
    raising
      CX_SY_ITAB_LINE_NOT_FOUND
      CX_SY_RANGE_OUT_OF_BOUNDS .
  methods SELECT_FIELDS_FOR_TABLE
    importing
      !IV_KOTABNR type KOTABNR
    raising
      ZCX_NO_DATA_SELECTED .
ENDCLASS.



CLASS ZCL_MATERIAL_FROM_VARIABLE_KEY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MATERIAL_FROM_VARIABLE_KEY->CALCULATE_OFFSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KOTABNR                     TYPE        KOTABNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_offset.

    DATA(ls_key) = VALUE ty_key( kotabnr = iv_kotabnr
                                 offset  = 0 ).

    LOOP AT mt_tmc1k
         ASSIGNING FIELD-SYMBOL(<ls_tmc1k>)
         WHERE gstru+1(3) EQ iv_kotabnr.
      IF <ls_tmc1k>-stfna EQ 'MATNR'.
        EXIT.
      ENDIF.
      ADD <ls_tmc1k>-leng TO ls_key-offset.
    ENDLOOP.

    INSERT ls_key INTO TABLE mt_keys.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MATERIAL_FROM_VARIABLE_KEY->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATERIAL_FROM_VARIABLE_KEY=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_MATERIAL_FROM_VARIABLE_KEY  TYPE REF TO ZIF_MATERIAL_FROM_VARIABLE_KEY
* | [!CX!] CX_SY_MOVE_CAST_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.

    IF mo_material_from_variable_key IS INITIAL.
      TRY.
          mo_material_from_variable_key = CAST zif_material_from_variable_key( NEW zcl_material_from_variable_key( ) ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.

    ro_material_from_variable_key = mo_material_from_variable_key.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MATERIAL_FROM_VARIABLE_KEY->RETRIEVE_MATERIAL_BY_OFFSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KOTABNR                     TYPE        KOTABNR
* | [--->] IV_VAKEY                       TYPE        VAKEY_LONG
* | [<-()] RV_MATNR                       TYPE        MATNR
* | [!CX!] CX_SY_ITAB_LINE_NOT_FOUND
* | [!CX!] CX_SY_RANGE_OUT_OF_BOUNDS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD retrieve_material_by_offset.

    TRY.
        DATA(lv_offset) = VALUE #( mt_keys[ kotabnr = iv_kotabnr ]-offset ).
        rv_matnr = iv_vakey+lv_offset(18).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      CATCH cx_sy_range_out_of_bounds.
        RAISE EXCEPTION TYPE cx_sy_range_out_of_bounds.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MATERIAL_FROM_VARIABLE_KEY->SELECT_FIELDS_FOR_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KOTABNR                     TYPE        KOTABNR
* | [!CX!] ZCX_NO_DATA_SELECTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_fields_for_table.

    IF line_exists( mt_keys[ kotabnr = iv_kotabnr ] ).
      RETURN.
    ENDIF.

    DATA(lv_gstru) = CONV gstru( |A| && iv_kotabnr ).
    SELECT FROM tmc1k
           INNER JOIN dd03l
                 ON tmc1k~qutab EQ dd03l~tabname AND
                    tmc1k~qufna EQ dd03l~fieldname
           FIELDS tmc1k~gstru, tmc1k~lfnr2, tmc1k~stfna, dd03l~leng
           WHERE tmc1k~gstru    EQ @lv_gstru
             AND dd03l~as4local EQ 'A'
             AND dd03l~as4vers  EQ '0000'
           INTO TABLE @DATA(lt_tmc1k).
    IF sy-subrc EQ 0.
      INSERT LINES OF lt_tmc1k INTO TABLE mt_tmc1k.
      calculate_offset( iv_kotabnr ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_no_data_selected.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATERIAL_FROM_VARIABLE_KEY->ZIF_MATERIAL_FROM_VARIABLE_KEY~GET_MATERIAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KOTABNR                     TYPE        KOTABNR
* | [--->] IV_VAKEY                       TYPE        VAKEY_LONG
* | [<-()] RV_MATNR                       TYPE        MATNR
* | [!CX!] ZCX_NO_DATA_SELECTED
* | [!CX!] CX_SY_ITAB_LINE_NOT_FOUND
* | [!CX!] CX_SY_RANGE_OUT_OF_BOUNDS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_material_from_variable_key~get_material.

    TRY.
        select_fields_for_table( iv_kotabnr ).
        rv_matnr = retrieve_material_by_offset( iv_kotabnr = iv_kotabnr
                                                iv_vakey   = iv_vakey ).
      CATCH zcx_no_data_selected.
      CATCH cx_sy_itab_line_not_found.
      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
