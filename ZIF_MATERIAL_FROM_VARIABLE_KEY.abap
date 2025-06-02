interface ZIF_MATERIAL_FROM_VARIABLE_KEY
  public .


  methods GET_MATERIAL
    importing
      !IV_KOTABNR type KOTABNR
      !IV_VAKEY type VAKEY_LONG
    returning
      value(RV_MATNR) type MATNR
    raising
      ZCX_NO_DATA_SELECTED
      CX_SY_ITAB_LINE_NOT_FOUND
      CX_SY_RANGE_OUT_OF_BOUNDS .
endinterface.
