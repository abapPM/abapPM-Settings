CLASS zcl_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Settings
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_settings.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_name      TYPE zif_settings=>ty_name DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO zif_settings
      RAISING
        zcx_error.

    CLASS-METHODS injector
      IMPORTING
        !iv_name TYPE zif_settings=>ty_name
        !ii_mock TYPE REF TO zif_settings.

    METHODS constructor
      IMPORTING
        !iv_name TYPE zif_settings=>ty_name
      RAISING
        zcx_error.

    CLASS-METHODS initialize_global_settings
      RAISING
        zcx_error.

    CLASS-METHODS get_setting_key
      IMPORTING
        !iv_name      TYPE zif_settings=>ty_name
      RETURNING
        VALUE(result) TYPE zif_persist_apm=>ty_key.

    CLASS-METHODS get_default
      RETURNING
        VALUE(result) TYPE zif_settings=>ty_settings.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE zif_settings=>ty_name,
        instance TYPE REF TO zif_settings,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CONSTANTS c_settings TYPE string VALUE 'SETTINGS'.

    CLASS-DATA:
      gi_persist   TYPE REF TO zif_persist_apm,
      gt_instances TYPE ty_instances.

    DATA:
      mv_key      TYPE zif_persist_apm=>ty_key,
      mv_name     TYPE zif_settings=>ty_name,
      ms_settings TYPE zif_settings=>ty_settings.

    CLASS-METHODS check_settings
      IMPORTING
        !is_settings  TYPE zif_settings=>ty_settings
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS merge_settings
      CHANGING
        !cs_settings TYPE zif_settings=>ty_settings.

ENDCLASS.



CLASS zcl_settings IMPLEMENTATION.


  METHOD check_settings.
    IF zcl_package_json_valid=>is_valid_url( is_settings-registry ) = abap_false.
      INSERT |Invalid registry URL: { is_settings-registry }| INTO TABLE result.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    gi_persist = zcl_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    IF iv_name IS INITIAL OR strlen( iv_name ) > 12.
      zcx_error=>raise( |Invalid name: { iv_name }| ).
    ENDIF.

    mv_name = iv_name.
    mv_key  = get_setting_key( mv_name ).

    TRY.
        zif_settings~load( ).
      CATCH zcx_error.
        IF mv_name = zif_settings=>c_global.
          ms_settings = get_default( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_settings
        EXPORTING
          iv_name = iv_name.

      ls_instance-name     = iv_name.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.
    " Default values for settings
    result-registry = zif_settings=>c_registry.
  ENDMETHOD.


  METHOD get_setting_key.
    result = |{ zif_persist_apm=>c_key_type-settings }:{ iv_name }|.
  ENDMETHOD.


  METHOD initialize_global_settings.

    DATA:
      li_global TYPE REF TO zif_settings,
      ls_global TYPE zif_settings=>ty_settings.

    li_global = factory( zif_settings=>c_global ).

    " Check if global settings exist already
    TRY.
        ls_global = li_global->load( )->get( ).
      CATCH zcx_error ##NO_HANDLER.
    ENDTRY.

    IF ls_global IS NOT INITIAL.
      RETURN.
    ENDIF.

    li_global->set( get_default( ) ).

    " Save defaults to global settings
    gi_persist->save(
      iv_key   = get_setting_key( zif_settings=>c_global )
      iv_value = li_global->get_json( ) ).

  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      <ls_instance>-instance = ii_mock.
    ELSE.
      ls_instance-name     = iv_name.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD merge_settings.

    DATA:
      ls_global  TYPE zif_settings=>ty_settings,
      ls_default TYPE zif_settings=>ty_settings.

    FIELD-SYMBOLS:
      <lv_value>   TYPE any,
      <lv_global>  TYPE any,
      <lv_default> TYPE any.

    TRY.
        ls_global  = factory( zif_settings=>c_global )->get( ).
      CATCH zcx_error ##NO_HANDLER.
        " Just use defaults
    ENDTRY.

    ls_default = get_default( ).

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE cs_settings TO <lv_value>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <lv_value> IS INITIAL.
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_global TO <lv_global>.
        ASSERT sy-subrc = 0.

        IF <lv_value> IS INITIAL.
          ASSIGN COMPONENT sy-index OF STRUCTURE ls_default TO <lv_default>.
          ASSERT sy-subrc = 0.

          <lv_value> = <lv_default>.
        ELSE.
          <lv_value> = <lv_global>.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_settings~delete.

    IF mv_name = zif_settings=>c_global.
      zcx_error=>raise( 'Global settings can not be deleted' ).
    ENDIF.

    gi_persist->delete( mv_key ).

  ENDMETHOD.


  METHOD zif_settings~get.
    result = ms_settings.
    IF mv_name <> zif_settings=>c_global.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_settings~get_json.

    DATA:
      li_json  TYPE REF TO zif_ajson,
      lx_error TYPE REF TO zcx_ajson_error.

    TRY.
        li_json = zcl_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = zif_settings~get( ) ).

        li_json = li_json->map( zcl_ajson_mapping=>create_to_camel_case( ) ).

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH zcx_ajson_error INTO lx_error.
        zcx_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_settings~is_valid.
    result = boolc( check_settings( ms_settings ) IS INITIAL ).
  ENDMETHOD.


  METHOD zif_settings~load.
    zif_settings~set_json( gi_persist->load( mv_key )-value ).
    result = me.
  ENDMETHOD.


  METHOD zif_settings~save.

    IF zif_settings~is_valid( ) = abap_false.
      zcx_error=>raise( 'Invalid settings' ).
    ENDIF.

    " Save complete JSON including empty values for easy editing
    gi_persist->save(
      iv_key   = mv_key
      iv_value = zif_settings~get_json( abap_true ) ).

  ENDMETHOD.


  METHOD zif_settings~set.

    IF check_settings( is_settings ) IS NOT INITIAL.
      zcx_error=>raise( 'Invalid settings' ).
    ENDIF.

    MOVE-CORRESPONDING is_settings TO ms_settings.
    result = me.

  ENDMETHOD.


  METHOD zif_settings~set_json.

    DATA:
      li_json  TYPE REF TO zif_ajson,
      ls_json  TYPE zif_settings=>ty_settings,
      lx_error TYPE REF TO zcx_ajson_error.

    TRY.
        li_json = zcl_ajson=>parse( iv_json ).
        " TODO: packageSettings does not map to package_setting
        " Looks like a ajson bug
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json ).

        IF check_settings( ls_json ) IS NOT INITIAL.
          zcx_error=>raise( 'Invalid settings' ).
        ENDIF.

        MOVE-CORRESPONDING ls_json TO ms_settings.
      CATCH zcx_ajson_error INTO lx_error.
        zcx_error=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
