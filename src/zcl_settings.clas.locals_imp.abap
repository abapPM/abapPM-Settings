* Same as zcl_ajson_filter_lib=>create_empty_filter( ) but also removing initial numbers and null
CLASS lcl_ajson_filters DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_ajson_filter.

    CLASS-METHODS create_empty_filter
      RETURNING
        VALUE(ri_filter) TYPE REF TO zif_ajson_filter
      RAISING
        zcx_ajson_error .

ENDCLASS.

CLASS lcl_ajson_filters IMPLEMENTATION.

  METHOD create_empty_filter.
    CREATE OBJECT ri_filter TYPE lcl_ajson_filters.
  ENDMETHOD.

  METHOD zif_ajson_filter~keep_node.

    rv_keep = boolc(
      ( iv_visit = zif_ajson_filter=>visit_type-value AND
        ( is_node-type = zif_ajson_types=>node_type-string AND is_node-value IS NOT INITIAL OR
          is_node-type = zif_ajson_types=>node_type-boolean OR
          is_node-type = zif_ajson_types=>node_type-number AND is_node-value <> 0 ) ) OR
      ( iv_visit <> zif_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).

  ENDMETHOD.

ENDCLASS.
