*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_UTILS
*&---------------------------------------------------------------------*
CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      c_field_width  TYPE i VALUE 16, "= columns of field
      c_field_height TYPE i VALUE 8,  "= rows of field

      c_box_width    TYPE i VALUE 4,  "= visual width of one box
      c_box_height   TYPE i VALUE 2.  "= visual height of one box

    CLASS-METHODS:
      "! Cause cl_abap_random sucks
      "! @parameter iw_from |
      "! @parameter iw_to |
      "! @parameter rw_random |
      get_random_number
        IMPORTING
                  iw_from          TYPE i DEFAULT 1
                  iw_to            TYPE i
        RETURNING VALUE(rw_random) TYPE i.

ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD get_random_number.

    DATA lw_xrandom TYPE xstring.

    CALL FUNCTION 'GENERATE_SEC_RANDOM'
      EXPORTING
        length = 1
      IMPORTING
        random = lw_xrandom.

    " TODO: implement iw_to and iw_from
    rw_random = iw_from + CONV i( lw_xrandom ) MOD iw_to.

  ENDMETHOD.

ENDCLASS.