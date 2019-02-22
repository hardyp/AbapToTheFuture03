*----------------------------------------------------------------------*
***INCLUDE LZCH05_01_02_TEST_SEAMST99.
*----------------------------------------------------------------------*
CLASS lcl_test_class DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT
FINAL.
  PRIVATE SECTION.
    METHODS fire_nuclear_missile FOR TESTING.
ENDCLASS.

CLASS lcl_test_class IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 05.04:  Test Injections for Test Seams
*--------------------------------------------------------------------*
* If you ever feel the urge to do this in real life, get down to the
* brain doctor just as fast as you can, if you can dodge the purple
* crocodiles that you see crawling out of the walls
*--------------------------------------------------------------------*
  METHOD fire_nuclear_missile."Test Method
    TEST-INJECTION read_database.
* Set assorted variables, just as if you had read them from the
* actual database
    END-TEST-INJECTION.

    TEST-INJECTION user_input.
      user_answer = '1'.
    END-TEST-INJECTION.

    PERFORM fire_nuclear_missile.

  ENDMETHOD.

ENDCLASS.
