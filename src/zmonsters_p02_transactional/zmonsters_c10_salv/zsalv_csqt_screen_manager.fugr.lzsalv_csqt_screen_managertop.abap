FUNCTION-POOL zsalv_csqt_screen_manager.    "MESSAGE-ID ..

DATA: okcode             TYPE sy-ucomm.
DATA: gr_content_manager TYPE REF TO if_salv_csqt_content_manager.
DATA: g_title            TYPE lvc_title.
DATA: gr_container       TYPE REF TO cl_gui_custom_container.
* INCLUDE LZSALV_CSQT_SCREEN_MANAGERD...     " Local class definition
