*&---------------------------------------------------------------------*
*& Include          ZIMM_DUPLICIDADE_NF
*&---------------------------------------------------------------------*

ASSIGN ('(SAPLMIGO)GOHEAD') TO FIELD-SYMBOL(<fs_gohead>).
ASSIGN ('(SAPLMIGO)GOITEM') TO FIELD-SYMBOL(<fs_goitem>).

IF sy-tcode EQ 'MIGO' AND
 ( sy-ucomm EQ 'OK_POST' OR sy-ucomm EQ 'OK_CHECK' ) AND
 ( <fs_gohead> IS ASSIGNED AND <fs_goitem> IS ASSIGNED ).

  NEW zcl_migo_procura_nfe( )->search_nfe( EXPORTING
                                             is_gohead = <fs_gohead>
                                             is_goitem = <fs_goitem>
                                           CHANGING
                                             ct_bapiret2 = et_bapiret2 ).

ENDIF.
