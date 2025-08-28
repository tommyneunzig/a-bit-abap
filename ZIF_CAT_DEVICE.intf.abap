INTERFACE zif_cat_device PUBLIC.
  METHODS print IMPORTING doc TYPE string.
  METHODS scan RETURNING VALUE(r) TYPE string.
  METHODS fax IMPORTING number TYPE string doc TYPE string.
  METHODS email IMPORTING to TYPE string doc TYPE string.
  METHODS shred.
ENDINTERFACE.
