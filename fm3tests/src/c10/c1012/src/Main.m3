MODULE Main

; TYPE O1
    = OBJECT
        F1 , F2 : BOOLEAN
      METHODS
        m0 ( pm0 : CHAR )
      ; m1 ( pm1 , pm2 : WIDECHAR ) : INTEGER RAISES { r1 }
      OVERRIDES
        m0 = P1 
      END

; TYPE O2
    = O1 BRANDED OBJECT END  

; PROCEDURE P1    ( )
 = BEGIN
   END P1

; BEGIN
  END Main
.

