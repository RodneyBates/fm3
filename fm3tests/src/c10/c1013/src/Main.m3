MODULE Main

(* Permutations of optional parts of object types. *)

; TYPE T1 = OBJECT END

; TYPE T2 = T1
            BRANDED
            OBJECT (* Nonempty lists NYI, 6/4/2025. *) 
            METHODS
              M1 ( ) 
            OVERRIDES
            END 

; TYPE T3 = BRANDED Brand
            OBJECT
            METHODS
            OVERRIDES
              M1 = P1 
            END 

; TYPE T = OBJECT NestedField : INTEGER END
           OBJECT END

; PROCEDURE P1 ( )
  = BEGIN
    END P1 

; BEGIN
  END Main
  
.

