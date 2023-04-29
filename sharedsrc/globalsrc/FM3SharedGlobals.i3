        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedGlobals

; FROM File IMPORT Byte  

(* FileTag characters for files: *)

(* For all FM3-specific file formats: Don't compress these. *) 
; CONST FM3FileTagLt
    = ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( ORD ( 'F' ) , Byte )
      , VAL ( ORD ( 'M' ) , Byte )
      , VAL ( ORD ( '3' ) , Byte )
      }
  (* For normal forward reading. *) 

; CONST FM3FileTagRtBwd
    = ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( ORD ( '3' ) , Byte )
      , VAL ( ORD ( 'M' ) , Byte )
      , VAL ( ORD ( 'F' ) , Byte )
      }
  (* For backward reading. *)

(* Specific Filekinds: These are all single ASCII chars,
   so compression would be an identity. *) 

; CONST FM3FileTagRdBackLt = VAL ( ORD ( 'A' ) , Byte )
  (* Left end of FM3RdBackFile. *) 
; CONST FM3FileTagRdBackRt = VAL ( ORD ( 'B' ) , Byte ) 
  (* Right end of FM3RdBackFile. *) 

; END FM3SharedGlobals
.


