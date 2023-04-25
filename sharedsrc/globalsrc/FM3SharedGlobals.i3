        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedGlobals

; FROM File IMPORT Byte  

(* Tag characters for files: *)

(* For all FM3-specific file formats: Don't compress these. *) 
; CONST FM3TagLt
    = ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( 'F' , Byte ) , VAL ( 'M' , Byte , VAL ( '3' , Byte ) }
  (* For normal forward reading. *) 

; CONST FM3TagRtBwd
    = ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( '3' , Byte ) , VAL ( 'M' , Byte , VAL ( 'F' , Byte ) }
  (* For backward reading. *)

(* Specific Filekinds: These are all single ASCII chars,
   so compression would be an identity. *) 

; CONST FM3TagRdBackLt = VAL ( 'A' , Byte )
  (* Left end of FM3RdBackFile. *) 
; CONST FM3TagRdBackRt = VAL ( 'B' , Byte ) 
  (* Right end of FM3RdBackFile. *) 

; END FM3SharedGlobals
.


