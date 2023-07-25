 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE DumpWork

; IMPORT Fmt
; IMPORT Wr 

; IMPORT FM3Compress 
; IMPORT RdBackFile 

(*EXPORTED:*) 
; PROCEDURE DumpNumericBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = VAR LLength , LValue : LONGINT

  ; BEGIN
      LOOP
        TRY 
          LLength := RdBackFile . LengthL ( RBT ) 
        ; LValue := FM3Compress . GetBwd ( RBT )
        EXCEPT
        | RdBackFile . BOF
        => Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( 0L) , 6 ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; EXIT 
        END (*EXCEPT*) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LLength ) , 6 ) )
      ; Wr . PutText ( WrT , " 16_" ) 
      ; Wr . PutText
          ( WrT
          , Fmt . Pad ( Fmt . LongInt ( LValue , 16 ) , 16 , padChar := '0' )
          )
      ; Wr . PutText ( WrT , " " ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LValue ) , 21 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (*LOOP*)
    END DumpNumericBwd 

(*EXPORTED:*) 
; PROCEDURE DumpInterpretBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = BEGIN
(* COMPLETEME: *) 
    END DumpInterpretBwd 

; BEGIN
  END DumpWork
.

