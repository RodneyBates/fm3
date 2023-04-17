        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A file that can be written (one byte at at time) normally, or
   read backwards.  Writes and reads can be interspersed, in which
   case, it behaves abstractly as a stack.
   It is maintained on a disk file.
*) 

INTERFACE RdBackFile

; IMPORT File
; IMPORT OSError 

; EXCEPTION Preexists
; EXCEPTION BOF 

; TYPE T <: REFANY

; TYPE ByteTyp = File . Byte 

; PROCEDURE Create
    ( Filename : TEXT ; Truncate (* To empty. *) := FALSE ) : T
  RAISES { OSError . E , Preexists }   

; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

; PROCEDURE LengthL ( RbFile : T ) : LONGCARD RAISES { OSError . E }
  (* Number of bytes in the file. *) 

; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than Length(F)=0L. *) 

; PROCEDURE Close ( RbFile : T ) RAISES { OSError . E }

; PROCEDURE Put ( RbFile : T ; Value : ByteTyp )  RAISES { OSError . E }  

; PROCEDURE GetBwd ( RbFile : T ) : ByteTyp  RAISES { BOF , OSError . E }  

; END RdBackFile
.

