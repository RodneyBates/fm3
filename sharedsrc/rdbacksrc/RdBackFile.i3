        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A file that can be written (one byte at at time) normally, or
   read backwards, either way, only at the right end.  Writes and
   reads can be interspersed, in which case, it behaves abstractly
   as a stack.
   
   It is maintained on a disk file during use, thus could grow very
   large, letting the OS deal with space management, possibly in memory.
   
   It also can be closed and reopened later.  When closed, its contents
   are indistinguishable from and interchangable with a plain file, thus
   handled by FileRd or FileWr. 
*) 

INTERFACE RdBackFile

; IMPORT OSError
; IMPORT Thread 

; EXCEPTION Preexists
; EXCEPTION BOF 

; TYPE T <: REFANY

; TYPE ByteTyp = [ 0 .. 16_FF ]

(* All lengths and offsets within the file are relative to after the prefix. *) 

; PROCEDURE Create
    ( Filename : TEXT ; Truncate (* To empty. *) := FALSE ) : T
  RAISES { OSError . E , Preexists (* Only if NOT Truncate *) }   

; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

; PROCEDURE FileName ( RbFile : T ) : TEXT 
  (* Never NIL.  Possibly empty. *) 

; PROCEDURE LengthL ( RbFile : T ) : LONGCARD
  RAISES { OSError . E }
  (* Number of bytes in the file. *) 

; PROCEDURE MaxLengthL ( RbFile : T ) : LONGCARD RAISES { OSError . E }
  (* Max LengthL ever was since Create or Open. Eccludes prefix. *) 

; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than LengthL(F)=0L. *) 

; PROCEDURE Flush ( RbFile : T )

; PROCEDURE Close ( RbFile : T ; TruncTo : LONGINT )
  (* TruncTo < 0 means max length. *) 

; PROCEDURE Put ( RbFile : T ; Value : ByteTyp )
  RAISES { OSError . E }  

; PROCEDURE GetBwd
    ( RbFile : T ; Consume := TRUE ) : ByteTyp RAISES { OSError . E , BOF }

; PROCEDURE Copy ( RbFile : T ; CopyFileName : TEXT ; TruncTo : LONGINT )
  (* TruncTo < 0 means max length. *)
  (* Does not alter RbFile *) 
  (* PRE & POST: RbFile is open. *)
  (* POST: The copy is closed. *)

; END RdBackFile
.

