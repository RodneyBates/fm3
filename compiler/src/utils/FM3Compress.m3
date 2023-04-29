        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE FM3Compress 

(* Reading and writing, to/from a RdBackFile.T,  a stream of 64-bit words,
   using a simple compression technique favoring values that, if viewed as
   64-bit integers have smaller absolute values. *) 

(* Compression scheme: 
   A variable length sequence of one to nine bytes, least significant to most
   significant.  The least significant 7 bits of each byte are data bits.
   The most significant bit is 1 to denote that another, more significant
   byte follows.  The 9th byte, if it occurs, has data in all 8 bits and
   never has a successor.  Nine bytes can hold any 64-bit value.  This
   compression scheme views the values as signed, but clients can view
   them either way, since it always reproduces the bit pattern.  

   For many values in the Unicode code point range of 0..16_10FFFF, this scheme
   is more compact than UTF-8, but is not self-synchronizing.

   Read procedures herein tolerate unnecessary bytes containing extra more 
   significant sign bits, but Write procedures do not produce these. 
*)   

; IMPORT Compiler
; IMPORT OSError 
; IMPORT RdBackFile 
; IMPORT Thread 
; IMPORT Long
; IMPORT Wr

; CONST DivL = Long . Divide 
; CONST AndL = Long . And 
; CONST OrL = Long . Or 
; CONST RSL = Long . RightShift
; CONST LSL = Long . LeftShift
; CONST Put = RdBackFile . Put 
; CONST GetRdBack = RdBackFile . GetBwd 

; CONST TwoTo6thL = Long . LeftShift ( 1L , 6 ) 
; CONST TwoTo13thL = Long . LeftShift ( 1L , 13 ) 
; CONST TwoTo20thL = Long . LeftShift ( 1L , 20 ) 
; CONST TwoTo27thL = Long . LeftShift ( 1L , 27 ) 
; CONST TwoTo34thL = Long . LeftShift ( 1L , 34 ) 
; CONST TwoTo41stL = Long . LeftShift ( 1L , 41 ) 
; CONST TwoTo48thL = Long . LeftShift ( 1L , 48 ) 
; CONST TwoTo55thL = Long . LeftShift ( 1L , 55 )

(* These will be performance hot-spots, on account of reading/writing a byte
   at a time.  In the interest of speed, these are coded with fully unrolled
   loops, to save loop control overhead.
*) 

(*EXPORTED*)
; PROCEDURE PutBwd ( File : RdBackFile . T ; ValueL : LONGINT )
  RAISES { OSError . E } 
  (* Write compressed bytes to File, in most- to least-significant order. *)

  (* For purposes here, consider the LSB as byte 0 and increasing towards
     more significant. *)

  (* All tests are done right-to-left, to allow their early termination on
     low absolute values--likely to be common.  But emission of bytes is
     done directly left-to-right, to avoid storing them up and emitting
     in reversed order.  This was tricky. *) 

  = VAR LSignBitsL : LONGINT

  ; BEGIN
      LSignBitsL := DivL ( ValueL , TwoTo6thL )
             (* ^Shift right 6 bits, with sign extension. *) 
    ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
      THEN (* One byte is not enough. *)
      
        LSignBitsL := DivL ( ValueL , TwoTo13thL ) 
      ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
        THEN (* Two bytes are not enough. *)

          LSignBitsL := DivL ( ValueL , TwoTo20thL ) 
        ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
          THEN (* Three bytes are not enough. *)

            LSignBitsL := DivL ( ValueL , TwoTo27thL ) 
          ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
            THEN (* Four bytes are not enough. *)

              LSignBitsL := DivL ( ValueL , TwoTo34thL ) 
            ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
              THEN (* Five bytes are not enough. *)

                LSignBitsL := DivL ( ValueL , TwoTo41stL ) 
              ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
                THEN (* Six bytes are not enough. *)

                  LSignBitsL := DivL ( ValueL , TwoTo48thL ) 
                ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
                  THEN (* Seven bytes are not enough. *)

                    LSignBitsL := DivL ( ValueL , TwoTo55thL ) 
                  ; IF LSignBitsL # 16_FFFFFFFFFFFFFFFFL AND LSignBitsL # 0L
                    THEN (* Eight bytes are not enough. *)

                      (* All 9 bytes 8..0 required. Emit byte 8 in entirity. *)
                        Put ( File , RSL ( ValueL , 56 ) ) 

                    (* Now byte 7, with continue bit. *) 
                    ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 49 ) , 16_7F ) , 16_80 ) )

                    ELSE (* Bytes 7..0 required. Emit byte 7 w/o continue bit. *)
                      Put ( File , AndL ( RSL ( ValueL , 49 ) , 16_7F ) ) 
                    END (*IF*)

                  (* Now byte 6, with continue bit. *) 
                  ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 42 ) , 16_7F ) , 16_80 ) )

                  ELSE (* Bytes 6..0 required. Emit byte 6 w/o continue bit. *)
                    Put ( File , AndL ( RSL ( ValueL , 42 ) , 16_7F ) ) 
                  END (*IF*)

                (* Now byte 5, with continue bit. *) 
                ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 35 ) , 16_7F ) , 16_80 ) )

                ELSE (* Bytes 5..0 required. Emit byte 5 w/o continue bit. *)
                  Put ( File , AndL ( RSL ( ValueL , 35 ) , 16_7F ) ) 
                END (*IF*)

              (* Now byte 4, with continue bit. *) 
              ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 28 ) , 16_7F ) , 16_80 ) )

              ELSE (* Bytes 4..0 required. Emit byte 4 w/o continue bit. *)
                Put ( File , AndL ( RSL ( ValueL , 28 ) , 16_7F ) ) 
              END (*IF*)
              
            (* Now byte 3, with continue bit. *) 
            ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 21 ) , 16_7F ) , 16_80 ) )

            ELSE (* Bytes 3..0 required. Emit byte 3 w/o continue bit. *)
              Put ( File , AndL ( RSL ( ValueL , 21 ) , 16_7F ) ) 
            END (*IF*) 

          (*Now byte 2, with continue bit. *) 
          ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 14 ) , 16_7F ) , 16_80 ) )
          
          ELSE (* Byte 2..0 required. Emit byte 2 w/o continue bit. *)
            Put ( File , AndL ( RSL ( ValueL , 14 ) , 16_7F ) )
          END (*IF*) 

        (* Now byte 1, with continue bit. *) 
        ; Put ( File , OrL ( AndL  ( RSL ( ValueL , 7 ) , 16_7F ) , 16_80 ) )
        
        ELSE (* Bytes 1 & 0 required. Emit byte 1 w/o continue bit. *)
          Put ( AndL ( RSL ( ValueL , 7 , 16_7F ) ) ) 
        END (*IF*)
        
      (* Now byte 0, with continue bit. *) 
      ; Put ( File , OrL ( AndL ( ValueL , 16_7F ) , 16_80 ) )
      
      ELSE (* Byte 0 alone suffices, w/o continue bit. *)
        Put ( File , AndL ( ValueL , 16_7F ) ) 
      END (*IF*) 
    END PutBwd

(*EXPORTED*) 
; PROCEDURE PutFwd  ( File : RdBackFile . T ; ValueL : LONGINT )
  RAISES { OSError . E } 
  (* Write compressed bytes to File, in least- to most-significant order. *)

  = VAR LSignBitsL : LONGINT
  ; VAR LResidueL : LONGINT

  ; BEGIN
    (* Byte 0. *) 
      LSignBitsL := DivL ( ValueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 1. *) 
    ; LResidueL := RSL ( ValueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 2. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 3. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 4. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 5. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 6. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 7. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
    ; LSignBitsL := DivL ( LResidueL , TwoTo6thL ) 
    ; IF LSignBitsL = 16_FFFFFFFFFFFFFFFFL OR LSignBitsL = 0L
      THEN (* Rightmost 7 bits of LResidueL are the leftmost byte needed. *)
        Put ( File , AndL ( LResidueL , 16_7F ) )
      ; RETURN
      END (*IF*) 
    ; Put ( File , OrL ( AndL ( LResidueL , 16_7F ) , 16_80 ) ) 

    (* Byte 8. *) 
    ; LResidueL := RSL ( LResidueL , 7 )  
      (* The rightmost 8 bits of LResidueL are the final leftmost byte.
         The other bits have had all zeros shifted into them. *)
    ; Put ( File , LResidueL )
    END PutFwd 

(*EXPORTED*) 
; PROCEDURE GetBwd  ( File : RdBackFile . T ) : LONGINT
  RAISES { OSError . E } 
  (* Read and decode compressed bytes from File.  Treat them as
     being in least- to most-significant order. *)

  = VAR LResultL : LONGINT
  ; VAR LByte : RdBackFile . ByteTyp 

  ; BEGIN
      LByte := GetRdBack ( File ) 
    ; LResultL := AndL ( LByte , 16_7F )
    ; IF AndL ( LByte , 16_80 ) # 0L
      THEN (* Byte 1 follows. *)
        LByte := GetRdBack ( File ) 
      ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 7 ) ) 
      ; IF AndL ( LByte , 16_80 ) # 0L
        THEN (* Byte 2 follows. *)
          LByte := GetRdBack ( File ) 
        ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 14 ) ) 
        ; IF AndL ( LByte , 16_80 ) # 0L
          THEN (* Byte 3 follows. *)
            LByte := GetRdBack ( File ) 
          ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 21 ) ) 
          ; IF AndL ( LByte , 16_80 ) # 0L
            THEN (* Byte 4 follows. *)
              LByte := GetRdBack ( File ) 
            ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 28 ) ) 
            ; IF AndL ( LByte , 16_80 ) # 0L
              THEN (* Byte 5 follows. *)
                LByte := GetRdBack ( File ) 
              ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 35 ) ) 
              ; IF AndL ( LByte , 16_80 ) # 0L
                THEN (* Byte 6 follows. *)
                  LByte := GetRdBack ( File ) 
                ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 42 ) ) 
                ; IF AndL ( LByte , 16_80 ) # 0L
                  THEN (* Byte 7 follows. *)
                    LByte := GetRdBack ( File ) 
                  ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_7F ) , 49 ) ) 
                  ; IF AndL ( LByte , 16_80 ) # 0L
                    THEN (* Byte 8 follows. *)
                      LByte := GetRdBack ( File ) 
                    ; LResultL := OrL ( LResultL , LSL ( AndL ( LByte , 16_FF ) , 49 ) ) 
                    END (*IF*) 
                  END (*IF*) 
                END (*IF*) 
              END (*IF*) 
            END (*IF*) 
          END (*IF*) 
        END (*IF*) 
      END (*IF*)
    ; RETURN LResultL 
    END GetBwd

; BEGIN 
  END FM3Compress 
. 

