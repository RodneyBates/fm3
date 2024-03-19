
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3CLToks.gentok", with command line 
     "./gentok -S -L -i -n FM3CLToks.gentok". *)

MODULE FM3CLToks

(*EXPORTED*)
; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 0 => RETURN "<null>"
      | 1 => RETURN "version"
      | 2 => RETURN "help"
      | 3 => RETURN "build-dir"
      | 4 => RETURN "keep"
      | 5 => RETURN "disasm-pass-1"
      | 6 => RETURN "disasm-pass-2"
      | 7 => RETURN "stderr"
      | 8 => RETURN "log"
      | 9 => RETURN "stdout"
      | 10 => RETURN "unit-log"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Image

(*EXPORTED*)
; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 0 => RETURN "CltNull"
      | 1 => RETURN "CltVersion"
      | 2 => RETURN "CltHelp"
      | 3 => RETURN "CltBuildDir"
      | 4 => RETURN "CltKeep"
      | 5 => RETURN "CltDisAsmPass1"
      | 6 => RETURN "CltDisAsmPass2"
      | 7 => RETURN "CltStdErr"
      | 8 => RETURN "CltFM3Log"
      | 9 => RETURN "CltStdOut"
      | 10 => RETURN "CltUnitLog"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3CLToks
.
