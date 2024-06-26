
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
      | 3 => RETURN "src-file"
      | 4 => RETURN "src-dir"
      | 5 => RETURN "import-dir"
      | 6 => RETURN "resource-dir"
      | 7 => RETURN "build-dir"
      | 8 => RETURN "disasm-passes"
      | 9 => RETURN "disasm"
      | 10 => RETURN "keep-passes"
      | 11 => RETURN "keep"
      | 12 => RETURN "remove-unused-decls"
      | 13 => RETURN "stderr"
      | 14 => RETURN "log"
      | 15 => RETURN "stdout"
      | 16 => RETURN "unit-log"
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
      | 3 => RETURN "CltSrcFile"
      | 4 => RETURN "CltSrcDir"
      | 5 => RETURN "CltImportDir"
      | 6 => RETURN "CltResourceDir"
      | 7 => RETURN "CltBuildDir"
      | 8 => RETURN "CltDisAsmPasses"
      | 9 => RETURN "CltDisAsm"
      | 10 => RETURN "CltKeepPasses"
      | 11 => RETURN "CltKeep"
      | 12 => RETURN "CltRemoveUnusedDecls"
      | 13 => RETURN "CltStdErr"
      | 14 => RETURN "CltFM3Log"
      | 15 => RETURN "CltStdOut"
      | 16 => RETURN "CltUnitLog"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3CLToks
.

