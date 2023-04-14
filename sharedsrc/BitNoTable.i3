
(* -----------------------------------------------------------------------1- *)
(* File BitNoTable.i3  Modula-3 source code.                                 *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically generated by GenBitNoArrays. *)

INTERFACE BitNoTable 

; CONST Least1BitNoInByte
    = ARRAY [ 0 .. 16_FF ] OF [ 0 .. 8 ]
        { (*   0 16_00 2_00000000 *) 8 , (*   1 16_01 2_00000001 *) 0
        , (*   2 16_02 2_00000010 *) 1 , (*   3 16_03 2_00000011 *) 0
        , (*   4 16_04 2_00000100 *) 2 , (*   5 16_05 2_00000101 *) 0
        , (*   6 16_06 2_00000110 *) 1 , (*   7 16_07 2_00000111 *) 0
        , (*   8 16_08 2_00001000 *) 3 , (*   9 16_09 2_00001001 *) 0
        , (*  10 16_0a 2_00001010 *) 1 , (*  11 16_0b 2_00001011 *) 0
        , (*  12 16_0c 2_00001100 *) 2 , (*  13 16_0d 2_00001101 *) 0
        , (*  14 16_0e 2_00001110 *) 1 , (*  15 16_0f 2_00001111 *) 0
        , (*  16 16_10 2_00010000 *) 4 , (*  17 16_11 2_00010001 *) 0
        , (*  18 16_12 2_00010010 *) 1 , (*  19 16_13 2_00010011 *) 0
        , (*  20 16_14 2_00010100 *) 2 , (*  21 16_15 2_00010101 *) 0
        , (*  22 16_16 2_00010110 *) 1 , (*  23 16_17 2_00010111 *) 0
        , (*  24 16_18 2_00011000 *) 3 , (*  25 16_19 2_00011001 *) 0
        , (*  26 16_1a 2_00011010 *) 1 , (*  27 16_1b 2_00011011 *) 0
        , (*  28 16_1c 2_00011100 *) 2 , (*  29 16_1d 2_00011101 *) 0
        , (*  30 16_1e 2_00011110 *) 1 , (*  31 16_1f 2_00011111 *) 0
        , (*  32 16_20 2_00100000 *) 5 , (*  33 16_21 2_00100001 *) 0
        , (*  34 16_22 2_00100010 *) 1 , (*  35 16_23 2_00100011 *) 0
        , (*  36 16_24 2_00100100 *) 2 , (*  37 16_25 2_00100101 *) 0
        , (*  38 16_26 2_00100110 *) 1 , (*  39 16_27 2_00100111 *) 0
        , (*  40 16_28 2_00101000 *) 3 , (*  41 16_29 2_00101001 *) 0
        , (*  42 16_2a 2_00101010 *) 1 , (*  43 16_2b 2_00101011 *) 0
        , (*  44 16_2c 2_00101100 *) 2 , (*  45 16_2d 2_00101101 *) 0
        , (*  46 16_2e 2_00101110 *) 1 , (*  47 16_2f 2_00101111 *) 0
        , (*  48 16_30 2_00110000 *) 4 , (*  49 16_31 2_00110001 *) 0
        , (*  50 16_32 2_00110010 *) 1 , (*  51 16_33 2_00110011 *) 0
        , (*  52 16_34 2_00110100 *) 2 , (*  53 16_35 2_00110101 *) 0
        , (*  54 16_36 2_00110110 *) 1 , (*  55 16_37 2_00110111 *) 0
        , (*  56 16_38 2_00111000 *) 3 , (*  57 16_39 2_00111001 *) 0
        , (*  58 16_3a 2_00111010 *) 1 , (*  59 16_3b 2_00111011 *) 0
        , (*  60 16_3c 2_00111100 *) 2 , (*  61 16_3d 2_00111101 *) 0
        , (*  62 16_3e 2_00111110 *) 1 , (*  63 16_3f 2_00111111 *) 0
        , (*  64 16_40 2_01000000 *) 6 , (*  65 16_41 2_01000001 *) 0
        , (*  66 16_42 2_01000010 *) 1 , (*  67 16_43 2_01000011 *) 0
        , (*  68 16_44 2_01000100 *) 2 , (*  69 16_45 2_01000101 *) 0
        , (*  70 16_46 2_01000110 *) 1 , (*  71 16_47 2_01000111 *) 0
        , (*  72 16_48 2_01001000 *) 3 , (*  73 16_49 2_01001001 *) 0
        , (*  74 16_4a 2_01001010 *) 1 , (*  75 16_4b 2_01001011 *) 0
        , (*  76 16_4c 2_01001100 *) 2 , (*  77 16_4d 2_01001101 *) 0
        , (*  78 16_4e 2_01001110 *) 1 , (*  79 16_4f 2_01001111 *) 0
        , (*  80 16_50 2_01010000 *) 4 , (*  81 16_51 2_01010001 *) 0
        , (*  82 16_52 2_01010010 *) 1 , (*  83 16_53 2_01010011 *) 0
        , (*  84 16_54 2_01010100 *) 2 , (*  85 16_55 2_01010101 *) 0
        , (*  86 16_56 2_01010110 *) 1 , (*  87 16_57 2_01010111 *) 0
        , (*  88 16_58 2_01011000 *) 3 , (*  89 16_59 2_01011001 *) 0
        , (*  90 16_5a 2_01011010 *) 1 , (*  91 16_5b 2_01011011 *) 0
        , (*  92 16_5c 2_01011100 *) 2 , (*  93 16_5d 2_01011101 *) 0
        , (*  94 16_5e 2_01011110 *) 1 , (*  95 16_5f 2_01011111 *) 0
        , (*  96 16_60 2_01100000 *) 5 , (*  97 16_61 2_01100001 *) 0
        , (*  98 16_62 2_01100010 *) 1 , (*  99 16_63 2_01100011 *) 0
        , (* 100 16_64 2_01100100 *) 2 , (* 101 16_65 2_01100101 *) 0
        , (* 102 16_66 2_01100110 *) 1 , (* 103 16_67 2_01100111 *) 0
        , (* 104 16_68 2_01101000 *) 3 , (* 105 16_69 2_01101001 *) 0
        , (* 106 16_6a 2_01101010 *) 1 , (* 107 16_6b 2_01101011 *) 0
        , (* 108 16_6c 2_01101100 *) 2 , (* 109 16_6d 2_01101101 *) 0
        , (* 110 16_6e 2_01101110 *) 1 , (* 111 16_6f 2_01101111 *) 0
        , (* 112 16_70 2_01110000 *) 4 , (* 113 16_71 2_01110001 *) 0
        , (* 114 16_72 2_01110010 *) 1 , (* 115 16_73 2_01110011 *) 0
        , (* 116 16_74 2_01110100 *) 2 , (* 117 16_75 2_01110101 *) 0
        , (* 118 16_76 2_01110110 *) 1 , (* 119 16_77 2_01110111 *) 0
        , (* 120 16_78 2_01111000 *) 3 , (* 121 16_79 2_01111001 *) 0
        , (* 122 16_7a 2_01111010 *) 1 , (* 123 16_7b 2_01111011 *) 0
        , (* 124 16_7c 2_01111100 *) 2 , (* 125 16_7d 2_01111101 *) 0
        , (* 126 16_7e 2_01111110 *) 1 , (* 127 16_7f 2_01111111 *) 0
        , (* 128 16_80 2_10000000 *) 7 , (* 129 16_81 2_10000001 *) 0
        , (* 130 16_82 2_10000010 *) 1 , (* 131 16_83 2_10000011 *) 0
        , (* 132 16_84 2_10000100 *) 2 , (* 133 16_85 2_10000101 *) 0
        , (* 134 16_86 2_10000110 *) 1 , (* 135 16_87 2_10000111 *) 0
        , (* 136 16_88 2_10001000 *) 3 , (* 137 16_89 2_10001001 *) 0
        , (* 138 16_8a 2_10001010 *) 1 , (* 139 16_8b 2_10001011 *) 0
        , (* 140 16_8c 2_10001100 *) 2 , (* 141 16_8d 2_10001101 *) 0
        , (* 142 16_8e 2_10001110 *) 1 , (* 143 16_8f 2_10001111 *) 0
        , (* 144 16_90 2_10010000 *) 4 , (* 145 16_91 2_10010001 *) 0
        , (* 146 16_92 2_10010010 *) 1 , (* 147 16_93 2_10010011 *) 0
        , (* 148 16_94 2_10010100 *) 2 , (* 149 16_95 2_10010101 *) 0
        , (* 150 16_96 2_10010110 *) 1 , (* 151 16_97 2_10010111 *) 0
        , (* 152 16_98 2_10011000 *) 3 , (* 153 16_99 2_10011001 *) 0
        , (* 154 16_9a 2_10011010 *) 1 , (* 155 16_9b 2_10011011 *) 0
        , (* 156 16_9c 2_10011100 *) 2 , (* 157 16_9d 2_10011101 *) 0
        , (* 158 16_9e 2_10011110 *) 1 , (* 159 16_9f 2_10011111 *) 0
        , (* 160 16_a0 2_10100000 *) 5 , (* 161 16_a1 2_10100001 *) 0
        , (* 162 16_a2 2_10100010 *) 1 , (* 163 16_a3 2_10100011 *) 0
        , (* 164 16_a4 2_10100100 *) 2 , (* 165 16_a5 2_10100101 *) 0
        , (* 166 16_a6 2_10100110 *) 1 , (* 167 16_a7 2_10100111 *) 0
        , (* 168 16_a8 2_10101000 *) 3 , (* 169 16_a9 2_10101001 *) 0
        , (* 170 16_aa 2_10101010 *) 1 , (* 171 16_ab 2_10101011 *) 0
        , (* 172 16_ac 2_10101100 *) 2 , (* 173 16_ad 2_10101101 *) 0
        , (* 174 16_ae 2_10101110 *) 1 , (* 175 16_af 2_10101111 *) 0
        , (* 176 16_b0 2_10110000 *) 4 , (* 177 16_b1 2_10110001 *) 0
        , (* 178 16_b2 2_10110010 *) 1 , (* 179 16_b3 2_10110011 *) 0
        , (* 180 16_b4 2_10110100 *) 2 , (* 181 16_b5 2_10110101 *) 0
        , (* 182 16_b6 2_10110110 *) 1 , (* 183 16_b7 2_10110111 *) 0
        , (* 184 16_b8 2_10111000 *) 3 , (* 185 16_b9 2_10111001 *) 0
        , (* 186 16_ba 2_10111010 *) 1 , (* 187 16_bb 2_10111011 *) 0
        , (* 188 16_bc 2_10111100 *) 2 , (* 189 16_bd 2_10111101 *) 0
        , (* 190 16_be 2_10111110 *) 1 , (* 191 16_bf 2_10111111 *) 0
        , (* 192 16_c0 2_11000000 *) 6 , (* 193 16_c1 2_11000001 *) 0
        , (* 194 16_c2 2_11000010 *) 1 , (* 195 16_c3 2_11000011 *) 0
        , (* 196 16_c4 2_11000100 *) 2 , (* 197 16_c5 2_11000101 *) 0
        , (* 198 16_c6 2_11000110 *) 1 , (* 199 16_c7 2_11000111 *) 0
        , (* 200 16_c8 2_11001000 *) 3 , (* 201 16_c9 2_11001001 *) 0
        , (* 202 16_ca 2_11001010 *) 1 , (* 203 16_cb 2_11001011 *) 0
        , (* 204 16_cc 2_11001100 *) 2 , (* 205 16_cd 2_11001101 *) 0
        , (* 206 16_ce 2_11001110 *) 1 , (* 207 16_cf 2_11001111 *) 0
        , (* 208 16_d0 2_11010000 *) 4 , (* 209 16_d1 2_11010001 *) 0
        , (* 210 16_d2 2_11010010 *) 1 , (* 211 16_d3 2_11010011 *) 0
        , (* 212 16_d4 2_11010100 *) 2 , (* 213 16_d5 2_11010101 *) 0
        , (* 214 16_d6 2_11010110 *) 1 , (* 215 16_d7 2_11010111 *) 0
        , (* 216 16_d8 2_11011000 *) 3 , (* 217 16_d9 2_11011001 *) 0
        , (* 218 16_da 2_11011010 *) 1 , (* 219 16_db 2_11011011 *) 0
        , (* 220 16_dc 2_11011100 *) 2 , (* 221 16_dd 2_11011101 *) 0
        , (* 222 16_de 2_11011110 *) 1 , (* 223 16_df 2_11011111 *) 0
        , (* 224 16_e0 2_11100000 *) 5 , (* 225 16_e1 2_11100001 *) 0
        , (* 226 16_e2 2_11100010 *) 1 , (* 227 16_e3 2_11100011 *) 0
        , (* 228 16_e4 2_11100100 *) 2 , (* 229 16_e5 2_11100101 *) 0
        , (* 230 16_e6 2_11100110 *) 1 , (* 231 16_e7 2_11100111 *) 0
        , (* 232 16_e8 2_11101000 *) 3 , (* 233 16_e9 2_11101001 *) 0
        , (* 234 16_ea 2_11101010 *) 1 , (* 235 16_eb 2_11101011 *) 0
        , (* 236 16_ec 2_11101100 *) 2 , (* 237 16_ed 2_11101101 *) 0
        , (* 238 16_ee 2_11101110 *) 1 , (* 239 16_ef 2_11101111 *) 0
        , (* 240 16_f0 2_11110000 *) 4 , (* 241 16_f1 2_11110001 *) 0
        , (* 242 16_f2 2_11110010 *) 1 , (* 243 16_f3 2_11110011 *) 0
        , (* 244 16_f4 2_11110100 *) 2 , (* 245 16_f5 2_11110101 *) 0
        , (* 246 16_f6 2_11110110 *) 1 , (* 247 16_f7 2_11110111 *) 0
        , (* 248 16_f8 2_11111000 *) 3 , (* 249 16_f9 2_11111001 *) 0
        , (* 250 16_fa 2_11111010 *) 1 , (* 251 16_fb 2_11111011 *) 0
        , (* 252 16_fc 2_11111100 *) 2 , (* 253 16_fd 2_11111101 *) 0
        , (* 254 16_fe 2_11111110 *) 1 , (* 255 16_ff 2_11111111 *) 0
        } 

; CONST Greatest1BitNoInByte
    = ARRAY [ 0 .. 16_FF ] OF [ 0 .. 8 ]
        { (*   0 16_00 2_00000000 *) 8 , (*   1 16_01 2_00000001 *) 0
        , (*   2 16_02 2_00000010 *) 1 , (*   3 16_03 2_00000011 *) 1
        , (*   4 16_04 2_00000100 *) 2 , (*   5 16_05 2_00000101 *) 2
        , (*   6 16_06 2_00000110 *) 2 , (*   7 16_07 2_00000111 *) 2
        , (*   8 16_08 2_00001000 *) 3 , (*   9 16_09 2_00001001 *) 3
        , (*  10 16_0a 2_00001010 *) 3 , (*  11 16_0b 2_00001011 *) 3
        , (*  12 16_0c 2_00001100 *) 3 , (*  13 16_0d 2_00001101 *) 3
        , (*  14 16_0e 2_00001110 *) 3 , (*  15 16_0f 2_00001111 *) 3
        , (*  16 16_10 2_00010000 *) 4 , (*  17 16_11 2_00010001 *) 4
        , (*  18 16_12 2_00010010 *) 4 , (*  19 16_13 2_00010011 *) 4
        , (*  20 16_14 2_00010100 *) 4 , (*  21 16_15 2_00010101 *) 4
        , (*  22 16_16 2_00010110 *) 4 , (*  23 16_17 2_00010111 *) 4
        , (*  24 16_18 2_00011000 *) 4 , (*  25 16_19 2_00011001 *) 4
        , (*  26 16_1a 2_00011010 *) 4 , (*  27 16_1b 2_00011011 *) 4
        , (*  28 16_1c 2_00011100 *) 4 , (*  29 16_1d 2_00011101 *) 4
        , (*  30 16_1e 2_00011110 *) 4 , (*  31 16_1f 2_00011111 *) 4
        , (*  32 16_20 2_00100000 *) 5 , (*  33 16_21 2_00100001 *) 5
        , (*  34 16_22 2_00100010 *) 5 , (*  35 16_23 2_00100011 *) 5
        , (*  36 16_24 2_00100100 *) 5 , (*  37 16_25 2_00100101 *) 5
        , (*  38 16_26 2_00100110 *) 5 , (*  39 16_27 2_00100111 *) 5
        , (*  40 16_28 2_00101000 *) 5 , (*  41 16_29 2_00101001 *) 5
        , (*  42 16_2a 2_00101010 *) 5 , (*  43 16_2b 2_00101011 *) 5
        , (*  44 16_2c 2_00101100 *) 5 , (*  45 16_2d 2_00101101 *) 5
        , (*  46 16_2e 2_00101110 *) 5 , (*  47 16_2f 2_00101111 *) 5
        , (*  48 16_30 2_00110000 *) 5 , (*  49 16_31 2_00110001 *) 5
        , (*  50 16_32 2_00110010 *) 5 , (*  51 16_33 2_00110011 *) 5
        , (*  52 16_34 2_00110100 *) 5 , (*  53 16_35 2_00110101 *) 5
        , (*  54 16_36 2_00110110 *) 5 , (*  55 16_37 2_00110111 *) 5
        , (*  56 16_38 2_00111000 *) 5 , (*  57 16_39 2_00111001 *) 5
        , (*  58 16_3a 2_00111010 *) 5 , (*  59 16_3b 2_00111011 *) 5
        , (*  60 16_3c 2_00111100 *) 5 , (*  61 16_3d 2_00111101 *) 5
        , (*  62 16_3e 2_00111110 *) 5 , (*  63 16_3f 2_00111111 *) 5
        , (*  64 16_40 2_01000000 *) 6 , (*  65 16_41 2_01000001 *) 6
        , (*  66 16_42 2_01000010 *) 6 , (*  67 16_43 2_01000011 *) 6
        , (*  68 16_44 2_01000100 *) 6 , (*  69 16_45 2_01000101 *) 6
        , (*  70 16_46 2_01000110 *) 6 , (*  71 16_47 2_01000111 *) 6
        , (*  72 16_48 2_01001000 *) 6 , (*  73 16_49 2_01001001 *) 6
        , (*  74 16_4a 2_01001010 *) 6 , (*  75 16_4b 2_01001011 *) 6
        , (*  76 16_4c 2_01001100 *) 6 , (*  77 16_4d 2_01001101 *) 6
        , (*  78 16_4e 2_01001110 *) 6 , (*  79 16_4f 2_01001111 *) 6
        , (*  80 16_50 2_01010000 *) 6 , (*  81 16_51 2_01010001 *) 6
        , (*  82 16_52 2_01010010 *) 6 , (*  83 16_53 2_01010011 *) 6
        , (*  84 16_54 2_01010100 *) 6 , (*  85 16_55 2_01010101 *) 6
        , (*  86 16_56 2_01010110 *) 6 , (*  87 16_57 2_01010111 *) 6
        , (*  88 16_58 2_01011000 *) 6 , (*  89 16_59 2_01011001 *) 6
        , (*  90 16_5a 2_01011010 *) 6 , (*  91 16_5b 2_01011011 *) 6
        , (*  92 16_5c 2_01011100 *) 6 , (*  93 16_5d 2_01011101 *) 6
        , (*  94 16_5e 2_01011110 *) 6 , (*  95 16_5f 2_01011111 *) 6
        , (*  96 16_60 2_01100000 *) 6 , (*  97 16_61 2_01100001 *) 6
        , (*  98 16_62 2_01100010 *) 6 , (*  99 16_63 2_01100011 *) 6
        , (* 100 16_64 2_01100100 *) 6 , (* 101 16_65 2_01100101 *) 6
        , (* 102 16_66 2_01100110 *) 6 , (* 103 16_67 2_01100111 *) 6
        , (* 104 16_68 2_01101000 *) 6 , (* 105 16_69 2_01101001 *) 6
        , (* 106 16_6a 2_01101010 *) 6 , (* 107 16_6b 2_01101011 *) 6
        , (* 108 16_6c 2_01101100 *) 6 , (* 109 16_6d 2_01101101 *) 6
        , (* 110 16_6e 2_01101110 *) 6 , (* 111 16_6f 2_01101111 *) 6
        , (* 112 16_70 2_01110000 *) 6 , (* 113 16_71 2_01110001 *) 6
        , (* 114 16_72 2_01110010 *) 6 , (* 115 16_73 2_01110011 *) 6
        , (* 116 16_74 2_01110100 *) 6 , (* 117 16_75 2_01110101 *) 6
        , (* 118 16_76 2_01110110 *) 6 , (* 119 16_77 2_01110111 *) 6
        , (* 120 16_78 2_01111000 *) 6 , (* 121 16_79 2_01111001 *) 6
        , (* 122 16_7a 2_01111010 *) 6 , (* 123 16_7b 2_01111011 *) 6
        , (* 124 16_7c 2_01111100 *) 6 , (* 125 16_7d 2_01111101 *) 6
        , (* 126 16_7e 2_01111110 *) 6 , (* 127 16_7f 2_01111111 *) 6
        , (* 128 16_80 2_10000000 *) 7 , (* 129 16_81 2_10000001 *) 7
        , (* 130 16_82 2_10000010 *) 7 , (* 131 16_83 2_10000011 *) 7
        , (* 132 16_84 2_10000100 *) 7 , (* 133 16_85 2_10000101 *) 7
        , (* 134 16_86 2_10000110 *) 7 , (* 135 16_87 2_10000111 *) 7
        , (* 136 16_88 2_10001000 *) 7 , (* 137 16_89 2_10001001 *) 7
        , (* 138 16_8a 2_10001010 *) 7 , (* 139 16_8b 2_10001011 *) 7
        , (* 140 16_8c 2_10001100 *) 7 , (* 141 16_8d 2_10001101 *) 7
        , (* 142 16_8e 2_10001110 *) 7 , (* 143 16_8f 2_10001111 *) 7
        , (* 144 16_90 2_10010000 *) 7 , (* 145 16_91 2_10010001 *) 7
        , (* 146 16_92 2_10010010 *) 7 , (* 147 16_93 2_10010011 *) 7
        , (* 148 16_94 2_10010100 *) 7 , (* 149 16_95 2_10010101 *) 7
        , (* 150 16_96 2_10010110 *) 7 , (* 151 16_97 2_10010111 *) 7
        , (* 152 16_98 2_10011000 *) 7 , (* 153 16_99 2_10011001 *) 7
        , (* 154 16_9a 2_10011010 *) 7 , (* 155 16_9b 2_10011011 *) 7
        , (* 156 16_9c 2_10011100 *) 7 , (* 157 16_9d 2_10011101 *) 7
        , (* 158 16_9e 2_10011110 *) 7 , (* 159 16_9f 2_10011111 *) 7
        , (* 160 16_a0 2_10100000 *) 7 , (* 161 16_a1 2_10100001 *) 7
        , (* 162 16_a2 2_10100010 *) 7 , (* 163 16_a3 2_10100011 *) 7
        , (* 164 16_a4 2_10100100 *) 7 , (* 165 16_a5 2_10100101 *) 7
        , (* 166 16_a6 2_10100110 *) 7 , (* 167 16_a7 2_10100111 *) 7
        , (* 168 16_a8 2_10101000 *) 7 , (* 169 16_a9 2_10101001 *) 7
        , (* 170 16_aa 2_10101010 *) 7 , (* 171 16_ab 2_10101011 *) 7
        , (* 172 16_ac 2_10101100 *) 7 , (* 173 16_ad 2_10101101 *) 7
        , (* 174 16_ae 2_10101110 *) 7 , (* 175 16_af 2_10101111 *) 7
        , (* 176 16_b0 2_10110000 *) 7 , (* 177 16_b1 2_10110001 *) 7
        , (* 178 16_b2 2_10110010 *) 7 , (* 179 16_b3 2_10110011 *) 7
        , (* 180 16_b4 2_10110100 *) 7 , (* 181 16_b5 2_10110101 *) 7
        , (* 182 16_b6 2_10110110 *) 7 , (* 183 16_b7 2_10110111 *) 7
        , (* 184 16_b8 2_10111000 *) 7 , (* 185 16_b9 2_10111001 *) 7
        , (* 186 16_ba 2_10111010 *) 7 , (* 187 16_bb 2_10111011 *) 7
        , (* 188 16_bc 2_10111100 *) 7 , (* 189 16_bd 2_10111101 *) 7
        , (* 190 16_be 2_10111110 *) 7 , (* 191 16_bf 2_10111111 *) 7
        , (* 192 16_c0 2_11000000 *) 7 , (* 193 16_c1 2_11000001 *) 7
        , (* 194 16_c2 2_11000010 *) 7 , (* 195 16_c3 2_11000011 *) 7
        , (* 196 16_c4 2_11000100 *) 7 , (* 197 16_c5 2_11000101 *) 7
        , (* 198 16_c6 2_11000110 *) 7 , (* 199 16_c7 2_11000111 *) 7
        , (* 200 16_c8 2_11001000 *) 7 , (* 201 16_c9 2_11001001 *) 7
        , (* 202 16_ca 2_11001010 *) 7 , (* 203 16_cb 2_11001011 *) 7
        , (* 204 16_cc 2_11001100 *) 7 , (* 205 16_cd 2_11001101 *) 7
        , (* 206 16_ce 2_11001110 *) 7 , (* 207 16_cf 2_11001111 *) 7
        , (* 208 16_d0 2_11010000 *) 7 , (* 209 16_d1 2_11010001 *) 7
        , (* 210 16_d2 2_11010010 *) 7 , (* 211 16_d3 2_11010011 *) 7
        , (* 212 16_d4 2_11010100 *) 7 , (* 213 16_d5 2_11010101 *) 7
        , (* 214 16_d6 2_11010110 *) 7 , (* 215 16_d7 2_11010111 *) 7
        , (* 216 16_d8 2_11011000 *) 7 , (* 217 16_d9 2_11011001 *) 7
        , (* 218 16_da 2_11011010 *) 7 , (* 219 16_db 2_11011011 *) 7
        , (* 220 16_dc 2_11011100 *) 7 , (* 221 16_dd 2_11011101 *) 7
        , (* 222 16_de 2_11011110 *) 7 , (* 223 16_df 2_11011111 *) 7
        , (* 224 16_e0 2_11100000 *) 7 , (* 225 16_e1 2_11100001 *) 7
        , (* 226 16_e2 2_11100010 *) 7 , (* 227 16_e3 2_11100011 *) 7
        , (* 228 16_e4 2_11100100 *) 7 , (* 229 16_e5 2_11100101 *) 7
        , (* 230 16_e6 2_11100110 *) 7 , (* 231 16_e7 2_11100111 *) 7
        , (* 232 16_e8 2_11101000 *) 7 , (* 233 16_e9 2_11101001 *) 7
        , (* 234 16_ea 2_11101010 *) 7 , (* 235 16_eb 2_11101011 *) 7
        , (* 236 16_ec 2_11101100 *) 7 , (* 237 16_ed 2_11101101 *) 7
        , (* 238 16_ee 2_11101110 *) 7 , (* 239 16_ef 2_11101111 *) 7
        , (* 240 16_f0 2_11110000 *) 7 , (* 241 16_f1 2_11110001 *) 7
        , (* 242 16_f2 2_11110010 *) 7 , (* 243 16_f3 2_11110011 *) 7
        , (* 244 16_f4 2_11110100 *) 7 , (* 245 16_f5 2_11110101 *) 7
        , (* 246 16_f6 2_11110110 *) 7 , (* 247 16_f7 2_11110111 *) 7
        , (* 248 16_f8 2_11111000 *) 7 , (* 249 16_f9 2_11111001 *) 7
        , (* 250 16_fa 2_11111010 *) 7 , (* 251 16_fb 2_11111011 *) 7
        , (* 252 16_fc 2_11111100 *) 7 , (* 253 16_fd 2_11111101 *) 7
        , (* 254 16_fe 2_11111110 *) 7 , (* 255 16_ff 2_11111111 *) 7
        } 

; CONST NoOf1BitsInByte
    = ARRAY [ 0 .. 16_FF ] OF [ 0 .. 8 ]
        { (*   0 16_00 2_00000000 *) 0 , (*   1 16_01 2_00000001 *) 1
        , (*   2 16_02 2_00000010 *) 1 , (*   3 16_03 2_00000011 *) 2
        , (*   4 16_04 2_00000100 *) 1 , (*   5 16_05 2_00000101 *) 2
        , (*   6 16_06 2_00000110 *) 2 , (*   7 16_07 2_00000111 *) 3
        , (*   8 16_08 2_00001000 *) 1 , (*   9 16_09 2_00001001 *) 2
        , (*  10 16_0a 2_00001010 *) 2 , (*  11 16_0b 2_00001011 *) 3
        , (*  12 16_0c 2_00001100 *) 2 , (*  13 16_0d 2_00001101 *) 3
        , (*  14 16_0e 2_00001110 *) 3 , (*  15 16_0f 2_00001111 *) 4
        , (*  16 16_10 2_00010000 *) 1 , (*  17 16_11 2_00010001 *) 2
        , (*  18 16_12 2_00010010 *) 2 , (*  19 16_13 2_00010011 *) 3
        , (*  20 16_14 2_00010100 *) 2 , (*  21 16_15 2_00010101 *) 3
        , (*  22 16_16 2_00010110 *) 3 , (*  23 16_17 2_00010111 *) 4
        , (*  24 16_18 2_00011000 *) 2 , (*  25 16_19 2_00011001 *) 3
        , (*  26 16_1a 2_00011010 *) 3 , (*  27 16_1b 2_00011011 *) 4
        , (*  28 16_1c 2_00011100 *) 3 , (*  29 16_1d 2_00011101 *) 4
        , (*  30 16_1e 2_00011110 *) 4 , (*  31 16_1f 2_00011111 *) 5
        , (*  32 16_20 2_00100000 *) 1 , (*  33 16_21 2_00100001 *) 2
        , (*  34 16_22 2_00100010 *) 2 , (*  35 16_23 2_00100011 *) 3
        , (*  36 16_24 2_00100100 *) 2 , (*  37 16_25 2_00100101 *) 3
        , (*  38 16_26 2_00100110 *) 3 , (*  39 16_27 2_00100111 *) 4
        , (*  40 16_28 2_00101000 *) 2 , (*  41 16_29 2_00101001 *) 3
        , (*  42 16_2a 2_00101010 *) 3 , (*  43 16_2b 2_00101011 *) 4
        , (*  44 16_2c 2_00101100 *) 3 , (*  45 16_2d 2_00101101 *) 4
        , (*  46 16_2e 2_00101110 *) 4 , (*  47 16_2f 2_00101111 *) 5
        , (*  48 16_30 2_00110000 *) 2 , (*  49 16_31 2_00110001 *) 3
        , (*  50 16_32 2_00110010 *) 3 , (*  51 16_33 2_00110011 *) 4
        , (*  52 16_34 2_00110100 *) 3 , (*  53 16_35 2_00110101 *) 4
        , (*  54 16_36 2_00110110 *) 4 , (*  55 16_37 2_00110111 *) 5
        , (*  56 16_38 2_00111000 *) 3 , (*  57 16_39 2_00111001 *) 4
        , (*  58 16_3a 2_00111010 *) 4 , (*  59 16_3b 2_00111011 *) 5
        , (*  60 16_3c 2_00111100 *) 4 , (*  61 16_3d 2_00111101 *) 5
        , (*  62 16_3e 2_00111110 *) 5 , (*  63 16_3f 2_00111111 *) 6
        , (*  64 16_40 2_01000000 *) 1 , (*  65 16_41 2_01000001 *) 2
        , (*  66 16_42 2_01000010 *) 2 , (*  67 16_43 2_01000011 *) 3
        , (*  68 16_44 2_01000100 *) 2 , (*  69 16_45 2_01000101 *) 3
        , (*  70 16_46 2_01000110 *) 3 , (*  71 16_47 2_01000111 *) 4
        , (*  72 16_48 2_01001000 *) 2 , (*  73 16_49 2_01001001 *) 3
        , (*  74 16_4a 2_01001010 *) 3 , (*  75 16_4b 2_01001011 *) 4
        , (*  76 16_4c 2_01001100 *) 3 , (*  77 16_4d 2_01001101 *) 4
        , (*  78 16_4e 2_01001110 *) 4 , (*  79 16_4f 2_01001111 *) 5
        , (*  80 16_50 2_01010000 *) 2 , (*  81 16_51 2_01010001 *) 3
        , (*  82 16_52 2_01010010 *) 3 , (*  83 16_53 2_01010011 *) 4
        , (*  84 16_54 2_01010100 *) 3 , (*  85 16_55 2_01010101 *) 4
        , (*  86 16_56 2_01010110 *) 4 , (*  87 16_57 2_01010111 *) 5
        , (*  88 16_58 2_01011000 *) 3 , (*  89 16_59 2_01011001 *) 4
        , (*  90 16_5a 2_01011010 *) 4 , (*  91 16_5b 2_01011011 *) 5
        , (*  92 16_5c 2_01011100 *) 4 , (*  93 16_5d 2_01011101 *) 5
        , (*  94 16_5e 2_01011110 *) 5 , (*  95 16_5f 2_01011111 *) 6
        , (*  96 16_60 2_01100000 *) 2 , (*  97 16_61 2_01100001 *) 3
        , (*  98 16_62 2_01100010 *) 3 , (*  99 16_63 2_01100011 *) 4
        , (* 100 16_64 2_01100100 *) 3 , (* 101 16_65 2_01100101 *) 4
        , (* 102 16_66 2_01100110 *) 4 , (* 103 16_67 2_01100111 *) 5
        , (* 104 16_68 2_01101000 *) 3 , (* 105 16_69 2_01101001 *) 4
        , (* 106 16_6a 2_01101010 *) 4 , (* 107 16_6b 2_01101011 *) 5
        , (* 108 16_6c 2_01101100 *) 4 , (* 109 16_6d 2_01101101 *) 5
        , (* 110 16_6e 2_01101110 *) 5 , (* 111 16_6f 2_01101111 *) 6
        , (* 112 16_70 2_01110000 *) 3 , (* 113 16_71 2_01110001 *) 4
        , (* 114 16_72 2_01110010 *) 4 , (* 115 16_73 2_01110011 *) 5
        , (* 116 16_74 2_01110100 *) 4 , (* 117 16_75 2_01110101 *) 5
        , (* 118 16_76 2_01110110 *) 5 , (* 119 16_77 2_01110111 *) 6
        , (* 120 16_78 2_01111000 *) 4 , (* 121 16_79 2_01111001 *) 5
        , (* 122 16_7a 2_01111010 *) 5 , (* 123 16_7b 2_01111011 *) 6
        , (* 124 16_7c 2_01111100 *) 5 , (* 125 16_7d 2_01111101 *) 6
        , (* 126 16_7e 2_01111110 *) 6 , (* 127 16_7f 2_01111111 *) 7
        , (* 128 16_80 2_10000000 *) 1 , (* 129 16_81 2_10000001 *) 2
        , (* 130 16_82 2_10000010 *) 2 , (* 131 16_83 2_10000011 *) 3
        , (* 132 16_84 2_10000100 *) 2 , (* 133 16_85 2_10000101 *) 3
        , (* 134 16_86 2_10000110 *) 3 , (* 135 16_87 2_10000111 *) 4
        , (* 136 16_88 2_10001000 *) 2 , (* 137 16_89 2_10001001 *) 3
        , (* 138 16_8a 2_10001010 *) 3 , (* 139 16_8b 2_10001011 *) 4
        , (* 140 16_8c 2_10001100 *) 3 , (* 141 16_8d 2_10001101 *) 4
        , (* 142 16_8e 2_10001110 *) 4 , (* 143 16_8f 2_10001111 *) 5
        , (* 144 16_90 2_10010000 *) 2 , (* 145 16_91 2_10010001 *) 3
        , (* 146 16_92 2_10010010 *) 3 , (* 147 16_93 2_10010011 *) 4
        , (* 148 16_94 2_10010100 *) 3 , (* 149 16_95 2_10010101 *) 4
        , (* 150 16_96 2_10010110 *) 4 , (* 151 16_97 2_10010111 *) 5
        , (* 152 16_98 2_10011000 *) 3 , (* 153 16_99 2_10011001 *) 4
        , (* 154 16_9a 2_10011010 *) 4 , (* 155 16_9b 2_10011011 *) 5
        , (* 156 16_9c 2_10011100 *) 4 , (* 157 16_9d 2_10011101 *) 5
        , (* 158 16_9e 2_10011110 *) 5 , (* 159 16_9f 2_10011111 *) 6
        , (* 160 16_a0 2_10100000 *) 2 , (* 161 16_a1 2_10100001 *) 3
        , (* 162 16_a2 2_10100010 *) 3 , (* 163 16_a3 2_10100011 *) 4
        , (* 164 16_a4 2_10100100 *) 3 , (* 165 16_a5 2_10100101 *) 4
        , (* 166 16_a6 2_10100110 *) 4 , (* 167 16_a7 2_10100111 *) 5
        , (* 168 16_a8 2_10101000 *) 3 , (* 169 16_a9 2_10101001 *) 4
        , (* 170 16_aa 2_10101010 *) 4 , (* 171 16_ab 2_10101011 *) 5
        , (* 172 16_ac 2_10101100 *) 4 , (* 173 16_ad 2_10101101 *) 5
        , (* 174 16_ae 2_10101110 *) 5 , (* 175 16_af 2_10101111 *) 6
        , (* 176 16_b0 2_10110000 *) 3 , (* 177 16_b1 2_10110001 *) 4
        , (* 178 16_b2 2_10110010 *) 4 , (* 179 16_b3 2_10110011 *) 5
        , (* 180 16_b4 2_10110100 *) 4 , (* 181 16_b5 2_10110101 *) 5
        , (* 182 16_b6 2_10110110 *) 5 , (* 183 16_b7 2_10110111 *) 6
        , (* 184 16_b8 2_10111000 *) 4 , (* 185 16_b9 2_10111001 *) 5
        , (* 186 16_ba 2_10111010 *) 5 , (* 187 16_bb 2_10111011 *) 6
        , (* 188 16_bc 2_10111100 *) 5 , (* 189 16_bd 2_10111101 *) 6
        , (* 190 16_be 2_10111110 *) 6 , (* 191 16_bf 2_10111111 *) 7
        , (* 192 16_c0 2_11000000 *) 2 , (* 193 16_c1 2_11000001 *) 3
        , (* 194 16_c2 2_11000010 *) 3 , (* 195 16_c3 2_11000011 *) 4
        , (* 196 16_c4 2_11000100 *) 3 , (* 197 16_c5 2_11000101 *) 4
        , (* 198 16_c6 2_11000110 *) 4 , (* 199 16_c7 2_11000111 *) 5
        , (* 200 16_c8 2_11001000 *) 3 , (* 201 16_c9 2_11001001 *) 4
        , (* 202 16_ca 2_11001010 *) 4 , (* 203 16_cb 2_11001011 *) 5
        , (* 204 16_cc 2_11001100 *) 4 , (* 205 16_cd 2_11001101 *) 5
        , (* 206 16_ce 2_11001110 *) 5 , (* 207 16_cf 2_11001111 *) 6
        , (* 208 16_d0 2_11010000 *) 3 , (* 209 16_d1 2_11010001 *) 4
        , (* 210 16_d2 2_11010010 *) 4 , (* 211 16_d3 2_11010011 *) 5
        , (* 212 16_d4 2_11010100 *) 4 , (* 213 16_d5 2_11010101 *) 5
        , (* 214 16_d6 2_11010110 *) 5 , (* 215 16_d7 2_11010111 *) 6
        , (* 216 16_d8 2_11011000 *) 4 , (* 217 16_d9 2_11011001 *) 5
        , (* 218 16_da 2_11011010 *) 5 , (* 219 16_db 2_11011011 *) 6
        , (* 220 16_dc 2_11011100 *) 5 , (* 221 16_dd 2_11011101 *) 6
        , (* 222 16_de 2_11011110 *) 6 , (* 223 16_df 2_11011111 *) 7
        , (* 224 16_e0 2_11100000 *) 3 , (* 225 16_e1 2_11100001 *) 4
        , (* 226 16_e2 2_11100010 *) 4 , (* 227 16_e3 2_11100011 *) 5
        , (* 228 16_e4 2_11100100 *) 4 , (* 229 16_e5 2_11100101 *) 5
        , (* 230 16_e6 2_11100110 *) 5 , (* 231 16_e7 2_11100111 *) 6
        , (* 232 16_e8 2_11101000 *) 4 , (* 233 16_e9 2_11101001 *) 5
        , (* 234 16_ea 2_11101010 *) 5 , (* 235 16_eb 2_11101011 *) 6
        , (* 236 16_ec 2_11101100 *) 5 , (* 237 16_ed 2_11101101 *) 6
        , (* 238 16_ee 2_11101110 *) 6 , (* 239 16_ef 2_11101111 *) 7
        , (* 240 16_f0 2_11110000 *) 4 , (* 241 16_f1 2_11110001 *) 5
        , (* 242 16_f2 2_11110010 *) 5 , (* 243 16_f3 2_11110011 *) 6
        , (* 244 16_f4 2_11110100 *) 5 , (* 245 16_f5 2_11110101 *) 6
        , (* 246 16_f6 2_11110110 *) 6 , (* 247 16_f7 2_11110111 *) 7
        , (* 248 16_f8 2_11111000 *) 5 , (* 249 16_f9 2_11111001 *) 6
        , (* 250 16_fa 2_11111010 *) 6 , (* 251 16_fb 2_11111011 *) 7
        , (* 252 16_fc 2_11111100 *) 6 , (* 253 16_fd 2_11111101 *) 7
        , (* 254 16_fe 2_11111110 *) 7 , (* 255 16_ff 2_11111111 *) 8
        } 

; END BitNoTable 
. 
