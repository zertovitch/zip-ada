with Length_limited_Huffman_code_lengths;

with Ada.Text_IO;

procedure Test_LLHC is
  use Ada.Text_IO;
  package IIO is new Integer_IO (Integer);
  use IIO;
  --
  procedure Test_1 is
    subtype Alphabet is Character range 'a' .. 'k';
    type Alpha_Array is array (Alphabet) of Natural;
    freq, len : Alpha_Array;
  begin
    freq := (10, 30, 12, 5, 17, 20, 17, 0, 20, 0, 15);
    for m in 4 .. 5 loop
      declare
        procedure LLHCL is new
          Length_limited_Huffman_code_lengths (Alphabet, Natural, Alpha_Array, Alpha_Array, m);
      begin
        LLHCL (freq, len);
      end;
      New_Line;
      Put ("Maximum Huffman binary code length (constraint):");
      Put (m, 3);
      Put_Line (" bits");
      Put_Line ("------------------------------------");
      for c in len'Range loop
        Put ("    " & c & " frequency (input) =");
        Put (freq (c), 3);
        Put (" code length (output) =");
        Put (len (c), 3);
        Put_Line (" bits");
      end loop;
    end loop;
  end Test_1;
  --
  procedure Test_2 is
    subtype Alphabet is Natural range 0 .. 18;
    type Alpha_Array is array (Alphabet) of Natural;
    freq, len : Alpha_Array;
    procedure LLHCL is new
      Length_limited_Huffman_code_lengths (Alphabet, Natural, Alpha_Array, Alpha_Array, 7);
  begin
    --  freq:= (11, 1, 1, 1, 13, 15, 16, 23, 42, 72, 94, 33, 3, 4, 2, 3, 1, 1, 1);  --  OK with max=7
    freq := (6, 1, 1, 2, 10, 13, 19, 33, 41, 78, 89, 25, 7, 4, 2, 3, 1, 1, 1);  --  OK after fixing LLHC
    New_Line;
    Put_Line ("Deflate alphabet for storing compression structures");
    Put_Line ("---------------------------------------------------");
    LLHCL (freq, len);
    for a in Alphabet loop
      Put ("    ");
      Put (a, 3);
      Put (" frequency (input) =");
      Put (freq (a), 5);
      Put (" code length (output) =");
      Put (len (a), 3);
      Put_Line (" bits");
    end loop;
  end Test_2;
  --
  procedure Test_3 is
    subtype Alphabet is Natural range 0 .. 287;
    type Alpha_Array is array (Alphabet) of Natural;
    freq, len : Alpha_Array;
    procedure LLHCL is new
      Length_limited_Huffman_code_lengths (Alphabet, Natural, Alpha_Array, Alpha_Array, 15);
  begin
    freq :=
      (0 => 1277,        1 => 163,         2 => 118,         3 => 152,         4 => 123,
          5 => 98,          6 => 52,          7 => 29,          8 => 55,          9 => 27,
         10 => 50,         11 => 76,         12 => 49,         13 => 35,         14 => 31,
         15 => 23,         16 => 59,         17 => 22,         18 => 21,         19 => 14,
         20 => 36,         21 => 16,         22 => 20,         23 => 15,         24 => 36,
         25 => 17,         26 => 16,         27 => 19,         28 => 39,         29 => 24,
         30 => 23,         31 => 15,         32 => 232,        33 => 24,         34 => 30,
         35 => 12,         36 => 30,         37 => 41,         38 => 27,         39 => 25,
         40 => 46,         41 => 29,         42 => 14,         43 => 20,         44 => 33,
         45 => 46,         46 => 42,         47 => 13,         48 => 78,         49 => 78,
         50 => 66,         51 => 66,         52 => 69,         53 => 45,         54 => 50,
         55 => 46,         56 => 76,         57 => 53,         58 => 52,         59 => 8,
         60 => 20,         61 => 17,         62 => 11,         63 => 12,         64 => 182,
         65 => 170,        66 => 75,         67 => 152,        68 => 101,        69 => 196,
         70 => 113,        71 => 54,         72 => 57,         73 => 110,        74 => 29,
         75 => 32,         76 => 84,         77 => 60,         78 => 99,         79 => 100,
         80 => 110,        81 => 16,         82 => 108,        83 => 77,         84 => 125,
         85 => 68,         86 => 35,         87 => 22,         88 => 44,         89 => 39,
         90 => 23,         91 => 20,         92 => 39,         93 => 12,         94 => 11,
         95 => 123,        96 => 45,         97 => 133,        98 => 38,         99 => 71,
        100 => 107,       101 => 193,       102 => 40,        103 => 45,        104 => 71,
        105 => 115,       106 => 16,        107 => 21,        108 => 107,       109 => 60,
        110 => 104,       111 => 113,       112 => 93,        113 => 19,        114 => 124,
        115 => 116,       116 => 120,       117 => 81,        118 => 25,        119 => 37,
        120 => 48,        121 => 37,        122 => 28,        123 => 18,        124 => 35,
        125 => 14,        126 => 19,        127 => 12,        128 => 32,        129 => 48,
        130 => 12,        131 => 44,        132 => 20,        133 => 12,        134 => 27,
        135 => 30,        136 => 28,        137 => 8,         138 => 8,         139 => 10,
        140 => 35,        141 => 19,        142 => 16,        143 => 10,        144 => 40,
        145 => 15,        146 => 23,        147 => 11,        148 => 32,        149 => 14,
        150 => 13,        151 => 13,        152 => 24,        153 => 13,        154 => 11,
        155 => 9,         156 => 29,        157 => 12,        158 => 12,        159 => 16,
        160 => 45,        161 => 10,        162 => 24,        163 => 13,        164 => 34,
        165 => 12,        166 => 16,        167 => 7,         168 => 30,        169 => 11,
        170 => 15,        171 => 13,        172 => 29,        173 => 6,         174 => 8,
        175 => 10,        176 => 40,        177 => 17,        178 => 12,        179 => 14,
        180 => 30,        181 => 12,        182 => 12,        183 => 15,        184 => 21,
        185 => 11,        186 => 20,        187 => 23,        188 => 32,        189 => 16,
        190 => 10,        191 => 11,        192 => 48,        193 => 26,        194 => 16,
        195 => 21,        196 => 33,        197 => 16,        198 => 14,        199 => 8,
        200 => 29,        201 => 12,        202 => 17,        203 => 11,        204 => 20,
        205 => 10,        206 => 14,        207 => 6,         208 => 46,        209 => 9,
        210 => 8,         211 => 7,         212 => 28,        213 => 15,        214 => 12,
        215 => 12,        216 => 26,        217 => 9,         218 => 15,        219 => 8,
        220 => 26,        221 => 8,         222 => 12,        223 => 18,        224 => 50,
        225 => 5,         226 => 11,        227 => 15,        228 => 24,        229 => 9,
        230 => 10,        231 => 6,         232 => 29,        233 => 6,         234 => 11,
        235 => 14,        236 => 28,        237 => 8,         238 => 7,         239 => 8,
        240 => 40,        241 => 18,        242 => 6,         243 => 8,         244 => 30,
        245 => 9,         246 => 16,        247 => 29,        248 => 28,        249 => 126,
        250 => 57,        251 => 7,         252 => 35,        253 => 36,        254 => 118,
        255 => 292,       256 => 1,         257 => 0,         258 => 1425,      259 => 512,
        260 => 302,       261 => 342,       262 => 275,       263 => 179,       264 => 147,
        265 => 322,       266 => 289,       267 => 174,       268 => 137,       269 => 182,
        270 => 159,       271 => 112,       272 => 60,        273 => 45,        274 => 22,
        275 => 24,        276 => 10,        277 => 7,         278 => 4,         279 => 1,
        280 => 5,         281 => 0,         282 => 3,         283 => 3,         284 => 4,
        285 => 6,         286 => 0,         287 => 0
    );
    New_Line;
    Put_Line ("Deflate alphabet for literal & LZ length");
    Put_Line ("----------------------------------------");
    LLHCL (freq, len);
    for a in Alphabet loop
      Put ("    ");
      Put (a, 3);
      Put (" frequency (input) =");
      Put (freq (a), 5);
      Put (" code length (output) =");
      Put (len (a), 3);
      Put_Line (" bits");
    end loop;
  end Test_3;
begin
  Test_1;
  Test_2;
  Test_3;
end Test_LLHC;
