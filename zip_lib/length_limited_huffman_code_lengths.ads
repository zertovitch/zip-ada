--  Copyright 2011 Google Inc. All Rights Reserved.

--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at

--  http://www.apache.org/licenses/LICENSE-2.0

--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

--  Author: lode.vandevenne [*] gmail [*] com (Lode Vandevenne)
--  Author: jyrki.alakuijala [*] gmail [*] com (Jyrki Alakuijala)

--  Bounded package merge algorithm, based on the paper
--  "A Fast and Space-Economical Algorithm for Length-Limited Coding
--  Jyrki Katajainen, Alistair Moffat, Andrew Turpin".

--  Translated by G. de Montmollin to Ada from katajainen.c (Zopfli project), 7-Feb-2016
--  Translation notes in procedure's body.

generic
  type Alphabet is (<>);  --  Any discrete type
  type Count_Type is range <>;
  --  ^ An integer type large enough for counting and indexing (see algo for bounds)
  type Count_Array is array(Alphabet) of Count_Type;
  type Length_Array is array(Alphabet) of Natural;
  max_bits: Positive;  --  Length limit in Huffman codes

procedure Length_limited_Huffman_code_lengths(
  frequencies : in  Count_Array;
  bit_lengths : out Length_Array
);
