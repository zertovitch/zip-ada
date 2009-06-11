--  ___  ____  ____  ____  ________  ___   ______       ______     ___
--  |.|  |../  |...\ |../ /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--  |.|  |.|   |.|\.\|.|     /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  |.|__|.|   |.| \...|   _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
--  |______|  /__|  \__|  /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip.Streams
----------------
-- UnZips as a stream a file which is compressed in a file archive

-- Legal licensing note:

--  Copyright (c) 1999..2007 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip, Zip_Streams;

with Ada.Streams.Stream_IO, Ada.IO_Exceptions;

package UnZip.Streams is

   subtype Stream_Access is Ada.Streams.Stream_IO.Stream_Access;

   type Zipped_File_Type is private;

   -- Opens an input stream for the compressed file named Name stored
   -- in the archive file named Archive_Name. The function Stream(..)
   -- then gives access to the opened stream.

   -- Version: Zip as a file
   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Name   : in String;               -- Name of archive file
      Name           : in String;               -- Name of zipped entry
      Password       : in String := "";         -- Decryption password
      Case_sensitive : in Boolean:= False
     );

   -- Version: Zip as a stream
   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Stream : in Zip_Streams.Zipstream_Class; -- Archive's stream
      Name           : in String;               -- Name of zipped entry
      Password       : in String := "";         -- Decryption password
      Case_sensitive : in Boolean:= False
     );

   -- Same as above, but uses a the pre-loaded contents of the archive's
   -- Central Directory; hence Archive_Info is passed instead of
   -- Archive_Name or Archive_Stream.
   -- You need to call Zip.Load( Archive_Info... ) prior to opening the
   -- compressed file.

   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Info   : in Zip.Zip_info;         -- Archive's Zip_info
      Name           : in String;               -- Name of zipped entry
      Password       : in String := "";         -- Decryption password
      Case_sensitive : in Boolean:= False
     );

   procedure Close (File : in out Zipped_File_Type);

   function Is_Open     (File : in Zipped_File_Type) return Boolean;
   function End_Of_File (File : in Zipped_File_Type) return Boolean;

   function Stream (File : Zipped_File_Type) return Stream_Access;

   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;

private

   type UZS_state is (
      uninitialized,
      data_uncompressed, -- In that model, all data is unzipped in one
                         --   time, into memory. If you have a smarter
                         --   idea (small buffer with tasking, write me!)
      end_of_zip         -- We have reached the end, not yet closed
     );

   type p_String is access String;

   type Unzip_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      state        : UZS_state:= uninitialized;
      archive_info : Zip.Zip_info; -- archive info (.zip file, directory)
      delete_info_on_closing : Boolean;
      file_name    : p_String; -- name of zipped file to unzip from archive
      uncompressed : p_Stream_Element_Array; -- whole uncompressed data
      index        : Ada.Streams.Stream_Element_Offset;
   end record;


   procedure Read
     (Stream : in out Unzip_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Unzip_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);

   type Zipped_File_Type is access Unzip_Stream_Type;

end UnZip.Streams;
