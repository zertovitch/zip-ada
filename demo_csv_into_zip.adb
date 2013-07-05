------------------------------------------------------------------------------
--  File:            Demo_csv_into_zip.adb
--  Description:     Demo / test / prototype derived from ZipTest.
--  Purpose:         Stuff many files directly into a zip file.
--                     Can be helpful when using network drives, for instance.
--  Date/version:    9-Jan-2013; 4-Feb-2009
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip_Streams;                       use Zip_Streams;
with Zip.Create;                        use Zip.Create;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;

procedure Demo_csv_into_zip is

  type Continent is
  ( Overseas,
    North_America,
    Latin_America,
    Europe,
    Other
  );

  type GroupCountries is
  ( France_Benelux,
    Northern_Europe,
    Central_and_Eastern_Europe,
    Southern_Europe,
    US,
    Canada_and_Greenland,
    Greater_China,
    South_East_Asia,
    Africa,
    Middle_East_and_North_Africa,
    Australasia,
    Indian_Peninsula,
    Japan_Korea,
    South_America,
    Central_America,
    Caribbean,
    Other
  );

  type Peril is
  (
    Drought,
    Earthquake,
    Flood,
    Frost,
    Hail,
    Windstorm
  );

  Peril_abbr: constant array(Peril) of String(1..2):=
   (
    Drought    => "DT",
    Earthquake => "EQ",
    Flood      => "FD",
    Frost      => "FT",
    Hail       => "HL",
    Windstorm  => "WS"
   );

  groupcountries_to_continent: constant array(Groupcountries) of Continent:=
  ( France_Benelux => Europe,
    Northern_Europe => Europe,
    Central_and_Eastern_Europe => Europe,
    Southern_Europe => Europe,
    US => North_America,
    Canada_and_Greenland => North_America,
    Greater_China => Overseas,
    South_East_Asia => Overseas,
    Africa => Overseas,
    Middle_East_and_North_Africa => Overseas,
    Australasia => Overseas,
    Indian_Peninsula => Overseas,
    Japan_Korea => Overseas,
    South_America => Latin_America,
    Central_America => Latin_America,
    Caribbean => Latin_America,
    Other => Other
  );

  procedure Output_results(
    g      : GroupCountries;
    p      : Peril;
    to_file: String
  ) is
    use Ada.Text_IO;
    f: File_Type;
    separator: constant Character:= ';';
  begin
    Create(f, Out_File, to_file);
    Put_Line(f, "Region: " & To_Lower(Groupcountries'Image(g)));
    Put_Line(f, "Continent: " & To_Lower(Continent'Image(groupcountries_to_continent(g))));
    Put_Line(f, "Peril type: " & To_Lower(Peril'Image(p)));
    New_Line(f);
    for i in 1..100 loop
      Put(f, Integer'Image(i) & ':' & separator);
      for j in 1..50 loop
        Put(f, Integer'Image(i*j) & separator);
      end loop;
      New_Line(f);
    end loop;
    Close(f);
  end Output_results;

  procedure Pack_results(
    g            : GroupCountries;
    p            : Peril;
    to_archive   : in out Zip_Create_info
  )
  is
    temp_name: constant String:= "temp.csv";
    final_name: constant String:=
      To_Lower(
        Peril'Image(p) & '/' &
        Continent'Image(groupcountries_to_continent(g)) & '/' &
        Groupcountries'Image(g) &
        '_' & Peril_abbr(p) & ".csv"
      );
  begin
    Output_results(g,p,temp_name);
    Zip.Create.Add_File(
      Info              => to_archive,
      Name              => temp_name,
      Name_in_archive   => final_name,
      Delete_file_after => True
    );
  end Pack_results;

  procedure Output_all_results is
    MyStream_file : aliased File_Zipstream; -- Zip archive as a file
    archive : Zip_Create_info;
  begin
    Create (archive,
      MyStream_file'Unchecked_Access,
      "detailed_results.zip"
    );
    for p in Peril loop
      for g in Groupcountries loop
        Pack_results(g,p,archive);
      end loop;
    end loop;
    Finish (archive);
  end Output_all_results;

begin
  Output_all_results;
end Demo_csv_into_zip;
