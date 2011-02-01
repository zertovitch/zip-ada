
package body Dual_IO is

   Log_open : Boolean:= False;

   Log_text : Text_IO.File_type;

   procedure Check_Log is
   begin
     if not Log_open then raise Log_not_open; end if;
   end Check_Log;

   procedure Create_Log (Name : in String) is
   begin
     if Log_open then raise Log_already_open; end if;
     Text_IO.Create( File => Log_text,
                     Mode => Text_IO.Out_File,
                     Name => Name );
     Log_open:= True;
   end Create_Log;

   procedure Append_Log (Name : in String) is
   begin
     if Log_open then raise Log_already_open; end if;
     Text_IO.Open( File => Log_text,
                   Mode => Text_IO.Append_File,
                   Name => Name );
     Log_open:= True;
   end Append_Log;

   procedure Close_Log is
   begin
     Check_Log;
     Text_IO.Close( Log_text );
     Log_open:= False;
   end Close_Log;

   procedure Flush is
   begin
     Text_IO.Flush;
     Check_Log;
     Text_IO.Flush( Log_text );
   end Flush;

   procedure New_Line (Spacing : in Positive_Count := 1) is
   begin
     Text_IO.New_Line( Spacing );
     Check_Log;
     Text_IO.New_Line( Log_text, Spacing );
   end New_Line;

   procedure Skip_Line (Spacing : in Positive_Count := 1) is
   begin
     Text_IO.Skip_Line( Spacing );           -- *in*  Standard
     Check_Log;
     Text_IO.New_Line( Log_text, Spacing );  -- *out* Log
   end Skip_Line;

   procedure New_Page is
   begin
     Text_IO.New_Page;
     Check_Log;
     Text_IO.New_Page( Log_text );
   end New_Page;

   procedure Skip_Page is
   begin
     Text_IO.Skip_Page;             -- *in*  Standard
     Check_Log;
     Text_IO.New_Page( Log_text );  -- *out* Log
   end Skip_Page;

   -----------------------------
   -- Characters Input-Output --
   -----------------------------

   procedure Get (Item : out Character) is
     C : Character;
   begin
     Text_IO.Get( C );            -- *in*  Standard
     Check_Log;
     Text_IO.Put( Log_text, C );  -- *out* Log
     Item:= C;
   end Get;

   procedure Put (Item : in Character) is
   begin
     Text_IO.Put( Item );
     Check_Log;
     Text_IO.Put( Log_text, Item );
   end Put;

   --------------------------
   -- Strings Input-Output --
   --------------------------

   procedure Get (Item : out String) is
     S : String( Item'Range );
   begin
     Text_IO.Get( S );            -- *in*  Standard
     Check_Log;
     Text_IO.Put( Log_text, S );  -- *out* Log
     Item:= S;
   end Get;

   procedure Put (Item : in String) is
   begin
     Text_IO.Put( Item );
     Check_Log;
     Text_IO.Put( Log_text, Item );
   end Put;

   procedure Get_Line
     (Item : out String;
      Last : out Natural) is
     S : String( Item'Range );
     L : Natural;
   begin
     Text_IO.Get_Line( S, L );               -- *in*  Standard
     Check_Log;
     Text_IO.Put_Line( Log_text, S(1..L) );  -- *out* Log
     Item(Item'First..Item'First+L-1):= S(1..L);
     Last:= L;
   end Get_Line;

   procedure Put_Line
     (Item : in String) is
   begin
     Text_IO.Put_Line( Item );
     Check_Log;
     Text_IO.Put_Line( Log_text, Item );
   end Put_Line;

   package body Integer_IO is

      package TIIO is new Text_IO.Integer_IO( Num );

      procedure Get(Item  : out Num;
                    Width : in  Field := 0) is
        I: Num;
      begin
        TIIO.Get( I, Width );            -- *in*  Standard
        Check_Log;
        TIIO.Put( Log_text, I, Width );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base) is
      begin
        TIIO.Put( Item, Width, Base );
        Check_Log;
        TIIO.Put( Log_text, Item, Width, Base );
      end Put;

   end Integer_IO;

   package body Float_IO is

      package TFIO is new Text_IO.Float_IO( Num );

      procedure Get(Item  : out Num;
                    Width : in  Field := 0) is
        I: Num;
      begin
        TFIO.Get( I, Width );     -- *in*  Standard
        Check_Log;
        TFIO.Put( Log_text, I );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      begin
        TFIO.Put( Item, Fore, Aft, Exp );
        Check_Log;
        TFIO.Put( Log_text, Item, Fore, Aft, Exp );
      end Put;

   end Float_IO;

   package body Fixed_IO is

      package TXIO is new Text_IO.Fixed_IO( Num );

      procedure Get(Item  : out Num;
                    Width : in  Field := 0) is
        I: Num;
      begin
        TXIO.Get( I, Width );     -- *in*  Standard
        Check_Log;
        TXIO.Put( Log_text, I );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      begin
        TXIO.Put( Item, Fore, Aft, Exp );
        Check_Log;
        TXIO.Put( Log_text, Item, Fore, Aft, Exp );
      end Put;

   end Fixed_IO;

   package body Decimal_IO is

      package TDIO is new Text_IO.Decimal_IO( Num );

      procedure Get
        (Item  : out Num;
         Width : in Field := 0) is
        I: Num;
      begin
        TDIO.Get( I, Width );     -- *in*  Standard
        Check_Log;
        TDIO.Put( Log_text, I );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put
        (Item : in Num;
         Fore : in Field := Default_Fore;
         Aft  : in Field := Default_Aft;
         Exp  : in Field := Default_Exp) is
      begin
        TDIO.Put( Item, Fore, Aft, Exp );
        Check_Log;
        TDIO.Put( Log_text, Item, Fore, Aft, Exp );
      end Put;

   end Decimal_IO;

   package body Modular_IO is

      package TMIO is new Text_IO.Modular_IO( Num );

      procedure Get
        (Item  : out Num;
         Width : in Field := 0) is
        I: Num;
      begin
        TMIO.Get( I, Width );            -- *in*  Standard
        Check_Log;
        TMIO.Put( Log_text, I, Width );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put
        (Item  : in Num;
         Width : in Field := Default_Width;
         Base  : in Number_Base := Default_Base) is
      begin
        TMIO.Put( Item, Width, Base );
        Check_Log;
        TMIO.Put( Log_text, Item, Width, Base );
      end Put;

   end Modular_IO;

   package body Enumeration_IO is

      package TEIO is new Text_IO.Enumeration_IO( Enum );

      procedure Get(Item : out Enum) is
        I: Enum;
      begin
        TEIO.Get( I );            -- *in*  Standard
        Check_Log;
        TEIO.Put( Log_text, I );  -- *out* Log
        Item:= I;
      end Get;

      procedure Put(Item  : in Enum;
                    Width : in Field    := Default_Width;
                    Set   : in Type_Set := Default_Setting) is
      begin
        TEIO.Put( Item, Width, Set );
        Check_Log;
        TEIO.Put( Log_text, Item, Width, Set );
      end Put;

   end Enumeration_IO;

end Dual_IO;
