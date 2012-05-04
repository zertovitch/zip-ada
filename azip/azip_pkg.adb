with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_azip; use Callbacks_azip;
with azip_Intl; use azip_Intl;
with Azip_Pkg.Callbacks; use Azip_Pkg.Callbacks;

package body Azip_Pkg is

procedure Gtk_New (Azip : out Azip_Access) is
begin
   Azip := new Azip_Record;
   Azip_Pkg.Initialize (Azip);
end Gtk_New;

procedure Initialize (Azip : access Azip_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Azip, Window_Toplevel);
   Set_Title (Azip, -"AZip");
   Set_Position (Azip, Win_Pos_None);
   Set_Modal (Azip, False);
   Set_Resizable (Azip, True);
   Set_Default_Size (Azip, 400, 500);

   Gtk_New_Vbox (Azip.Vbox2, False, 0);

   Gtk_New (Azip.Menubar1);

   Gtk_New_With_Mnemonic (Azip.Menuitem4, -("_File"));

   Gtk_New (Azip.Menuitem4_Menu);

   Gtk_New (Azip.New_Archive, -"New archive...");
   Gtk_New (Azip.Image13 , "gtk-new", Gtk_Icon_Size'Val (1));
   Set_Alignment (Azip.Image13, 0.5, 0.5);
   Set_Padding (Azip.Image13, 0, 0);
   Set_Image (Azip.New_Archive, Azip.Image13);


   Append (Azip.Menuitem4_Menu, Azip.New_Archive);
   Gtk_New (Azip.Open_Archive, -"Open archive...");
   Gtk_New (Azip.Image14 , "gtk-open", Gtk_Icon_Size'Val (1));
   Set_Alignment (Azip.Image14, 0.5, 0.5);
   Set_Padding (Azip.Image14, 0, 0);
   Set_Image (Azip.Open_Archive, Azip.Image14);


   Append (Azip.Menuitem4_Menu, Azip.Open_Archive);
   Gtk_New (Azip.Save_Archive_As, -"Save archive as...");
   Gtk_New (Azip.Image15 , "gtk-save-as", Gtk_Icon_Size'Val (1));
   Set_Alignment (Azip.Image15, 0.5, 0.5);
   Set_Padding (Azip.Image15, 0, 0);
   Set_Image (Azip.Save_Archive_As, Azip.Image15);


   Append (Azip.Menuitem4_Menu, Azip.Save_Archive_As);
   Gtk_New_With_Mnemonic (Azip.Recent, -("_Recent"));

   Append (Azip.Menuitem4_Menu, Azip.Recent);
   Gtk_New (Azip.Separatormenuitem1);

   Append (Azip.Menuitem4_Menu, Azip.Separatormenuitem1);
   Gtk_New_From_Stock (Azip.Quit, "gtk-quit");

   Append (Azip.Menuitem4_Menu, Azip.Quit);
   Set_Submenu (Azip.Menuitem4, Azip.Menuitem4_Menu);
   Append (Azip.Menubar1, Azip.Menuitem4);
   Gtk_New_With_Mnemonic (Azip.Item1, -("_Edit"));

   Gtk_New (Azip.Item1_Menu);

   Gtk_New_From_Stock (Azip.Cut, "gtk-cut");

   Append (Azip.Item1_Menu, Azip.Cut);
   Gtk_New_From_Stock (Azip.Copy, "gtk-copy");

   Append (Azip.Item1_Menu, Azip.Copy);
   Gtk_New_From_Stock (Azip.Paste, "gtk-paste");

   Append (Azip.Item1_Menu, Azip.Paste);
   Gtk_New_From_Stock (Azip.Delete, "gtk-delete");

   Append (Azip.Item1_Menu, Azip.Delete);
   Set_Submenu (Azip.Item1, Azip.Item1_Menu);
   Append (Azip.Menubar1, Azip.Item1);
   Gtk_New_With_Mnemonic (Azip.Menuitem5, -("_Tools"));

   Gtk_New (Azip.Menuitem5_Menu);

   Gtk_New_With_Mnemonic (Azip.Test_Archive, -("_Test archive"));

   Append (Azip.Menuitem5_Menu, Azip.Test_Archive);
   Gtk_New_With_Mnemonic (Azip.Recompress_Archive, -("_Recompress archive"));

   Append (Azip.Menuitem5_Menu, Azip.Recompress_Archive);
   Gtk_New_With_Mnemonic (Azip.Find_File_In_Archive, -("Find _file in archive"));

   Append (Azip.Menuitem5_Menu, Azip.Find_File_In_Archive);
   Gtk_New_With_Mnemonic (Azip.Find_Contents_In_Archive, -("Find _contents in archive"));

   Append (Azip.Menuitem5_Menu, Azip.Find_Contents_In_Archive);
   Gtk_New_With_Mnemonic (Azip.Compare_Archive_With, -("_Compare archive with..."));

   Append (Azip.Menuitem5_Menu, Azip.Compare_Archive_With);
   Set_Submenu (Azip.Menuitem5, Azip.Menuitem5_Menu);
   Append (Azip.Menubar1, Azip.Menuitem5);
   Gtk_New_With_Mnemonic (Azip.Menuitem6, -("_View"));

   Gtk_New (Azip.Menuitem6_Menu);

   Gtk_New_With_Mnemonic (Azip.Flat_View1, -("_Flat view"));

   Append (Azip.Menuitem6_Menu, Azip.Flat_View1);
   Gtk_New_With_Mnemonic (Azip.Tree_View1, -("_Tree view"));

   Append (Azip.Menuitem6_Menu, Azip.Tree_View1);
   Set_Submenu (Azip.Menuitem6, Azip.Menuitem6_Menu);
   Append (Azip.Menubar1, Azip.Menuitem6);
   Gtk_New_With_Mnemonic (Azip.Menuitem7, -("_Help"));

   Gtk_New (Azip.Menuitem7_Menu);

   Gtk_New_With_Mnemonic (Azip.About, -("_About"));

   Append (Azip.Menuitem7_Menu, Azip.About);
   Set_Submenu (Azip.Menuitem7, Azip.Menuitem7_Menu);
   Append (Azip.Menubar1, Azip.Menuitem7);
   Pack_Start
     (Azip.Vbox2,
      Azip.Menubar1,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Azip.Toolbar1, Orientation_Horizontal, Toolbar_Both);
   Set_Tooltips (Azip.Toolbar1, True);

   Pack_Start
     (Azip.Vbox2,
      Azip.Toolbar1,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Azip.Scrolledwindow1);
   Set_Policy (Azip.Scrolledwindow1, Policy_Always, Policy_Always);
   Set_Shadow_Type (Azip.Scrolledwindow1, Shadow_None);
   Set_Placement (Azip.Scrolledwindow1, Corner_Top_Left);

   Gtk_New (Azip.Treeview1);
   Set_Headers_Visible (Azip.Treeview1, True);
   Set_Rules_Hint (Azip.Treeview1, False);
   Set_Reorderable (Azip.Treeview1, False);
   Set_Enable_Search (Azip.Treeview1, True);

   Set_Size_Request (Azip.Treeview1, 555, 222);
   Add (Azip.Scrolledwindow1, Azip.Treeview1);
   Pack_Start
     (Azip.Vbox2,
      Azip.Scrolledwindow1,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Gtk_New (Azip.Statusbar1);

   Pack_Start
     (Azip.Vbox2,
      Azip.Statusbar1,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Azip, Azip.Vbox2);

   --  Connect signals

   Image_Menu_Item_Callback.Connect
     (Azip.New_Archive, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_New_Archive_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Open_Archive, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Open_Archive_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Save_Archive_As, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Save_Archive_As_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Recent, "activate",
      Menu_Item_Callback.To_Marshaller (On_Recent_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Quit, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Quit_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Cut, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Cut_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Copy, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Copy_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Paste, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Paste_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Delete, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Delete_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Item1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Item1_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Test_Archive, "activate",
      Menu_Item_Callback.To_Marshaller (On_Test_Archive_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Recompress_Archive, "activate",
      Menu_Item_Callback.To_Marshaller (On_Recompress_Archive_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Find_File_In_Archive, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_File_In_Archive_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Find_Contents_In_Archive, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_Contents_In_Archive_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Compare_Archive_With, "activate",
      Menu_Item_Callback.To_Marshaller (On_Compare_Archive_With_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Flat_View1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Flat_View1_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.Tree_View1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Tree_View1_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.About, "activate",
      Menu_Item_Callback.To_Marshaller (On_About_Activate'Access), False);
end Initialize;

end Azip_Pkg;
