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
   Set_Default_Size (Azip, 400, -1);

   Gtk_New_Vbox (Azip.Vbox2, False, 0);

   Gtk_New (Azip.Menubar1);

   Gtk_New_With_Mnemonic (Azip.Menuitem4, -("_File"));

   Gtk_New (Azip.Menu4);

   Gtk_New_From_Stock (Azip.New1, "gtk-new");

   Append (Azip.Menu4, Azip.New1);
   Gtk_New_From_Stock (Azip.Open1, "gtk-open");

   Append (Azip.Menu4, Azip.Open1);
   Gtk_New_From_Stock (Azip.Save1, "gtk-save");

   Append (Azip.Menu4, Azip.Save1);
   Gtk_New_From_Stock (Azip.Save_As1, "gtk-save-as");

   Append (Azip.Menu4, Azip.Save_As1);
   Gtk_New (Azip.Separatormenuitem1);

   Append (Azip.Menu4, Azip.Separatormenuitem1);
   Gtk_New_From_Stock (Azip.Quit1, "gtk-quit");

   Append (Azip.Menu4, Azip.Quit1);
   Set_Submenu (Azip.Menuitem4, Azip.Menu4);
   Append (Azip.Menubar1, Azip.Menuitem4);
   Gtk_New_With_Mnemonic (Azip.Menuitem5, -("_Edit"));

   Gtk_New (Azip.Menu5);

   Gtk_New_From_Stock (Azip.Cut1, "gtk-cut");

   Append (Azip.Menu5, Azip.Cut1);
   Gtk_New_From_Stock (Azip.Copy1, "gtk-copy");

   Append (Azip.Menu5, Azip.Copy1);
   Gtk_New_From_Stock (Azip.Paste1, "gtk-paste");

   Append (Azip.Menu5, Azip.Paste1);
   Gtk_New_From_Stock (Azip.Delete1, "gtk-delete");

   Append (Azip.Menu5, Azip.Delete1);
   Set_Submenu (Azip.Menuitem5, Azip.Menu5);
   Append (Azip.Menubar1, Azip.Menuitem5);
   Gtk_New_With_Mnemonic (Azip.Menuitem6, -("_View"));

   Gtk_New (Azip.Menu6);

   Set_Submenu (Azip.Menuitem6, Azip.Menu6);
   Append (Azip.Menubar1, Azip.Menuitem6);
   Gtk_New_With_Mnemonic (Azip.Menuitem7, -("_Help"));

   Gtk_New (Azip.Menu7);

   Gtk_New_With_Mnemonic (Azip.About1, -("_About"));

   Append (Azip.Menu7, Azip.About1);
   Set_Submenu (Azip.Menuitem7, Azip.Menu7);
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
     (Azip.New1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_New1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Open1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Open1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Save1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Save1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Save_As1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Save_As1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Quit1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Quit1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Cut1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Cut1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Copy1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Copy1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Paste1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Paste1_Activate'Access), False);
   Image_Menu_Item_Callback.Connect
     (Azip.Delete1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Delete1_Activate'Access), False);
   Menu_Item_Callback.Connect
     (Azip.About1, "activate",
      Menu_Item_Callback.To_Marshaller (On_About1_Activate'Access), False);
end Initialize;

end Azip_Pkg;
