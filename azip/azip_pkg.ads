with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Image; use Gtk.Image;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;
package Azip_Pkg is

   type Azip_Record is new Gtk_Window_Record with record
      Vbox2 : Gtk_Vbox;
      Menubar1 : Gtk_Menu_Bar;
      Menuitem4 : Gtk_Menu_Item;
      Menuitem4_Menu : Gtk_Menu;
      New_Archive : Gtk_Image_Menu_Item;
      Image13 : Gtk_Image;
      Open_Archive : Gtk_Image_Menu_Item;
      Image14 : Gtk_Image;
      Save_Archive_As : Gtk_Image_Menu_Item;
      Image15 : Gtk_Image;
      Recent : Gtk_Menu_Item;
      Separatormenuitem1 : Gtk_Menu_Item;
      Quit : Gtk_Image_Menu_Item;
      Item1 : Gtk_Menu_Item;
      Item1_Menu : Gtk_Menu;
      Cut : Gtk_Image_Menu_Item;
      Copy : Gtk_Image_Menu_Item;
      Paste : Gtk_Image_Menu_Item;
      Delete : Gtk_Image_Menu_Item;
      Menuitem5 : Gtk_Menu_Item;
      Menuitem5_Menu : Gtk_Menu;
      Test_Archive : Gtk_Menu_Item;
      Recompress_Archive : Gtk_Menu_Item;
      Find_File_In_Archive : Gtk_Menu_Item;
      Find_Contents_In_Archive : Gtk_Menu_Item;
      Compare_Archive_With : Gtk_Menu_Item;
      Menuitem6 : Gtk_Menu_Item;
      Menuitem6_Menu : Gtk_Menu;
      Flat_View1 : Gtk_Menu_Item;
      Tree_View1 : Gtk_Menu_Item;
      Menuitem7 : Gtk_Menu_Item;
      Menuitem7_Menu : Gtk_Menu;
      About : Gtk_Menu_Item;
      Toolbar1 : Gtk_Toolbar;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Treeview1 : Gtk_Tree_View;
      Statusbar1 : Gtk_Statusbar;
   end record;
   type Azip_Access is access all Azip_Record'Class;

   procedure Gtk_New (Azip : out Azip_Access);
   procedure Initialize (Azip : access Azip_Record'Class);

end Azip_Pkg;
