with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
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
      Menu4 : Gtk_Menu;
      New1 : Gtk_Image_Menu_Item;
      Open1 : Gtk_Image_Menu_Item;
      Save1 : Gtk_Image_Menu_Item;
      Save_As1 : Gtk_Image_Menu_Item;
      Separatormenuitem1 : Gtk_Separator_Menu_Item;
      Quit1 : Gtk_Image_Menu_Item;
      Menuitem5 : Gtk_Menu_Item;
      Menu5 : Gtk_Menu;
      Cut1 : Gtk_Image_Menu_Item;
      Copy1 : Gtk_Image_Menu_Item;
      Paste1 : Gtk_Image_Menu_Item;
      Delete1 : Gtk_Image_Menu_Item;
      Menuitem6 : Gtk_Menu_Item;
      Menu6 : Gtk_Menu;
      Menuitem7 : Gtk_Menu_Item;
      Menu7 : Gtk_Menu;
      About1 : Gtk_Menu_Item;
      Toolbar1 : Gtk_Toolbar;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Treeview1 : Gtk_Tree_View;
      Statusbar1 : Gtk_Statusbar;
   end record;
   type Azip_Access is access all Azip_Record'Class;

   procedure Gtk_New (Azip : out Azip_Access);
   procedure Initialize (Azip : access Azip_Record'Class);

end Azip_Pkg;
