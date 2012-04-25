with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Azip_Pkg.Callbacks is
   procedure On_New_Archive_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Open_Archive_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Save_Archive_As_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Recent_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Quit1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Cut1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Copy1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Paste1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Delete1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Test_Archive1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_About_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

end Azip_Pkg.Callbacks;
