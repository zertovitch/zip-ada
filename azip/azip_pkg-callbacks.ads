with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Azip_Pkg.Callbacks is
   procedure On_New1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Open1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Save1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Save_As1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

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

   procedure On_About1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

end Azip_Pkg.Callbacks;
