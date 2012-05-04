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

   procedure On_Quit_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Cut_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Copy_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Paste_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Delete_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Test_Archive_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Recompress_Archive_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Find_File_In_Archive_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Find_Contents_In_Archive_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Compare_Archive_With_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Flat_View1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Tree_View1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_About_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

end Azip_Pkg.Callbacks;
