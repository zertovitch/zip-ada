with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Azip_Pkg.Callbacks is

   use Gtk.Arguments;

   -----------------------------
   -- On_New_Archive_Activate --
   -----------------------------

   procedure On_New_Archive_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_New_Archive_Activate;

   ------------------------------
   -- On_Open_Archive_Activate --
   ------------------------------

   procedure On_Open_Archive_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Open_Archive_Activate;

   ---------------------------------
   -- On_Save_Archive_As_Activate --
   ---------------------------------

   procedure On_Save_Archive_As_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Save_Archive_As_Activate;

   ------------------------
   -- On_Recent_Activate --
   ------------------------

   procedure On_Recent_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Recent_Activate;

   -----------------------
   -- On_Quit1_Activate --
   -----------------------

   procedure On_Quit1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Quit1_Activate;

   ----------------------
   -- On_Cut1_Activate --
   ----------------------

   procedure On_Cut1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Cut1_Activate;

   -----------------------
   -- On_Copy1_Activate --
   -----------------------

   procedure On_Copy1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Copy1_Activate;

   ------------------------
   -- On_Paste1_Activate --
   ------------------------

   procedure On_Paste1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Paste1_Activate;

   -------------------------
   -- On_Delete1_Activate --
   -------------------------

   procedure On_Delete1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Delete1_Activate;

   -----------------------
   -- On_Item1_Activate --
   -----------------------

   procedure On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Item1_Activate;

   -------------------------------
   -- On_Test_Archive1_Activate --
   -------------------------------

   procedure On_Test_Archive1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Test_Archive1_Activate;

   -----------------------
   -- On_About_Activate --
   -----------------------

   procedure On_About_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_About_Activate;

end Azip_Pkg.Callbacks;
