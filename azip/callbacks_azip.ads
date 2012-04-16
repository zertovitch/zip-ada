with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package Callbacks_azip is

   package Image_Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Image_Menu_Item_Record);

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

end Callbacks_azip;
