with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Azip_Pkg; use Azip_Pkg;

procedure Azip is
   Azip : Azip_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Azip);
   Show_All (Azip);
   Gtk.Main.Main;
end Azip;
