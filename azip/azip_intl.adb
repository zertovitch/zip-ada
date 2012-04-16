with Gtkada.Intl; use Gtkada.Intl;

package body azip_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("azip", Msg);
   end "-";

end azip_Intl;
