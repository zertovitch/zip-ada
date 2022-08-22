--  Package with items common to test_za.hac and test_rz.hac.

with HAT;

package Test_Common is

  use HAT;

  function test_dir return VString;

  function test_files return VString;

end Test_Common;
