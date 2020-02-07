#!/bin/bash

echo ""
echo " ________  ___   ______       ______      ___"
echo "/___..._/  I.I   I.___.\     /. __ .\   __I.I   ____"
echo "   /../    I.I   I.____/     I.I__I.I  /....I  __\..\ "
echo " _/../___  I.I   I.I    ===  I..__..I I. = .I I = ..I"
echo "/_______/  I_I  /__I        /__I  I_I  \__\_I  \__\_I"
echo ""
echo "/=====================================================\ "
echo "   Build in progress..."
echo "\=====================================================/"
echo ""

if [[ ( $1 == "") ]]; then
gprbuild -p -Pzipada.gpr
else
gprbuild -p -Pzipada.gpr -XZip_Build_Mode=$1
fi

