#!/bin/sh
set -eux
mkdir -p ~/.var/app/org.mozilla.firefox/config/fontconfig
cat > ~/.var/app/org.mozilla.firefox/config/fontconfig/fonts.conf << EOF
<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
    <!-- Disable bitmap fonts. -->
    <selectfont><rejectfont><pattern>
        <patelt name="scalable"><bool>false</bool></patelt>
    </pattern></rejectfont></selectfont>
</fontconfig>
EOF
