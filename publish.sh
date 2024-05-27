 #!/bin/bash

set -u                                                                                                                                       
set -e 

CHROME_DIST_ZIP=../s2f-chrome.zip
FIREFOX_DIST_ZIP=../s2f-firefox.zip
FIREFOX_SOURCE_ZIP=./s2f-source.zip

elm make src/Main.elm --optimize --output=dist-chrome/elm.js

# Chrome
pushd dist-chrome
cp ../background.js .
cp ../content.js .
cp ../elm-import.js .
cp ../manifest.json .
cp ../popup.html .
cp ../s2f-logo.png .
cp ../s2f-logo-48x48.png .
cp ../s2f-logo-128x128.png .
 
[ -f "$CHROME_DIST_ZIP" ] && rm $CHROME_DIST_ZIP
zip -r $CHROME_DIST_ZIP *
popd

elm make src/Main.elm --optimize --output=dist-firefox/elm.js

# Firefox
pushd dist-firefox
cp ../firefox/background.html .
cp ../background.js .
cp ../content.js .
cp ../elm-import.js .
cp ../firefox/manifest.json .
cp ../popup.html .
cp ../s2f-logo.png .
cp ../s2f-logo-48x48.png .
cp ../s2f-logo-128x128.png .
 
[ -f "$FIREFOX_DIST_ZIP" ] && rm $FIREFOX_DIST_ZIP
zip -r $FIREFOX_DIST_ZIP *
popd

[ -f "$FIREFOX_SOURCE_ZIP" ] && rm $FIREFOX_SOURCE_ZIP
zip -r $FIREFOX_SOURCE_ZIP src build.sh
