 #!/bin/bash

set -u                                                                                                                                       
set -e 

elm make src/Main.elm --optimize --output=dist/elm.js

pushd dist
cp ../background.js .
cp ../content.js .
cp ../elm-import.js .
cp ../manifest.json .
cp ../popup.html .
cp ../z2f-logo.png .
cp ../z2f-logo-48x48.png .
cp ../z2f-logo-128x128.png .
 
rm ../z2f.zip
zip -r ../z2f.zip *
popd
