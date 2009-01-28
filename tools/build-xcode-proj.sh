#!/bin/sh
pushd . >> /dev/null
cd $PROJ_PATH
xcodebuild -configuration Debug -sdk iphonesimulator2.2 $1 | egrep "(warning:|error:|\*\*)"
rm -rf $PROJ_PATH/TAGS
find . -name "*.h" -o -name "*.m" -o -name "*.cs" -o -name "*.vb" | xargs -I {} etags -o .tmpTAGS -a "{}"
#ctags does not play well with Objective C 2 @property, @synthesize and @class declarations
#next line removes such decls from tags file
grep -v "\(@property\|@synthesize\|@class\)" .tmpTAGS > TAGS
rm -rf .tmpTAGS
mkid -m ~/Projects/tools/id-lang.map
popd >> /dev/null
