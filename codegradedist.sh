rm -r codegradedist
mkdir -p codegradedist
cd codegradedist
wget https://gitlab.com/vu-oofp/gamebase/-/archive/tetris/gamebase-tetris.tar.gz
tar -xvf gamebase-tetris.tar.gz --strip-components=1
rm gamebase-tetris.tar.gz
rm *.png
rm -r lib
rm -r src/tetris/game/
rm -r src/engine/GameBase.scala src/engine/graphics/
tar cvzf tetris.tar.gz *
