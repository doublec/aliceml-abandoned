# invoke like
#   ./check-names.sh install/

ALICEDUMP='install/bin/alicelink -d'
EXT=stc

function CHECK {
  for FILE in `ls $1 | grep $EXT`
  do
    if $ALICEDUMP $1$FILE | grep "(\* Free names:" >/dev/null
    then
      echo $1$FILE:
      $ALICEDUMP $1$FILE | grep "^ \* "
    fi
  done
  for DIR in `ls -p $1 | grep /`
  do
    CHECK $1$DIR
  done
}

CHECK $1

