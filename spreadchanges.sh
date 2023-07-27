function e {
  echo $@
  if eval $@ ; 
  then  :
  else 
    exit 1 
  fi 
}  
e git checkout snake
e git merge master
e git push 
e git checkout sokoban
e git merge master
e git push
e git checkout tetris
e git merge master
e git push
e git checkout connectfour
e git merge master
e git push
