#!/usr/bin/env sh

hist_file='/tmp/.scm_sh_hist'
dmenu_str="dmenu -i -fn 'Px437 ATI 8x14' -nb \#1c1c1c -nf \#eaeaea -sb \#edd035 -sf \#1c1c1c"

touch $hist_file

cmd=$(cat $hist_file | eval $dmenu_str)

if [ $? -eq 1 ]; then
  exit
fi

if [ $cmd == "_clr" ]; then
  rm $hist_file
  exit
fi

echo $cmd >> $hist_file
sort -u -o $hist_file $hist_file
echo $cmd | gsi -e '(let ((cmd (read))) (display (eval cmd)) (newline) (display cmd))' | eval $dmenu_str
