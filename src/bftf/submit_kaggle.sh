for OUTPUT in $(ls  $1)
do
    kaggle competitions submit -c dmeyf2021segunda -f $OUTPUT -m "Message" 
done