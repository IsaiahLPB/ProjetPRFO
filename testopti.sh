#!/bin/bash

COUNT=50

for i in $(seq 1 $COUNT); do
  if [ "$i" -eq 1 ]; then
  ./steiner < data.txt > calcul.txt
  else
  ./steiner < data.txt >> calcul.txt
  fi
done

# Initialiser les variables
sum=0

while read -r line; do
  valmax=$(echo $line | awk '{print $1}')
  valmin=$(echo $line | awk '{print $2}')

  percentage=$(echo "$valmin / $valmax * 100" | bc -l)
  
  sum=$(echo "$sum + $percentage" | bc -l)
done < calcul.txt

if [ $COUNT -ne 0 ]; then
  mean=$(echo "$sum / $COUNT" | bc -l)
  echo "Moyenne des pourcentages : $mean%"
fi