# ProjetPRFO Simon Darnault

## Execution principale du programme
Comment éxecuter le code fourni ?
Dans votre terminal, compilez l'ensemble du projet avec "make".
Puis, pour éxecuter le programme résultant, rentrez dans votre terminal ./steiner.
Le programme prend en entrée soit des coordonnées entières soit flottantes.

Pour plus de fluidité, un fichier data.txt est fourni et permet de rentrer le nombre
de points désirés et ensuite leurs coordonées.
Un exemple de fichier data.txt :
3
-5 11
-7.7 -4.2
6.8 16.4


Attention, il faut impérativement un retour à la ligne après avoir rentré la dernière coordonnées
pour une lecture correcte du programme.

Pour modifier le nombre d'itération dans la boucle principal de l'algorithme, vous pouvez modifier la valeur nb_iter ligne 386.
La valeur initiale est posée à 10 000 mais on obtient de meilleurs résulats en augmentant ce nombre.

A l'éxécution, entrez une première fois sur 'Entrée' après avoir visualisé le résultat suivant la première
étape de l'algorithme Hill Climbing. Puis, après calcul, visualisez le résultat de l'algorithme Hill Climbing
et appuyez sur 'Entrée' pour terminer le programme.

## Execution du script de test

Afin de déterminer efficacement l'efficacité du programme, un script nommé "testopti.sh" permet d'éxecuter
plusieurs fois ./steiner et de faire la moyenne de l'efficacité de ses résultats.

Pour l'éxécuter, n'oubliez pas de lui donner les droits d'éxecution sur votre machine.

De plus, pour plus de practicité, commentez les appels aux fonctions "draw_steiner" et "setup_graph" pour
ne pas avoir à observer chacun des graphes produit au cours de chaque execution. Dans ce cas, il faut aussi
rajouter l'instruction "()" ligne 407 pour que le programme compile bien.