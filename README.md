# shortestPath

## Phase 1 :  
Dans cette phase on doit construire un graphe à partir d’un fichier « input.txt » passé en ligne de commande, calculer les plus courts entre la source et le puits mentionnés dans les deux premières lignes et écrire ces chemins sur un fichier output.txt.

## Phase2 :  
Le code initial de la phase 2 se trouve dans le fichier phase2.php, malheureusement cette phase n’est pas complète à cause des difficultés que j’avais rencontrés dans l’implémentation de l’algorithme de Dinic.
J’ai commencé par la création d’une fonction pour stocker les noeuds présents dans le fichier. Chaque arc et représenté par un couple de quatre éléments : les deux premiers représente l’ars et le troisième représente sa capacité initial (0) et le quatrième représente sa capacité maximale.
Ensuite il faut construire le graphe en utilisant les deux première éléments de chaque couple.
