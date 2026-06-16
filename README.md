# Génération markovienne de texte en langage naturel

Il s'agit d'une implémentation du projet de l'UE de Programmation Fonctionnelle
de Etienne Lozes en 2023-2024.

Page du cours : https://webusers.i3s.unice.fr/~elozes/enseignement/PF/ 

Page du projet : https://webusers.i3s.unice.fr/~elozes/enseignement/PF/projet/sujet/sujet-projet-2023-2024.html

Le sujet précise que les dépôts peuvent être rendus public à la fin du semestre.


- La question 1.1 (encodage de caractères) a été traitée.
- La question 1.2 (encodage de mots) a été traitée. Le choix a été fait d'avoir une implémentation avec une solution plus simple mais sous optimale car les questions suivantes ont pour but d'implémenter de meilleurs algorithmes.
- La question 1.3 (encodage de prefixes) a été traitée. Le décodage est un copier coller du décodagede la 1.2 parce qu'il n'est pas autorisé par le sujet de modifier la chaîne de compilation.
- La question 1.4 (encodage d'arbre préfixe) a été traitée.

- La question 2.1 (N-grammes) a été traitée
- La question 2.2 (random walk) a été traitée
- La question 2.3 (markov chain learning) a été traitée
- La question 2.4 (bpe) a été traitée. L'algorithme est pas forcément ultra optimal (c'est pas tail recursive), et comme aucun test ne demande d'implémenter encode, je ne l'ai pas implémenté.
- La question 2.5 (text generator) a été traitée

Si on charge avec le texte swann.txt on a un stack overflow.
En l'état actuel, on est aussi limité par le fait que je n'ai implémenté que les fonctions demandées. Or, si on veut utiliser prefix tree, il faudrait coder sa fonction learn.
