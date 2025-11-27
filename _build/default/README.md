Projet infératrice :

Victor Jourdan
Zacharie Moughanim
Siméon Laporte

Introduction : 

    Tout d'abord, notre projet est à compiler avec dune.
    
    Pendant ces trois jours de projet, nous avons pu nous pencher sur le fonctionnement d'une récente découverte archéologique : l'infératrice ! Notre statut d'archéo-informaticiens nous a mené à la mission de reconstruire l'infératrice à partir du peu de données que les fouilles archéo-électroniques ont trouvé. 
    Selon ces fouilles, l'infératrice était capable de trouver des dérivations grâce à des règles d'inférences.
    
    Cependant, le fonctionnement global de l'infératrice nous est expliqué par les fichiers historiques : ast.ml - term.mli - query.mli - convert.mli - unify.mli. Nous avons donc du écrire les fichiers .ml correspondants à chacun de ces fichiers .mli. Par soucis pratique, nous avons aussi écrit un fichier utils.ml (associé à utils.mli).

Arbre de dépendance de compilation: 
(une flèche a -> b signifie que b dépend de a)

    utils.ml -> term.ml -> unify.ml -> query.ml -> convert.ml
                        -> ast.ml -> convert.ml

Présentation des différents fichiers :

    1) utils.ml : Nous avons créé nous-même ce fichier, il contient des fonctions assez générales et qui nous sont utiles dans plusieurs autres fichiers

    2) term.ml : Dans ce fichier, on déclare plusieurs types importants dans le projet. Par exemple, le type Term.t, le type Term.var et le type Term.obs_t. C'est aussi ici qu'on déclare le binding (qui est, dans ce projet, un tableau de taille une puissance de 2 variable) et qu'on écrit les fonctions de base permettant de manipuler ces types.

    3) ast.ml : Dans ce fichier on déclare plusieurs modules qui sont utiles dans la suite. Par exemple, on déclare modules Ast.Term, Ast.Atom et Ast.Rule. Qui contiennent chacun un type (respectivement un terme, un atome et une règle) et les fonctions pour les manipuler.

    4) unify.ml : Ici on n'a que deux fonctions. La plus importante est la fonction unify qui permet d'unifier deux termes (Term.t) si c'est possible. En d'autres termes, elle essaye d'égaliser deux termes en créant un binding adéquat.

    5) convert.ml : Ce fichier contient deux fonctions principales. La fonction query qui crée une conjonction d'atomes et une fonction d'affichage. La fonction rules va passer d'une représentation des règles comme une liste de règles à une vision sous forme d'une fonction atom_to_query_t (elle utilise la fonction rule, qui crée un query à l'aide d'une règle d'inférence et d'un atome).

    6) query.ml : Ici, on recrée l'infératrice ! 
    La fonction search est la plus importante, elle prend en entrée un query et affiche tous les binding qui satisfont ce query.
    La fonction has_solution indique si un query est satisfiable ou non et se déduit facilement de la fonction search.

Résumé des features principales :

    Notre infératrice est finalement capable, en prenant un entrée un query, d'afficher à l'écran l'ensemble des bidings qui rendent cette requête vraie. Evidemment, et pour être toujours plus fidèle aux fouilles archéologiques, l'infératrice répond "42" quand on lui demende quelle est la réponse de la vie.