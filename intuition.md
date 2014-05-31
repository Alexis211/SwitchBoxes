Version française (NJ)
----------------------

On travaille sur `n = 2^p` fils. L'idée est de permettre à n'importe quel fil de se brancher à n'importe quel autre. Une boite permet de **swap** ou **ne pas swap** deux fils entre eux. Son comportement est étroitement lié à celui des deux fils.

J'ai décidé d'imaginer un **multiplexage** suivi d'un **demultiplexage**, qui permettrait de regrouper tous les fils en un point, puis de décider qui va ou. Puisqu'une boite concerne deux fils, on peut imaginer que sa sortie soit un seul gros fil contenant les deux informations. L'idée alors est relativement simple :

- On part de n = 2^p fils
- On les regroupe deux par deux, en 2^(p-1) fils de taille 2
- On regroupe les fils de taille 2 deux par deux, en 2^(p-2) fils de taille 4
- ...
- On regroupe les fils de taille 2^(p-1) deux par deux, en un fil de taille 2^p

Commence alors le demultiplexage :

- On part d'un fil de taille 2^p, qu'on découpe en deux fils de taille 2^(p-1)
- ...
- On part de 2^(p-1) fils de taille 2, qu'on découpe en 2^p fils de taille 1

Pour ce qui est du **regroupement** ou du **découpage**, il nécessite un certain nombre de boites de base, et j'ai essayé de mettre pile ce nombre là. Regrouper des fils de taille k demande k boites.

> cf svg/intuition.svg

J'ai donc essayé, et ça marchait !

Nombre de boites
----------------

Soit `n = 2^p` le nombre de fils. A chaque *ligne*, on a `n/2` boites qui font des regroupements ou des découpages. Il faut donc compter les regroupements et les découpages. On part de `n = 2^p` fils, pour arriver à 1 fil (donc p regroupements), puis on part de 1 fil pour arriver à 2^p (donc p découpages). Un tout petit truc subtil, on peut gagner un **regroupement** / **découpage**. En effet, le dernier regroupement a exactement les mêmes boites (disjointes) que le premier découpage. Or, deux mêmes boites à la suite sont aussi utiles qu'une seule, d'où le regroupement ou le découpage d'inutile.  
Finalement :

    #boites = (#decoupages + #regroupements - 1) * #boites_par_ligne
            = (2 * p - 1) * n / 2
            = n * (p - 1/2)
            = n * (log2 n - 1/2)

Voila...
