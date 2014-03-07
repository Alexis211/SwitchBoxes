
Essai d'algorithme glouton :

	n nombre de fils
	B list de SwitchBoxes
	S ensemble de permutations
	B <- []
	S <- { id }
	Tant que #S < n!
		On cherche la boite b qui, ajoutée à ce qu on a déjà, augmente le plus #S
		S <- le nouveau S
		B <- b::B
	Retourner B

Ne marche pas
