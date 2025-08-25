En te connectant a Rstudio pour voir le df + mes analyses
Je regarde les colectomies subtotales pour colites aiguës graves dans les MICI
Ce que je cherche a évaluer, c'est si les nouveaux traitements n'ont pas crée une sorte de "nouvelle entité" qui permet un contrôle partiel de la maladie, la sortie du patient mais souvent sa réhospitalisation 
en gros tous les malades du df ont été opéré d'une colectomie subtotale pour une colite aiguë grave
j'ai toutes les lignes de leur traitement (tu peux regarder le script CAG script.R qui explicite bien la structure de mon df et les données dedans)
ce que je veux regarder c'est voir quel est le profil des patients qui "sortent" pendant le traitement de la CAG et revienne pour se faire opérer moins de 30 jours apres la sortie (df$delai_sup_30 codé 0 si revienne moins de 30 jours apres la sortie pendant le même episode ou codé 1 si delai de plus de 30 jours ou si pas de sortie, dans ce cas délai de plus de 30 jours c'est comme si c'était une autres poussée)
Apres on pourra regarder en détail la sortie si elle est a plus de 30 jours mais pas pour l'instant, reste sur 2 groupes 
je ne suis pas tres sur que ce soit le bon angle pour aborder la question comment j'ai fait
je me demande si tu ne pourrais pas faire mouliner mon df pour voir si tu arrives a des conclusions, même si elles sont completement differentes 
tu peux deja verifier la mienne puis en emettre d'autre a tester si tu veux
uelques pistes mais tu peux tester d'autres trucs
- quels sont les patients qui "sortent" pendant une poussee (démographie + caractéristiques cliniques et biologiques de la poussée actuelle + antécédents de CAG (colite aigue grave) + antériorités de poussées et historiques de traitements)
- quels sont les traitements de la poussée qui permettent la sortie 
- est ce que ca allonge le délai entre le début des symptômes et la chirurgie ou entre le diagnostic et la chirurgie (pas d'interêt du délai entre admission et chirurgie donc)
- et est ce qu'il y a une modification des suites ?

Attention : tous les patients sont opérés (ce n'est donc pas valide de dire que 67% des patients sortis pendant le traitement médical sont réadmis dans les 30 jours, suggérant un phénomène de "contrôle partiel transitoire" induit par les nouveaux traitements (car je n'ai pas les patients qui n'ont pas été opérés finalement))
En pratique : 
Ma base de données est construite a partir de patients qui ont tous une colectomie subtotale pour CAG.
Je veux donc essayer de distinguer des profils "préopératoires". Une de mes hypothèses était de regarder les patients qui étaient sorti au cours du traitement, je pense que c'est pas mal, pour voir s'il y avait une différence entre ces deux profils.
Peut être qu'il faut aussi regarder en fonction des traitements utilisés de fond ou de la poussée
Peut être que ca ne vaut pas le coup de pousser ce type d'analyse mais je pense qu'elle est pertinente
Le taux de controle et lui aussi difficilement evaluable : je n'ai pas les patients qui n'ont pas été opérés
Idem pour les modèles de prédiction : encore une fois difficile a dire car pas les patients pas opérés

Il faut donc bien refaire tourner tout ca en fonction de ces informations car la base concerne 100% des patients finalement opérés

Attention a cette information : tous les patients ont finalement été opérés et je n'ai pas l'information des traitements médicaux qui ont réussi

Essaie de faire plus en fonction de by = delai_sup_30

Fais comme si :

1 concerne des patients non sortis ou alors avec une précédente hospit qui ne concernait pas l'épisode actuel

0 concerne les patients sortis pendant l'épisode actuel