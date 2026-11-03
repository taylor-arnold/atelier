# Notes de Nicolas Ballier (reçues le 2 novembre 2026)

Idées d'expériences pour la formation CORLI du 1er décembre 2026,
telles qu'envoyées (non éditées).

---

Expérience 1 dialoguer avec la version débridée ou chinoise de Qwen

Prépaper quelques questions sur l'histoire de la Chine (Tienanmen), le statut de Taiwan, le traitement des Ouïgours
tester le modèle Qwen 3.8 dans sa version censurée ou non censurée

Objectif:
- conserver le mode raisonnement

Expérience 2 : PhonologyBench: un banc d'essai des "compétences phonologiques"
https://aclanthology.org/2024.knowllm-1.1.pdf
- réplication d'articles: Phonologybench ou sa version chinoise
Phin Bench phonobench Chinese
https://aclanthology.org/2026.acl-long.1041/

Expérience 3
L'analyse multilingue des composés
Le jeu de données n'est pas exempt de reproches
https://arxiv.org/abs/2305.14214
Apprendre à récupérer des données sur HuggingFace
le forat .json
transformer un format .json en format .csv
Expliquer la sous-tokenisation et son rôle potentiel

Expérience 4 The imperfective paradox
https://github.com/boleima/ImperfectiveParadox/tree/main
https://raw.githubusercontent.com/boleima/ImperfectiveParadox/refs/heads/main/run_inference.py
https://aclanthology.org/2026.acl-long.689/


Expérience 5
Projets sur les biais des modèles
https://anr-inextenso.loria.fr/publications/
Gender:
https://inria.hal.science/hal-05708359/file/6.pdf
https://osf.io/vnzfh/overview?view_only=b450f27ab9274ee4b90a1b424fc3cd4e%20sur%20OSF


Expérience 6:

tâche : prédiction de niveau de langue en anglais  (Automated essay grading, AEG):
prédire le niveau de langues en anglais à partir du texte brut (prédire la 4e colonne avec l'avant dernière:  Texte_etudiant).
Le CSV peut-être récupéré sur le dépôt Nakala
https://www.nakala.fr/10.34847/nkl.98cb790l#ecf290d654b576fb3c872aec7ac3b77707c334b0
Corpus du CELVA.SP collecté à Rennes dans le cadre des traavux de l'ANR A4LL
https://lidile.recherche.univ-rennes2.fr/article/a4ll-analytics-language-learning

réplication d'article réalisé sur des LLM (python):
Machine–human agreement in marking undergraduate assignments
is specific to model, prompt and institution
Giulio Corsi et al
https://arxiv.org/pdf/2610.18019
# Code: https://github.com/giuliocorsi/machine-human-marking-agreement
# Replication data: https://zenodo.org/records/21676733
