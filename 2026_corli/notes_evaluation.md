# Évaluation des idées de Nicolas (2 novembre 2026)

Ce que j'ai vérifié en ligne pour chaque expérience, et une proposition de
programme pour la journée. Voir `notes_nicolas.md` pour le texte d'origine.

## Vérifications

| Exp. | Ressource | Statut |
|---|---|---|
| 1 Qwen | `ollama pull qwen3:8b` (5,2 Go), `huihui_ai/qwen3-abliterated:8b` sur Ollama | OK. Qwen3 a un mode « thinking » activable/désactivable. |
| 2 PhonologyBench (EN) | Données : GitHub 404, HuggingFace 401 | Non trouvées. L'article disait « à venir dans la version finale ». Tâches : G2P, comptage de syllabes, rimes. 4k items. |
| 2 Phun-Bench (ZH) | github.com/xing-stellus-yue/Phun-Bench, JSON | OK mais tout en chinois (homophones, rimes, similarité). |
| 3 CompoundPiece | huggingface.co/datasets/benjamin/compoundpiece | OK. Sous-ensemble `wiktionary` 255k mots, 56 langues (dont fr, de, en), colonnes word / segmentation / lang. CSV+parquet, MIT. Tâche = découpage de composés, pas interprétation. |
| 4 ImperfectiveNLI | github.com/boleima/ImperfectiveParadox, `data/imperfectiveNLI.json` | OK. 400 items, 4 groupes de 100, labels True/False/Unknown. 4 types de prompt (zero-shot, dap, cot, counterfactual). Script Python ne calcule pas les scores (trivial). ACL 2026 best paper, modèles 7-9B. |
| 5 Biais | HAL (PDF bloqué par anti-bot), OSF (page vide, lien view-only) | Pas de données accessibles en l'état. Demander les fichiers à Nicolas. |
| 6 CELVA.SP | nakala.fr (403 pour un script ; essayer à la main) | À vérifier à la main. Corsi et al. 2026 : 761 copies, 3 modèles commerciaux, 27 prompts, QWK/MAE ; textes NON distribués, seulement les notes. |

## Proposition de journée (en ligne, ~6h)

1. **Mise en route (1h)** — Ollama + R (`ellmer::chat_ollama()`), premier dialogue,
   conservation des échanges dans un tableau / fichier local, mode thinking
   de Qwen3. Exp. 1 comme fil conducteur (questions sensibles, modèle de base
   vs abliterated).
2. **Construire un banc d'essai (2h)** — ImperfectiveNLI : JSON -> tibble -> CSV,
   boucle sur 400 items, 2-3 types de prompt, exactitude par groupe, comparaison
   avec les chiffres de l'article. C'est la réplication la plus propre.
3. **Composés et tokenisation (1h)** — CompoundPiece : télécharger depuis
   HuggingFace, échantillon fr/de, demander au modèle de découper, mesurer,
   montrer les sous-tokens.
4. **Noter des copies (1h30-2h)** — CELVA.SP : prédire le niveau CECRL à partir
   du texte, QWK contre les annotations humaines, effet du prompt, tendance
   centrale (réutilise la méthode de Corsi et al. sans leurs données).

À condenser ou reporter : PhonologyBench (données introuvables ; possible
mini-version maison avec Lexique 3 pour le français : nbsyll, rimes),
Phun-Bench (chinois), biais (en attente des données).
