1. Generate the exercises files (strips solutions from the solutions files):

```
Rscript make_atelier_exercices.R
```

This reads atelier_exercices_solutions_fr.Rmd and atelier_exercices_solutions_en.Rmd and writes atelier_exercices_fr.Rmd and atelier_exercices_en.Rmd.

2. Render the exercises (produces atelier_exercices_fr.html and atelier_exercices_en.html):

```
quarto render atelier_exercices_solutions_fr.Rmd -o atelier_exercices_fr.html
quarto render atelier_exercices_solutions_en.Rmd -o atelier_exercices_en.html
```

3. Render the notes — one or both language versions:

```
quarto render index.qmd
quarto render index_en.qmd
```

4. Rebuild the atelier_r_linguistique directory and zip:

```
cp atelier_exercices_fr.Rmd atelier_exercices_en.Rmd funs.R atelier_r_linguistique/
cp -r donnees/. atelier_r_linguistique/donnees/
zip -r atelier_r_linguistique.zip atelier_r_linguistique
```
