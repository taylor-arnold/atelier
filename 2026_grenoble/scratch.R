Sys.setenv(LANG = "fr")

source("funs.R")
set.seed(1L)

library(dplyr)
library(ggplot2)
library(forcats)
options(dplyr.summarise.inform = FALSE)
theme_set(theme_minimal())

msg <- "Combien de personnes vivent dans le 11e arrondissement de Paris ?"

res <- oai_chat(
  base_url = "http://127.0.0.1:1234",
  model = "google/gemma-4-31b",
  msg = msg,
  temperature = 0.7,
)
cat(res)

msg <- c(
  "I will give you a short text that needs to be categorized into one of five",
  "categories: X, Y, Z, A, B. Here are examples of what the categories look like",
  "X: "
  
)



msg <- c(
  'Determine if the following adjective is evaluative or descriptive/neutral.',
  'Respond with data structured as JSON like this (do no include the word json',
  'or any other markdown): ',
  '{"evaluative": true}.\n\n'
)

document <- "necessary"

res <- oai_chat(
  base_url = "http://127.0.0.1:1234",
  model = "google/gemma-4-31b",
  msg = paste(msg, document, collapse=" "),
  temperature = 0.7,
)
res

library(jsonlite)

as_tibble(fromJSON(res))

adj <- read_csv("~/Desktop/adj_list.csv")
adj$evaluative <- NA_character_

for (j in seq_along(adj$lemma))
{
  res <- oai_chat(
    base_url = "http://127.0.0.1:1234",
    model = "google/gemma-4-31b",
    msg = paste(msg, adj$lemma[j], collapse=" "),
    temperature = 0.7,
  )
  adj$evaluative[j] <- fromJSON(res)$evaluative
  print(adj[j,])
}

write_csv(adj, "~/Desktop/adj_list_eval.csv")

###########################################################################
library(tidyverse)
library(readxl)
library(udpipe)
library(stringi)
library(reticulate)

use_virtualenv("~/gh/projects-2026/2026_cinemetrics/.venv", required = TRUE)
py_config()

st <- import("sentence_transformers")
model <- st$SentenceTransformer("sentence-transformers/all-mpnet-base-v2")
epg <- read_excel("~/Desktop/epgw1.xlsx")

epg_18 <- epg |>
  filter(step == "1.8") |>
  filter(cntry != "Unknown")


df <- select(epg_18, text, cntry)
embeddings <- model$encode(df$text, normalize_embeddings = TRUE)
dim(embeddings)

hdbscan  <- import("hdbscan")
clusterer <- hdbscan$HDBSCAN(
  min_cluster_size = 5L,   
  min_samples      = 3L,
  metric           = "euclidean"
)

clusterer$fit(embeddings)

results <- tibble(
  text       = df$text,
  cluster    = as.numeric(clusterer$labels_),
  confidence = as.numeric(clusterer$probabilities_)
)

res <- results |>
  filter(cluster != -1) |>
  arrange(cluster)

write_csv(res, "clustering_epg_18.csv")



###########################################################################
library(tidyverse)
library(readxl)
library(udpipe)
library(stringi)
library(reticulate)

use_virtualenv("~/gh/projects-2026/2026_cinemetrics/.venv", required = TRUE)
py_config()

st <- import("sentence_transformers")
model <- st$SentenceTransformer("sentence-transformers/all-mpnet-base-v2")
epg <- read_excel("~/Desktop/epgw1.xlsx")

audio <- read_delim("~/Desktop/Audio 03_20241212173258.csv", delim = ";")

df <- select(audio, text = Commentaire, debut = `Début de section`, fin = `Fin de section`)
embeddings <- model$encode(df$text, normalize_embeddings = TRUE)
dim(embeddings)

hdbscan  <- import("hdbscan")
clusterer <- hdbscan$HDBSCAN(
  min_cluster_size = 3L,   
  min_samples      = 1L,
  metric           = "euclidean"
)

clusterer$fit(embeddings)

results <- tibble(
  text       = df$text,
  debut      = df$debut,
  fin        = df$fin,
  cluster    = as.numeric(clusterer$labels_),
  confidence = as.numeric(clusterer$probabilities_)
)

res <- results |>
  #filter(cluster != -1) |>
  arrange(cluster)

write_csv(res, "clustering_epg_18.csv")

(
  res |>
    ggplot(aes(debut, factor(cluster))) +
      geom_segment(aes(
        xend = fin, yend = factor(cluster), color = factor(cluster)), alpha = 0.2, linewidth = 5
      ) +
      theme_minimal()
)

ref <- model$encode("je ne comprends pas de vocabulaire", normalize_embeddings = TRUE)
res$score <- as.numeric(embeddings %*% ref)
res <- arrange(res, desc(score))

(
  res |>
    mutate(trop_rapide = factor(score > 0.6)) |>
    ggplot(aes(debut, factor(trop_rapide))) +
    geom_segment(aes(
      xend = fin, yend = factor(trop_rapide), color = factor(trop_rapide)), alpha = 0.2, linewidth = 5
    ) +
    theme_minimal()
)






audio <- read_delim("~/Desktop/audio3_cat_utf8.csv", delim = ";")

df <- select(audio, text = Commentaire)
embeddings <- model$encode(df$text, normalize_embeddings = TRUE)
dim(embeddings)

hdbscan  <- import("hdbscan")
clusterer <- hdbscan$HDBSCAN(
  min_cluster_size = 5L,   
  min_samples      = 1L,
  metric           = "euclidean"
)

clusterer$fit(embeddings)

results <- tibble(
  text       = df$text,
  FACTEUR_1 = audio$FACTEUR_1,
  COMP_DIM_1 = audio$COMP_DIM_1,
  PHENOMENE_1 = audio$PHENOMENE_1,
  cluster    = as.numeric(clusterer$labels_),
  confidence = as.numeric(clusterer$probabilities_)
)

table(results$cluster, results$COMP_DIM_1)

#####
epg <- read_excel("~/Desktop/epgw1.xlsx")

table(epg$cntry, epg$gndr)

epg |>
  group_by(cntry, gndr) |>
  summarize(n = n()) |>
  pivot_wider(id_cols = "cntry", values_from = "n", names_from = "gndr", values_fill = 0) |>
  print(n = Inf)

epg |>
  group_by(cntry, gndr) |>
  summarize(n = n()) |>
  pivot_wider(id_cols = "cntry", values_from = "n", names_from = "gndr", values_fill = 0) |>
  mutate(f_ratio = female / (male + female)) |>
  arrange(f_ratio) |>
  print(n = Inf)









