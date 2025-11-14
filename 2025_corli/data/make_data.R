library(phonTools)
library(tidyverse)


xsampa <- tibble(
  vowel = c("{", "3'", "A", "E", "i", "I", "O", "u", "U", "V", "e", "o"),
  ipa = c("æ", "ɜ˞", "ɑ", "ɛ", "i", "ɪ", "ɔ", "u", "ʊ", "ʌ", "e", "o")
)
types <- tibble(
  type = c("c", "m", "w"),
  groupe = c("enfant", "homme", "femme")
)
types2 <- tibble(
  type = c("b", "g", "m", "w"),
  groupe = c("fille", "garçon", "home", "femme")
)
gender <- tibble(
  sex = c("m", "w"),
  genre = c("masculin", "féminin")
)


data(pb52)   # Peterson & Barney (1952); type => child, man, woman
pb52 <- as_tibble(pb52)
write_csv2(pb52, "peterson_barney_voyelle_eng.csv")
pb52 <- read_csv2("peterson_barney_voyelle_eng.csv")
pb52 <- left_join(pb52, xsampa, by = c("vowel"))
pb52 <- left_join(pb52, types, by = c("type"))
pb52 <- left_join(pb52, gender, by = c("sex"))
pb52 <- select(pb52, id=speaker, groupe, genre, ipa, f0, f1, f2, f3)
write_csv2(pb52, "peterson_barney_voyelle_eng.csv")


data(h95)    # Hillenbrand et al. (1995)
h95 <- as_tibble(h95)
write_csv2(h95, "hillenbrand_voyelle_eng.csv")
h95 <- read_csv2("hillenbrand_voyelle_eng.csv")
h95 <- left_join(h95, xsampa, by = c("vowel"))
h95 <- left_join(h95, types2, by = c("type"))
h95 <- select(h95, id=speaker, groupe, ipa, dur, f0, f1, f2, f3)
write_csv2(h95, "hillenbrand_voyelle_eng.csv")

# LEXIQUE
df <- read_delim("Lexique382.tsv.bz2", delim="\t")


############################################################################
# Keylogs
library(tidyverse)
library(stringi)

kl <- read_csv("KUPA-KEYS-TASK-2.csv.bz2", na="NA")
meta <- read_csv("KUPA-KEYS-META.csv.bz2")


kl <- kl |>
  arrange(id, time) |>
  filter(stri_length(key) <= 1L) |>
  filter(type %in% c("down", "up")) |>
  group_by("key_code") |>
  mutate(time_up = lead(time, n=1)) |>
  filter(type == "down") |>
  mutate(dur = time_up - time) |>
  mutate(key = if_else(key == "", " ", key)) |>
  ungroup() |>
  select(id, t0=time, t1=time_up, dur, touche=key, code=key_code)

mots <- kl |>
  mutate(mot_id = 1L + cumsum(touche == " ")) |>
  filter(touche != " ") |>
  mutate(t0_s = t0, t1_s = t1) |>
  group_by(id, mot_id) |>
  summarize(
    mot = paste0(touche, collapse=""),
    t0 = first(t0_s),
    t1 = last(t0_s),
    t2 = last(t1_s)
  ) |>
  ungroup() |>
  arrange(id, t0) |>
  group_by(id) |>
  mutate(
    char_mot = stri_length(mot),
    dur_mot = t2 - t0,
    d1 = lead(t0, n=1) - t1,
    d2 = lead(t0, n=1) - t2    
  ) |>
  ungroup() |>
  filter(!is.na(d2)) |>
  select(id, mot, char_mot, dur_mot, d1, d2)

mots

langs <- tibble(
  nativelang = c("English", "Greek", "German", "French", "Italian", "Polish", "Portuguese", "Spanish"),
  lang = c("eng", "ell", "deu", "fra", "ita", "pol", "por", "spa")
)
levels <- tibble(
  cefrself = c("Beginner (A1, A2)", "Intermediate (B1, B2)", "Advanced (C1, C2)"),
  cefr = c("A1/A2", "B1/B2", "C1/C2")
)


meta <- meta |>
  semi_join(mots, by="id") |>
  mutate(cefrself = if_else(
    nativelang == "English", "Advanced (C1, C2)", cefrself)) |>
  inner_join(levels, by="cefrself") |>
  inner_join(langs, by="nativelang") |>
  mutate(lang = nativelang) |>
  select(id, age, lang, cefr)


kl <- kl |>
  semi_join(meta, by="id")

mots <- mots |>
  semi_join(meta, by="id")

write_csv2(meta, "keylog-meta.csv.bz2")
write_csv2(kl, "keylog-touches.csv.bz2")
write_csv2(mots, "keylog-mots.csv.bz2")



mots |>
  group_by(id) |>
  summarize(n = n()) |>
  left_join(meta, by = "id") |>
  group_by(lang) |>
  summarize(mu = mean(n)) |>
  arrange(desc(mu))



mots |>
  left_join(meta, by = "id") |>
  group_by(cefr) |>
  summarize(mu = mean(char_mot)) |>
  arrange(desc(mu))

mots |>
  left_join(meta, by = "id") |>
  group_by(lang) |>
  summarize(mu = median(d1)) |>
  arrange(desc(mu))

mots |>
  left_join(meta, by = "id") |>
  mutate(end_sent = (stri_sub(mot, -1, -1) %in% c(".", "?", "!"))) |>
  mutate(end_phrase = (stri_sub(mot, -1, -1) %in% c(",", ";"))) |>
  group_by(lang, end_sent) |>
  summarize(mu = median(d1)) |>
  arrange(desc(mu))

######

library(tidyverse)
library(stringi)

z <- read_csv2("data/combined.csv.bz2")


z |>
  filter(lemma %in% c("pouvoir", "vouloir", "devoir", "savoir")) |>
  filter(pos == "VERB") |>
  mutate(tense = stri_extract(morph, regex="Tense=[^|]+")) |>
  mutate(tense = stri_sub(tense, 7L, -1L)) |>
  filter(!is.na(tense)) |>
  group_by(lemma, tense) |>
  summarize(n = n()) |>
  group_by(lemma) |>
  mutate(prop = n / sum(n) * 100) |>
  arrange(lemma, desc(prop))


z |>
  filter(head %in% c("il", "elle")) |>
  filter(dep == "nmod")


##################################################################

x <- readr::read_csv2("fruitlegumes.csv")

vec_file <- "large/cc.fr.300.vec"
emb <- read.table(vec_file, 
                  skip = 1,
                  quote = "", 
                  comment.char = "",
                  stringsAsFactors = FALSE)


emb <- emb[which(!is.na(emb[,1])),]

rownames(emb) <- emb[,1]
emb <- as.matrix(emb[,-1])
norms <- sqrt(rowSums(emb^2))
emb_norm <- emb / norms

idx <- match(x$nom, rownames(emb))
X <- emb_norm[x$nom,]
readr::write_rds(X, "embed.rds")

##################################################################
library(tidyverse)


z <- read_csv2("large/combined.csv.bz2")

ids <- sort(table(z$doc_id), decreasing=TRUE)

z <- z[z$doc_id %in% names(ids)[1:250],]
write_csv2(z, "wiki_parsed.csv.bz2")




