from pathlib import Path
from PIL import Image
import polars as pl
from polars import col as c

src = Path("data/movie_poster_images")
dst = Path("data/mp_med")
dst.mkdir(parents=True, exist_ok=True)

for img_path in sorted(src.rglob("*.jpg")):
    year = img_path.parent.name
    out_path = dst / f"{year}_{img_path.name}"

    with Image.open(img_path) as img:
        if img.mode != "RGB":
            img = img.convert("RGB")

        w, h = img.size
        scale = 384 / max(w, h)
        new_size = (round(w * scale), round(h * scale))
        img.resize(new_size, Image.LANCZOS).save(out_path, quality=70, optimize=True)

df = pl.read_csv("data/movies_50_years_meta.csv")
missing = (
    df
    .filter(~c("filepath").map_elements(lambda p: Path(p).exists(), return_dtype=pl.Boolean))
    .select("year", "title", "filepath")
)

if missing.is_empty():
    print("All entries have a corresponding image.")
else:
    print(f"{len(missing)} missing image(s):")
    print(missing)


import polars as pl
from polars import col as c

GENRE_FR = {
    "Action": "Action",
    "Adventure": "Aventure",
    "Animation": "Animation",
    "Biography": "Biographie",
    "Comedy": "Comédie",
    "Crime": "Policier",
    "Drama": "Drame",
    "Family": "Famille",
    "Fantasy": "Fantaisie",
    "History": "Histoire",
    "Horror": "Horreur",
    "Music": "Musique",
    "Musical": "Comédie musicale",
    "Mystery": "Mystère",
    "Romance": "Romance",
    "Sci-Fi": "Science-fiction",
    "Short": "Court-métrage",
    "Sport": "Sport",
    "Thriller": "Thriller",
    "War": "Guerre",
    "Western": "Western",
}

(
    pl.read_csv("data/movies_50_years_genre_eng.csv")
    .with_columns(
        c("genre").replace(GENRE_FR)
    )
    .write_csv("data/movies_50_years_genre_fra.csv")
)
