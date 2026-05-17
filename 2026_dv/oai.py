import base64
import json
import os
from concurrent.futures import ThreadPoolExecutor, as_completed
from enum import Enum

import polars as pl
from openai import OpenAI
from pydantic import BaseModel, Field

client = OpenAI()

os.makedirs("cache_oai", exist_ok=True)

posters_list = pl.read_csv("data/movies_50_years_meta.csv").to_dicts()


def encode_image(path):
    with open(path, "rb") as f:
        return base64.b64encode(f.read()).decode("utf-8")


def run_pool(fn, n, label):
    items = list(enumerate(posters_list))
    with ThreadPoolExecutor(max_workers=20) as pool:
        futures = {pool.submit(fn, item): item[0] for item in items}
        done = 0
        for future in as_completed(futures):
            future.result()
            done += 1
            if done % 20 == 0 or done == n:
                print(f"  {label} : {done}/{n}")


# --- 2.12 : VLM texte libre ---

question_vlm = "Describe the dominant colors in this movie poster"


def fetch_vlm(args):
    i, poster = args
    path = f"cache_oai/vlm_{i:04d}.json"
    if os.path.exists(path):
        return
    b64 = encode_image(poster["filepath"])
    resp = client.chat.completions.create(
        model="gpt-5.4-nano-2026-03-17",
        messages=[{
            "role": "user",
            "content": [
                {"type": "image_url", "image_url": {"url": f"data:image/jpeg;base64,{b64}"}},
                {"type": "text", "text": question_vlm}
            ]
        }],
        max_completion_tokens=512
    )
    with open(path, "w") as f:
        json.dump({
            "year": poster["year"],
            "title": poster["title"],
            "question": question_vlm,
            "response": resp.choices[0].message.content
        }, f)


if os.path.exists("cache/posters_vlm.parquet"):
    print("2.12 : cache existant, chargement.")
else:
    n = len(posters_list)
    print(f"2.12 : {n} affiches à traiter...")
    run_pool(fetch_vlm, n, "vlm")
    records = []
    for i in range(n):
        with open(f"cache_oai/vlm_{i:04d}.json") as f:
            records.append(json.load(f))
    pl.DataFrame(records).write_parquet("cache/posters_vlm.parquet")
    print("2.12 : sauvegardé dans cache/posters_vlm.parquet")


# --- 2.13 : VLM sortie structurée ---

class PosterColor(str, Enum):
    black = "noir"
    white = "blanc"
    gray = "gris"
    red = "rouge"
    orange = "orange"
    yellow = "jaune"
    green = "vert"
    blue = "bleu"
    purple = "violet"
    pink = "rose"
    brown = "marron"
    teal = "sarcelle"
    gold = "or"
    silver = "argent"


class ColorAnalysis(BaseModel):
    background_color: PosterColor = Field(description="La couleur dominante de l'arrière-plan de l'affiche.")
    foreground_color: PosterColor = Field(description="La couleur dominante du premier plan (sujet principal ou personnage).")
    text_color: PosterColor = Field(description="La couleur principale utilisée pour le texte sur l'affiche.")
    accent_color: PosterColor = Field(description="La couleur d'accent ou de mise en valeur utilisée.")
    vibe_description: str = Field(description="Une brève description de l'ambiance ou de l'atmosphère créée par ces couleurs.")


question_struct = "Analysez cette affiche de film et identifiez la couleur de l'arrière-plan, la couleur du premier plan, la couleur du texte et la couleur d'accent."


def fetch_struct(args):
    i, poster = args
    path = f"cache_oai/struct_{i:04d}.json"
    if os.path.exists(path):
        return
    b64 = encode_image(poster["filepath"])
    resp = client.beta.chat.completions.parse(
        model="gpt-5.4-nano-2026-03-17",
        messages=[{
            "role": "user",
            "content": [
                {"type": "image_url", "image_url": {"url": f"data:image/jpeg;base64,{b64}"}},
                {"type": "text", "text": question_struct}
            ]
        }],
        response_format=ColorAnalysis,
        max_completion_tokens=512
    )
    parsed = resp.choices[0].message.parsed
    if parsed is not None:
        with open(path, "w") as f:
            json.dump({
                "year": poster["year"],
                "title": poster["title"],
                "background_color": parsed.background_color.value,
                "foreground_color": parsed.foreground_color.value,
                "text_color": parsed.text_color.value,
                "accent_color": parsed.accent_color.value,
                "vibe_description": parsed.vibe_description
            }, f)


if os.path.exists("cache/posters_vlm_struct.parquet"):
    print("2.13 : cache existant, chargement.")
else:
    n = len(posters_list)
    print(f"2.13 : {n} affiches à traiter...")
    run_pool(fetch_struct, n, "struct")
    records = []
    for i in range(n):
        path = f"cache_oai/struct_{i:04d}.json"
        if os.path.exists(path):
            with open(path) as f:
                records.append(json.load(f))
    pl.DataFrame(records).write_parquet("cache/posters_vlm_struct.parquet")
    print("2.13 : sauvegardé dans cache/posters_vlm_struct.parquet")
