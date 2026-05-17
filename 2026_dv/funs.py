from pathlib import Path

import re
import numpy as np
import pandas as pd
import polars as pl
from plotnine import geom_text

__all__ = [
    "print_rows",
    "Path",
    "plot_image_grid",
    "clean_json_string"
]

def print_rows(df: pl.DataFrame, n= -1):
    with pl.Config(tbl_rows=n):
        try:
            from IPython.display import display
            display(df)
        except ImportError:
            print(df)


def plot_image_grid(df, ncol=10, label_name="label", filepath="filepath", limit=100):
    import matplotlib.pyplot as plt
    from PIL import Image
    import math

    df = df.head(limit)
    n = df.height
    if n == 0:
        return

    nrow = math.ceil(n / ncol)

    paths = df.select(filepath).to_series().to_list()
    labels = None
    if label_name is not None and label_name in df.columns:
        labels = df.select(label_name).to_series().to_list()


    fig, axes = plt.subplots(nrow, ncol, figsize=(ncol * 2, nrow * 2))
    axes = np.array(axes).ravel()

    for i, ax in enumerate(axes):
        if i < n:
            img = Image.open(paths[i])
            cmap = "gray" if img.mode == "L" else None
            ax.imshow(img, cmap=cmap)

            if labels is not None:
                ax.set_title(str(labels[i]), fontsize=8)

            w, h = img.size
            max_dim = max(h, w)
            ax.set_xlim(-0.5 + (w - max_dim) / 2, w - 0.5 + (max_dim - w) / 2)
            ax.set_ylim(h - 0.5 + (max_dim - h) / 2, -0.5 + (h - max_dim) / 2)
            ax.set_aspect("equal")
            ax.axis("off")
        else:
            ax.axis("off")

    fig.set_constrained_layout(True)
    plt.show()


def clean_json_string(text: str) -> str:
    """Removes markdown code fences like ```json and ``` if present."""
    text = text.strip()
    cleaned = re.sub(r'^```(?:json)?\s*|\s*\n```$', '', text, flags=re.IGNORECASE)
    return cleaned.strip()


