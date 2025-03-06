# %%
from ucimlrepo import fetch_ucirepo
import pandas as pd

heart_disease = fetch_ucirepo(id=45)

# %%
df = heart_disease.data.features
df["num"] = heart_disease.data.targets.num
df.to_csv(
    "04-Regression_in_Practice/R-day/Binary_Outcomes/data/raw.csv",
    index=False,
)
# %%
