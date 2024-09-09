
import pandas as pd
from os.path import splitext


def filtering(df, startYear=1970, endYear=2019):
    """Discard countries that are not in each input datasets' scopes
    (listed in "DB Countries Intersection.csv").
    Remove shocks happening outside the chosen time period."""

    countryFilter = pd.read_csv("DB Countries Intersection.csv")
    df = df[df["Country name"].isin(countryFilter["Country name"].values)]
    df = df[(df["Year"] >= startYear) & (df["Year"] <= endYear)]

    return df


def normalizeShocks(df):
    """Normalize the number of shocks of each type by dividing by
    the world means over the study period.
    Then divide by the number of shock types per category to get comparable sizes"""

    worldSumPerYear = df.groupby(["Year", "Shock type"]).sum()
    worldMeans = worldSumPerYear.groupby("Shock type").mean()
    norm = df/worldMeans

    catSize = df.groupby(["Shock category"]).apply(
        lambda x: len(x.index.get_level_values("Shock type").unique()))
    norm = norm.div(catSize, axis=0)

    return norm


if __name__ == "__main__":
    filepath = "../Shock counts.csv"
    df = pd.read_csv(filepath)

    df = filtering(df)
    df.set_index(["Country name", "Year", "Shock category", "Shock type"], inplace=True)
    norm = normalizeShocks(df)

    # Save to csv without "nan" nor ".0"
    norm.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv("../Shock norm.csv")
