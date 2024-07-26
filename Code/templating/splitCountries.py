
import pandas as pd
from ast import literal_eval


def splitCountries(df):
    """Split the lists in 'Country name' column into individual rows,
    with the 'count' distributed proportionally to country sizes (Area x Population)"""

    df.set_index(["Country name", "Year", "Shock category", "Shock type"], inplace=True)

    pop = pd.read_csv("Population per year.coco.csv", index_col=["Country name"])
    area = pd.read_csv("Area per year.coco.csv", index_col=["Country name"])
    size = area*pop

    # Divisions : weighted split
    toDrop = []
    for countryNamesStr in df.index.get_level_values("Country name").unique():
        try:
            countryNamesList = literal_eval(countryNamesStr)
        except:
            continue
        if type(countryNamesList) == list:
            toDrop.append(countryNamesStr)

            weights = size.loc[countryNamesList]/size.loc[countryNamesList].sum()
            weights = weights.melt(var_name="Year", ignore_index=False)
            weights["Year"] = weights["Year"].astype(int)
            weights.set_index("Year", append=True, inplace=True)

            obsolete = df.loc[countryNamesStr].groupby(
                ["Year", "Shock category", "Shock type"]).sum()

            splited = obsolete.mul(weights["value"], axis=0)
            splited.dropna(how="all", inplace=True)

            splited = splited.reorder_levels(
                ["Country name", "Year", "Shock category", "Shock type"])

            # Append rows to original dataframe with appropriate dtypes (rounding down int)
            df = pd.concat([df, splited.astype(df.dtypes)])

    # Remove obsolete lists
    df = df.loc[df.index.drop(toDrop).unique()]

    # Aggregate currents
    return df.groupby(["Country name", "Year", "Shock category", "Shock type"]).sum()


if __name__ == "__main__":
    from os.path import splitext

    filepath = "raw datasets/GTD 1993.final.csv"
    df = pd.read_csv(filepath)
    # df.set_index(["Country name", "Year", "Shock category", "Shock type"], inplace=True)

    df = splitCountries(df)
    # Save to csv without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".split.csv")
