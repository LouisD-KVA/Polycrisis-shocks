
import pandas as pd

from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def formatNguyen(filepath):
    """Format the economic dataset from Nguyen et. al (2022)"""

    df = pd.read_csv(filepath, usecols=["Country", "Year",
                                        "Banking Crises", "Currency Crises", "Debt Crises"])
    df.rename(columns={"Country": "Country name"}, inplace=True)

    df = df.melt(id_vars=["Country name", "Year"], var_name="Shock type", value_name="count")
    df["Shock category"] = "ECONOMIC"

    df.set_index(["Country name", "Year", "Shock category", "Shock type"], inplace=True)
    df.dropna(inplace=True)
    df = df[df["count"] == 1].astype({"count": int})
    df.reset_index(inplace=True)

    df = countryName(df)
    df.set_index(["Country name", "Year", "Shock category", "Shock type"], inplace=True)
    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/Nguyen2022.csv"

    df = formatNguyen(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
