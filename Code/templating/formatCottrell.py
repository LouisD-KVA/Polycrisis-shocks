
import pandas as pd

from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def formatCottrell(filepath):
    """Format the food production shocks dataset (Cottrell 2018)"""

    df = pd.read_csv(filepath, usecols=["Country name", "Year", "Sector"])
    df.rename(columns={"Sector": "Shock type"}, inplace=True)

    df["Shock category"] = "ECOLOGICAL"

    df = countryName(df)
    df = grpCount(df)
    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/Food shocks.csv"

    df = formatCottrell(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
