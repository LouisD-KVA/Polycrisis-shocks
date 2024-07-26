
import pandas as pd


from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def formatDON(filepath):
    """Format the Disease Outbreak News dataset"""

    df = pd.read_csv(filepath, usecols=["Country", "Year"])
    df.rename(columns={"Country": "Country name"}, inplace=True)

    df["Shock category"] = "ECOLOGICAL"
    df["Shock type"] = "Infectious disease"

    df = countryName(df)
    df = grpCount(df)
    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/Outbreaks.csv"

    df = formatDON(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
