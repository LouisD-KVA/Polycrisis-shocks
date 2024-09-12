
import pandas as pd

from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def formatGTD(filepath):
    """Format the Global Terrorism Database
    (Year 1993 is missing and added after)"""

    df = pd.read_csv(filepath, usecols=["country_txt", "iyear"])
    df.rename(columns={"country_txt": "Country name", "iyear": "Year"}, inplace=True)

    df["Shock category"] = "CONFLICTS"
    df["Shock type"] = "Terrorist attack"

    df = countryName(df)
    df = grpCount(df)
    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/globalterrorismdb.csv"

    df = formatGTD(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
