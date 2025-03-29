
import pandas as pd

from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def renameCategories(df):
    catDict = {"Biological": "ECOLOGICAL",
               "Climatological": "CLIMATIC",
               "Extra-terrestrial": "GEOPHYSICAL",
               "Geophysical": "GEOPHYSICAL",
               "Hydrological": "CLIMATIC",
               "Meteorological": "CLIMATIC",
               "Industrial accident": "TECHNOLOGICAL",
               "Miscellaneous accident": "TECHNOLOGICAL",
               "Transport": "TECHNOLOGICAL"}

    typeDict = {"Epidemic": "Infectious disease"}

    return df.replace({"Shock category": catDict, "Shock type": typeDict})


def formatEmDat(filepath):
    """Format the EmDat disaster dataset
    (Only take 'Infectious disease' before 1996, prior to DON)"""

    df = pd.read_csv(filepath, usecols=["Country", "Start Year",
                                        "Disaster Subgroup", "Disaster Type"])
    df.rename(columns={"Country": "Country name",
                       "Start Year": "Year",
                       "Disaster Subgroup": "Shock category",
                       "Disaster Type": "Shock type"}, inplace=True)

    df = renameCategories(df)
    df = df[df["Shock type"] != "Animal Incident"]

    df = countryName(df)
    df = grpCount(df)

    # Remove 'Infectious disease after 1996'
    df.drop(df.loc[:, 1996:, :, "Infectious disease"].index, inplace=True)

    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/public_emdat_incl_hist_2024-07-22.csv"

    df = formatEmDat(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
