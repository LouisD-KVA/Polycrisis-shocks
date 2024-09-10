
import pandas as pd
from ast import literal_eval

from countryNameFormating import countryName
from grpCount import grpCount
from obsoleteCountries import obsoleteCountries
from splitCountries import splitCountries


def mergeCountries(row):
    """Merge the countries of 'row' into a list"""

    countryList = []
    for cell in row:
        if pd.isna(cell):
            continue
        try:
            cell = literal_eval(cell)
            assert type(cell) == list
        except:
            cell = [cell]
        countryList += cell

    countryList = list(set(countryList))
    if len(countryList) == 1:
        return countryList[0]
    else:
        return str(countryList)


def explodeCountries(df, columnName="Country name"):
    """Similar to pd.explode, split country lists in individual rows"""
    def evalList(countryNames):
        try:
            return literal_eval(countryNames)
        except:
            return countryNames

    df[columnName] = df[columnName].apply(evalList)
    return df.explode(columnName, ignore_index=True)


def formatUCDP(filepath):
    """Format the Uppsala Conflict Data Program conflicts dataset"""

    df = pd.read_csv(filepath, usecols=["location", "side_a", "side_a_2nd", "side_b",
                                        "side_b_2nd", "territory_name", "year", "type_of_conflict"])
    df.rename(columns={"year": "Year", "type_of_conflict": "Shock type"}, inplace=True)
    df.replace({"Shock type": {1: "Extrasystemic conflict",
                               2: "Interstate conflict",
                               3: "Intrastate conflict",
                               4: "Intrastate conflict"}}, inplace=True)

    df["Shock category"] = "CONFLICTS"

    countryCols = ["location", "side_a", "side_a_2nd", "side_b", "side_b_2nd", "territory_name"]
    df["Country name"] = df[countryCols].apply(mergeCountries, axis=1)
    df.drop(columns=countryCols, inplace=True)

    df = countryName(df)
    df = explodeCountries(df)
    df = grpCount(df)
    df = obsoleteCountries(df)
    df = splitCountries(df)

    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/UcdpPrioConflict_v24_1.csv"

    df = formatUCDP(filepath)

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".final.csv")
