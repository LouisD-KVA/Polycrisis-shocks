
import pandas as pd
from ast import literal_eval


UNIONS = {
    "Yemen": ["North Yemen", "South Yemen"],
    "Germany": ["West Germany", "East Germany"]
}
INV_UNIONS = {e: k for k, v in UNIONS.items() for e in v}

SPLITS = {
    "Soviet Union (former)": ["Armenia",
                              "Azerbaijan",
                              "Belarus",
                              "Estonia",
                              "Georgia",
                              "Kazakhstan",
                              "Kyrgyz Republic",
                              "Latvia",
                              "Lithuania",
                              "Moldova",
                              "Russia",
                              "Tajikistan",
                              "Turkmenistan",
                              "Ukraine",
                              "Uzbekistan"],
    "Czechoslovakia": ["Czechia", "Slovakia"],
    "Yugoslavia": ["Bosnia and Herzegovina", "Croatia", "North Macedonia", "Montenegro", "Serbia", "Slovenia"],
    "Netherlands Antilles": ["Aruba", "Curacao", "Sint Maarten"],
    "Serbia and Montenegro": ["Serbia", "Montenegro"]
}


def obsoleteToCurrents(counList):
    """Replace obsolete countries by the list of the resulting ones"""
    try:
        counList = literal_eval(counList)
    except:
        counList = [counList]

    for obsolete, currents in SPLITS.items():
        try:
            counList.remove(obsolete)
        except:
            continue
        counList += currents

    counList = list(set(counList))
    if len(counList) == 1:
        return counList[0]
    else:
        return counList


def obsoleteCountries(df):
    df.reset_index(inplace=True)
    # Unions : replace obsolete by current
    df.replace({"Country name": INV_UNIONS}, regex=True, inplace=True)

    # Splits : apply the 'obsoleteToCurrents' function
    df["Country name"] = df["Country name"].apply(obsoleteToCurrents).astype(str)
    return df


if __name__ == "__main__":
    from os.path import splitext
    filepath = "../CONFLICTS/Interstate armed conflict/UCDP Countries pdf.coco.csv"

    df = pd.read_csv(filepath)
    df = obsoleteCountries(df)

    df.set_index(["Country name"], inplace=True)
    # Save to csv without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".former.csv")
