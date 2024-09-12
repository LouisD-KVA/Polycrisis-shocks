
import pandas as pd
import country_converter as coco


def countryName(df):
    """Convert the 'Country name' column with the standardized names"""

    cc = coco.CountryConverter(include_obsolete=True, additional_data="custom_countries.csv")

    df["Country name"] = cc.pandas_convert(df["Country name"], to="name").astype(str)

    # Remove geographical zones not in the list
    return df[df["Country name"] != "not found"]


if __name__ == "__main__":
    from os.path import splitext
    filepath = "../CONFLICTS/UcdpPrioConflict_v24_1.tmp.csv"

    df = countryName(pd.read_csv(filepath))

    # Save to csv without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".coco.csv")
