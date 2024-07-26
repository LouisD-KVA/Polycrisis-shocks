
import pandas as pd


def grpCount(df):
    """Sum entries to get counts per year"""

    df.insert(0, "count", 1)

    return df.groupby(["Country name", "Year", "Shock category", "Shock type"]).sum()


if __name__ == "__main__":
    from os.path import splitext
    filepath = "raw datasets/GTD 1993.final.csv"
    df = pd.read_csv(filepath)

    df = grpCount(df)

    df.set_index("Country name", inplace=True)
    # Save to csv without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv(splitext(filepath)
                                                                    [0]+".agg.csv")
