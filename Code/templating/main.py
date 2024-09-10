
import pandas as pd

from formatCottrell import formatCottrell
from formatDON import formatDON
from formatEmDat import formatEmDat
from formatGTD import formatGTD
from formatNguyen import formatNguyen
from formatUCDP import formatUCDP


if __name__ == "__main__":
    """Assemble the 6 databases into one csv file"""

    df = pd.concat([
        formatCottrell("raw datasets/Food shocks.csv"),
        formatDON("raw datasets/Outbreaks.csv"),
        formatEmDat("raw datasets/public_emdat_incl_hist_2024-07-22.csv"),
        formatGTD("raw datasets/globalterrorismdb.csv"),
        pd.read_csv("raw datasets/GTD 1993.final.csv",
                    index_col=["Country name", "Year", "Shock category", "Shock type"]),
        formatNguyen("raw datasets/Nguyen2022.csv"),
        formatUCDP("raw datasets/UcdpPrioConflict_v24_1.csv")])

    df.sort_index(inplace=True)
    df = df[df["count"] != 0]

    # Save without "nan" nor ".0"
    df.astype(str).replace(r"(\.0+$)|^nan$", "", regex=True).to_csv("../Shock counts.csv")
