from folktables import ACSDataSource, ACSIncome
from sklearn.preprocessing import LabelEncoder
import us
import numpy as np
import pandas as pd

def get_ACSIncome():
    print("Generating ACSIncome data...")
    states = [state.abbr for state in us.states.STATES]
    data_source = ACSDataSource(survey_year='2018', horizon='1-Year', survey='person')
    data = data_source.get_data(states=states, download=True)
    features, labels, _ = ACSIncome.df_to_numpy(data)

    # save as pandas df
    data_cat = np.c_[features, labels]
    feats = ['AGEP', 'COW', 'SCHL', 'MAR', 'OCCP', 'POBP', 'RELP', 'WKHP', 'SEX', 'RAC1P', 'TARGET']
    df = pd.DataFrame(data_cat, columns=feats)
    df.to_csv('ACSIncome_2018.csv', index=False)
    print("Done!")


def prepare_uci_adult():
    print("Generating UCI Adult data...")
    df = pd.read_csv("data/uci_adult.csv", header=None)
    df.columns = ["age", "workclass", "fnlwgt", "education", "education-num", "marital", "occupation", \
                "relationship", "race", "sex", "capital-gain", "capital-loss", "hours", "native-country", "income"]

    # keep only certain features
    df = df[["age", "workclass", "education", "marital", "hours", "sex", "income"]]

    # recode sex and target
    recodes = {"sex": {" Male": 0, " Female":1}, "income": {" <=50K": 0, " >50K":1}}
    df = df.replace(recodes)

    # encode other categorical features
    cat_feats = ["workclass", "education", "marital"]

    for feat in cat_feats:
        encoder = LabelEncoder()
        encoded_col = encoder.fit_transform(df[feat])
        df[feat] = encoded_col

    # rename columns to be consistent with ACSIncome
    df.columns = ["AGEP", "COW", "SCHL", "MAR", "WKHP", "SEX", "INCOME"]
    df.to_csv("uci_adult.csv", columns=["AGEP", "COW", "SCHL", "MAR", "WKHP", "SEX", "INCOME"], index=False)
    print("Done!")



if __name__ == '__main__':
    get_ACSIncome()
    prepare_uci_adult()