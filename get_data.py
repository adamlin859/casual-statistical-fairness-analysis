from folktables import ACSDataSource, ACSIncome
import numpy as np
import pandas as pd

def get_data(states=['NY']):
    data_source = ACSDataSource(survey_year='2018', horizon='1-Year', survey='person')
    data = data_source.get_data(states=states, download=True)
    features, labels, _ = ACSIncome.df_to_numpy(data)

    # save as pandas df
    data_cat = np.c_[features, labels]
    feats = ['AGEP', 'COW', 'SCHL', 'MAR', 'OCCP', 'POBP', 'RELP', 'WKHP', 'SEX', 'RAC1P', 'TARGET']
    df = pd.DataFrame(data_cat, columns=feats)
    df.to_csv('ACSIncome_NY_2018.csv', index=False)


if __name__ == '__main__':
    get_data()