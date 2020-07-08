import os
import pandas as pd

def find_variables_file(alternate_csv_file):    
    # Default path for variables.csv
    dir_path = os.path.dirname(os.path.realpath(__file__))
    csv_file = os.path.join(dir_path, 'variables.csv')
    variables = pd.read_csv(csv_file,
                names=['name','lsd','min_value','max_value'],
                header=0, index_col=0)

    # Check for an extra variables.csv file in the cases folder
    # If a variable is defined in both, this one overwrites the default one
    if os.path.exists(alternate_csv_file):
        variables_bis = pd.read_csv(alternate_csv_file,
                        names=['name','lsd','min_value','max_value'],
                        header=0, index_col=0)
        variables = variables_bis.combine_first(variables)

    return variables


