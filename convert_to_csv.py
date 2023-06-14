# importing pandas module
import pandas as pd

# input excel file path
inputExcelFile ="./"

# Reading an excel file
excelFile = pd.read_excel (inputExcelFile)

# Converting excel file into CSV file
excelFile.to_csv ("", index = None, header=True)

