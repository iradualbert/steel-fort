# importing pandas module
import pandas as pd

# input excel file path
inputExcelFile ="Steel Sections.xlsx"

# Reading an excel file
excelFile = pd.read_excel (inputExcelFile)

# Converting excel file into CSV file
excelFile.to_csv ("Steel Sections.csv", index = None, header=True)

