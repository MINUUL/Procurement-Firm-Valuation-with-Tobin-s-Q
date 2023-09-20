import pandas as pd
import numpy as np
from glob import glob
path = 'C:/Users/0630r/inha2/Research/Dart/Dart_file/support_data/employee/'

list_20 = sorted(glob(path + "/2020/*.xls"))
list_21 = sorted(glob(path + "/2021/*.xls"))
list_22 = sorted(glob(path + "/2022/*.xls"))  
#folders= os.listdir('marcap/data')
#for files in forders:
lst = np.array([list_20, list_21, list_22])

df1 = pd.DataFrame()
n = 1

for file in list_20:
    df = pd.read_excel(file, header = 2)
    df.insert(3, '분기', str(n)+'Q')
    df.insert(2, '연도', 2020)
    df_2020 = pd.concat([df1, df], axis = 0)
    df_2020 = df_2020[['회사명', '종목코드', '분기', '사업부문', '성별', '합계']]
    n += 1

df_2020