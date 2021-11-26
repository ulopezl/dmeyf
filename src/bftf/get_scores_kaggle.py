import pandas as pd

filename = 'DM EyF Segunda Kaggle.txt'
first_split_by = 'E1006'
get_score_split = 'Message'
experimento = 'E1006'
output_filename = f'ganancias_kaggle_{experimento}.csv'
with open(filename, 'r') as f:
    contents = f.read()

data = []
for chunk_of_text in contents.split(first_split_by):
    if '<https://www.kaggle.com/submissions' in chunk_of_text:
        print(chunk_of_text)
        semilla = chunk_of_text.split('_semilla_')[1].split('.csv')[0]
        score = chunk_of_text.split('Message')[1].split('\n')[1]
        data.append({'semilla':semilla,'score':score})
        print(f'score: {score}')
        print(f'semilla: {semilla}')
        print('PEDAZO DE TEXTO','\n'*5)

df = pd.DataFrame(data)
df.to_csv(output_filename)