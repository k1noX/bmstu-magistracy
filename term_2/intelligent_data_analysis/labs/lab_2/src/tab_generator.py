import os
import random

dataset_path = r'E:\Dev\flowers ds\flowers'

output_file = 'flowers_dataset.tab'
subset_ratio = 0.2

def write_tab_file(file_name, data):
    with open(file_name, 'w') as f:
        f.write('Path\tClass\n')
        for path, class_name in data:
            f.write(f'{path}\t{class_name}\n')

all_data = []

for class_name in os.listdir(dataset_path):
    class_path = os.path.join(dataset_path, class_name)
    
    if os.path.isdir(class_path):
        images = [os.path.join(class_path, img) for img in os.listdir(class_path)]
        
        all_data.extend([(img, class_name) for img in images])

random.shuffle(all_data)

subset_size = int(len(all_data) * subset_ratio)
subset_data = all_data[:subset_size]

write_tab_file(output_file, subset_data)

print(f'Поддатасет сохранён в {output_file}')
print(f'Количество изображений: {len(subset_data)}')
