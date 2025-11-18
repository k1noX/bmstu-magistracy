import os
from pathlib import Path

def move_report_files_to_docs_same_dir(directory):
    directory_path = Path(directory)

    if not directory_path.is_dir():
        print(f"Ошибка: {directory} — не директория или не существует.")
        return

    # Найти все файлы вида report.<ext> рекурсивно
    for file_path in directory_path.rglob('report.*'):
        if file_path.is_file():
            # Папка docs в той же директории, где находится файл
            parent_dir = file_path.parent

            if parent_dir.name == 'docs':
                continue  # уже в нужной папке — пропустить

            docs_path = parent_dir / 'docs'
            docs_path.mkdir(exist_ok=True)  # создать, если нет

            # Определить новое имя (на случай, если файл с таким именем уже есть в docs)
            target_path = docs_path / file_path.name

            if target_path.exists():
                continue

            try:
                file_path.rename(target_path)
                print(f"Перемещён: {file_path} -> {target_path}")
            except Exception as e:
                print(f"Ошибка при перемещении {file_path}: {e}")

# Пример использования:
move_report_files_to_docs_same_dir("./")