import glob
import os
import random


def get_jpg_files_in_directory(directory: str) -> list[str]:
    """Получает список всех JPG/JPEG файлов в указанной директории."""
    if not os.path.isdir(directory):
        raise FileNotFoundError(f"Directory not found: {directory}")

    pattern = os.path.join(directory, "*.jpg")
    files = glob.glob(pattern, recursive=False)
    pattern_upper = os.path.join(directory, "*.JPG")
    files += glob.glob(pattern_upper, recursive=False)
    return sorted(files)

def create_random_mask(
        size: int,
        true_probability: float,
        seed: int | None = None,
) -> list[bool]:
    """
    Создаёт булевский вектор заданного размера с заданной вероятностью True.
    """
    if not (0.0 <= true_probability <= 1.0):
        raise ValueError("true_probability must be in [0.0, 1.0]")

    if seed is not None:
        random.seed(seed)

    return [random.random() < true_probability for _ in range(size)]
