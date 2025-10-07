import argparse
import os
from typing import List, Tuple

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from bow import (
    create_feature_detector,
    train_vocabulary_kmeans,
    BoWExtractor,
    extract_features_dataset,
    evaluate_classifier,
)
from sklearn.ensemble import RandomForestClassifier
from utils import get_jpg_files_in_directory, create_random_mask

SEEDS = [42, 101, 2024]
DETECTORS = ["SIFT", "ORB", "AKAZE"]
VOC_SIZES = [10, 25, 50, 100]
RF_N_ESTIMATORS = [50, 100, 200, 500]
RF_MAX_DEPTHS = [5, 10, 20, None]


def load_data(folder_1: str, folder_2: str) -> Tuple[List[str], np.ndarray]:
    """Загружает изображения и метки."""
    files1 = get_jpg_files_in_directory(folder_1)
    files2 = get_jpg_files_in_directory(folder_2)
    all_files = files1 + files2
    labels = np.array([1] * len(files1) + [-1] * len(files2), dtype=np.int32)
    return all_files, labels


def run_single_experiment(
        all_files: List[str],
        labels: np.ndarray,
        detector_name: str,
        voc_size: int,
        n_estimators: int,
        max_depth: int | None,
        seed: int,
        train_proportion: float,
) -> float:
    """Выполняет один эксперимент и возвращает ошибку."""
    try:
        split_mask = create_random_mask(
            len(all_files), train_proportion, seed=seed,
        )

        detector = create_feature_detector(detector_name)

        vocab = train_vocabulary_kmeans(
            all_files, split_mask, detector, voc_size,
        )
        bow = BoWExtractor(vocab)

        X_train, train_idx = extract_features_dataset(
            all_files, split_mask, detector, bow,
        )
        y_train = labels[train_idx]
        X_test, test_idx = extract_features_dataset(
            all_files,
            [not m for m in split_mask],
            detector,
            bow,
        )
        y_test = labels[test_idx]

        if X_train.size == 0 or X_test.size == 0:
            return np.nan

        clf = RandomForestClassifier(
            n_estimators=n_estimators,
            max_depth=max_depth,
            random_state=seed,
            n_jobs=-1
        )
        clf.fit(X_train, y_train)
        y_pred = clf.predict(X_test)

        return evaluate_classifier(y_test, y_pred)

    except Exception as e:
        print(
            f"Failed: {detector_name}, voc={voc_size}, n_est={n_estimators}, "
            f"depth={max_depth}, seed={seed} | {e}"
        )
        return np.nan


def run_detector_voc_experiment(
        folder_1: str,
        folder_2: str,
        train_proportion: float,
) -> pd.DataFrame:
    """Исследование: детектор + размер словаря."""
    all_files, labels = load_data(folder_1, folder_2)
    results = []

    for det in DETECTORS:
        for voc in VOC_SIZES:
            errors = []
            for seed in SEEDS:
                err = run_single_experiment(
                    all_files, labels, det, voc,
                    n_estimators=200, max_depth=None, seed=seed,
                    train_proportion=train_proportion,
                )
                if not np.isnan(err):
                    errors.append(err)
            if errors:
                results.append({
                    "detector": det,
                    "voc_size": voc,
                    "mean_error": np.mean(errors),
                    "std_error": np.std(errors)
                })
    return pd.DataFrame(results)


def run_rf_params_experiment(
        folder_1: str,
        folder_2: str,
        train_proportion: float,
) -> pd.DataFrame:
    """Исследование: параметры Random Forest."""
    all_files, labels = load_data(folder_1, folder_2)
    results = []

    best_det = "SIFT"
    best_voc = 50

    for n_est in RF_N_ESTIMATORS:
        for depth in RF_MAX_DEPTHS:
            errors = []
            for seed in SEEDS:
                err = run_single_experiment(
                    all_files, labels, best_det, best_voc,
                    n_estimators=n_est, max_depth=depth, seed=seed,
                    train_proportion=train_proportion,
                )
                if not np.isnan(err):
                    errors.append(err)
            if errors:
                results.append({
                    "n_estimators": n_est,
                    "max_depth": str(depth) if depth is not None else "None",
                    "mean_error": np.mean(errors),
                    "std_error": np.std(errors)
                })
    return pd.DataFrame(results)


def plot_detector_voc(df: pd.DataFrame):
    """Строит график зависимости ошибки от детектора и размера словаря."""
    plt.figure(figsize=(10, 6))
    for det in df["detector"].unique():
        subset = df[df["detector"] == det]
        plt.errorbar(
            subset["voc_size"], subset["mean_error"],
            yerr=subset["std_error"], label=det, marker="o",
        )
    plt.xlabel("Vocabulary Size")
    plt.ylabel("Misclassification Error")
    plt.title("Error vs Vocabulary Size by Detector")
    plt.legend()
    plt.grid(True, linestyle="--", alpha=0.7)
    plt.tight_layout()
    plt.savefig("../results/detector_voc_error.png", dpi=150)
    plt.show()


def plot_rf_params(df: pd.DataFrame):
    """Тепловая карта ошибки для параметров RF."""
    df_pivot = df.pivot(index="max_depth", columns="n_estimators",
                        values="mean_error")
    plt.figure(figsize=(8, 6))
    sns.heatmap(df_pivot, annot=True, fmt=".4f", cmap="viridis",
                cbar_kws={'label': 'Error'})
    plt.title("Random Forest: Error vs Parameters")
    plt.tight_layout()
    plt.savefig("../results/rf_params_error.png", dpi=150)
    plt.show()


def parse_args():
    parser = argparse.ArgumentParser(
        description="Bag-of-Words Image Classifier for two classes."
    )
    parser.add_argument(
        "folder1",
        type=str,
        help="Path to first class directory",
    )
    parser.add_argument(
        "folder2",
        type=str,
        help="Path to second class directory",
    )
    parser.add_argument(
        "train_proportion",
        type=float,
        help="Train proportion (0.0–1.0)",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    print("Running detector + vocabulary size experiment...")
    df1 = run_detector_voc_experiment(
        folder_1=args.folder1,
        folder_2=args.folder2,
        train_proportion=args.train_proportion,
    )
    os.makedirs('../results', exist_ok=True)
    df1.to_csv("../results/detector_voc_results.csv", index=False)
    print(df1)
    plot_detector_voc(df1)

    print("\nRunning Random Forest parameters experiment...")
    df2 = run_rf_params_experiment(
        folder_1=args.folder1,
        folder_2=args.folder2,
        train_proportion=args.train_proportion,
    )
    df2.to_csv("../results/rf_params_results.csv", index=False)
    print(df2)
    plot_rf_params(df2)


if __name__ == "__main__":
    main()