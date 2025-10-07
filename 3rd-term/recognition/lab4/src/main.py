import argparse
import sys
from typing import List, Tuple

import numpy as np
from sklearn.svm import SVC

from .bow import (
    train_random_forest,
    create_feature_detector,
    train_vocabulary_kmeans,
    BoWExtractor,
    extract_features_dataset,
    evaluate_classifier,
    train_vocabulary_gmm,
)
from .utils import get_jpg_files_in_directory, create_random_mask


def train_svm_rbf(
        X: np.ndarray,
        y: np.ndarray,
        C: float = 1.0,
        gamma: str = 'scale',
        seed: int = 42
) -> SVC:
    """Обучает SVM с RBF ядром."""
    clf = SVC(kernel='rbf', C=C, gamma=gamma, random_state=seed)
    clf.fit(X, y)
    return clf


def find_misclassified(
        image_paths: List[str],
        y_true: np.ndarray,
        y_pred: np.ndarray
) -> set[Tuple[str, int, int]]:
    """Находит неправильно классифицированные изображения."""
    return set(
        (path, true, pred)
        for path, true, pred in zip(image_paths, y_true, y_pred)
        if true != pred
    )


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
        "detector",
        type=str,
        choices=["SIFT"],
        help="Feature detector",
    )
    parser.add_argument(
        "descriptor",
        type=str,
        choices=["SIFT", "AKAZE", "ORB"],
        help="Descriptor type",
    )
    parser.add_argument(
        "voc_size",
        type=int,
        help="Vocabulary size",
    )
    parser.add_argument(
        "train_proportion",
        type=float,
        help="Train proportion (0.0–1.0)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for reproducibility",
    )
    return parser.parse_args()


def main():
    args = parse_args()

    try:
        files1 = get_jpg_files_in_directory(args.folder1)
        files2 = get_jpg_files_in_directory(args.folder2)
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    if not files1 or not files2:
        print("Error: One or both directories are empty.", file=sys.stderr)
        sys.exit(1)

    all_files = files1 + files2
    labels = np.array([1] * len(files1) + [-1] * len(files2), dtype=np.int32)

    split_mask = create_random_mask(
        len(all_files), args.train_proportion, seed=args.seed,
    )

    detector = create_feature_detector(args.detector)

    print("=== Training vocabulary with K-Means...")
    vocab_kmeans = train_vocabulary_kmeans(
        all_files, split_mask, detector, args.voc_size,
    )
    bow_kmeans = BoWExtractor(vocab_kmeans)

    X_train, train_indices = extract_features_dataset(
        all_files, split_mask, detector, bow_kmeans
    )
    y_train = labels[train_indices]

    print("=== Training classifiers...")
    rf_clf = train_random_forest(
        X_train, y_train, n_estimators=200, seed=args.seed,
    )
    svm_clf = train_svm_rbf(X_train, y_train, C=1.0, seed=args.seed)

    test_mask = [not m for m in split_mask]
    X_test, test_indices = extract_features_dataset(
        all_files, test_mask, detector, bow_kmeans
    )
    y_test = labels[test_indices]

    rf_pred = rf_clf.predict(X_test)
    svm_pred = svm_clf.predict(X_test)

    rf_error = evaluate_classifier(y_test, rf_pred)
    svm_error = evaluate_classifier(y_test, svm_pred)

    print(f"\n=== Results (Vocabulary: K-Means, Size: {args.voc_size}) ===")
    print(f"Random Forest error: {rf_error:.4f}")
    print(f"SVM (RBF) error:     {svm_error:.4f}")

    misclassified_rf = find_misclassified(
        [all_files[i] for i in test_indices], y_test, rf_pred
    )
    if misclassified_rf:
        print(
            f"\nMisclassified by Random Forest ({len(misclassified_rf)} "\
            "items):"
        )
        for path, true, pred in misclassified_rf:
            print(f"  {path} | True: {true}, Pred: {pred}")
    else:
        print("\nRandom Forest: No misclassifications!")

    print("\n=== Training with GMM vocabulary...")
    try:
        vocab_gmm = train_vocabulary_gmm(
            all_files, split_mask, detector, args.voc_size, seed=args.seed
        )
        bow_gmm = BoWExtractor(vocab_gmm)
        X_train_gmm, _ = extract_features_dataset(
            all_files, split_mask, detector, bow_gmm
        )
        rf_gmm = train_random_forest(X_train_gmm, y_train, seed=args.seed)
        X_test_gmm, _ = extract_features_dataset(
            all_files, test_mask, detector, bow_gmm
        )
        pred_gmm = rf_gmm.predict(X_test_gmm)
        gmm_error = evaluate_classifier(y_test, pred_gmm)
        print(f"GMM + Random Forest error: {gmm_error:.4f}")
    except Exception as e:
        print(f"GMM training failed: {e}")

    print("\nDone.")


if __name__ == "__main__":
    main()
