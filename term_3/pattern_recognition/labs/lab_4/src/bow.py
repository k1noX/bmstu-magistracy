from typing import List, Tuple, Optional

import cv2
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
from sklearn.mixture import GaussianMixture


class BoWExtractor:
    """Класс для извлечения признаков Bag-of-Words."""

    def __init__(self, vocabulary: np.ndarray):
        """."""
        if vocabulary.ndim != 2:
            raise ValueError("Vocabulary must be 2D array (K, D)")
        self.vocabulary = vocabulary.astype(np.float32)
        self.matcher = cv2.BFMatcher(cv2.NORM_L2, crossCheck=False)

    def __call__(self, descriptors: Optional[np.ndarray]) -> np.ndarray:
        """Преобразует дескрипторы изображения в гистограмму BoW."""
        if descriptors is None or descriptors.shape[0] == 0:
            return np.zeros(self.vocabulary.shape[0], dtype=np.float32)

        descriptors = descriptors.astype(np.float32)
        matches = self.matcher.match(descriptors, self.vocabulary)
        indices = [m.trainIdx for m in matches]
        hist, _ = np.histogram(
            indices,
            bins=np.arange(self.vocabulary.shape[0] + 1),
        )
        return hist.astype(np.float32)


def create_feature_detector(detector_name: str) -> cv2.Feature2D:
    """Создаёт детектор/дескриптор по имени."""
    if detector_name.upper() == "SIFT":
        return cv2.SIFT.create()
    elif detector_name.upper() == "AKAZE":
        return cv2.AKAZE.create()
    elif detector_name.upper() == "ORB":
        return cv2.ORB.create()
    else:
        msg = f"Unsupported detector {detector_name}, only 'SIFT' is supported."
        raise ValueError(msg)


def train_vocabulary_kmeans(
        image_paths: List[str],
        mask: List[bool],
        detector: cv2.Feature2D,
        vocab_size: int,
) -> np.ndarray:
    """Обучает словарь дескрипторов с использованием K-Means."""
    all_descriptors = []

    for path, use_for_vocab in zip(image_paths, mask):
        if not use_for_vocab:
            continue
        img = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
        if img is None:
            continue
        _, descriptors = detector.detectAndCompute(img, None)
        if descriptors is not None:
            all_descriptors.append(descriptors.astype(np.float32))

    if not all_descriptors:
        raise ValueError("No descriptors collected for vocabulary training")

    all_descriptors = np.vstack(all_descriptors)
    msg = f"Collected {all_descriptors.shape[0]} descriptors for vocabulary "\
          "training."
    print(msg)

    criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 100, 0.1)
    _, _, centers = cv2.kmeans(
        data=all_descriptors,
        K=vocab_size,
        bestLabels=None,
        criteria=criteria,
        attempts=3,
        flags=cv2.KMEANS_RANDOM_CENTERS
    )
    return centers


def train_vocabulary_gmm(
        image_paths: List[str],
        mask: List[bool],
        detector: cv2.Feature2D,
        vocab_size: int,
        seed: int = 42
) -> np.ndarray:
    """Обучает словарь с использованием Gaussian Mixture Model."""
    all_descriptors = []
    for path, use_for_vocab in zip(image_paths, mask):
        if not use_for_vocab:
            continue
        img = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
        if img is None:
            continue
        _, descriptors = detector.detectAndCompute(img, None)
        if descriptors is not None:
            all_descriptors.append(descriptors.astype(np.float32))

    if not all_descriptors:
        raise ValueError("No descriptors collected for GMM training")

    all_descriptors = np.vstack(all_descriptors)
    print(f"Collected {all_descriptors.shape[0]} descriptors for GMM training.")

    gmm = GaussianMixture(
        n_components=vocab_size,
        covariance_type='full',
        reg_covar=1e-3,
        random_state=seed,
        max_iter=100
    )
    gmm.fit(all_descriptors)
    return gmm.means_.astype(np.float32)


def extract_features_dataset(
        image_paths: List[str],
        mask: List[bool],
        detector: cv2.Feature2D,
        bow_extractor: BoWExtractor
) -> Tuple[np.ndarray, np.ndarray]:
    """Извлекает признаки для подмножества изображений."""
    features = []
    selected_indices = []

    for i, (path, selected) in enumerate(zip(image_paths, mask)):
        if not selected:
            continue
        img = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
        if img is None:
            features.append(
                np.zeros(bow_extractor.vocabulary.shape[0], dtype=np.float32)
            )
        else:
            _, descriptors = detector.detectAndCompute(img, None)
            hist = bow_extractor(descriptors)
            features.append(hist)
        selected_indices.append(i)

    return np.array(features, dtype=np.float32), np.array(selected_indices)


def train_random_forest(
        X: np.ndarray,
        y: np.ndarray,
        n_estimators: int = 200,
        max_depth: Optional[int] = None,
        seed: int = 42
) -> RandomForestClassifier:
    """Обучает Random Forest."""
    clf = RandomForestClassifier(
        n_estimators=n_estimators,
        max_depth=max_depth,
        random_state=seed,
        n_jobs=-1
    )
    clf.fit(X, y)
    return clf


def evaluate_classifier(
        y_true: np.ndarray,
        y_pred: np.ndarray
) -> float:
    """Вычисляет ошибку классификации."""
    return 1.0 - accuracy_score(y_true, y_pred)
