import typing as t
import numpy as np


class CmyChannels(t.TypedDict):
    cyan: np.ndarray[np.float64]
    magenta: np.ndarray[np.float64]
    yellow: np.ndarray[np.float64]


class BrightnessAdjustment(t.TypedDict):
    cyan: float
    magenta: float
    yellow: float
