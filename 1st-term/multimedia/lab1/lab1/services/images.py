import typing as t

import numpy as np
from PyQt6.QtGui import QPixmap, QImage

from models.images import CmyChannels, BrightnessAdjustment


class ImageService:

    def __init__(self, rgb_array: np.array):
        self._image: np.ndarray = rgb_array
        self._adjusted: np.ndarray | None = None
        self._channels = None

    @classmethod
    def from_pixmap(cls, pixmap: QPixmap) -> t.Self:
        image = pixmap.toImage()
        image = image.convertToFormat(QImage.Format.Format_RGB888)

        width = image.width()
        height = image.height()
        ptr = image.bits().asstring(width * height * 3)
        image = np.frombuffer(ptr, np.uint8).reshape((height, width, 3))
        return cls(image)

    def adjusted_to_pixmap(self) -> QPixmap:
        return self._to_pixmap(self._adjusted)

    def to_pixmap(self) -> QPixmap:
        return self._to_pixmap(self._image)

    def adjust_brightness(self, adjustment: BrightnessAdjustment) -> np.array:
        channels = self._channels or self._parse_image_channels(self._image)
        for channel, percent in adjustment.items():
            channels[channel] = self._adjust_channel_brightness(
                channel=channels[channel],
                percent=percent,
            )
        self._adjusted = self._convert_cmy_to_rgb(channels)
        return self._adjusted

    @classmethod
    def _to_pixmap(cls, image: np.array) -> QPixmap:
        channels = cls._parse_image_channels(image)
        image = cls._convert_cmy_to_rgb(channels)

        height, width, channels = image.shape
        bytes_per_line = channels * width
        return QPixmap.fromImage(
            QImage(
                image.data, width, height,
                bytes_per_line, QImage.Format.Format_RGB888,
            )
        )

    @classmethod
    def _parse_image_channels(cls, rgb_array: np.array) -> CmyChannels:
        cyan = 255 - rgb_array[:, :, 0].astype(np.float64)
        cyan /= cyan.max() or 1

        magenta = 255 - rgb_array[:, :, 1].astype(np.float64)
        magenta /= magenta.max() or 1

        yellow = 255 - rgb_array[:, :, 2].astype(np.float64)
        yellow /= yellow.max() or 1

        return CmyChannels(
            cyan=cyan,
            magenta=magenta,
            yellow=yellow,
        )

    @classmethod
    def _convert_cmy_to_rgb(cls, channels: CmyChannels) -> np.array:
        red = 255 - (channels['cyan'] * 255).astype(np.uint8)
        green = 255 - (channels['magenta'] * 255).astype(np.uint8)
        blue = 255 - (channels['yellow'] * 255).astype(np.uint8)
        return np.dstack([red, green, blue])

    @classmethod
    def _adjust_channel_brightness(
            cls,
            channel: np.ndarray[np.float64],
            percent: float,
    ) -> np.ndarray[np.uint8]:
        multiplicative = 1 - percent / 100
        channel *= multiplicative
        channel[channel > 1] = 1
        channel[channel < 0] = 0
        return channel
