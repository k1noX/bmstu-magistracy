import numpy as np
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QPixmap, QImage
from PyQt6.QtWidgets import (
    QMainWindow, QVBoxLayout, QWidget, QLayout,
    QGroupBox, QSlider, QHBoxLayout, QLabel, QPushButton, QFileDialog,
)

from models.images import BrightnessAdjustment
from services.images import ImageService


class MainWindow(QMainWindow):
    """."""

    def __init__(self):
        super(MainWindow, self).__init__()

        self.setWindowTitle('Brightness Adjustment Tool')

        widget = QWidget()
        layout = self._construct_layout()
        widget.setLayout(layout)
        self.setCentralWidget(widget)
        self._images: ImageService | None = None

    def _construct_image_layout(self):
        self._image_layout = QVBoxLayout()
        self._image_view = QLabel()
        self._image_view.setFixedSize(600, 600)
        self._image_view.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._image_layout.addWidget(self._image_view)
        self.adjustSize()
        return self._image_layout

    def _construct_control_layout(self):
        brightness_control_layout = QVBoxLayout()

        cyan_label = QLabel('Cyan')
        brightness_control_layout.addWidget(cyan_label)
        self._cyan_slider = QSlider(Qt.Orientation.Horizontal)
        self._cyan_slider.setValue(50)
        self._cyan_slider.valueChanged.connect(self._adjust_channel)
        brightness_control_layout.addWidget(self._cyan_slider)

        magenta_label = QLabel('Magenta')
        brightness_control_layout.addWidget(magenta_label)
        self._magenta_slider = QSlider(Qt.Orientation.Horizontal)
        self._magenta_slider.setValue(50)
        self._magenta_slider.valueChanged.connect(self._adjust_channel)
        brightness_control_layout.addWidget(self._magenta_slider)

        yellow_label = QLabel('Yellow')
        brightness_control_layout.addWidget(yellow_label)
        self._yellow_slider = QSlider(Qt.Orientation.Horizontal)
        self._yellow_slider.setValue(50)
        self._yellow_slider.valueChanged.connect(self._adjust_channel)
        brightness_control_layout.addWidget(self._yellow_slider)

        overall_label = QLabel('All Channels')
        brightness_control_layout.addWidget(overall_label)
        self._overall_slider = QSlider(Qt.Orientation.Horizontal)
        self._overall_slider.setValue(50)
        self._overall_slider.valueChanged.connect(self._adjust_overall)
        brightness_control_layout.addWidget(self._overall_slider)

        brightness_control_group = QGroupBox('Brightness Control')
        brightness_control_group.setLayout(brightness_control_layout)

        image_control_group = QGroupBox('Image Loading')
        image_control_layout = QHBoxLayout()
        button = QPushButton('Load Image')
        button.clicked.connect(self._load_image)
        image_control_layout.addWidget(button)
        image_control_group.setLayout(image_control_layout)

        layout = QVBoxLayout()
        layout.addWidget(brightness_control_group)
        layout.addWidget(image_control_group)
        layout.addStretch(1)

        return layout

    def _construct_layout(self) -> QLayout:
        layout = QHBoxLayout()

        layout.addLayout(self._construct_image_layout())
        layout.addLayout(self._construct_control_layout())

        return layout

    def _load_image(self):
        image = QFileDialog.getOpenFileName(
            None, 'OpenFile', '', "Image file(*.jpg)"
            )
        image_path = image[0]
        pixmap = QPixmap(image_path)
        self._initial_image = pixmap
        self._images = ImageService.from_pixmap(self._initial_image)
        self._set_image(self._images.to_pixmap())

    def _set_image(self, pixmap: QPixmap):
        ratio = pixmap.width() / pixmap.height()

        if pixmap.width() < pixmap.height():
            scaled = pixmap.scaled(
                int(600 * ratio),
                600,
                Qt.AspectRatioMode.KeepAspectRatio,
            )
        else:
            scaled = pixmap.scaled(
                600,
                int(600 / ratio),
                Qt.AspectRatioMode.KeepAspectRatio,
            )

        self._image_view.setPixmap(scaled)
        self._image_layout.update()
        self.adjustSize()

    def _get_brightness_adjustment(self) -> BrightnessAdjustment:
        return BrightnessAdjustment(
            cyan=self._cyan_slider.value() - 50,
            yellow=self._yellow_slider.value() - 50,
            magenta=self._magenta_slider.value() - 50,
        )

    def _adjust_channel(self, _: int):
        self._images.adjust_brightness(self._get_brightness_adjustment())
        self._set_image(self._images.adjusted_to_pixmap())

    def _adjust_overall(self, value: int):
        adjustment = BrightnessAdjustment(
            cyan=value - 50,
            yellow=value - 50,
            magenta=value - 50,
        )
        self._images.adjust_brightness(adjustment)
        self._set_image(self._images.adjusted_to_pixmap())
