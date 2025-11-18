from PIL import Image, ImageDraw, ImageFont, ImageFilter
import os

# Параметры изображения
width = 10
height = 16
font_size = 20
threshold = 128
output_dir = "../data/alphabet/"

if not os.path.exists(output_dir):
    os.makedirs(output_dir)

alphabet = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

font = ImageFont.truetype("Silkscreen.ttf", font_size)

for letter in alphabet:
    img = Image.new('L', (width, height), color=255)
    draw = ImageDraw.Draw(img)
    
    text_width = draw.textlength(letter, font=font)
    text_height = font.size
    x = (width - text_width) / 2
    y = (height - text_height) / 2
    
    draw.text((x, y), letter, font=font)
    img = img.filter(ImageFilter.SHARPEN)
    threshold = 200
    binary_img = img.point(lambda p: 255 if p > threshold else 0, mode='1')
    
    img.save(f"{output_dir}{letter}.png")

print("Генерация завершена!")