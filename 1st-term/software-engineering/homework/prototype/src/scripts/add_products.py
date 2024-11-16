import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
sys.path.append(str(project_root))

from models.database import Product, SessionLocal


def add_products():
    products = [
        Product(name="iPhone 15", description="Новый iPhone 15 с OLED экраном, улучшенной камерой и процессором A17.", price=999.99, image_url="static/images/iphone_15.jpg"),
        Product(name="MacBook Air M2", description="Легкий и мощный ноутбук с чипом M2, идеально подходит для работы и развлечений.", price=1199.99, image_url="static/images/macbook_air_m2.jpg"),
        Product(name="AirPods Pro 2", description="Беспроводные наушники с активным шумоподавлением и улучшенным качеством звука.", price=249.99, image_url="static/images/airpods_pro_2.jpg"),
        Product(name="Samsung Galaxy S23", description="Смартфон с AMOLED экраном и камерами высокого разрешения.", price=899.99, image_url="static/images/samsung_galaxy_s23.jpg"),
        Product(name="iRobot Roomba 960", description="Интеллектуальный робот-пылесос с системой навигации и возможностью управления через приложение.", price=499.99, image_url="static/images/irobot_roomba_960.jpg")
    ]
    
    # Добавление товаров в сессию
    with SessionLocal() as session:
        session.add_all(products)
        session.commit()


if __name__ == "__main__":
    add_products()
    print("Товары добавлены в базу данных.")