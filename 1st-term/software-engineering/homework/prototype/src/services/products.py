from models.database import db_session
from models.database import Product


class ProductsService:
    """
    Сервис для управления данными о продуктах.
    """

    @classmethod
    def get_all_products(cls):
        """
        Возвращает список всех продуктов из базы данных.
        """
        with db_session() as session:
            products = session.query(Product).all()
            return [
                {
                    "id": product.id,
                    "name": product.name,
                    "description": product.description,
                    "price": product.price,
                    "image_url": product.image_url,  # Добавляем изображение в результат
                }
                for product in products
            ]

    @classmethod
    def get_product_by_id(cls, product_id):
        """
        Возвращает продукт по его ID.
        """
        with db_session() as session:
            product = session.query(Product).filter_by(id=product_id).first()
            if product:
                return {
                    "id": product.id,
                    "name": product.name,
                    "description": product.description,
                    "price": product.price,
                    "image_url": product.image_url,  # Добавляем изображение в результат
                }
            return None
