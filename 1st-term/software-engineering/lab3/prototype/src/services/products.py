from sqlalchemy.orm import Session
import typing as t

from models import Product, ProductCreation


class ProductsService:
    """."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """."""

        self._session_acquirer = session_acquirer

    def get_all(self) -> list[dict]:
        session = self._session_acquirer()
        customers = session.query(Product).all()
        return [c.to_dict() for c in customers] # noqa

    def get_by_id(self, product_id: int) -> dict | None:
        session = self._session_acquirer()
        product = session.query(Product).filter_by(id=product_id).first()
        if product is None:
            return None
        return product.to_dict() # noqa

    def add(self, creation: ProductCreation) -> dict:
        session = self._session_acquirer()
        product = Product(**creation)
        session.add(product)
        session.commit()
        session.refresh(product)
        return product.to_dict()

    def remove(self, ident: int) -> dict:
        session = self._session_acquirer()
        product = session.query(Product).filter_by(id=ident).first()
        session.delete(product)
        session.commit()
        return product.to_dict() # noqa
