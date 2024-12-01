from sqlalchemy.orm import Session
import typing as t

from models import Product, ProductCreation
from models.exception import ProductNotFoundError, ProductDeletionError, \
    ProductCreationError


class ProductsService:
    """Service for managing products."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """Initialize the service with a session acquirer."""
        self._session_acquirer = session_acquirer

    def get_all(self) -> list[Product]:
        """Retrieve all products."""
        session = self._session_acquirer()
        products = session.query(Product).all()
        return products # noqa

    def get_by_id(self, product_id: int) -> Product:
        """Retrieve a product by ID."""
        session = self._session_acquirer()
        product = session.query(Product).filter_by(id=product_id).first()
        if not product:
            raise ProductNotFoundError(product_id=product_id)
        return product # noqa

    def add(self, creation: ProductCreation) -> Product:
        """Add a new product."""
        session = self._session_acquirer()

        try:
            product = Product(**creation)
            session.add(product)
            session.commit()
            session.refresh(product)
            return product
        except Exception:
            session.rollback()
            raise ProductCreationError

    def remove(self, ident: int) -> Product:
        """Remove a product by ID."""
        session = self._session_acquirer()

        try:
            product = session.query(Product).filter_by(id=ident).first()
            if not product:
                raise ProductNotFoundError(product_id=ident)
            session.delete(product)
            session.commit()
            return product # noqa
        except ProductNotFoundError as e:
            session.rollback()
            raise e
        except Exception:
            session.rollback()
            raise ProductDeletionError(product_id=ident)
