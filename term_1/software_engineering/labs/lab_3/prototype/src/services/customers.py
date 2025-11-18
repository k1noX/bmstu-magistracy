from sqlalchemy.orm import Session
import typing as t

from models import Customer, CustomerCreation
from models.exception import (
    CustomerNotFoundError,
    CustomerCreationError,
    CustomerDeletionError,
)


class CustomersService:
    """Service for managing customers."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """Initialize the service with a session acquirer."""
        self._session_acquirer = session_acquirer

    def get_all(self) -> list[Customer]:
        """Retrieve all customers."""
        session = self._session_acquirer()
        customers = session.query(Customer).all()
        return customers  # noqa

    def get_by_id(self, customer_id: int) -> Customer:
        """Retrieve a customer by ID."""
        session = self._session_acquirer()
        customer = session.query(Customer).filter_by(id=customer_id).first()
        if not customer:
            raise CustomerNotFoundError(customer_id=customer_id)
        return customer # noqa

    def add(self, creation: CustomerCreation) -> Customer:
        """Add a new customer."""
        session = self._session_acquirer()

        try:
            customer = Customer(**creation)
            session.add(customer)
            session.commit()
            session.refresh(customer)
            return customer  # noqa
        except Exception:
            session.rollback()
            raise CustomerCreationError

    def remove(self, ident: int) -> Customer:
        """Remove a customer by ID."""
        session = self._session_acquirer()

        try:
            customer = session.query(Customer).filter_by(id=ident).first()
            if not customer:
                raise CustomerNotFoundError(customer_id=ident)
            session.delete(customer)
            session.commit()
            return customer  # noqa
        except CustomerNotFoundError as e:
            session.rollback()
            raise e
        except Exception:
            session.rollback()
            raise CustomerDeletionError(customer_id=ident)
