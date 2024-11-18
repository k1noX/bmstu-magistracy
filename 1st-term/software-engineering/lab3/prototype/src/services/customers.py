from sqlalchemy.orm import Session
import typing as t

from models import Customer, CustomerCreation


class CustomersService:
    """."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """."""

        self._session_acquirer = session_acquirer

    def get_all(self) -> list[dict]:
        session = self._session_acquirer()
        customers = session.query(Customer).all()
        return [c.to_dict() for c in customers] # noqa

    def get_by_id(self, customer_id: int) -> dict | None:
        session = self._session_acquirer()
        customer = session.query(Customer).filter_by(id=customer_id).first()
        if customer is None:
            return None
        return customer.to_dict() # noqa

    def add(self, creation: CustomerCreation) -> dict:
        session = self._session_acquirer()
        customer = Customer(**creation)
        session.add(customer)
        session.commit()
        session.refresh(customer)
        return customer.to_dict()

    def remove(self, ident: int) -> dict:
        session = self._session_acquirer()
        customer = session.query(Customer).filter_by(id=ident).first()
        session.delete(customer)
        session.commit()
        return customer.to_dict() # noqa
