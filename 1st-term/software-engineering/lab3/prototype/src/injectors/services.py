from services.customers import CustomersService
from services.orders import OrdersService
from services.products import ProductsService
from services.users import UsersService
from .connections import connections_injector


class ServicesInjector:
    """."""

    def __init__(self):
        """."""

    @classmethod
    def customers(cls) -> CustomersService:
        """."""
        return CustomersService(connections_injector.acquire_session)

    @classmethod
    def products(cls) -> ProductsService:
        """."""
        return ProductsService(connections_injector.acquire_session)

    @classmethod
    def orders(cls) -> OrdersService:
        """."""
        return OrdersService(connections_injector.acquire_session)

    @classmethod
    def users(cls) -> UsersService:
        """."""
        return UsersService(connections_injector.acquire_session)
