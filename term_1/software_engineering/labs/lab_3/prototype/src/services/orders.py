import typing as t
from decimal import Decimal

import flask
from flask import render_template
from sqlalchemy.orm import Session
from models import Order, OrderCreation, Product, Customer, User
from models.exception import (
    CustomerNotFoundError,
    ProductNotFoundError,
    OrderNotFoundError,
    OrderDeletionError,
    OrderCreationError,
)
from models.order import OrderItems, OrderView
from weasyprint import HTML

from models.translator import ModelsTranslator


class OrdersService:
    """Service for managing orders."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """Initialize the service with a session acquirer."""
        self._session_acquirer = session_acquirer

    def add(self, creation: OrderCreation, employee_id: int) -> OrderView:
        """Add a new order."""
        session = self._session_acquirer()

        try:
            customer = session.query(Customer).get(creation['customer_id'])
            if not customer:
                raise CustomerNotFoundError(
                    customer_id=creation['customer_id'],
                )

            new_order = Order(
                customer_id=creation['customer_id'],
                employee_id=employee_id,
            )
            session.add(new_order)
            session.commit()

            total_price = 0
            for item in creation['items']:
                product = session.query(Product).get(item['product_id'])
                if not product:
                    raise ProductNotFoundError(
                        product_id=creation['customer_id'],
                    )

                order_item = OrderItems(
                    order_id=new_order.id,
                    product_id=item['product_id'],
                    quantity=int(item['quantity']),
                    price=product.price,
                    name=product.name,
                )
                session.add(order_item)
                total_price += product.price * int(item['quantity'])

            new_order.total_price = Decimal(total_price)
            session.commit()

            return ModelsTranslator.order_view(new_order)  # noqa
        except ProductNotFoundError as e:
            raise e
        except Exception:
            session.rollback()
            raise OrderCreationError

    def get_all(self) -> list[OrderView]:
        """Retrieve all orders with their details."""
        session = self._session_acquirer()
        orders = session.query(Order).all()

        res = []
        for order in orders:
            customer = session.query(Customer).get(order.customer_id)
            employee = session.query(User).get(order.employee_id)
            order_view = ModelsTranslator.order_view(
                order, # noqa
                customer,
                employee,
            )

            res.append(order_view)

        return res

    def remove(self, ident: int) -> OrderView:
        """Remove an order by ID."""
        session = self._session_acquirer()

        try:
            order = session.query(Order).get(ident)
            if not order:
                raise OrderNotFoundError(order_id=ident)
            session.query(OrderItems).filter_by(order_id=order.id).delete()
            session.delete(order)
            session.commit()

            return ModelsTranslator.order_view(order)  # noqa
        except OrderNotFoundError as e:
            raise e
        except Exception:
            session.rollback()
            raise OrderDeletionError(order_id=ident)

    def get_invoice(self, ident: int) -> flask.Response:
        """Generate a PDF invoice for a specific order."""
        session = self._session_acquirer()

        order = session.query(Order).get(ident)
        if not order:
            raise OrderNotFoundError(order_id=ident)

        customer = session.query(Customer).get(order.customer_id)
        employee = session.query(User).get(order.employee_id)
        order_view = ModelsTranslator.order_view(order, customer, employee)

        rendered_html = render_template(
            'invoice.html',
            order=order_view,
        )
        pdf = HTML(string=rendered_html).write_pdf()

        response = flask.make_response(pdf)
        response.headers['Content-Type'] = 'application/pdf'
        response.headers['Content-Disposition'] = \
            f'inline; filename=invoice_{order.id}.pdf'

        return response
