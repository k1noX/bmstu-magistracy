import typing as t

import flask
from flask import render_template
from sqlalchemy.orm import Session

from models import Order, OrderCreation, Product, Customer, User
from models.order import OrderItems
from weasyprint import HTML


class OrdersService:
    """."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """."""
        self._session_acquirer = session_acquirer

    def add(self, creation: OrderCreation, employee_id: int) -> Order:
        session = self._session_acquirer()
        customer = session.query(Customer).get(creation['customer_id'])

        if not customer:
            raise ValueError(
                f"Customer with ID {creation['customer_id']} not found.",
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
                raise ValueError(
                    f"Product with ID {item['product_id']} not found.",
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

        new_order.total_price = total_price
        session.commit()

        return new_order

    def get_all(self) -> list[dict]:
        session = self._session_acquirer()
        orders = session.query(Order).all()

        res = []

        for order in orders:
            order_data = {
                "order_id": order.id,
                "total_price": f'{order.total_price:,.2f}',
                "order_items": [],
            }

            customer = session.query(Customer).get(order.customer_id)

            order_data["customer_name"] = customer.fullname \
                if customer is not None else ''

            employee = session.query(User).get(order.employee_id)

            order_data["employee_name"] = employee.fullname \
                if employee is not None else ''

            for item in order.order_items:
                order_data["order_items"].append({
                    "name": item.name,
                    "product_id": item.product_id,
                    "quantity": item.quantity,
                    "price": f'{item.price:,.2f}',
                    "total_price": f'{item.price * item.quantity:,.2f}',
                })

            res.append(order_data)

        return res

    def remove(self, ident: int) -> Order:
        session = self._session_acquirer()
        order = session.query(Order).filter_by(id=ident).first()
        session.query(OrderItems).filter_by(order_id=order.id).delete()
        session.delete(order)
        session.commit()
        return order # noqa

    def get_invoice(self, ident: int) -> flask.Response:
        session = self._session_acquirer()
        order = session.query(Order).get(ident)

        if not order:
            return flask.abort(404)

        customer = session.query(Customer).filter(
            Customer.id == order.customer_id).one_or_none()
        employee = session.query(User).filter(
            User.id == order.employee_id).one_or_none()
        order_items = [
            {
                "product_id": item.product_id,
                "name": item.name,
                "quantity": item.quantity,
                "price": f'{item.price:,.2f}',
                "total_price": f'{item.quantity * item.price:,.2f}',
                "total_numeric_price": item.quantity * item.price,
            }
            for item in order.order_items
        ]
        total_price = sum(item["total_numeric_price"] for item in order_items)

        rendered_html = render_template(
            'invoice.html',
            order=order,
            customer=customer,
            employee=employee,
            order_items=order_items,
            total_price=f'{total_price:,.2f}',
        )
        pdf = HTML(string=rendered_html).write_pdf()

        response = flask.make_response(pdf)
        response.headers['Content-Type'] = 'application/pdf'
        response.headers['Content-Disposition'] = \
            f'inline; filename=invoice_{order.id}.pdf'

        return response