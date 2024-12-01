import dataclasses as dc
import datetime
import typing
from decimal import Decimal

from sqlalchemy import (
    Column,
    Integer,
    Numeric,
    ForeignKey,
    String,
    DateTime,
    func,
)
from sqlalchemy.orm import relationship

from . import User, Customer
from .base import BaseOrmMappedModel, BaseModel


@dc.dataclass
class Order(BaseOrmMappedModel):
    """."""

    __tablename__ = 'orders'
    customer_id: int = dc.field(
        metadata={
            'sa': Column(
                Integer,
                ForeignKey('customers.id', ondelete='SET NULL'),
            ),
        },
    )
    employee_id: int = dc.field(
        metadata={
            'sa': Column(
                Integer,
                ForeignKey('users.id', ondelete='SET NULL'),
            ),
        },
    )
    id: int | None = dc.field(
        default=None,
        metadata={'sa': Column(Integer, primary_key=True, autoincrement=True)},
    )
    total_price: Decimal = dc.field(
        default=Decimal(0),
        metadata={
            'sa': Column(Numeric, nullable=False),
        },
    )
    created_at: datetime.datetime | None = dc.field(
        default=None,
        metadata={'sa': Column(DateTime, server_default=func.now())},
    )

    order_items = relationship(
        'OrderItems',
        back_populates='order',
        cascade='all, delete-orphan',
        passive_deletes=True,
    )


BaseOrmMappedModel.REGISTRY.mapped(Order)


@dc.dataclass
class OrderItems(BaseOrmMappedModel):
    """."""
    __tablename__ = 'order_items'

    order_id: int = dc.field(
        metadata={
            'sa': Column(Integer, ForeignKey('orders.id'), nullable=False),
        },
    )
    product_id: int = dc.field(
        metadata={
            'sa': Column(
                Integer,
                ForeignKey('products.id', ondelete='SET NULL'),
            ),
        },
    )
    name: str = dc.field(
        metadata={
            'sa': Column(String, nullable=False),
        },
    )
    quantity: int = dc.field(
        metadata={
            'sa': Column(Integer, nullable=False),
        },
    )
    price: Decimal = dc.field(
        metadata={
            'sa': Column(Numeric, nullable=False),
        },
    )
    id: int | None = dc.field(
        default=None,
        metadata={'sa': Column(Integer, primary_key=True, autoincrement=True)},
    )

    order = relationship('Order', back_populates='order_items')


BaseOrmMappedModel.REGISTRY.mapped(OrderItems)


class OrderItemCreation(typing.TypedDict):
    """."""
    product_id: int
    quantity: int


class OrderCreation(typing.TypedDict):
    """."""
    items: list[OrderItemCreation]
    customer_id: int


@dc.dataclass
class OrderItemView(BaseModel):
    """."""
    name: str
    product_id: int
    quantity: int
    price: Decimal
    total_price: Decimal


@dc.dataclass
class OrderView(BaseModel):
    """."""
    order_id: int
    total_price: Decimal
    employee: User | None
    customer: Customer | None
    created_at: datetime.datetime | None
    order_items: list[OrderItemView] = dc.field(default_factory=list)
