import dataclasses as dc
import typing
from decimal import Decimal

from sqlalchemy import Column, Integer, String, Numeric, Text

from .base import BaseOrmMappedModel


@dc.dataclass
class Product(BaseOrmMappedModel):
    """."""

    __tablename__ = 'products'

    name: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    price: Decimal = dc.field(
        metadata={'sa': Column(Numeric, nullable=False)},
    )
    description: str = dc.field(
        metadata={'sa': Column(Text, nullable=False)},
    )
    id: int | None = dc.field(
        default=None,
        metadata={'sa': Column(Integer, primary_key=True, autoincrement=True)},
    )


BaseOrmMappedModel.REGISTRY.mapped(Product)


class ProductCreation(typing.TypedDict):
    """."""

    name: str
    price: Decimal
    description: str
