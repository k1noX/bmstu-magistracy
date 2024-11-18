import typing

from sqlalchemy import Column, Integer, String

import dataclasses as dc
from .base import BaseOrmMappedModel


@dc.dataclass
class Customer(BaseOrmMappedModel):
    """."""

    __tablename__ = 'customers'
    fullname: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    email: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    phone: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    id: int | None = dc.field(
        default=None,
        metadata={'sa': Column(Integer, primary_key=True, autoincrement=True)},
    )


BaseOrmMappedModel.REGISTRY.mapped(Customer)


class CustomerCreation(typing.TypedDict):
    """."""

    full_name: str
    email: str
    phone: str
