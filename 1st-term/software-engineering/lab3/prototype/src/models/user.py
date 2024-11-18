from sqlalchemy import Column, Integer, String
from flask_login import UserMixin

from .base import BaseOrmMappedModel
import dataclasses as dc


@dc.dataclass
class User(BaseOrmMappedModel, UserMixin):
    """."""

    __tablename__ = 'users'
    username: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    fullname: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    position: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    password: str = dc.field(
        metadata={'sa': Column(String, nullable=False)},
    )
    id: int | None = dc.field(
        default=None,
        metadata={'sa': Column(Integer, primary_key=True, autoincrement=True)},
    )

    def is_authenticated(self):
        return True

    def is_active(self):
        return True

    def is_anonymous(self):
        return False

    def get_id(self):
        return self.id


BaseOrmMappedModel.REGISTRY.mapped(User)
