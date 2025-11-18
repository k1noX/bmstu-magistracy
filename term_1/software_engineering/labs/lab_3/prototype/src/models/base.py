from sqlalchemy.orm import registry
import typing as t
from dataclasses import asdict, is_dataclass, dataclass
from typing import Any


@dataclass
class BaseModel:
    """Base model providing a to_dict method."""

    def to_dict(self) -> dict:
        """Convert the model instance to a dictionary."""
        def serialize(obj: Any) -> Any:
            """Helper function to serialize nested objects."""
            if is_dataclass(obj):
                return asdict(obj) # noqa
            if isinstance(obj, list):
                return [serialize(item) for item in obj]
            if isinstance(obj, dict):
                return {key: serialize(value) for key, value in obj.items()}
            return obj

        return serialize(self)


@dataclass
class BaseOrmMappedModel(BaseModel):
    """."""
    __tablename__: t.ClassVar = None
    __sa_dataclass_metadata_key__: t.ClassVar = "sa"
    REGISTRY: t.ClassVar = registry()

    def to_dict(self) -> dict:
        return {
            field.name: getattr(self, field.name)
            for field in self.__table__.c
        }
