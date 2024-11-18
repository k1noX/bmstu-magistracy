from sqlalchemy.orm import registry
import typing as t
import dataclasses as dc


@dc.dataclass
class BaseOrmMappedModel:
    """."""
    __tablename__: t.ClassVar = None
    __sa_dataclass_metadata_key__: t.ClassVar = "sa"
    REGISTRY: t.ClassVar = registry()

    def to_dict(self):
        return {
            field.name: getattr(self, field.name)
            for field in self.__table__.c
        }
