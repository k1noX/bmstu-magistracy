import dataclasses as dc


@dc.dataclass
class PgConfig:
    """."""

    host: str = dc.field(default='postgres')
    port: int = dc.field(default=5432)
    user: str = dc.field(default='postgres')
    password: str = dc.field(default='postgres')
    database: str = dc.field(default='orders')


config = PgConfig()
