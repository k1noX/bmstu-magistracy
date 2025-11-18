import sqlalchemy as sa
from sqlalchemy.orm import Session, sessionmaker
from sqlalchemy_utils import database_exists, create_database

from config import PgConfig, config
from models import BaseOrmMappedModel


class ConnectionsInjector:
    """."""

    def __init__(self, pg_config: PgConfig):
        self._config = pg_config
        self._pg: Session | None = None

    def acquire_session(self) -> Session:
        if not self._pg:
            self._init_db()

        return self._pg

    def _init_db(self):
        engine = sa.create_engine(
            sa.engine.URL.create(
                'postgresql+psycopg2',
                self._config.user,
                self._config.password,
                self._config.host,
                self._config.port,
                self._config.database
            ),
            query_cache_size=0
        )
        if not database_exists(engine.url):
            create_database(engine.url)

        with engine.connect() as conn:
            BaseOrmMappedModel.REGISTRY.metadata.create_all(bind=conn)
            conn.commit()

        session_fabric = sessionmaker(engine, expire_on_commit=False)
        self._pg = sa.orm.scoped_session(session_fabric)


connections_injector = ConnectionsInjector(config)
