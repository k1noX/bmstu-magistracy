import typing as t

from sqlalchemy.orm import Session
from werkzeug.security import generate_password_hash, check_password_hash

from models import User


class UsersService:
    """."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """."""

        self._session_acquirer = session_acquirer

    def get_all(self) -> list[User]:
        session = self._session_acquirer()
        return session.query(User).all()

    def get_by_username(self, username: str) -> User | None:
        session = self._session_acquirer()
        user = session.query(User).filter_by(
            username=username
        ).one_or_none()
        return user if user else None # noqa

    def get_by_id(self, ident: int) -> User | None:
        session = self._session_acquirer()
        user = session.query(User).filter_by(
            id=ident
        ).one_or_none()
        return user if user else None # noqa

    def add(self, username: str, password: str) -> User:
        session = self._session_acquirer()
        user = User(
            username=username,
            password=generate_password_hash(password),
            position='',
            fullname='',
        )
        session.add(user)
        session.commit()
        session.refresh(user)
        return user

    def validate(self, username: str, password: str) -> User | None:
        session = self._session_acquirer()
        user = session.query(User).filter_by(
            username=username,
        ).one_or_none()
        if not user:
            return None

        if check_password_hash(user.password, password):
            return user

        return None
