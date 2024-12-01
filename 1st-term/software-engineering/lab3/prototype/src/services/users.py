import typing as t

import flask
import flask_login
from sqlalchemy.orm import Session
from werkzeug.security import generate_password_hash, check_password_hash
from models import User
from models.exception import UserValidationError
from models.user import UserLogin


class UsersService:
    """Service for managing users."""

    def __init__(self, session_acquirer: t.Callable[[], Session]):
        """Initialize the service with a session acquirer."""
        self._session_acquirer = session_acquirer

    def get_all(self) -> list[User]:
        """Retrieve all users."""
        session = self._session_acquirer()
        return session.query(User).all() # noqa

    def get_by_username(self, username: str) -> User | None:
        """Retrieve a user by username."""
        session = self._session_acquirer()
        return session.query(User).filter_by(username=username).one_or_none()

    def get_by_id(self, ident: int) -> User | None:
        """Retrieve a user by ID."""
        session = self._session_acquirer()
        return session.query(User).filter_by(id=ident).one_or_none()

    def add(self, username: str, password: str) -> User:
        """Add a new user with hashed password."""
        session = self._session_acquirer()
        try:
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
        except Exception as e:
            session.rollback()
            raise e

    def _validate(self, username: str, password: str) -> User:
        """Validate a user's credentials."""
        session = self._session_acquirer()
        user = session.query(User).filter_by(username=username).one_or_none()
        if not user:
            raise UserValidationError('несуществующий пользователь')

        if not check_password_hash(user.password, password):
            raise UserValidationError('пароль не подходит')

        return user # noqa

    def login_user(self, login_request: UserLogin) -> flask.Response:
        """Login user."""
        user = self._validate(
            username=login_request['username'],
            password=login_request['password'],
        )
        flask_login.login_user(user)
        return flask.redirect(flask.url_for('main.index'))
